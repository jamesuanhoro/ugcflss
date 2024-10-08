#' Compute requested statistic for either entire sample or by group
#'
#' @param res_obj Object returned by main function
#' @param by_group If TRUE, compute by group.
#' If FALSE, compute for entire sample.
#' @param stat Statistic of interest, one of "median", "mean", "sd"
#' or "quantile".
#' @param interval Some quantile interval between 0 and 1
#' @param tau If stat = "quantile", the value of the probability between 0 and 1
#' @param return_draws If TRUE, do not summarize the draws.
#' If FALSE, summarize the draws.
#' @import data.table
#' @return Returns dataset.
#' @export
ugcflss_describe <- function(
    res_obj, by_group = FALSE, stat = "median", interval = .89, tau = .5,
    return_draws = FALSE) {
  if (!(stat %in% c("median", "mean", "sd", "quantile"))) {
    statement <- paste(
      "`stat` must be one of \"median\", \"mean\", \"sd\" or \"quantile\".",
      sep = " "
    )
    stop(statement)
  }

  check_tau_interval(interval, "interval")
  check_tau_interval(tau, "tau")

  stat_real <- stat
  if (stat == "median") {
    stat_real <- "quantile"
    tau <- .5
  }

  lower_lim <- (1 - interval) / 2 # nolint

  if (isTRUE(by_group)) {
    stat_post <- do.call(
      "cbind", lapply(seq_len(res_obj$user_input$n_grp), function(i) {
        tmp_d <- as.data.frame(ugcflss_compute_stats(
          res_obj = res_obj, which_groups = i,
          stat = stat_real, tau = tau
        ))
        colnames(tmp_d) <- paste0(stat, "[", res_obj$user_input$grps[i], "]")
        return(tmp_d)
      })
    )
  } else {
    stat_post <- as.data.frame(ugcflss_compute_stats(
      res_obj = res_obj, which_groups = seq_len(res_obj$user_input$n_grp),
      stat = stat_real, tau = tau
    ))
    colnames(stat_post) <- stat
  }

  warmup <- res_obj$model@sim$warmup
  total_iter <- res_obj$model@sim$iter
  samples <- total_iter - warmup

  n_iter <- nrow(stat_post)
  stat_post$.chain <- (seq_len(n_iter) - 1) %/% samples + 1
  stat_post$.iteration <- (seq_len(n_iter) - 1) %% samples + 1
  stat_post$.draw <- seq_len(n_iter)

  if (isTRUE(return_draws)) {
    return(stat_post)
  }

  stat_post_draws <- posterior::as_draws(stat_post)
  result <- posterior::summarise_draws(
    stat_post_draws,
    mean,
    sd = stats::sd,
    ~ posterior::quantile2(.x, probs = c(lower_lim, 1 - lower_lim)),
    posterior::default_convergence_measures()
  )

  return(result)
}

#' Plot requested statistic for entire sample and by group
#'
#' @inheritParams ugcflss_describe
#' @return Returns plot
#' @export
ugcflss_describe_plot <- function(
    res_obj, stat = "median", interval = .89, tau = .5) {
  stat_post_all <- ugcflss_describe(
    res_obj = res_obj, by_group = FALSE, stat = stat,
    interval = interval, tau = tau
  )
  stat_post_group <- ugcflss_describe(
    res_obj = res_obj, by_group = TRUE, stat = stat,
    interval = interval, tau = tau
  )
  stat_post <- rbind(stat_post_all, stat_post_group)
  setnames(stat_post, 4:5, c("lo", "hi"))

  stat_post$bar <- stat_post$mean[1]
  stat_post$group_i <- seq_len(nrow(stat_post))
  stat_post$group_t <- c("Overall", res_obj$user_input$grps)
  stat_post$model <- c(0, rep(1, res_obj$user_input$n_grp))

  group_t <- group_i <- lo <- mean <- hi <- bar <- NULL

  p_out <- ggplot2::ggplot(stat_post, ggplot2::aes(
    stats::reorder(group_t, -group_i), mean
  )) +
    ggplot2::geom_hline(
      ggplot2::aes(yintercept = bar),
      linetype = 2, linewidth = .25, alpha = .25
    ) +
    ggplot2::geom_point() +
    ggplot2::geom_linerange(ggplot2::aes(
      ymin = lo, ymax = hi
    ), alpha = .75, position = ggplot2::position_dodge(0)) +
    ggplot2::geom_label(
      ggplot2::aes(label = scales::number(mean, .01)),
      vjust = -.5, size = 3,
      label.padding = ggplot2::unit(0, "mm"), label.size = 0
    ) +
    ggplot2::geom_label(
      ggplot2::aes(label = scales::number(lo, .01), y = lo),
      vjust = 1.5, size = 2, hjust = 0,
      label.padding = ggplot2::unit(0, "mm"), label.size = 0
    ) +
    ggplot2::geom_label(
      ggplot2::aes(label = scales::number(hi, .01), y = hi),
      vjust = 1.5, size = 2, hjust = 1,
      label.padding = ggplot2::unit(0, "mm"), label.size = 0
    ) +
    ggplot2::scale_shape_manual(values = c(1, 4)) +
    ggplot2::theme_classic() +
    ggplot2::coord_flip() +
    ggplot2::facet_grid(model ~ ., space = "free", scales = "free") +
    ggplot2::labs(y = paste0(
      stat, " (w/ ", scales::percent(interval), " quantile interval)"
    )) +
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      legend.position = "top",
      panel.grid.major.x = ggplot2::element_line(linewidth = .25),
      strip.text = ggplot2::element_blank()
    )
  return(p_out)
}

#' Perform pairwise comparisons of requested statistic
#'
#' @inheritParams ugcflss_describe
#' @param stat Statistic of interest, one of "median", "mean", "sd",
#' "quantile" or "ps". ps = probability of superiority.
#' @param comparison One of difference, ratio or log-ratio. Log-ratio
#' will lead to an error if any of the values to be logged are negative
#' though this should not happen.
#' @return Returns dataset
#' @export
ugcflss_pairwise <- function(
    res_obj, stat = "median", interval = .89, tau = .5,
    comparison = "difference", return_draws = FALSE) {
  if (!comparison %in% c("difference", "ratio", "log-ratio")) {
    statement <- paste(
      "`comparison` must be one of \"difference\", \"ratio\" or \"log-ratio\""
    )
    stop(statement)
  }
  if (!(stat %in% c("median", "mean", "sd", "quantile", "ps"))) {
    statement <- paste(
      "`stat` must be one of \"median\", \"mean\", \"sd\", \"quantile\"",
      "or \"ps\".",
      sep = " "
    )
    stop(statement)
  }

  if (stat != "ps") {
    stat_post_group <- ugcflss_describe(
      res_obj = res_obj, by_group = TRUE, stat = stat,
      interval = interval, tau = tau, return_draws = TRUE
    )
    col_names <- colnames(stat_post_group)
  } else {
    col_names <- res_obj$user_input$grps
  }

  col_comparisons <- which(
    lower.tri(diag(res_obj$user_input$n_grp)),
    arr.ind = TRUE
  )

  result <- as.data.frame(apply(col_comparisons, 1, function(idxs) {
    if (stat == "ps") {
      ret <- ugcflss_compute_ps(res_obj = res_obj, which_groups = idxs)
    } else if (comparison == "difference") {
      ret <- stat_post_group[, idxs[1], drop = TRUE] -
        stat_post_group[, idxs[2], drop = TRUE]
    } else if (comparison == "ratio") {
      ret <- stat_post_group[, idxs[1], drop = TRUE] /
        stat_post_group[, idxs[2], drop = TRUE]
    } else if (comparison == "log-ratio") {
      ret <- log(stat_post_group[, idxs[1], drop = TRUE]) -
        log(stat_post_group[, idxs[2], drop = TRUE])
    }
    return(ret)
  }))

  var_a <- col_names[col_comparisons[, 1]]
  var_b <- col_names[col_comparisons[, 2]]

  if (stat == "ps") {
    new_colnames <- paste0("PS(", var_a, ">", var_b, ")")
  } else {
    new_colnames <- paste0(
      ifelse(grepl("log", comparison), "log(", ""),
      var_a,
      ifelse(grepl("diff", comparison), "-", "/"),
      var_b,
      ifelse(grepl("log", comparison), ")", "")
    )
  }
  colnames(result) <- new_colnames

  warmup <- res_obj$model@sim$warmup
  total_iter <- res_obj$model@sim$iter
  samples <- total_iter - warmup

  n_iter <- nrow(result)
  result$.chain <- (seq_len(n_iter) - 1) %/% samples + 1
  result$.iteration <- (seq_len(n_iter) - 1) %% samples + 1
  result$.draw <- seq_len(n_iter)

  if (isTRUE(return_draws)) {
    return(result)
  }

  null_value <- 0
  if (stat == "ps") {
    null_value <- .5
  } else if (comparison == "ratio") {
    null_value <- 1
  }

  p_direction <- sapply(seq_len(ncol(result) - 3), function(col_id) {
    p <- mean(result[, col_id] > null_value)
    p <- ifelse(p < .5, 1 - p, p)
    return(p)
  })

  check_tau_interval(interval, "interval")

  lower_lim <- (1 - interval) / 2 # nolint

  result <- posterior::as_draws(result)
  result <- posterior::summarise_draws(
    result,
    mean,
    sd = stats::sd,
    ~ posterior::quantile2(.x, probs = c(lower_lim, 1 - lower_lim)),
    posterior::default_convergence_measures()
  )
  result$group_1 <- res_obj$user_input$grps[col_comparisons[, 1]]
  result$group_2 <- res_obj$user_input$grps[col_comparisons[, 2]]
  result$group_1_i <- col_comparisons[, 1]
  result$group_2_i <- col_comparisons[, 2]
  result$p_direction <- p_direction

  return(result)
}

#' Perform pairwise comparisons of requested statistic
#'
#' @inheritParams ugcflss_pairwise
#' @return Returns dataset
#' @export
ugcflss_pairwise_plot <- function(
    res_obj, stat = "median", interval = .89, tau = .5,
    comparison = "difference") {
  pair_results <- ugcflss_pairwise(
    res_obj = res_obj, stat = stat, interval = interval,
    tau = tau, comparison = comparison, return_draws = FALSE
  )
  colnames(pair_results)[4:5] <- c("lo", "hi")

  penalty <- ifelse(stat == "ps", .5, ifelse(comparison == "ratio", 1, 0))
  pair_results$shade <-
    (pair_results$lo - penalty) * (pair_results$hi - penalty) > 0

  pair_results$pd_text <- paste0(
    ", ", scales::percent(pair_results$p_direction, 1)
  )
  if (stat == "ps") {
    pair_results$disp <- scales::percent(pair_results$mean, 1)
    pair_results$disp_lo <- scales::percent(pair_results$lo, 1)
    pair_results$disp_hi <- scales::percent(pair_results$hi, 1)
    pair_results$pd_text <- ""
  } else {
    pair_results$disp <- scales::number(pair_results$mean, .01)
    pair_results$disp_lo <- scales::number(pair_results$lo, .01)
    pair_results$disp_hi <- scales::number(pair_results$hi, .01)
  }

  group_1 <- group_1_i <- group_2 <- group_2_i <-
    disp <- disp_lo <- disp_hi <- shade <-
    pd_text <- p_direction <- NULL

  p_out <- ggplot2::ggplot(
    pair_results,
    ggplot2::aes(
      stats::reorder(group_2, group_2_i),
      stats::reorder(group_1, -group_1_i)
    )
  ) +
    ggplot2::geom_tile(
      ggplot2::aes(fill = factor(shade)),
      col = 1, size = .05, alpha = 1
    ) +
    ggplot2::geom_text(ggplot2::aes(
      label = paste0(
        disp, pd_text,
        "\n[", disp_lo, ", ", disp_hi, "]"
      ),
      alpha = p_direction - .5
    )) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      strip.background = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(0, 0, 0, 0)
    ) +
    ggplot2::scale_fill_manual(values = c("white", "#d3d3d3")) +
    ggplot2::scale_alpha(range = c(.5, 1)) +
    ggplot2::guides(alpha = "none", fill = "none")

  return(p_out)
}
