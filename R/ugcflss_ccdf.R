#' Compute CCDF for entire sample
#'
#' @param res_obj Object returned by main function
#' @param interval Some quantile interval between 0 and 1
#' @param convergence If TRUE, returns CCDF convergence statistics.
#' If FALSE, creates dataset showing CCDF which can be used for plotting
#' the CCDF.
#' @import data.table
#' @return Returns dataset.
#' @export
ugcflss_exceed_all <- function(res_obj, interval = .89, convergence = FALSE) {
  if (!(interval > 0 && interval < 1)) {
    statement <- paste(
      "Interval must be a number between 0 and 1."
    )
    stop(statement)
  }

  loc <- count <- points <- NULL

  lower_lim <- (1 - interval) / 2

  curr_points <- lss_points(res_obj$user_input)
  curr_points_t <- paste0(
    "P(X>", formatC(curr_points, format = "f", digits = 2), ")"
  )

  ccdf_post <- as.data.frame(ugcflss_compute_stats(
    res_obj = res_obj, which_groups = seq_len(res_obj$user_input$n_grp),
    stat = "ccdf"
  ))
  colnames(ccdf_post) <- curr_points_t

  if (isTRUE(convergence)) {
    warmup <- res_obj$model@sim$warmup
    total_iter <- res_obj$model@sim$iter
    samples <- total_iter - warmup

    n_iter <- nrow(ccdf_post)
    ccdf_post$.chain <- (seq_len(n_iter) - 1) %/% samples + 1
    ccdf_post$.iteration <- (seq_len(n_iter) - 1) %% samples + 1
    ccdf_post$.draw <- seq_len(n_iter)

    ccdf_post_draws <- posterior::as_draws(ccdf_post)
    result <- posterior::summarise_draws(
      ccdf_post_draws,
      mean,
      sd = stats::sd,
      ~ posterior::quantile2(.x, probs = c(lower_lim, 1 - lower_lim)),
      posterior::default_convergence_measures()
    )
  } else {
    result <- data.table::as.data.table(t(apply(
      ccdf_post, 2, stats::quantile,
      probs = c(lower_lim, .5, 1 - lower_lim)
    )))
    setnames(result, c("lo", "md", "hi"))
    result[, points := curr_points]
    result[, loc := seq_len(.N)]
    count_by_level <- rowSums(res_obj$stan_data_list$count_mat)
    result[, count := sapply(loc, function(x) {
      weighter <- count_by_level
      stats::weighted.mean(seq_len(length(count_by_level)) > x, weighter)
    })]
  }

  return(result[])
}

#' Plot CCDF for entire sample
#'
#' @inheritParams ugcflss_exceed_all
#' @param show_intervals If TRUE, show intervals on plot,
#' if FALSE, do not show intervals
#' @return Returns plot
#' @export
ugcflss_exceed_all_plot <- function(
    res_obj, interval = .89, show_intervals = FALSE) {
  lo <- md <- hi <- points <- NULL

  user_input <- res_obj$user_input

  exc_dt <- ugcflss_exceed_all(
    res_obj = res_obj, interval = interval, convergence = FALSE
  )
  p_out <- ggplot2::ggplot(exc_dt, ggplot2::aes(points, md)) +
    ggplot2::geom_point(size = .5) +
    ggplot2::geom_line(linewidth = .25, group = 1) +
    ggplot2::scale_x_continuous(breaks = user_input$min:user_input$max) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(1), breaks = seq(0, 1, .2)
    ) +
    ggplot2::theme_bw() +
    ggplot2::coord_cartesian(
      xlim = c(user_input$min, user_input$max), ylim = c(0, 1)
    ) +
    ggplot2::labs(
      x = "Value on response scale", y = "Proportion of sample > Value"
    )
  if (show_intervals) {
    p_out <- p_out +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = lo, ymax = hi),
        alpha = .25, group = 1
      )
  }
  return(p_out)
}

#' Compute CCDF by group
#'
#' @inheritParams ugcflss_exceed_all
#' @import data.table
#' @return Returns dataset.
#' @export
ugcflss_exceed_group <- function(res_obj, interval = .89, convergence = FALSE) {
  if (!(interval > 0 && interval < 1)) {
    statement <- paste(
      "Interval must be a number between 0 and 1."
    )
    stop(statement)
  }

  loc <- count <- points <- gr <- gr_f <- NULL

  lower_lim <- (1 - interval) / 2

  curr_points <- lss_points(res_obj$user_input)
  curr_points_t <- paste0(
    "P(X>", formatC(curr_points, format = "f", digits = 2), ")"
  )

  ccdf_post_group <- lapply(seq_len(res_obj$user_input$n_grp), function(i) {
    tmp_d <- as.data.frame(ugcflss_compute_stats(
      res_obj = res_obj, which_groups = i,
      stat = "ccdf"
    ))
    colnames(tmp_d) <- curr_points_t
    return(tmp_d)
  })

  if (isTRUE(convergence)) {
    warmup <- res_obj$model@sim$warmup
    total_iter <- res_obj$model@sim$iter
    samples <- total_iter - warmup

    result <- lapply(ccdf_post_group, function(ccdf_post) {
      n_iter <- nrow(ccdf_post)
      ccdf_post$.chain <- (seq_len(n_iter) - 1) %/% samples + 1
      ccdf_post$.iteration <- (seq_len(n_iter) - 1) %% samples + 1
      ccdf_post$.draw <- seq_len(n_iter)

      ccdf_post_draws <- posterior::as_draws(ccdf_post)
      posterior::summarise_draws(
        ccdf_post_draws,
        mean,
        sd = stats::sd,
        ~ posterior::quantile2(.x, probs = c(lower_lim, 1 - lower_lim)),
        posterior::default_convergence_measures()
      )
    })
  } else {
    result <- data.table::rbindlist(
      lapply(ccdf_post_group, function(ccdf_post) {
        data.table::as.data.table(t(apply(
          ccdf_post, 2, stats::quantile,
          probs = c(lower_lim, .5, 1 - lower_lim)
        )))
      }),
      idcol = "gr"
    )
    setnames(result, 2:4, c("lo", "md", "hi"))
    result[, loc := seq_len(.N), gr]
    result[, points := curr_points[loc]]
    result[, count := 0]
    sapply(seq_len(res_obj$stan_data_list$n_grp), function(i) {
      y <- seq_len(nrow(res_obj$stan_data_list$count_mat))
      weighter <- res_obj$stan_data_list$count_mat[, i]
      result[gr == i, count := sapply(
        loc, function(x) stats::weighted.mean(y > x, weighter)
      )]
    })
    result[, gr_f := factor(
      gr, seq_len(res_obj$stan_data_list$n_grp), res_obj$user_input$grps
    )]
  }

  return(result[])
}

#' Plot CCDF by group
#'
#' @inheritParams ugcflss_exceed_all_plot
#' @return Returns plot
#' @export
ugcflss_exceed_group_plot <- function(
    res_obj, interval = .89, show_intervals = FALSE) {
  lo <- md <- hi <- points <- gr_f <- NULL

  user_input <- res_obj$user_input

  exc_dt <- ugcflss_exceed_group(
    res_obj = res_obj, interval = interval, convergence = FALSE
  )
  p_out <- ggplot2::ggplot(exc_dt, ggplot2::aes(
    points, md,
    group = gr_f, col = gr_f, linetype = gr_f
  )) +
    ggplot2::geom_point(size = .5) +
    ggplot2::geom_line(linewidth = .25) +
    ggplot2::scale_x_continuous(breaks = user_input$min:user_input$max) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(1), breaks = seq(0, 1, .2)
    ) +
    ggplot2::theme_bw() +
    ggplot2::scale_color_manual(values = get_cb_pal()) +
    ggplot2::coord_cartesian(
      xlim = c(user_input$min, user_input$max), ylim = c(0, 1)
    ) +
    ggplot2::labs(
      x = "Value on response scale", y = "Proportion of sample > Value",
      color = "", linetype = ""
    ) +
    ggplot2::theme(legend.position = "top")
  if (show_intervals) {
    p_out <- p_out +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = lo, ymax = hi), alpha = .25)
  }
  return(p_out)
}
