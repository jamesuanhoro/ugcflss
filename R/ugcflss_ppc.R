#' Compute posterior predictive distribution
#'
#' @inheritParams ugcflss_describe
#' @param ppd_samples Number of iterations to use to obtain
#' the posterior predictive distribution.
#' @return Returns dataset
#' @export
ugcflss_ppd <- function(res_obj, ppd_samples = 200) {
  if (is.na(ppd_samples) || is.nan(ppd_samples) || ppd_samples <= 0) {
    statement <- paste(
      "`ppd_samples` must be a number greater than 0."
    )
    stop(statement)
  }

  n_by_group <- colSums(res_obj$stan_data_list$count_mat)

  curr_points <- lss_points(res_obj$user_input)

  ppd_list <- lapply(seq_len(res_obj$user_input$n_grp), function(j) {
    n_group <- n_by_group[j]
    pmf <- ugcflss_compute_stats(
      res_obj = res_obj, which_groups = j, stat = "pmf"
    )
    iter_count <- min(ppd_samples, nrow(pmf))
    iter_seq <- sample(seq_len(nrow(pmf)), size = iter_count)
    y_mat <- matrix(nrow = iter_count, ncol = n_group)
    pos <- 1
    for (i in iter_seq) {
      y_mat[pos, ] <- sample(
        x = curr_points, size = n_group, prob = pmf[i, ], replace = TRUE
      )
      pos <- pos + 1
    }
    return(y_mat)
  })

  ppd_mat <- matrix(nrow = nrow(ppd_list[[1]]), ncol = sum(n_by_group))

  for (i in seq_len(res_obj$user_input$n_grp)) {
    ppd_mat[, res_obj$stan_data_list$grp == i] <- ppd_list[[i]]
  }
  colnames(ppd_mat) <- paste0("y_sim[", seq_len(sum(n_by_group)), "]")

  return(ppd_mat)
}

#' Basic PPD density overlay plot
#'
#' @inheritParams ugcflss_ppd
#' @param by_group If TRUE, plot posterior predictive distribution by group.
#' If FALSE, plot for entire sample.
#' @return Returns density overlay plot
#' @export
ugcflss_ppd_plot <- function(res_obj, ppd_samples = 200, by_group = FALSE) {
  ppd_mat <- ugcflss_ppd(res_obj = res_obj, ppd_samples = ppd_samples)

  y <- (
    res_obj$stan_data_list$y_ord + res_obj$user_input$n_items - 1
  ) / res_obj$user_input$n_items

  if (isTRUE(by_group)) {
    plt <- bayesplot::ppc_dens_overlay_grouped(
      y = y,
      yrep = ppd_mat,
      group = res_obj$user_input$grps[res_obj$stan_data_list$grp]
    ) +
      ggplot2::facet_wrap(~group, scales = "free_y")
  } else {
    plt <- bayesplot::ppc_dens_overlay(
      y = y,
      yrep = ppd_mat
    )
  }

  return(plt)
}
