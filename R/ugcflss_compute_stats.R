#' Compute some requested statistic
#'
#' @param res_obj Object returned by main function
#' @param which_groups One or more group id(s) (integer)
#' @param stat One of "pmf", "ccdf", "quantile", "mean", or "sd"
#' @param tau The probability for quantile calculations
#' @return Returns requested statistic.
#' @keywords internal
ugcflss_compute_stats <- function(
    res_obj, which_groups, stat = "pmf", tau = .5) {
  dl <- res_obj$stan_data_list

  n_by_group <- colSums(dl$count_mat)

  intercept <- as.data.frame(res_obj$model, "intercept")[, 1, drop = TRUE]
  phi_mat <- as.data.frame(res_obj$model, "phi")

  curr_points <- lss_points(res_obj$user_input)
  min_curr_points <- min(curr_points)

  simplex_times_n <- lapply(which_groups, function(group_id) {
    # filter to row of phi_matrix that has weights for current group
    idx_s <- which(
      as.integer(gsub("phi\\[\\d+,|\\]", "", colnames(phi_mat))) == group_id
    )
    ret <- t(sapply(seq_len(nrow(phi_mat)), function(i) {
      diff(c(
        0,
        # get spline product
        stats::plogis(
          dl$spl_mat %*% as.numeric(phi_mat[i, idx_s]) - intercept[i]
        ),
        1
      ))
    }))
    ret * n_by_group[group_id]
  })

  comb_pmf <- Reduce("+", simplex_times_n) / sum(n_by_group[which_groups])

  if (stat == "pmf") {
    res <- comb_pmf
  } else if (stat == "ccdf") {
    res <- t(1 - apply(comb_pmf, 1, cumsum))
  } else if (stat == "quantile") {
    res <- apply(comb_pmf, 1, function(x) {
      stats::approx(c(0, cumsum(x)), c(min_curr_points, curr_points), tau)$y
    })
  } else if (stat %in% c("mean", "sd")) {
    res <- apply(comb_pmf, 1, function(x) {
      ret <- sum(curr_points * x)
      if (stat == "sd") ret <- sqrt(sum(x * (curr_points - ret)^2))
      ret
    })
  }

  return(res)
}

#' Compute Probability of Superiority
#'
#' @param res_obj Object returned by main function
#' @param which_groups Two group ids
#' @return Returns PS.
#' @keywords internal
ugcflss_compute_ps <- function(res_obj, which_groups) {
  dl <- res_obj$stan_data_list

  intercept <- as.data.frame(res_obj$model, "intercept")[, 1, drop = TRUE]
  phi_mat <- as.data.frame(res_obj$model, "phi")

  idx_a <- which(as.integer(
    gsub("phi\\[\\d+,|\\]", "", colnames(phi_mat))
  ) == which_groups[1])
  idx_b <- which(as.integer(
    gsub("phi\\[\\d+,|\\]", "", colnames(phi_mat))
  ) == which_groups[2])

  ps_vec <- sapply(seq_len(nrow(phi_mat)), function(i) {
    pmf_a <- diff(c(
      0,
      stats::plogis(
        dl$spl_mat %*% as.numeric(phi_mat[i, idx_a]) - intercept[i]
      ),
      1
    ))
    pmf_b <- diff(c(
      0,
      stats::plogis(
        dl$spl_mat %*% as.numeric(phi_mat[i, idx_b]) - intercept[i]
      ),
      1
    ))
    ccdf_a <- 1 - cumsum(pmf_a)
    sum(pmf_b * ccdf_a + .5 * pmf_b * pmf_a)
  })

  return(ps_vec)
}
