n_warm <- 50
n_sampling <- 50
n_chains <- 3

expect_error(suppressWarnings(fit_mod <- ugcflss_fit_model(
  data = dt_correct,
  grouping_variable = "grp", sum_score = "likert_ss",
  minimum_item_response = 1, maximum_item_response = 5, number_items = 6,
  warmup = n_warm, sampling = n_sampling, chains = n_chains, cores = n_chains
)), NA)

# interval and tau failure tests ----

test_that("Fail on wrong interval and tau", {
  interval_fail_message <- paste(
    "interval", "is not a number between 0 and 1.",
    sep = " "
  )
  tau_fail_message <- paste(
    "tau", "is not a number between 0 and 1.",
    sep = " "
  )

  test_tau <- sample(c(runif(1, -500, 0), runif(1, 1, 500)), 1)
  test_interval <- sample(c(runif(1, -500, 0), runif(1, 1, 500)), 1)

  # tau tests ----
  expect_error(
    ugcflss_describe(fit_mod, tau = test_tau),
    tau_fail_message
  )
  expect_error(
    ugcflss_describe_plot(fit_mod, tau = test_tau),
    tau_fail_message
  )
  expect_error(
    ugcflss_pairwise(fit_mod, tau = test_tau),
    tau_fail_message
  )
  expect_error(
    ugcflss_pairwise_plot(fit_mod, tau = test_tau),
    tau_fail_message
  )

  # interval tests ----
  expect_error(
    ugcflss_describe(fit_mod, interval = test_interval),
    interval_fail_message
  )
  expect_error(
    ugcflss_describe_plot(fit_mod, interval = test_interval),
    interval_fail_message
  )
  expect_error(
    ugcflss_pairwise(fit_mod, interval = test_interval),
    interval_fail_message
  )
  expect_error(
    ugcflss_pairwise_plot(fit_mod, interval = test_interval),
    interval_fail_message
  )
  expect_error(
    ugcflss_exceed_all(fit_mod, interval = test_interval),
    interval_fail_message
  )
  expect_error(
    ugcflss_exceed_all_plot(fit_mod, interval = test_interval),
    interval_fail_message
  )
  expect_error(
    ugcflss_exceed_group(fit_mod, interval = test_interval),
    interval_fail_message
  )
  expect_error(
    ugcflss_exceed_group_plot(fit_mod, interval = test_interval),
    interval_fail_message
  )
})

# CCDF matrix tests ----

test_that("CCDF for all and by group returns expected classes", {
  convergence_s <- list(TRUE, FALSE, "4")

  lapply(convergence_s, function(conv) {
    rand_interval <- runif(1, .2, .95)
    if (isTRUE(conv)) {
      suppressWarnings(output <- ugcflss_exceed_all(
        fit_mod,
        interval = rand_interval, convergence = conv
      ))
      suppressWarnings(output_g <- ugcflss_exceed_group(
        fit_mod,
        interval = rand_interval, convergence = conv
      ))
      expect_true(any(class(output) == "draws_summary"))
      expect_true(all(unlist(
        lapply(output_g, function(out) any(class(out) == "draws_summary"))
      )))
    } else {
      output <- ugcflss_exceed_all(
        fit_mod,
        interval = rand_interval, convergence = conv
      )
      output_g <- ugcflss_exceed_group(
        fit_mod,
        interval = rand_interval, convergence = conv
      )
      expect_true(any(class(output) == "data.table"))
      expect_true(any(class(output_g) == "data.table"))
    }
  })
})

test_that("CCDF plot for all and by group returns ggplot", {
  rand_interval <- runif(1, .2, .95)
  output <- ugcflss_exceed_all_plot(
    fit_mod,
    interval = rand_interval, show_intervals = sample(c(TRUE, FALSE), 1)
  )
  output_g <- ugcflss_exceed_group_plot(
    fit_mod,
    interval = rand_interval, show_intervals = sample(c(TRUE, FALSE), 1)
  )
  expect_true(any(class(output) == "ggplot"))
  expect_true(any(class(output_g) == "ggplot"))
})

# stat type failures ----

test_that("Fail on invalid stat", {
  rand_stat <- function() {
    rand_len <- 1 + stats::rpois(1, 3)
    ret <- paste0(sample(letters, rand_len), collapse = "")
    while (ret %in% c("median", "mean", "sd", "quantile", "ps")) {
      ret <- paste0(sample(letters, rand_len), collapse = "")
    }
    return(ret)
  }
  stat_fail_message_1 <- paste(
    "`stat` must be one of \"median\", \"mean\", \"sd\" or \"quantile\".",
    sep = " "
  )
  stat_fail_message_2 <- paste(
    "`stat` must be one of \"median\", \"mean\", \"sd\", \"quantile\"",
    "or \"ps\".",
    sep = " "
  )

  expect_error(
    ugcflss_describe(fit_mod, stat = rand_stat()),
    stat_fail_message_1
  )
  expect_error(
    ugcflss_describe_plot(fit_mod, stat = rand_stat()),
    stat_fail_message_1
  )
  expect_error(
    ugcflss_pairwise(fit_mod, stat = rand_stat()),
    stat_fail_message_2
  )
  expect_error(
    ugcflss_pairwise_plot(fit_mod, stat = rand_stat()),
    stat_fail_message_2
  )
})

# stat successes ----

test_that("Describe successes", {
  grid <- expand.grid(
    stat = c("median", "mean", "sd", "quantile"),
    by_group = list(TRUE, FALSE, sample(letters, 1)),
    return_draws = list(TRUE, FALSE, sample(letters, 1))
  )

  apply(grid, 1, function(condition) {
    stat <- as.character(condition$stat)
    by_group <- condition$by_group
    return_draws <- condition$return_draws

    rand_interval <- runif(1, .2, .95)
    rand_tau <- runif(1, .2, .95)

    if (isTRUE(return_draws)) {
      # should be data.frame not draws_summary
      output <- ugcflss_describe(
        fit_mod,
        by_group = by_group, stat = stat,
        interval = rand_interval, tau = rand_tau, return_draws = return_draws
      )
      expect_true(all(c(
        any(class(output) == "data.frame"),
        !any(class(output) == "draws_summary")
      )))
      expect_true(nrow(output) == n_sampling * n_chains)
      if (isTRUE(by_group)) {
        expect_true(ncol(output) == (fit_mod$stan_data_list$n_grp + 3))
      } else {
        expect_true(ncol(output) == 4)
      }
      col_names <- colnames(output)
      col_names <- col_names[seq_len(ncol(output) - 3)]
      expect_true(all(grepl(stat, col_names)))
    } else {
      # should be draws_summary
      suppressWarnings(output <- ugcflss_describe(
        fit_mod,
        by_group = by_group, stat = stat,
        interval = rand_interval, tau = rand_tau, return_draws = return_draws
      ))
      expect_true(any(class(output) == "draws_summary"))
      if (isTRUE(by_group)) {
        expect_true(nrow(output) == fit_mod$stan_data_list$n_grp)
      } else {
        expect_true(nrow(output) == 1)
      }
    }
  })
})

# stat-plot successes ----

test_that("Describe plot successes", {
  grid <- expand.grid(
    stat = c("median", "mean", "sd", "quantile")
  )

  apply(grid, 1, function(condition) {
    stat <- as.character(condition["stat"])

    rand_interval <- runif(1, .2, .95)
    rand_tau <- runif(1, .2, .95)

    suppressWarnings(output <- ugcflss_describe_plot(
      fit_mod,
      stat = stat, interval = rand_interval, tau = rand_tau
    ))
    expect_true(any(class(output) == "ggplot"))
  })
})

# comparison type failures ----

test_that("Fail on invalid comparison", {
  rand_compare <- function() {
    rand_len <- 6 + stats::rpois(1, 6)
    ret <- paste0(sample(letters, rand_len), collapse = "")
    while (ret %in% c("difference", "ratio", "log-ratio")) {
      ret <- paste0(sample(letters, rand_len), collapse = "")
    }
    return(ret)
  }
  compare_fail_msg <- paste0(
    "`comparison` must be one of \"difference\", \"ratio\" or \"log-ratio\""
  )

  expect_error(
    ugcflss_pairwise(fit_mod, comparison = rand_compare()),
    compare_fail_msg
  )
})

# pairwise tests ----

test_that("Pairwise success", {
  grid <- expand.grid(
    stat = c("median", "mean", "sd", "quantile", "ps"),
    comparison = c("difference", "ratio", "log-ratio"),
    return_draws = list(TRUE, FALSE, sample(letters, 1))
  )

  apply(grid, 1, function(condition) {
    stat <- as.character(condition$stat)
    comparison <- as.character(condition$comparison)
    return_draws <- condition$return_draws

    rand_interval <- runif(1, .2, .95)
    rand_tau <- runif(1, .2, .95)

    n_grp <- fit_mod$stan_data_list$n_grp
    n_compare <- (n_grp * (n_grp - 1)) %/% 2

    if (isTRUE(return_draws)) {
      # should be data.frame not draws_summary
      output <- ugcflss_pairwise(
        fit_mod,
        stat = stat, interval = rand_interval, tau = rand_tau,
        comparison = comparison, return_draws = return_draws
      )
      expect_true(all(c(
        any(class(output) == "data.frame"),
        !any(class(output) == "draws_summary")
      )))
      expect_true(nrow(output) == n_sampling * n_chains)
      expect_true(ncol(output) == (n_compare + 3))
      col_names <- colnames(output)
      col_names <- col_names[seq_len(ncol(output) - 3)]
      if (stat != "ps") expect_true(all(grepl(stat, col_names)))
    } else {
      # should be draws_summary
      suppressWarnings(output <- ugcflss_pairwise(
        fit_mod,
        stat = stat, interval = rand_interval, tau = rand_tau,
        comparison = comparison, return_draws = return_draws
      ))
      expect_true(any(class(output) == "draws_summary"))
      expect_true(nrow(output) == n_compare)
    }
  })
})

# pairwise-plot tests ----

test_that("Pairwise plot success", {
  grid <- expand.grid(
    stat = c("median", "mean", "sd", "quantile", "ps"),
    comparison = c("difference", "ratio", "log-ratio")
  )

  apply(grid, 1, function(condition) {
    stat <- as.character(condition["stat"])
    comparison <- as.character(condition["comparison"])

    rand_interval <- runif(1, .2, .95)
    rand_tau <- runif(1, .2, .95)

    suppressWarnings(output <- ugcflss_pairwise_plot(
      fit_mod,
      stat = stat, interval = rand_interval, tau = rand_tau,
      comparison = comparison
    ))
    expect_true(any(class(output) == "ggplot"))
  })
})

# ppd failure tests ----

test_that("PPD fail on non-number or less than 0", {
  fail_samples <- list(letters[4], 0, -5)

  lapply(fail_samples, function(fail_candidate) {
    expect_error(
      ugcflss_ppd(fit_mod, ppd_samples = fail_candidate),
      paste(
        "`ppd_samples` is not a positive whole number.",
        sep = " "
      )
    )
    expect_error(
      ugcflss_ppd_plot(
        fit_mod,
        ppd_samples = fail_candidate, by_group = sample(c(TRUE, FALSE), 1)
      ),
      paste(
        "`ppd_samples` is not a positive whole number.",
        sep = " "
      )
    )
  })
})

# ppd success tests ----

test_that("PPD succeeds", {
  ppd_samples <- sample(50:5e3, 1)

  output <- ugcflss_ppd(fit_mod, ppd_samples = ppd_samples)
  expect_true(ncol(output) == length(fit_mod$stan_data_list$grp))
  expect_true(nrow(output) == min(c(ppd_samples, n_sampling * n_chains)))
})

# TODO: ppd plotting
