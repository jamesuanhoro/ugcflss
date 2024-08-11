dt_correct <- data.frame(grp = sample(LETTERS[1:6], 500, replace = TRUE))
dt_correct$likert_ss <- as.integer(cut(stats::rnorm(
  nrow(dt_correct), as.integer(factor(dt_correct$grp)), 10
), stats::qnorm(seq(0, 1, length.out = 26), 0, 10))) + 5
dt_correct$likert_ss_invalid <- dt_correct$likert_ss
dt_correct$likert_ss_invalid[1] <- "dfdgf"

dt_more_20 <- data.frame(grp = rep(LETTERS, length.out = 500))
dt_more_20$likert_ss <- as.integer(cut(stats::rnorm(
  nrow(dt_more_20), as.integer(factor(dt_more_20$grp)), 10
), stats::qnorm(seq(0, 1, length.out = 26), 0, 10))) + 5

n_warm <- 50
n_sampling <- 50
n_chains <- 3

expect_error(suppressWarnings(fit_mod <- ugcflss_fit_model(
  data = dt_correct,
  grouping_variable = "grp", sum_score = "likert_ss",
  minimum_item_response = 1, maximum_item_response = 5, number_items = 6,
  warmup = n_warm, sampling = n_sampling, chains = n_chains, cores = n_chains
)), NA)
