# generic data checks ----

test_that("When not data", {
  expect_error(ugcflss_fit_model(
    grouping_variable = "grp", sum_score = "likert_ss",
    minimum_item_response = 1, maximum_item_response = 5, number_items = 7,
    warmup = 250, sampling = 250, chains = 3, cores = 3
  ), paste(
    "Could not make the object passed as data into a data.frame.",
    "Please check this object to be sure it is a dataset.",
    sep = "\n"
  ))
})

test_that("When group variable is misnamed", {
  expect_error(ugcflss_fit_model(
    data = dt_correct,
    grouping_variable = "grp1", sum_score = "likert_ss",
    minimum_item_response = 1, maximum_item_response = 5, number_items = 7,
    warmup = 250, sampling = 250, chains = 3, cores = 3
  ), paste(
    "The `grouping_variable` is not among the variables in the dataset",
    sep = " "
  ))
})

test_that("When sum score is misnamed", {
  expect_error(ugcflss_fit_model(
    data = dt_correct,
    grouping_variable = "grp", sum_score = "likert_ss1",
    minimum_item_response = 1, maximum_item_response = 5, number_items = 7,
    warmup = 250, sampling = 250, chains = 3, cores = 3
  ), paste(
    "The `sum_score` is not among the variables in the dataset",
    sep = " "
  ))
})

test_that("When both variables are misnamed", {
  expect_error(ugcflss_fit_model(
    data = dt_correct,
    grouping_variable = "grp1", sum_score = "likert_ss1",
    minimum_item_response = 1, maximum_item_response = 5, number_items = 7,
    warmup = 250, sampling = 250, chains = 3, cores = 3
  ), paste(
    "Neither the `grouping_variable` nor the `sum_score` is among the",
    "variables in the dataset",
    sep = "\n"
  ))
})

# generic numeric checks ----

test_that("When sum scores contains non-numerics", {
  expect_error(ugcflss_fit_model(
    data = dt_correct,
    grouping_variable = "grp", sum_score = "likert_ss_invalid",
    minimum_item_response = 1, maximum_item_response = 5, number_items = 6,
    warmup = 250, sampling = 250, chains = 3, cores = 3
  ), paste(
    "Check that the `sum_score` variable contains whole numbers.",
    sep = " "
  ))
})

test_that("Missing minimum item", {
  expect_error(ugcflss_fit_model(
    data = dt_correct,
    grouping_variable = "grp", sum_score = "likert_ss",
    maximum_item_response = 5, number_items = 6,
    warmup = 250, sampling = 250, chains = 3, cores = 3
  ), paste(
    "`minimum_item_response` is not a whole number.",
    sep = " "
  ))
})

test_that("Text minimum item", {
  expect_error(ugcflss_fit_model(
    data = dt_correct,
    grouping_variable = "grp", sum_score = "likert_ss",
    minimum_item_response = "e", maximum_item_response = 5, number_items = 6,
    warmup = 250, sampling = 250, chains = 3, cores = 3
  ), paste(
    "`minimum_item_response` is not a whole number.",
    sep = " "
  ))
})

test_that("Missing maximum item", {
  expect_error(ugcflss_fit_model(
    data = dt_correct,
    grouping_variable = "grp", sum_score = "likert_ss",
    minimum_item_response = 1, number_items = 6,
    warmup = 250, sampling = 250, chains = 3, cores = 3
  ), paste(
    "`maximum_item_response` is not a whole number.",
    sep = " "
  ))
})

test_that("Text maximum item", {
  expect_error(ugcflss_fit_model(
    data = dt_correct,
    grouping_variable = "grp", sum_score = "likert_ss",
    minimum_item_response = 1, maximum_item_response = "r", number_items = 6,
    warmup = 250, sampling = 250, chains = 3, cores = 3
  ), paste(
    "`maximum_item_response` is not a whole number.",
    sep = " "
  ))
})

test_that("Missing number items", {
  expect_error(ugcflss_fit_model(
    data = dt_correct,
    grouping_variable = "grp", sum_score = "likert_ss",
    minimum_item_response = 1, maximum_item_response = 5,
    warmup = 250, sampling = 250, chains = 3, cores = 3
  ), paste(
    "`number_items` is not a positive whole number.",
    sep = " "
  ))
})

test_that("Text number items", {
  expect_error(ugcflss_fit_model(
    data = dt_correct,
    grouping_variable = "grp", sum_score = "likert_ss",
    minimum_item_response = 1, maximum_item_response = 5, number_items = "er",
    warmup = 250, sampling = 250, chains = 3, cores = 3
  ), paste(
    "`number_items` is not a positive whole number.",
    sep = " "
  ))
})

test_that("Non-positive number items", {
  n_i <- sample(-20:0, 1)
  expect_error(ugcflss_fit_model(
    data = dt_correct,
    grouping_variable = "grp", sum_score = "likert_ss",
    minimum_item_response = 1, maximum_item_response = 5, number_items = n_i,
    warmup = 250, sampling = 250, chains = 3, cores = 3
  ), paste(
    "`number_items` is not a positive whole number.",
    sep = " "
  ))
})

test_that("Minimum not less than maximum", {
  min <- 5
  max <- sample(seq_len(min - 1), 1)
  expect_error(ugcflss_fit_model(
    data = dt_correct,
    grouping_variable = "grp", sum_score = "likert_ss",
    minimum_item_response = min, maximum_item_response = max, number_items = 6,
    warmup = 250, sampling = 250, chains = 3, cores = 3
  ), paste(
    "`minimum_item_response` must be less than `maximum_item_response`.",
    sep = "\n"
  ))
})

test_that("When sum scores too low due to number items", {
  expect_error(ugcflss_fit_model(
    data = dt_correct,
    grouping_variable = "grp", sum_score = "likert_ss",
    minimum_item_response = 1, maximum_item_response = 5, number_items = 7,
    warmup = 250, sampling = 250, chains = 3, cores = 3
  ), paste(
    "The lowest value of the scale score was less than the theoretical",
    "minimum specified by the user based on the `minimum_item_response`",
    "and `number_items`. This should not happen.",
    sep = "\n"
  ))
})

test_that("When sum scores too high due to number items", {
  expect_error(ugcflss_fit_model(
    data = dt_correct,
    grouping_variable = "grp", sum_score = "likert_ss",
    minimum_item_response = 1, maximum_item_response = 5, number_items = 5,
    warmup = 250, sampling = 250, chains = 3, cores = 3
  ), paste(
    "The highest value of the scale score was greater than the theoretical",
    "maximum specified by the user based on the `maximum_item_response`",
    "and `number_items`. This should not happen.",
    sep = "\n"
  ))
})

test_that("When sum scores too high due to wrong maximum", {
  expect_error(ugcflss_fit_model(
    data = dt_correct,
    grouping_variable = "grp", sum_score = "likert_ss",
    minimum_item_response = 1, maximum_item_response = 4, number_items = 6,
    warmup = 250, sampling = 250, chains = 3, cores = 3
  ), paste(
    "The highest value of the scale score was greater than the theoretical",
    "maximum specified by the user based on the `maximum_item_response`",
    "and `number_items`. This should not happen.",
    sep = "\n"
  ))
})

# test_that("When more than 20 groups", {
#   expect_error(ugcflss_fit_model(
#     data = dt_more_20,
#     grouping_variable = "grp", sum_score = "likert_ss",
#     minimum_item_response = 1, maximum_item_response = 5, number_items = 6,
#     warmup = 250, sampling = 250, chains = 3, cores = 3
#   ), paste(
#     "If you have more than 20 groups to compare,",
#     "set `override_twenty_groups = TRUE` when calling",
#     "`ugcflss_fit_model()`. This is just to ensure you have not selected",
#     "the wrong grouping variable. There is nothing special about 20.",
#     sep = "\n"
#   ))
# })
