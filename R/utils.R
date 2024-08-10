#' Colour-blind palette
#' @return As above
#' @keywords internal
get_cb_pal <- function() {
  c(
    "#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
    "#0072B2", "#D55E00", "#CC79A7"
  )
}

#' Create Spline object
#' @return Return spline
#' @param user_input additional user input
#' @keywords internal
create_splines <- function(user_input) {
  n_cuts <- length(lss_points(user_input)) - 1
  # df = number of internal cuts + degree + intercept
  df <- min(15 + 2 + 1, n_cuts - 2)
  spl_mat <- -.5 + splines2::iSpline(
    seq_len(n_cuts),
    df = df, degree = 2, intercept = TRUE
  )
  return(spl_mat)
}

#' What data points to expect
#' @return As above
#' @keywords internal
lss_points <- function(user_input) {
  with(user_input, seq(min * n_items, max * n_items) / n_items)
}

#' Create Stan data list object
#' @return Data list object for Stan
#' @param y_ord numeric input
#' @param grp grouping variable
#' @param user_input additional user input
#' @import data.table
#' @keywords internal
create_stan_data <- function(y_ord, grp, user_input) {
  N <- NULL # nolint
  curr_points <- lss_points(user_input)
  curr_points_len <- length(curr_points)
  t_orig <- data.table::data.table(y_ord = y_ord, grp = grp)
  t_wide <- data.table::data.table(
    grp = rep.int(
      seq_len(user_input$n_grp),
      rep(curr_points_len, user_input$n_grp)
    ),
    y_ord = rep(seq_len(curr_points_len), user_input$n_grp)
  )
  t_wide <- merge(
    t_wide,
    t_orig[, .N, list(y_ord, grp)][order(grp, y_ord)], # nolint
    all.x = TRUE
  )
  t_wide[, N := ifelse(is.na(N), 0, N)]
  t_wide <- stats::reshape(
    t_wide,
    direction = "wide", timevar = "y_ord", idvar = "grp"
  )
  t_wide <- t_wide[, -1]
  spl_mat <- create_splines(user_input)
  return(list(
    n_grp = nrow(t_wide), n_cuts = ncol(t_wide) - 1,
    count_mat = t(t_wide),
    # count_arr = t(t_wide), # nolint
    n_gamma = ncol(spl_mat), spl_mat = spl_mat,
    y_ord = y_ord, grp = grp
  ))
}

#' User input checker
#' @returns Check user input
#' @inheritParams ugcflss_fit_model
#' @keywords internal
check_user_input <- function(
    data, grouping_variable, sum_score,
    minimum_item_response, maximum_item_response, number_items) {
  dt <- generic_data_checks(
    data = data, grouping_variable = grouping_variable,
    sum_score = sum_score
  )
  ss_i_min_shift <- generic_numeric_checks(
    dt = dt, minimum_item_response = minimum_item_response,
    maximum_item_response = maximum_item_response, number_items = number_items
  )

  return(list(dt = dt, ss_i_min_shift = ss_i_min_shift))
}

#' Generic data checks
#' @return Data
#' @inheritParams ugcflss_fit_model
#' @keywords internal
generic_data_checks <- function(data, grouping_variable, sum_score) {
  tryCatch(
    dt <- as.data.frame(data),
    error = function(e) {
      statement <- paste(
        "Could not make the object passed as data into a data.frame.",
        "Please check this object to be sure it is a dataset.",
        sep = "\n"
      )
      stop(statement)
    }
  )

  # variable names must be in data
  var_names <- colnames(dt)
  present_grouping_variable <- grouping_variable %in% var_names
  present_sum_score <- sum_score %in% var_names
  present_both <- present_grouping_variable + present_sum_score
  if (present_both < 2) {
    if (present_both == 0) {
      statement <- paste(
        "Neither the `grouping_variable` nor the `sum_score` is among the",
        "variables in the dataset",
        sep = "\n"
      )
    } else if (present_both == 1) {
      if (isFALSE(present_grouping_variable)) {
        statement <- paste(
          "The `grouping_variable` is not among the variables in the dataset",
          sep = " "
        )
      } else if (isFALSE(present_sum_score)) {
        statement <- paste(
          "The `sum_score` is not among the variables in the dataset",
          sep = " "
        )
      }
    }
    stop(statement)
  }

  tryCatch(
    dt <- stats::na.omit(dt[, c(grouping_variable, sum_score)]),
    error = function(e) {
      statement <- paste(
        "This is a strange one, for some reason, we cannot subset the",
        "dataset to the variables you provided even though both variables",
        "are in the dataset.",
        sep = "\n"
      )
      stop(statement)
    }
  )

  return(dt)
}

#' Generic numeric checks
#' @param dt Modified form of original data
#' @return The adjusted sum scores
#' @inheritParams ugcflss_fit_model
#' @keywords internal
generic_numeric_checks <- function(
    dt, minimum_item_response, maximum_item_response, number_items) {
  tryCatch(
    ss_i <- stats::na.omit(as.integer(dt[, 2])),
    warning = function(e) {
      statement <- paste(
        "Check that the `sum_score` variable contains whole numbers.",
        sep = " "
      )
      stop(statement)
    },
    error = function(e) {
      statement <- paste(
        "Check that the `sum_score` variable contains whole numbers.",
        sep = " "
      )
      stop(statement)
    }
  )

  if (length(ss_i) != nrow(dt)) {
    statement <- paste(
      "When converting the `sum_score` variable to whole numbers,",
      "some of the values resulted in missing data meaning that",
      "not all values were whole numbers.",
      sep = "\n"
    )
    stop(statement)
  }

  # minimum_item_response must be number
  if (is.na(minimum_item_response) || is.nan(minimum_item_response)) {
    statement <- paste(
      "Could not successfully convert the `minimum_item_response` to a",
      "whole number.",
      sep = "\n"
    )
    stop(statement)
  }
  # maximum_item_response must be number
  if (is.na(maximum_item_response) || is.nan(maximum_item_response)) {
    statement <- paste(
      "Could not successfully convert the `maximum_item_response` to a",
      "whole number.",
      sep = "\n"
    )
    stop(statement)
  }
  # number_items must be number
  if (is.na(number_items) || is.nan(number_items)) {
    statement <- paste(
      "Could not successfully convert the `number_items` to a",
      "whole number.",
      sep = "\n"
    )
    stop(statement)
  }

  minimum_scale_score <- minimum_item_response * number_items
  if (min(ss_i) < minimum_scale_score) {
    statement <- paste(
      "The lowest value of the scale score was less than the theoretical",
      "minimum specified by the user based on the `minimum_item_response`",
      "and `number_items`. This should not happen.",
      sep = "\n"
    )
    stop(statement)
  }
  maximum_scale_score <- maximum_item_response * number_items
  if (max(ss_i) > maximum_scale_score) {
    statement <- paste(
      "The lowest value of the scale score was greater than the theoretical",
      "maximum specified by the user based on the `maximum_item_response`",
      "and `number_items`. This should not happen.",
      sep = "\n"
    )
    stop(statement)
  }

  ss_i_min_shift <- ss_i - minimum_scale_score + 1

  return(ss_i_min_shift)
}
