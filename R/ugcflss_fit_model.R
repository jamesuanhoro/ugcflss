#' Analyze a dataset
#'
#' @param data A dataset, ideally a data.frame.
#' @param grouping_variable The name of the grouping_variable in the dataset.
#' @param sum_score The name of the sum_score variable in the dataset. This
#' variable must be whole numbers.
#' @param minimum_item_response Theorical minimum value on items summed to
#' create sum score. For example, with Likert data with 5 response categories
#' coded 0,1,2,3,4, this argument would be `minimum_item_response = 0`.
#' @param maximum_item_response Theorical maximum value on items summed to
#' create sum score. For example, with Likert data with 3 response categories
#' coded 1,2,3, this argument would be `maximum_item_response = 3`.
#' @param number_items Number of items summed to create the sum score.
#' Must be at least 1.
#' @param override_twenty_groups By default, we assume your grouping_variable
#' does not have more than 20 levels. If you want to override this default,
#' add `override_twenty_groups = TRUE` to your function call.
#' @param show_messages (Logical) If TRUE, show messages from Stan sampler,
#' if FALSE, hide messages.
#' @param warmup Number of iterations used to warmup the sampler, per chain.
#' @param sampling Number of iterations retained for inference, per chain.
#' @param refresh (Positive whole number) How often to print the status of
#' the sampler.
#' @param adapt_delta Number in (0,1). Increase to resolve divergent
#' transitions.
#' @param max_treedepth (Positive whole number) Increase to resolve problems
#' with maximum tree depth.
#' @param chains Number of chains to use.
#' @param cores Number of cores to use.
#' @param seed Random seed.
#'
#' @return Objects containing analysis results.
#' @export
#'
#' @examples
#' \dontrun{
#' model_1 <- ugcflss_fit_model(
#'   data, grouping_variable, sum_score,
#'   minimum_item_response, maximum_item_response, number_items
#' )
#' }
ugcflss_fit_model <- function(
    data,
    grouping_variable = NA_character_,
    sum_score = NA_character_,
    minimum_item_response = NA_integer_,
    maximum_item_response = NA_integer_,
    number_items = NA_integer_,
    warmup = 750,
    sampling = 750,
    refresh = max((warmup + sampling) %/% 10, 1),
    adapt_delta = .9,
    max_treedepth = 10,
    chains = 3,
    cores = min(chains, max(parallel::detectCores() - 2, 1)),
    seed = sample.int(.Machine$integer.max, 1),
    override_twenty_groups = FALSE,
    show_messages = TRUE) {
  checked_dt <- check_user_input(
    data, grouping_variable, sum_score,
    minimum_item_response, maximum_item_response, number_items
  )

  dt <- checked_dt$dt
  grp_f <- factor(dt[, 1])
  grp_levs <- levels(grp_f)
  grp_i <- as.integer(grp_f)

  if (max(grp_i) > 20 && !isTRUE(override_twenty_groups)) {
    statement <- paste(
      "If you have more than 20 groups to compare,",
      "set `override_twenty_groups = TRUE` when calling",
      "`ugcflss_fit_model()`. This is just to ensure you have not selected",
      "the wrong grouping variable. There is nothing special about 20.",
      sep = "\n"
    )
    stop(statement)
  }

  user_input <- list(
    n_items = number_items, min = minimum_item_response,
    max = maximum_item_response, n_grp = max(grp_i),
    grps = grp_levs
  )

  dl <- create_stan_data(
    y_ord = checked_dt$ss_i_min_shift, grp = grp_i, user_input = user_input
  )

  is_positive_whole_number(refresh)
  is_positive_whole_number(max_treedepth)
  check_tau_interval(adapt_delta, "adapt_delta")

  model <- rstan::sampling(
    stanmodels$ugcflss,
    data = dl,
    iter = warmup + sampling, warmup = warmup, refresh = refresh,
    chains = chains, cores = cores,
    init = function() {
      list(
        intercept = 0, sigma_0 = 1, sigma_1 = 1, gamma = rep(.5, dl$n_gamma),
        ln_lambda = matrix(0, nrow = dl$n_gamma, ncol = dl$n_grp)
      )
    },
    seed = seed,
    control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth),
    show_messages = !isFALSE(show_messages)
  )

  result_object <- list(
    model = model, stan_data_list = dl, user_input = user_input
  )
  return(result_object)
}
