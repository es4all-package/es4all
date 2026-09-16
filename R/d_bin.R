#' Dominance effect sizes for binary variables
#'
#' Calculate dominance-based effect sizes for binary variables.
#'
#' Calculate the following:
#'
#' * risk difference, which, for binary variables, is a special case of stochastic difference (aka Glass rank-biserial correlation);
#' * adjusted stochastic difference (aka Goodman and Kruskal's gamma);
#' * number needed to treat;
#' * dominance (aka common language effect size);
#' * measure of stochastic superiority (aka Vargha and Delaney's A);
#' * adjusted dominance;
#' * odds ratio, which, for binary variables, is a special case of generalized odds ratio; and
#' * win odds.
#'
#' `x` can contain either known parameter values, summary statistics, or microdata.
#'
#' * If `x` contains known parameter values or summary statistics, it should be
#' a `data.frame` with 2 rows. It can optionally contain a variable called `group`,
#' indicating the name of the group. In addition, it must contain one of the following:
#'
#'    * event probability (`p`), which is a known parameter; or
#'    * the number of events (`k`) (also called "successes") and the number of experiments or trials (`n`); or
#'    * proportion of events (`prop`) and `n`; or
#'    * `prop` and its standard error (`se`).
#'
#' * If `x` contains microdata, it should be a named `list` with 2 elements, each
#' being a vector of `logical` data.
#'
#' Optionally, `x` can have an attribute called `"meta"`, which must be a named list.
#'
#' @param x data. See details.
#' @param meta either `NULL` or a named list.
#'
#' @return Estimates with verbose explanations.
#'
#' @export
#' @family es
#'
#' @examples
#' # Number of events and number of trials
#' d_bin(comparison_data$UTI)
#' d_bin(comparison_data$DVT)
#' d_bin(comparison_data$MI)
#'
#' # Proportion estimates and their standard errors (SE)
#' d_bin(comparison_data$drug.use)
#'
#' # Known parameter values (based on the UTI example)
#' d_bin(data.frame(p = c(11/13, 2/8)))
d_bin = function(x, meta = NULL) {
  ## Check arguments
  tab_data = list(input = deparse1(substitute(x)))
  fo = .check_args(x = x, meta = meta)
  x = fo$x
  meta = fo$meta
  data_type = fo$data_type

  ## Clean data
  if (data_type == "micro") {
    newx = data.frame(group = names(x))
    newx[,c("k", "n")] = NA
    for (ii in 1:2) {
      assert_that(is.logical(x[[ii]]))
      newx$k[ii] = sum(x[[ii]] == TRUE)
      newx$n[ii] = length(x[[ii]])
    }
    x = newx
  }
  tab_data$group_input = x$group

  x_type = if(setequal(names(x), c("group", "k", "n"))) {
    assert_that(all(x$k >= 0), all(x$k <= x$n), all(x$n >= 1), all(x$n < Inf))
    x$prop = x$k / x$n

    tab_data$title = "Proportion (# Events / # Trials)"
    tab_data$value = glue("the proportion of {x$group} who had the event was \\
                          {x$prop %>% .pp_p1} ({x$k}/{x$n})") %>% paste0(collapse = "; ")
    tab_data$sh = glue("$k_{1:nrow(x)} = {x$k}$, $n_{1:nrow(x)} = {x$n}$") %>% paste0(collapse = "; ")
    "stat"
  } else if(setequal(names(x), c("group", "prop", "n"))) {
    assert_that(all(x$prop >= 0), all(x$prop <= 1), all(x$n >= 1))
    x$k = x$n * x$prop

    tab_data$title = "Proportion (n)"
    tab_data$value = glue("the proportion of {x$group} who had the event was \\
                          {x$prop} (n = {x$n})") %>% paste0(collapse = "; ")
    tab_data$sh = glue("$v_{1:nrow(x)} = {x$prop}$, $n_{1:nrow(x)} = {x$n}$") %>% paste0(collapse = "; ")
    "stat"
  } else if(setequal(names(x), c("group", "prop", "se"))) {
    assert_that(all(x$prop >= 0), all(x$prop <= 1), all(x$se > 0))
    x$n = round( x$prop * (1 - x$prop) / (x$se ^ 2) )
    x$k = x$n * x$prop

    tab_data$title = "Proportion (SE)"
    tab_data$value = glue("the proportion of {x$group} who had the event was \\
                          {x$prop} (SE = {x$se})") %>% paste0(collapse = "; ")
    tab_data$sh = glue("$v_{1:nrow(x)} = {x$prop}$, $s_{1:nrow(x)} = {x$se}$") %>% paste0(collapse = "; ")
    "stat"
  } else if(setequal(names(x), c("group", "p"))) {
    assert_that(all(x$p >= 0), all(x$p <= 1))
    x$prop = x$p

    tab_data$title = "Probability"
    tab_data$value = glue("the probability of {x$group} having the event was {x$p}") %>% paste0(collapse = "; ")
    tab_data$sh = glue("$\\pi_{1:nrow(x)} = {x$p}$") %>% paste0(collapse = "; ")
    "param"
  } else {
    stop("`x` contains unknown statistics.")
  }

  ## Calculate
  assert_that(all(x$prop >= 0), all(x$prop <= 1))
  ret_pe = .cp2es(gt = x$prop[1] * (1 - x$prop[2])
                  , lt = (1 - x$prop[1]) * x$prop[2]
                  , bool.cont = FALSE, bool.bin = TRUE, bool.flip = TRUE)
  if (ret_pe$flip) x = x[2:1,]
  tab_data$group = x$group

  ## CI
  ret = if (x_type == "stat") {
    assert_that(all(x$n >= 1), all(x$n < Inf))

    ndraws_D = .get_ndraws_D()
    .my_set.seed()
    p1 = rbinom(n = ndraws_D, size = x$n[1], prob = x$prop[1]) / x$n[1]
    p2 = rbinom(n = ndraws_D, size = x$n[2], prob = x$prop[2]) / x$n[2]
    assert_that(all(p1 >= 0), all(p1 <= 1), all(p2 >= 0), all(p2 <= 1))

    out_dist = .cp2es(gt = p1 * (1 - p2)
                      , lt = (1 - p1) * p2
                      , bool.cont = FALSE, bool.bin = TRUE, bool.flip = FALSE)
    .calc_ci(out_dist$es, ret_pe)
  } else {
    .pe2ret(ret_pe)
  }

  ret %>% .finalize_tab(tab_data = tab_data, meta = meta, func = "d_bin")
}
