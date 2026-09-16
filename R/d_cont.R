#' Dominance effect sizes for continuous variables
#'
#' Calculate dominance-based effect sizes for continuous (normally distributed)
#' variables.
#'
#' Calculate the following:
#'
#' * dominance (aka common language effect size);
#' * stochastic difference (aka Glass rank-biserial correlation);
#' * odds, which, for continuous variables, is a special case of generalized odds ratio; and
#' * standardized mean difference (aka Cohen's d).
#'
#' For continuous variables, the following effect sizes simplify to one of the
#' above-mentioned effect sizes. Thus, they are not reported separately.
#'
#' * measure of stochastic superiority (aka Vargha and Delaney's A), which simplifies to dominance;
#' * adjusted dominance, which simplifies to dominance;
#' * adjusted stochastic difference (aka Goodman and Kruskal's gamma), which simplifies to stochastic difference; and
#' * win odds, which simplifies to odds.
#'
#' `x` can contain either known parameter values, summary statistics, or microdata.
#'
#' * If `x` contains known parameter values or summary statistics, it should be
#' a `data.frame` with 2 rows. It can optionally contain a variable called `group`,
#' indicating the name of the group. In addition, it must contain one of the following:
#'
#'    * expected value (`mu`) and standard deviation (`sigma`), which are known parameters; or
#'    * sample mean (`xbar`), sample standard deviation (`sd`), and sample size (`n`); or
#'    * `xbar`, standard error of the mean (`sem`), and `n`.
#'
#'    `sd` is the "sample" standard deviation, with denominator `n - 1`. This is
#'    what is returned by the `sd()` function.
#'
#' * If `x` contains microdata, it should be a named `list` with 2 elements, each
#' being a vector of `numeric` data.
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
#' # Known parameter values
#' d_cont(comparison_data$stature1)
#'
#' # Average, standard deviation (SD), and sample size
#' d_cont(comparison_data$stature2)
#' d_cont(comparison_data$Vitamin.D)
#' d_cont(comparison_data$cholesterol)
#'
#' # Average, standard error of the mean (SEM), and sample size
#' d_cont(comparison_data$serotonin)
d_cont = function(x, meta = NULL) {
  ## Check arguments
  tab_data = list(input = deparse1(substitute(x)))
  fo = .check_args(x = x, meta = meta)
  x = fo$x
  meta = fo$meta
  data_type = fo$data_type

  ## Clean data
  if (data_type == "micro") {
    newx = data.frame(group = names(x))
    newx[,c("xbar", "sd", "n")] = NA
    for (ii in 1:2) {
      assert_that(is.numeric(x[[ii]]))
      newx$xbar[ii] = mean(x[[ii]])
      newx$sd[ii] = sd(x[[ii]])
      newx$n[ii] = length(x[[ii]])
    }
    x = newx
  }
  tab_data$group_input = x$group

  x_type = if(setequal(names(x), c("group", "mu", "sigma"))) {
    tab_data$title = "Expected value (SD)"
    tab_data$value = glue("the expected value of the [variable] for {x$group} was {x$mu} (SD = {x$sigma})") %>% paste0(collapse = "; ")
    tab_data$sh = glue("$\\mu_{1:nrow(x)} = {x$mu}$, $\\sigma_{1:nrow(x)} = {x$sigma}$") %>% paste0(collapse = "; ")

    x$xbar = x$mu
    x$sd = x$sigma
    "param"
  } else if(setequal(names(x), c("group", "xbar", "sd", "n"))) {
    tab_data$title = "Average (SD)"
    tab_data$value = glue("the average [variable] for {x$group} was {x$xbar} (SD = {x$sd})") %>% paste0(collapse = "; ")
    tab_data$sh = glue("$\\bar{{x}}_{1:nrow(x)} = {x$xbar}$, $s_{1:nrow(x)} = {x$sd}$, $n_{1:nrow(x)} = {x$n}$") %>% paste0(collapse = "; ")

    assert_that(all(x$n > 1))
    x$sd = x$sd * sqrt( (x$n - 1) / x$n ) # MLE
    "stat"
  } else if(setequal(names(x), c("group", "xbar", "sem", "n"))) {
    tab_data$title = "Average (SEM)"
    tab_data$value = glue("the average [variable] for {x$group} was {x$xbar} (SEM = {x$sem})") %>% paste0(collapse = "; ")
    tab_data$sh = glue("$\\bar{{x}}_{1:nrow(x)} = {x$xbar}$, $s_{{\\bar{{x}},{1:nrow(x)}}} = {x$sem}$, $n_{1:nrow(x)} = {x$n}$") %>% paste0(collapse = "; ")

    assert_that(all(x$sem >= 0), all(x$n >= 0))
    x$sd = x$sem * sqrt(x$n)
    "stat"
  } else {
    stop("`x` contains unknown statistics.")
  }

  ## Calculate
  den1 = sum(x$sd^2)
  assert_that(all(x$sd >= 0), den1 > 0)
  val1 = (x$xbar[1] - x$xbar[2]) / sqrt(den1)
  gt = pnorm(val1)
  ret_pe = .cp2es(gt = gt, lt = 1 - gt, bool.cont = TRUE, bool.bin = FALSE, bool.flip = TRUE)
  if (ret_pe$flip) x = x[2:1,]
  tab_data$group = x$group

  ## CI
  ret = if (x_type == "stat") {
    assert_that(all(x$n >= 1), all(x$n < Inf))

    ndraws_D = .get_ndraws_D()
    .my_set.seed()
    nd1 = .norm_dist(xbar1 = x$xbar[1], sd1 = x$sd[1], n1 = x$n[1], ndraws = ndraws_D)
    nd2 = .norm_dist(xbar1 = x$xbar[2], sd1 = x$sd[2], n1 = x$n[2], ndraws = ndraws_D)
    names(nd2) = paste0(names(nd2), "2")
    nd = cbind(nd1, nd2)
    nd$den1 = nd$vx + nd$vx2
    assert_that(all(nd$den1 > 0))

    # gt, lt draws are related
    val1 = (nd$mx - nd$mx2) / sqrt(nd$den1)
    gt = pnorm(val1)
    out_dist = .cp2es(gt = gt, lt = 1 - gt, bool.cont = TRUE, bool.bin = FALSE, bool.flip = FALSE)
    .calc_ci(out_dist$es, ret_pe)
  } else {
    .pe2ret(ret_pe)
  }

  ret %>% .finalize_tab(tab_data = tab_data, meta = meta, func = "d_cont")
}

.norm_dist = function(xbar1, sd1, n1, ndraws) {
  ret = data.frame(
    mx = rnorm(n = ndraws, mean = xbar1, sd = sd1 / sqrt(n1))
    , vx = (sd1^2) / n1 * rchisq(n = ndraws, df = n1 - 1)
  )
  assert_that(all(ret$vx >= 0))
  ret
}
