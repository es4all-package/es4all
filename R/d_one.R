#' Dominance effect sizes for one-sample continuous variables
#'
#' Calculate dominance-based effect sizes for continuous (normally distributed)
#' variables in the one-sample or paired scenario.
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
#' * adjusted dominance, which simplifies to dominance; and
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
#'  One of the rows describes the population. The other row describes the constant.
#'  For the row that describes the constant, either `mu` or `xbar` is equal to the
#'  constant, while the other variables are set to `NA`.
#'
#' * If `x` contains microdata, it should be a named `list` with 2 elements, each
#' being a vector of `numeric` data. The element describing the constant must have
#' length `1`, while the element describing the population must have a length of
#' `> 1`.
#'
#' Optionally, `x` can have an attribute called `"meta"`, which must be a named list.
#'
#' @param x data. See details.
#' @param meta either `NULL` or a named list.
#' @param .input (don't use)
#'
#' @return Estimates with verbose explanations.
#'
#' @export
#' @family es
#'
#' @examples
#' d_one(comparison_data$stroke)
#'
#' # Paired data. Compare with `d_pair()`.
#' df1 = rbind(comparison_data$IOP, data.frame(group = "Zero", xbar = 0, sd = NA, n = NA))
#' d_one(df1)
d_one = function(x, meta = NULL, .input = NULL) {
  ## Check arguments
  tab_data = list(input = if (is.null(.input)) deparse1(substitute(x)) else .input)
  fo = .check_args(x = x, meta = meta, one = TRUE)
  x = fo$x
  meta = fo$meta
  data_type = fo$data_type

  ## Clean data
  if (data_type == "micro") {
    idx_const = which(sapply(x, length) == 1L)
    assert_that(length(idx_const) == 1)
    idx_data = setdiff(1:2, idx_const)
    assert_that(is.numeric(x[[idx_data]]), is.numeric(x[[idx_const]]))

    newx = data.frame(group = c(names(x)[idx_data], names(x)[idx_const]))
    newx$xbar = c(mean(x[[idx_data]]), x[[idx_const]])
    newx$sd   = c(sd(x[[idx_data]]),   NA_real_)
    newx$n    = c(length(x[[idx_data]]), NA_real_)
    x = newx
  }
  tab_data$group_input = x$group

  ## Constant is always in the second row
  idx_c = which(names(x) %in% c("sigma", "sd", "sem"))
  assert_that(length(idx_c) == 1)
  idx = which(is.na(x[,idx_c]))
  assert_that(length(idx) == 1, idx %in% 1:2)
  if (idx == 1) {
    x = x[2:1,]
  }

  x_type = if(setequal(names(x), c("group", "mu", "sigma"))) {
    tab_data$title = "Expected value (SD)"
    tab_data$value = glue("the expected value of the [variable] for {x$group[1]} was {x$mu[1]} (SD = {x$sigma[1]}); constant {x$group[2]} was {x$mu[2]}")
    tab_data$sh = glue("$\\mu = {x$mu[1]}$, $\\sigma = {x$sigma[1]}$, $m = {x$mu[2]}$")

    x$xbar = x$mu
    x$sd = x$sigma
    "param"
  } else if(setequal(names(x), c("group", "xbar", "sd", "n"))) {
    tab_data$title = "Average (SD)"
    tab_data$value = glue("the average [variable] for {x$group[1]} was {x$xbar[1]} (SD = {x$sd[1]}); constant {x$group[2]} was {x$xbar[2]}")
    tab_data$sh = glue("$\\bar{{x}} = {x$xbar[1]}$, $s = {x$sd[1]}$, $n = {x$n[1]}$, $m = {x$xbar[2]}$")

    assert_that(all(x$n[1] > 1))
    x$sd[1] = x$sd[1] * sqrt( (x$n[1] - 1) / x$n[1] ) # MLE
    "stat"
  } else if(setequal(names(x), c("group", "xbar", "sem", "n"))) {
    tab_data$title = "Average (SEM)"
    tab_data$value = glue("the average [variable] for {x$group[1]} was {x$xbar[1]} (SEM = {x$sem[1]}); constant {x$group[2]} was {x$xbar[2]}")
    tab_data$sh = glue("$\\bar{{x}} = {x$xbar[1]}$, $s_{{\\bar{{x}} }} = {x$sem[1]}$, $n = {x$n[1]}$, $m = {x$xbar[2]}$")

    assert_that(all(x$sem[1] >= 0), all(x$n[1] >= 0))
    x$sd[1] = x$sem[1] * sqrt(x$n[1])
    "stat"
  } else {
    stop("`x` contains unknown statistics.")
  }

  ## Calculate
  assert_that(x$sd[1] > 0)
  val1 = (x$xbar[1] - x$xbar[2]) / x$sd[1]
  gt = pnorm(val1)
  ret_pe = .cp2es(gt = gt, lt = 1 - gt, bool.cont = TRUE, bool.bin = FALSE, bool.flip = TRUE)
  if (ret_pe$flip) x = x[2:1,]
  tab_data$group = x$group

  ## CI
  if (ret_pe$flip) {
    idx_const = 1
    idx_data = 2
  } else {
    idx_const = 2
    idx_data = 1
  }
  ret = if (x_type == "stat") {
    assert_that(all(x$n[idx_data] >= 1), all(x$n[idx_data] < Inf))

    ndraws_D = .get_ndraws_D()
    .my_set.seed()
    nd_data = .norm_dist(xbar1 = x$xbar[idx_data], sd1 = x$sd[idx_data], n1 = x$n[idx_data], ndraws = ndraws_D)
    nd_const = data.frame(const = rep_len(x$xbar[idx_const], ndraws_D) )
    nd = cbind(nd_data, nd_const)

    assert_that(all(nd$vx > 0))

    # gt, lt draws are related
    num1 = nd$mx - nd$const
    if (ret_pe$flip) num1 = -num1
    val1 = num1 / sqrt(nd$vx)
    gt = pnorm(val1)
    out_dist = .cp2es(gt = gt, lt = 1 - gt, bool.cont = TRUE, bool.bin = FALSE, bool.flip = FALSE)
    .calc_ci(out_dist$es, ret_pe)
  } else {
    .pe2ret(ret_pe)
  }

  ret %>% .finalize_tab(tab_data = tab_data, meta = meta, func = "d_one")
}
