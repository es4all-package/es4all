#' Dominance effect sizes for paired continuous data
#'
#' Calculate dominance-based effect sizes for paired continuous (normally
#' distributed) data. A convenience wrapper around [d_one()].
#'
#' The within-pair differences (e.g., post minus pre) are compared against a
#' constant of zero. For full control, such as testing against a non-zero
#' constant, construct the two-row `data.frame` (or two-element `list`) and
#' call [d_one()] directly.
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
#' `x` can contain either summary statistics or microdata.
#'
#' * If `x` contains summary statistics, it should be a `data.frame` with 1 row.
#' It can optionally contain a variable called `group`. In addition, it must
#' contain one of the following:
#'
#'    * sample mean (`xbar`), sample standard deviation (`sd`), and sample size (`n`); or
#'    * `xbar`, standard error of the mean (`sem`), and `n`; or
#'    * expected value (`mu`) and standard deviation (`sigma`), which are known parameters.
#'
#' * If `x` contains microdata, it should be a named `list` with 1 element
#' being a vector of `numeric` paired differences.
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
#' # Summary statistics. Equivalent to:
#' # df1 = rbind(comparison_data$IOP, data.frame(group = "Zero", xbar = 0, sd = NA, n = NA))
#' # d_one(df1)
#' d_pair(comparison_data$IOP)
#'
#' # Microdata. Equivalent to:
#' # d_one(list(differences = c(3.1, -0.5, 2.2, 1.8, 4.0, 0.3), Zero = 0))
#' d_pair(list(differences = c(3.1, -0.5, 2.2, 1.8, 4.0, 0.3)))
d_pair = function(x, meta = NULL) {
  .input = deparse1(substitute(x))

  if (is.list(x) && !is.data.frame(x)) {
    ## Microdata: 1-element named list of paired differences
    assert_that(length(x) == 1L, !is.null(names(x)), all(names(x) %>% nzchar))
    assert_that(is.numeric(x[[1]]), length(x[[1]]) > 1)

    meta_attr = attr(x, "meta", exact = TRUE)
    x_list = c(x, list(Zero = 0))
    attr(x_list, "meta") = meta_attr
    d_one(x = x_list, meta = meta, .input = .input)
  } else {
    ## Summary statistics: 1-row data.frame
    assert_that(is.data.frame(x), nrow(x) == 1L)

    nms_data = setdiff(names(x), "group")
    if (!("group" %in% names(x))) x$group = "X1"

    row0 = if (setequal(nms_data, c("mu", "sigma"))) {
      data.frame(group = "Zero", mu = 0, sigma = NA_real_)
    } else if (setequal(nms_data, c("xbar", "sd", "n"))) {
      data.frame(group = "Zero", xbar = 0, sd = NA_real_, n = NA_real_)
    } else if (setequal(nms_data, c("xbar", "sem", "n"))) {
      data.frame(group = "Zero", xbar = 0, sem = NA_real_, n = NA_real_)
    } else {
      stop("`x` contains unknown statistics.")
    }

    x2 = rbind(x, row0)
    attr(x2, "meta") = attr(x, "meta", exact = TRUE)
    d_one(x = x2, meta = meta, .input = .input)
  }
}
