#' Dominance effect sizes for ordinal variables
#'
#' Calculate dominance-based effect sizes for ordinal variables.
#'
#' Calculate the following:
#'
#' * stochastic difference (aka Glass rank-biserial correlation);
#' * adjusted stochastic difference (aka Goodman and Kruskal's gamma);
#' * dominance (aka common language effect size);
#' * measure of stochastic superiority (aka Vargha and Delaney's A);
#' * adjusted dominance; and
#' * generalized odds ratio; and
#' * win odds.
#'
#' `x` can contain either known parameter values or observed counts.
#'
#' `x` should be a `data.frame` with 2 rows. It can optionally contain a variable called `group`,
#' indicating the name of the group. In addition, it must contain one variable for each
#' possible outcome (2 or more outcomes). The outcome variables **must be ordered from best to worst**.
#'
#' The outcome variables can contain one of the following:
#'
#'  * event probabilities, which are known parameters, and which must be between 0 and 1 and add up to 1; or
#'  * the number of events.
#'
#' In the special case when there are exactly two outcome variables and they are called
#' `k` and `n`, this is taken to be binary data: `k` is the number of events;
#' `n` is the sample size -- it is the number of events plus the number of non-events.
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
#' # Number of events for each outcome
#' d_ord(comparison_data$TB)
#' d_ord(comparison_data$tonsils)
#'
#' # Binary data (number of events and sample size). Compare with `d_bin()`.
#' d_ord(comparison_data$UTI)
#'
#' # Known parameter values (based on the tonsils example)
#' d_ord(data.frame(a1 = c(497 / 1326, 19 / 72), a2 = c(560 / 1326, 29 / 72)
#' , a3 = c(269 / 1326, 24 / 72)))
d_ord = function(x, meta = NULL) {
  ## Check arguments
  tab_data = list(input = deparse1(substitute(x)))
  fo = .check_args(x = x, meta = meta)
  x = fo$x
  meta = fo$meta
  data_type = fo$data_type

  ## Clean data
  if (data_type == "micro") {
    stop("Cannot use microdata with d_ord().")
  }

  idx = which(names(x) == "group")
  assert_that(length(idx) == 1)
  x.g = x[,idx, drop=FALSE]
  x.v = x[,-idx, drop=FALSE]
  rm(x)
  assert_that(x.g$group %>% is.numeric %>% `!`
              , all(sapply(x.v, is.numeric))
              , ncol(x.v) >= 2)
  tab_data$group_input = x.g$group

  x_type = if (all(x.v >= 0) && all(x.v <= 1)) {
    assert_that(are_equal(rowSums(x.v), c(1,1) ))

    tab_data$title = "Probability"
    bb = c(NA, NA)
    for (ii in 1:2) {
      aa = x.v[ii,]
      bb[ii] = glue("{names(aa)} ({aa})") %>% paste(collapse = " > ")
      bb[ii] = paste(glue("the probability of {x.g$group[ii]} having each outcome:"), bb[ii])
    }
    tab_data$value = bb %>% paste0(collapse = "; ")

    sh = c(NA, NA)
    for (ii in 1:2) {
      sh[ii] = glue("$\\pi_{{{ii},{1:ncol(x.v)}}} = {x.v[ii,]}$") %>% paste(collapse = ", ")
    }
    tab_data$sh = sh %>% paste(collapse = "; ")

    "param"
  } else if(all(x.v >= 1 | x.v == 0)) {
    if (setequal(names(x.v), c("k", "n"))) {
      assert_that(all(x.v$n >= x.v$k), all(x.v$k >= 0), all(x.v$n < Inf))
      x.v = x.v[,c("k", "n")]
      x.v$n = x.v$n - x.v$k
      names(x.v) = c("Events", "Non-events")
    }

    tab_data$title = "# Events"
    bb = c(NA, NA)
    for (ii in 1:2) {
      aa = x.v[ii,]
      bb[ii] = glue("{names(aa)} ({aa})") %>% paste(collapse = " > ")
      bb[ii] = paste(glue("the number of {x.g$group[ii]} who had each outcome:"), bb[ii])
    }
    tab_data$value = bb %>% paste0(collapse = "; ")

    sh = c(NA, NA)
    for (ii in 1:2) {
      sh[ii] = glue("$k_{{{ii},{1:ncol(x.v)}}} = {x.v[ii,]}$") %>% paste(collapse = ", ")
    }
    tab_data$sh = sh %>% paste(collapse = "; ")

    "stat"
  } else {
    stop("`x` contains unknown statistics.")
  }

  ## Calculate
  x.rs = rowSums(x.v)
  assert_that(all(x.v >= 0), all(x.v < Inf), all(x.rs >= 1), all(x.rs < Inf))
  x.prop = x.v / x.rs
  ret_pe = .cp2es(gt = .calc_ord(x.prop), lt = .calc_ord(x.prop[2:1,]), bool.cont = FALSE, bool.bin = FALSE, bool.flip = TRUE)
  if (ret_pe$flip) {
    x.g = x.g[2:1,, drop = FALSE]
    x.prop = x.prop[2:1,]
    x.rs %<>% rev
  }
  tab_data$group = x.g$group

  ## CI
  ret = if (x_type == "stat") {
    x.rs %<>% round(0)
    assert_that(all(x.rs >= 1), all(x.rs < Inf))

    ndraws_D = .get_ndraws_D()
    .my_set.seed()
    p1 = rmultinom(n = ndraws_D, size = x.rs[1], prob = x.prop[1,]) / x.rs[1]
    p2 = rmultinom(n = ndraws_D, size = x.rs[2], prob = x.prop[2,]) / x.rs[2]
    assert_that(all(p1 >= 0), all(p1 <= 1), all(p2 >= 0), all(p2 <= 1))

    fo = mapply(FUN = function(v1, v2) {
      tmp1 = v1 %>% t %>% as.data.frame
      tmp2 = v2 %>% t %>% as.data.frame
      x.tmp = rbind(tmp1, tmp2)
      rbind(
        .calc_ord(x.tmp)
        , .calc_ord(x.tmp[2:1,]) )
    }, as.data.frame(p1), as.data.frame(p2), USE.NAMES = FALSE)
    csfo = colSums(fo)
    assert_that(all(csfo >= 0), all(csfo <= 1))

    out_dist = .cp2es(gt = fo[1,], lt = fo[2,], bool.cont = FALSE, bool.bin = FALSE, bool.flip = FALSE)
    .calc_ci(out_dist$es, ret_pe)
  } else {
    .pe2ret(ret_pe)
  }

  ret %>% .finalize_tab(tab_data = tab_data, meta = meta, func = "d_ord")
}

.calc_ord = function(x) {
  ret = 0
  nc = ncol(x)
  for (ii in 1:(nc - 1)) {
    ret = ret + x[1,ii] * sum(x[2,(ii + 1):nc])
  }
  ret
}
