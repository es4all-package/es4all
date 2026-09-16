#' Dominance effect sizes using non-parametric estimation
#'
#' Calculate dominance-based effect sizes using non-parametric estimation. Can
#' be used with continuous, binary, or ordinal data.
#'
#' Calculate the following:
#'
#' * stochastic difference (aka Glass rank-biserial correlation);
#' * adjusted stochastic difference (aka Goodman and Kruskal's gamma);
#' * dominance (aka common language effect size);
#' * measure of stochastic superiority (aka Vargha and Delaney's A);
#' * adjusted dominance;
#' * generalized odds ratio; and
#' * win odds.
#'
#' `x` can contain microdata or certain summary statistics.
#'
#' * If `x` contains microdata, it should be a named `list` with 2 elements, each
#' being a vector of data.
#'
#' * If `x` contains summary statistics, it should be a `data.frame` with 2 rows.
#' It can optionally contain a variable called `group`, indicating the name of the group.
#' In addition, it must contain one of the following:
#'
#'    * **Binary data.** The number of events (`k`) (also called "successes") and the
#'    number of experiments or trials (`n`). Or
#'    * **Ordinal data.** One variable for each possible outcome (2 or more outcomes).
#'    The outcome variables **must be ordered from best to worst**. The outcome
#'    variables must contain the number of events for that outcome.
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
#' # Binary data (number of events and sample size). Compare with `d_bin()`.
#' d_np(comparison_data$UTI)
#' d_np(comparison_data$DVT)
#' d_np(comparison_data$MI)
#'
#' # Ordinal data (number of events for each outcome). Compare with `d_ord()`.
#' d_np(comparison_data$TB)
#' d_np(comparison_data$tonsils)
d_np = function(x, meta = NULL) {
  ## Check arguments
  tab_data = list(input = deparse1(substitute(x)))
  fo = .check_args(x = x, meta = meta)
  x = fo$x
  meta = fo$meta
  data_type = fo$data_type

  ## Clean data
  if (data_type == "summary") {
    idx = which(names(x) == "group")
    assert_that(length(idx) == 1)
    x.g = x[,idx, drop=FALSE]
    x.v = x[,-idx, drop=FALSE]
    rm(x)
    assert_that(x.g$group %>% is.numeric %>% `!`
                , all(sapply(x.v, is.numeric))
                , ncol(x.v) >= 2)

    if (setequal(names(x.v), c("mu", "sigma"))
        || setequal(names(x.v), c("xbar", "sd", "n"))
        || setequal(names(x.v), c("xbar", "sem", "n"))) {
      stop("`x` contains unknown statistics.")
    }

    if(setequal(names(x.v), c("k", "n"))) {
      assert_that(all(x.v$n >= x.v$k), all(x.v$k >= 0), all(x.v$n < Inf))
      x.v = x.v[,c("k", "n")]
      x.v$n = x.v$n - x.v$k
      names(x.v) = c("Events", "Non-events")
    }

    x.rs = rowSums(x.v)
    assert_that(all(x.v >= 0), all(x.v < Inf), all(x.rs >= 1), all(x.rs < Inf))

    newx = .ord2micro(x.v)
    names(newx) = x.g$group
    x = newx
  }
  tab_data$group_input = names(x)

  signif_digits = getOption("es4all.np_signif_digits")
  assert_that(signif_digits >= 2)
  for (ii in 1:2) {
    x[[ii]] %<>% signif(digits = signif_digits)
  }
  tab_data$group = names(x)

  tab_data$title = "Median (IQR)"
  bb = c(NA, NA)
  for (ii in 1:2) {
    v1 = quantile(x[[ii]], probs = c(0.25, 0.5, 0.75), names = FALSE, type = 8) %>% signif(digits = getOption("es4all.np_signif_digits"))
    bb[ii] = glue("the median the [variable] for {names(x[ii])} was {v1[2]} \\
                  (IQR: {v1[1]}-{v1[3]})")
  }
  tab_data$value = paste0(bb[1], "; ", bb[2])

  sh = c(NA, NA)
  for (ii in 1:2) {
    v1 = quantile(x[[ii]], probs = c(0.25, 0.5, 0.75), names = FALSE, type = 8) %>% signif(digits = getOption("es4all.np_signif_digits"))
    sh[ii] = glue("$m_{ii} = {v1[2]}$, $Q_{{1,{ii}}} = {v1[1]}$, $Q_{{3,{ii}}} = {v1[3]}$")
  }
  tab_data$sh = sh %>% paste(collapse = "; ")

  ## Calculate
  .my_set.seed() # !!
  ret_pe = .cp2es(gt = .np1(x1 = x[[1]], x2 = x[[2]])
                  , lt = .np1(x1 = x[[2]], x2 = x[[1]])
                  , bool.cont = FALSE, bool.bin = FALSE, bool.flip = TRUE)
  if (ret_pe$flip) x %<>% rev
  tab_data$group = names(x)

  ## CI
  ndraws_D = .get_ndraws_D()
  # .my_set.seed() # called above
  yy = list()
  for (ii in 1:2) {
    lx = length(x[[ii]])
    yy[[ii]] = resample(x[[ii]], size = ndraws_D * lx
                        , replace = TRUE) %>% matrix(nrow = lx) %>% as.data.frame()
  }
  gt = mapply(FUN = .np1, yy[[1]], yy[[2]], USE.NAMES = FALSE)
  lt = mapply(FUN = .np1, yy[[2]], yy[[1]], USE.NAMES = FALSE)
  out_dist = .cp2es(gt = gt, lt = lt, bool.cont = FALSE, bool.bin = FALSE, bool.flip = FALSE)
  ret = .calc_ci(out_dist$es, ret_pe)

  ret %>% .finalize_tab(tab_data = tab_data, meta = meta, func = "d_np")
}

.ord2micro = function(x.v) {
  assert_that(all( sapply(x.v, is.integer) | (
    sapply(x.v, is.numeric) & sapply(x.v, function(x) all(x == trunc(x)))
  ) ))
  nc = ncol(x.v)
  x1 = x2 = c()
  for (ii in 1:nc) {
    val = nc - ii + 1
    x1 %<>% c(rep_len(val, x.v[1, ii]))
    x2 %<>% c(rep_len(val, x.v[2, ii]))
  }
  list(x1 = x1, x2 = x2)
}

.np1 = function(x1, x2) {
  max_nr = getOption("es4all.np_max_nr")
  assert_that(max_nr >= 100)
  if (length(x1) * length(x2) <= max_nr) {
    eg = expand.grid(x1 = x1, x2 = x2, KEEP.OUT.ATTRS = FALSE)
  } else {
    nr = getOption("es4all.np_nr")
    assert_that(nr >= 100)
    eg = data.frame(x1 = resample(x1, size = nr, replace = TRUE)
                    , x2 = resample(x2, size = nr, replace = TRUE))
  }
  eg$d = eg$x1 - eg$x2
  sum(eg$d > 0) / nrow(eg)
}

# ?sample
resample = function(x, ...) x[sample.int(length(x), ...)]
