#' Estimator properties of dominance-based effect sizes
#'
#' @description
#' Calculate the root mean squared error (RMSE), coverage, power, and
#' false positive rate (FPR) of dominance-based effect size estimators -- the
#' maximum likelihood estimator (MLE) and the non-parametric estimator (NPE).
#' Compares power and FPR with that of certain conventional tests.
#'
#' **Warning.** This function takes a very long time to run as it uses simulations.
#'
#' @details
#' `x` is a `data.frame` with 2 rows that specify how data is generated. It
#' has a variable called `n` (sample size). In addition, it has the following variables:
#'
#' * Continuous (normally distributed) data: `mu` and `sigma`.
#' * Binary data: `p`.
#'
#' @param x a 2-row `data.frame` with simulation parameters
#' @param nsim number of simulations
#' @param seed random number seed (ignored if `NA`)
#' @param Sys.sleep_time occasional calls to `Sys.sleep()`
#'
#' @return Estimator properties.
#' @export
#' @family properties
#'
#' @examples
#' \donttest{
#' # **Warning.** This function takes a very long time to run as it uses simulations.
#' # In a real run, update the value of `nsim`.
#' # These examples have a theoretical power of 80% and a theoretical FPR of 5%.
#' nsim = 2 ## 5000
#' estimator_properties(data.frame(mu = c(105, 100), sigma = c(10, 10), n = c(64, 64)), nsim = nsim)
#' estimator_properties(data.frame(p = c(0.6, 0.4), n = c(97, 97)), nsim = nsim)
#' }
estimator_properties = function(x, nsim, seed = 42, Sys.sleep_time = if (nsim > 100) 5 else 0) {
  assert_that(is.data.frame(x), nrow(x) == 2, noNA(x)
    , "n" %in% names(x), all(x$n >= 1), all(x$n < Inf)
    , nsim >= 2)

  op_ = options(es4all.flip = FALSE
                , es4all.output_raw = TRUE
                , es4all.set.seed_seed = seed)
  on.exit(options(op_))

  .my_set.seed()
  obj1 = .ep1(x = x, nsim = nsim, Sys.sleep_time = Sys.sleep_time)

  Sys.sleep(Sys.sleep_time)
  x_means = colMeans(x) %>% t %>% as.data.frame
  x_null = rbind(x_means, x_means)
  x_null$n = x$n
  obj0 = .ep1(x = x_null, nsim = nsim, Sys.sleep_time = Sys.sleep_time)

  ret = list(
    rmse_mle = obj1$mle$rmse
    , rmse_npe = obj1$npe$rmse
    , coverage = data.frame(mle = obj1$mle$ci$coverage, npe = obj1$npe$ci$coverage)
    , power = data.frame(mle = obj1$mle$ci$prr, npe = obj1$npe$ci$prr
                         , obj1$conv_prr, check.names = FALSE)
    , fpr = data.frame(mle = obj0$mle$ci$prr, npe = obj0$npe$ci$prr
                         , obj0$conv_prr, check.names = FALSE)
  )

  ret$x = x
  ret %>% .finalize_ep_tab()
}

.ep1 = function(x, nsim, Sys.sleep_time = Sys.sleep_time) {
  if (setequal(names(x), c("mu", "sigma", "n"))) {
    ## Continuous (normally distributed) variables
    assert_that(all(x$sigma > 0))
    es_true = d_cont(x[,c("mu", "sigma")])

    v1 = rnorm(n = x$n[1] * nsim, mean = x$mu[1], sd = x$sigma[1]) %>% matrix(ncol = nsim) %>% as.data.frame()
    v2 = rnorm(n = x$n[2] * nsim, mean = x$mu[2], sd = x$sigma[2]) %>% matrix(ncol = nsim) %>% as.data.frame()

    conv_tests = .ep_conv_tests_cont(v1 = v1, v2 = v2)
    .ep_analyze_cont(es_true = es_true, v1 = v1, v2 = v2, conv_tests = conv_tests, Sys.sleep_time = Sys.sleep_time)
  } else if (setequal(names(x), c("p", "n"))) {
    ## Binary variables
    assert_that(all(x$p >= 0), all(x$p <= 1))
    es_true = d_bin(x[,c("p"), drop = FALSE])

    v1 = rbinom(n = nsim, size = x$n[1], prob = x$p[1])
    v2 = rbinom(n = nsim, size = x$n[2], prob = x$p[2])
    conv_tests = .ep_conv_tests_bin(v1 = v1, v2 = v2, x = x)
    .ep_analyze_bin(es_true = es_true, v1 = v1, v2 = v2, x = x, conv_tests = conv_tests, Sys.sleep_time = Sys.sleep_time)
  } else {
    stop("`x` contains unknown simulation parameters.")
  }
}
