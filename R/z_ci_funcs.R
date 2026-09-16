.get_ndraws_D = function() {
  conf.level = getOption("es4all.conf.level")
  max_ndraws_D = getOption("es4all.max_ndraws_D")
  n_D_tail = getOption("es4all.n_D_tail")
  assert_that(conf.level > 0.5, conf.level < 1
              , max_ndraws_D >= 500, n_D_tail >= 10)

  alpha = 1 - conf.level
  ndraws_D = ceiling(2 * n_D_tail / alpha)

  if (ndraws_D > max_ndraws_D) {
    glue("Need more simulations to accurately calculate CI. \\
      Check these options: es4all.max_ndraws_D, es4all.n_D_tail, and es4all.conf.level.") %>% warning
    ndraws_D = max_ndraws_D
  }
  ndraws_D
}

.calc_ci = function(D1, ret_pe) {
  ## pval
  assert_that("SD" %in% names(D1))

  # CI / pval duality
  # p1 = pnorm( mean(D1[["SD"]]) / sd(D1[["SD"]]) )
  # p1 = (sum(D1[["SD"]] < 0) + sum(D1[["SD"]] == 0) * 0.5) / length(D1[["SD"]])
  p1 = sum(D1[["SD"]] <= 0) / length(D1[["SD"]])
  # pval = 2 * min(p1, 1 - p1)
  p2 = sum(D1[["SD"]] >= 0) / length(D1[["SD"]])
  pval = 2 * min(p1, p2)

  ## ci
  conf.level = getOption("es4all.conf.level")
  assert_that(conf.level > 0.5, conf.level < 1)
  alpha = 1 - conf.level
  ret = NULL
  for (nn in names(D1)) {
    v1 = D1[[nn]]
    # Generally, type = 8 is good. But here, need type = 1 for CI / pval duality.
    fo = quantile(v1, probs = c(alpha / 2, 1 - alpha / 2), type = 1, names = FALSE)
    ret %<>% rbind( data.frame(
      name = nn
      , est.pe = ret_pe$es[[nn]]
      , flip = ret_pe$flip
      , est.MC = mean(v1), ll = fo[1], ul = fo[2]
      , pval = pval) )
  }

  ret
}

# name est.pe flip
.pe2ret = function(ret_pe) {
  ret = NULL
  for (nn in names(ret_pe$es)) {
    ret %<>% rbind(data.frame(
      name = nn
      , est.pe = ret_pe$es[[nn]]
      , flip = ret_pe$flip
    ))
  }
  ret
}
