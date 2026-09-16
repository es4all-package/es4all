.ep_analyze_bin = function(es_true, v1, v2, x, conv_tests, Sys.sleep_time) {
  nn = x$n
  ldf = mapply(
    function(k1, k2) {
      data.frame(k = c(k1, k2), n = nn)
    }, v1, v2, USE.NAMES = FALSE, SIMPLIFY = FALSE)

  es_sim = mapply(
    function(df1) {
      d_bin(df1)
    }, ldf, SIMPLIFY = FALSE, USE.NAMES = FALSE)

  obj_mle = .es_sim_info(es_sim = es_sim, es_true = es_true)

  Sys.sleep(Sys.sleep_time)
  es_sim = mapply(
    function(df1) {
      d_np(df1)
    }, ldf, SIMPLIFY = FALSE, USE.NAMES = FALSE)

  obj_npe = .es_sim_info(es_sim = es_sim, es_true = es_true)

  ## Check: `prr = `
  conf.level = getOption("es4all.conf.level")
  assert_that(conf.level > 0.5, conf.level < 1)
  alpha = 1 - conf.level

  obj_conv = list()
  for (nn in names(conv_tests)) {
    obj_conv[[nn]] = mean(conv_tests[[nn]] < alpha)
  }
  obj_conv %<>% unlist %>% t %>% as.data.frame()

  list(mle = obj_mle, npe = obj_npe, conv_prr = obj_conv)
}

####
.ep_conv_tests_bin = function(v1, v2, x) {
  tabs = mapply(
    function(s1, s2) {
      matrix( c(s1, x$n[1] - s1, s2, x$n[2] - s2), nrow = 2, byrow = TRUE)
    }, v1, v2, USE.NAMES = FALSE, SIMPLIFY = FALSE
  )

  list(`Fisher's exact test` = mapply(
    FUN = function(tab) {
      fisher.test(x = tab, conf.int = FALSE)$p.value
    }, tabs, USE.NAMES = FALSE)

    , `Pearson's chi-squared test` = mapply(
    FUN = function(tab) {
      chisq.test(x = tab, correct = FALSE)$p.value
    }, tabs, USE.NAMES = FALSE)
  )
}

.ep_conv_tests_cont = function(v1, v2) {
  list(`Welch's t-test` = mapply(
    FUN = function(x1, x2) {
      t.test(x = x1, y = x2, alternative = "two.sided", var.equal = FALSE)$p.value
    }, v1, v2, USE.NAMES = FALSE)

    , `Student's t-test` = mapply(
      FUN = function(x1, x2) {
        t.test(x = x1, y = x2, alternative = "two.sided", var.equal = TRUE)$p.value
      }, v1, v2, USE.NAMES = FALSE)

    , `WMW test` = mapply(
      FUN = function(x1, x2) {
        wilcox.test(x = x1, y = x2, alternative = "two.sided")$p.value
      }, v1, v2, USE.NAMES = FALSE)
  )
}
