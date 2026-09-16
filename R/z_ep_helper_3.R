.ep_analyze_cont = function(es_true, v1, v2, x, conv_tests, Sys.sleep_time) {
  es_sim = mapply(
    function(x1, x2) {
      d_cont(list(x1 = x1, x2 = x2))
    }, v1, v2, SIMPLIFY = FALSE, USE.NAMES = FALSE)

  obj_mle = .es_sim_info(es_sim = es_sim, es_true = es_true)

  Sys.sleep(Sys.sleep_time)
  es_sim = mapply(
    function(x1, x2) {
      d_np(list(x1 = x1, x2 = x2))
    }, v1, v2, SIMPLIFY = FALSE, USE.NAMES = FALSE)

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
