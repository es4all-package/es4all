.es_sim_info = function(es_sim, es_true) {
  ##
  info = list()
  for (ii in 1:length(es_sim)) {
    es1 = es_sim[[ii]]
    for (jj in 1:nrow(es1)) {
      esx = es1[jj,]
      info[[esx$name]] %<>% rbind(esx)
    }
  }

  ## RMSE
  ret_rmse = NULL
  for (nn in intersect(names(info), es_true$name) ) {
    idx = which(es_true$name == nn)
    assert_that(length(idx) == 1)
    es1 = es_true[idx,]

    info_1 = info[[nn]]
    r1 = data.frame(name = nn
                    , value = es1$est.pe
                    , rmse = sqrt(mean( (info_1$est.pe - es1$est.pe)^2 )) )
    ret_rmse %<>% rbind(r1)
  }

  ##
  conf.level = getOption("es4all.conf.level")
  assert_that(conf.level > 0.5, conf.level < 1)
  alpha = 1 - conf.level

  ## CI / pval duality
  assert_that("SD" %in% names(info))
  info_sd = info[["SD"]]
  info_sd$eq = (info_sd$ll <= 0 & info_sd$ul >= 0)
  info_sd$pv = (info_sd$pval >= alpha)
  # idx = which(info_sd$eq != info_sd$pv)
  # if (length(idx) > 0) {
  #   warning("CI / pval: no duality", immediate. = TRUE)
  #   browser()
  # }
  assert_that(are_equal(info_sd$eq, info_sd$pv), msg = "CI / pval: no duality")

  ## coverage and prr (Prob reject) are identical for all ES's
  idx = which(es_true$name == "SD")
  assert_that(length(idx) == 1)
  es1 = es_true[idx,]
  ret_ci = data.frame(
    coverage = mean(es1$est.pe >= info_sd$ll & es1$est.pe <= info_sd$ul)
    , prr = mean(info_sd$pval < alpha) )
  ## Check: `prr = `

  list(rmse = ret_rmse, ci = ret_ci)
}
