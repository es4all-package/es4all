.pp_r = function(x, digits = 3) {
  round(x, digits)
}

.pp_p1 = function(x) {
  sprintf("%.1f", round(x * 100, 1)) %>% paste0("%")
}

.pp_p0 = function(x) {
  sprintf("%.0f", round(x * 100, 0)) %>% paste0("%")
}

.pp_ci = function(ll, ul) {
  ul_num = suppressWarnings(as.numeric(ul))
  if (is.infinite(ul_num) && ul_num > 0) {
    glue("{ll}+")
  } else {
    glue("{ll}-{ul}")
  }
}

.pp_pval = function(x) {
  if (is.na(x) || x < 0) x = Inf
  x %<>% round(3)
  if (x > 0) {
    sprintf("%.3f", x)
  } else {
    "< .001"
  }
}
