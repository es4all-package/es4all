.finalize_tab = function(df1, tab_data, meta, func) {
  ##
  assert_that(is.data.frame(df1))

  attr(df1, "tab_data") = tab_data
  attr(df1, "meta") = meta
  attr(df1, "func") = func

  if (is.null(meta)) {
    attr(df1, "meta") = list(`_` = "-")
  }
  if (getOption("es4all.output_raw")) {
    return(df1)
  }

  ##
  ax = attributes(df1)
  assert_that(names(ax$tab_data) %>% setequal(c("input", "group_input", "group"
                                                , "title", "value", "sh")))
  bool.est = df1 %>% .df1_bool.est()
  xx = df1 %>% .df1_clean(bool.est = bool.est)

  pval = if (bool.est) {
    assert_that(all(df1$pval == df1$pval[1]))
    df1$pval[1] %>% .pp_pval
  } else {
    NULL
  }

  ##
  out = list()
  out$Data = data.frame(
    What = c("Input", "Groups", ax$tab_data$title, "Data")
    , Value = c(ax$tab_data$input
                , glue("{ax$tab_data$group_input[1]} vs {ax$tab_data$group_input[2]}")
                , ax$tab_data$value
                , ax$tab_data$sh
    )
  )
  out$Data = .astra_mark_raw_markup(out$Data, col = "Value", rows = nrow(out$Data))

  out$About = data.frame(What = c(), Value = c())
  if (!is.null(ax$meta)) {
    for (nn in names(ax$meta)) {
      out$About %<>% rbind(data.frame(What = nn
                                      , Value = ax$meta[[nn]]))
    }
  }

  es = getOption("es4all.output_es")
  if (length(es) == 1 && is.na(es)) {
    es = xx$name
  } else if (!is.character(es)) {
    stop("Please specify a valid `es` argument.")
  }

  glue_str = if (bool.est) {
    "{xx1$name} ({ax$tab_data$group[1]} vs. {ax$tab_data$group[2]}) = {xx1$est.pe} ({getOption('es4all.conf.level') %>% .pp_p0()} CI: {.pp_ci(xx1$ll, xx1$ul)}); p-value: {pval}"
  } else {
    "{xx1$name} ({ax$tab_data$group[1]} vs. {ax$tab_data$group[2]}) = {xx1$est.pe}"
  }
  alpha = 1 - getOption('es4all.conf.level')
  do_ed = (ax$func %in% c("d_bin", "d_np", "d_ord"))
  idx_meaning = if (ax$func %in% c("d_cont", "d_one", "d_pair")) {
    "meaning_cont"
  } else if (ax$func == "d_np") {
    "meaning_np"
  } else if (ax$func == "d_bin") {
    "meaning_bin"
  } else if (ax$func == "d_ord") {
    "meaning_ord"
  } else {
    stop("Unknown variable type")
  }
  for (es1 in es) {
    idx = which(es1 == xx$name)
    if (length(idx) == 1) {
      xx1 = xx[idx,]
      idx = which(es1 == es_info$short)
      assert_that(length(idx) == 1)
      esi = es_info[idx,]

      name1 = glue("Results: {esi$full}")
      out[[name1]] = data.frame(What = "Effect size", Value = esi$full)
      out[[name1]] %<>% rbind(data.frame(What = "Result", Value = glue(glue_str)))
      out[[name1]] %<>% rbind(data.frame(What = "Meaning", Value = esi[,idx_meaning] %>% glue() ))

      if (do_ed) {
        ed_str = if (es1 %in% c("D", "SD", "NNT", "MSS", "WO")) {
          "Equivalence dependent. This effect size's possible range narrows toward the point of \\
          probabilistic equality (no effect) as the probability of a tied outcome grows, so it correctly reports a small \\
          effect when most pairs would have the same outcome regardless of the group. This is a desirable \\
          property in many settings, including the treatment and control group setting, since ties are part \\
          of the answer, not a nuisance to be divided out." %>% glue()
        } else if (es1 %in% c("GOR", "ASD", "AD")) {
          "Not equivalence dependent. This effect size's range does not depend on the probability of a tie, \\
          because tied pairs are divided out and only the pairs that are distinguishable are measured. It can therefore \\
          portray a relationship as large even when ties are common." %>% glue()
        } else {
          stop("Unknown effect size")
        }
        out[[name1]] %<>% rbind(data.frame(What = "Equivalence dependence", Value = ed_str))
      }

      out[[name1]] %<>% rbind(data.frame(What = "Other names", Value = esi$other_names ))
    }
  }

  ##
  out$Inference = data.frame(What = c(), Value = c())
  if (bool.est) {
    tmp = if (df1$pval[1] < alpha) {
      "The result **is** statistically significant (p-value: {pval} < {alpha})."
    } else {
      "The result is **not** statistically significant (p-value: {pval} >= {alpha}). \\
          You can treat this as if there is no effect.
          "
    }
    out$Inference %<>% rbind(data.frame(What = "Inference", Value = tmp %>% glue() ))
  }

  glue_str = if(ax$func == "d_bin") {
    "The effect is in favor of the event being more likely for {ax$tab_data$group[1]} than for {ax$tab_data$group[2]}."
  } else if (ax$func %in% c("d_cont", "d_one", "d_pair", "d_np")) {
    "The effect is in favor of the [variable] being greater for {ax$tab_data$group[1]} than for {ax$tab_data$group[2]}."
  } else if (ax$func == "d_ord") {
    "The effect is in favor of the [variable] being better for {ax$tab_data$group[1]} than for {ax$tab_data$group[2]}."
  } else {
    stop("Unknown data type")
  }
  out$Inference %<>% rbind(data.frame(What = "Side", Value = glue_str %>% glue))

  ## Methods and Bibliography: 2 items
  version = packageVersion("es4all")
  package = .astra_dquote("es4all")

  #
  str.calc_p = if (bool.est) {
    "maximum likelihood estimation (MLE)"
  } else {
    "known values of distribution parameters"
  }
  str.calc_np = if (bool.est) {
    "non-parametric estimation (NPE)"
  } else {
    "???"
  }
  str.desc = switch(ax$func
               , d_bin = "for binary variables, calculated using {str.calc_p}" %>% glue()
               , d_cont = "for continuous (normally distributed) variables, calculated using {str.calc_p}" %>% glue()
               , d_np = "calculated using {str.calc_np}" %>% glue()
               , d_one = "for continuous (normally distributed) variables in the one-sample or paired scenario, calculated using {str.calc_p}" %>% glue()
               , d_ord = "for ordinal variables, calculated using {str.calc_p}" %>% glue()
               , "Unknown!")

  out$`Methods` = data.frame(What = "Methods"
      , Value = 'Please include this or similar in your Methods section: \\
      We present [effect size name], a dominance-based effect size {str.desc}, along with its \\
      {getOption("es4all.conf.level") %>% .pp_p0()} confidence interval and p-value. \\
      Results were considered to be statistically significant if p < {1 - getOption("es4all.conf.level")}. \\
      Data analyses were performed using the R package {package} (version {version}).' %>% glue())
  #### !!! If making changes, update: : CITATION, DESCRIPTION, .astra_producer (2 locations), .finalize_tab (Methods and Bibliography)

  ##
  # df1$est.MC = NULL
  # out$`Raw output` = df1

  ##
  for (nn in names(out)) {
    class( out[[nn]] ) = c("astra_table", "data.frame")
    attr( out[[nn]], "title") = nn
  }
  class(out) = c("astra_list", "list")

  out
}

.df1_bool.est = function(df1) {
  if (names(df1) %>% setequal(c("name", "est.pe", "flip", "est.MC", "ll", "ul", "pval"))) {
    TRUE
  } else if (names(df1) %>% setequal(c("name", "est.pe", "flip"))) {
    FALSE
  } else {
    stop("Unknown calculation type.")
  }
}

.df1_clean = function(df1, bool.est) {
  cc_round = if (bool.est) {
    c(2,4:6)
  } else {
    c(2)
  }

  if (attr(df1, "func") %in% c("d_cont", "d_one")) {
    cc = c("D", "SD", "GOR", "SMD")
    xx = df1[match(cc, df1$name), ]

    xx[1,cc_round] = as.numeric(xx[1,cc_round]) %>% .pp_p1
    xx[2,cc_round] = as.numeric(xx[2,cc_round]) %>% .pp_r(digits = 2)
    xx[3,cc_round] = as.numeric(xx[3,cc_round]) %>% .pp_r(digits = 1)
    xx[4,cc_round] = as.numeric(xx[4,cc_round]) %>% .pp_r(digits = 2)
  } else if (attr(df1, "func") %in% c("d_bin")) {
    cc = c("SD", "ASD", "NNT", "D", "MSS", "AD", "GOR", "WO")
    xx = df1[match(cc, df1$name), ]

    xx[1,cc_round] = as.numeric(xx[1,cc_round]) %>% .pp_r(digits = 2)
    xx[2,cc_round] = as.numeric(xx[2,cc_round]) %>% .pp_r(digits = 2)
    xx[3,cc_round] = as.numeric(xx[3,cc_round]) %>% .pp_r(digits = 1)
    xx[4,cc_round] = as.numeric(xx[4,cc_round]) %>% .pp_p1
    xx[5,cc_round] = as.numeric(xx[5,cc_round]) %>% .pp_p1
    xx[6,cc_round] = as.numeric(xx[6,cc_round]) %>% .pp_p1
    xx[7,cc_round] = as.numeric(xx[7,cc_round]) %>% .pp_r(digits = 1)
    xx[8,cc_round] = as.numeric(xx[8,cc_round]) %>% .pp_r(digits = 1)
  } else if (attr(df1, "func") %in% c("d_ord", "d_np")) {
    cc = c("SD", "ASD", "D", "MSS", "AD", "GOR", "WO")
    xx = df1[match(cc, df1$name), ]

    xx[1,cc_round] = as.numeric(xx[1,cc_round]) %>% .pp_r(digits = 2)
    xx[2,cc_round] = as.numeric(xx[2,cc_round]) %>% .pp_r(digits = 2)
    xx[3,cc_round] = as.numeric(xx[3,cc_round]) %>% .pp_p1
    xx[4,cc_round] = as.numeric(xx[4,cc_round]) %>% .pp_p1
    xx[5,cc_round] = as.numeric(xx[5,cc_round]) %>% .pp_p1
    xx[6,cc_round] = as.numeric(xx[6,cc_round]) %>% .pp_r(digits = 1)
    xx[7,cc_round] = as.numeric(xx[7,cc_round]) %>% .pp_r(digits = 1)
  } else {
    stop("?")
  }
  xx
}
