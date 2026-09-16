es_info = tibble::tribble(
  ~short, ~full, ~meaning_np
  #---------------
  , "SD", "Stochastic difference (SD)", "The correlation between group ({ax$tab_data$group[1]} vs. {ax$tab_data$group[2]}) and outcome is {xx1$est.pe}."
  , "ASD", "Adjusted stochastic difference (ASD)", "The correlation between group ({ax$tab_data$group[1]} vs. {ax$tab_data$group[2]}) and outcome is {xx1$est.pe}, if we ignore all pairs with the same outcome."
  , "D", "Dominance (D)", "The probability of a greater [variable] for {ax$tab_data$group[1]} vs. {ax$tab_data$group[2]} is {xx1$est.pe}."
  , "MSS", "Measure of stochastic superiority (MSS)", "The probability of a greater latent variable for [variable] for {ax$tab_data$group[1]} vs. {ax$tab_data$group[2]} is {xx1$est.pe}."
  , "AD", "Adjusted dominance (AD)", "The probability of a greater [variable] for {ax$tab_data$group[1]} vs. {ax$tab_data$group[2]} is {xx1$est.pe}, if we ignore all pairs with the same outcome. Alternatively, the probability of a greater latent variable for [variable] for {ax$tab_data$group[1]} vs. {ax$tab_data$group[2]} is {xx1$est.pe} (but using different assumptions than for MSS)."
  , "GOR", "Generalized odds ratio (GOR)", "The ratio of the probability of a greater [variable] for {ax$tab_data$group[1]} to the probability of a greater [variable] for {ax$tab_data$group[2]} is {xx1$est.pe}."
  , "WO", "Win odds (WO)", "The odds of a greater latent variable for [variable] for {ax$tab_data$group[1]} vs. {ax$tab_data$group[2]} is {xx1$est.pe}."
  , "NNT", "Number needed to treat (NNT)", ""
  , "SMD", "Standardized mean difference (SMD)", ""
) |> as.data.frame()

es_info$meaning_cont = c(
  "The correlation between group ({ax$tab_data$group[1]} vs. {ax$tab_data$group[2]}) and outcome is {xx1$est.pe}."
  , ""
  , "The probability of a greater [variable] for {ax$tab_data$group[1]} vs. {ax$tab_data$group[2]} is {xx1$est.pe}."
  , ""
  , ""
  , "The odds of a greater [variable] for {ax$tab_data$group[1]} vs. {ax$tab_data$group[2]} is {xx1$est.pe}."
  , "The odds of a greater latent variable for [variable] for {ax$tab_data$group[1]} vs. {ax$tab_data$group[2]} is {xx1$est.pe}."
  , ""
  , "The difference between the means of {ax$tab_data$group[1]} and {ax$tab_data$group[2]} is {xx1$est.pe} standard deviations."
)

es_info$meaning_ord = c(
  "The correlation between group ({ax$tab_data$group[1]} vs. {ax$tab_data$group[2]}) and outcome is {xx1$est.pe}."
  , "The correlation between group ({ax$tab_data$group[1]} vs. {ax$tab_data$group[2]}) and outcome is {xx1$est.pe}, if we ignore all pairs with the same outcome."
  , "The probability of a better [variable] for {ax$tab_data$group[1]} vs. {ax$tab_data$group[2]} is {xx1$est.pe}."
  , "The probability of a better latent variable for [variable] for {ax$tab_data$group[1]} vs. {ax$tab_data$group[2]} is {xx1$est.pe}."
  , "The probability of a better [variable] for {ax$tab_data$group[1]} vs. {ax$tab_data$group[2]} is {xx1$est.pe}, if we ignore all pairs with the same outcome. Alternatively, the probability of a better latent variable for [variable] for {ax$tab_data$group[1]} vs. {ax$tab_data$group[2]} is {xx1$est.pe} (but using different assumptions than for MSS)."
  , "The ratio of the probability of a better [variable] for {ax$tab_data$group[1]} to the probability of a better [variable] for {ax$tab_data$group[2]} is {xx1$est.pe}."
  , "The odds of a better latent variable for [variable] for {ax$tab_data$group[1]} vs. {ax$tab_data$group[2]} is {xx1$est.pe}."
  , ""
  , ""
)

es_info$meaning_bin = c(
  "The correlation between group ({ax$tab_data$group[1]} vs. {ax$tab_data$group[2]}) and outcome is {xx1$est.pe}."
  , "The correlation between group ({ax$tab_data$group[1]} vs. {ax$tab_data$group[2]}) and outcome is {xx1$est.pe}, if we ignore all pairs with the same outcome."
  , "The probability is {xx1$est.pe} that the event happens for {ax$tab_data$group[1]} but not for {ax$tab_data$group[2]}."
  , "The probability is {xx1$est.pe} that the latent variable in favor of the event is greater for {ax$tab_data$group[1]} than for {ax$tab_data$group[2]}."
  , "The probability is {xx1$est.pe} that the event happens for {ax$tab_data$group[1]} but not for {ax$tab_data$group[2]}, if we ignore all pairs with the same outcome. Alternatively, the probability is {xx1$est.pe} that the latent variable in favor of the event is greater for {ax$tab_data$group[1]} than for {ax$tab_data$group[2]} (but using different assumptions than for MSS)."
  , "The odds ratio for {ax$tab_data$group[1]} vs. {ax$tab_data$group[2]} is {xx1$est.pe}."
  , "The odds is {xx1$est.pe} that the latent variable in favor of the event is greater for {ax$tab_data$group[1]} than for {ax$tab_data$group[2]}."
  , "Group size needs to be {xx1$est.pe} to have 1 more expected event in the {ax$tab_data$group[1]} group than in the {ax$tab_data$group[2]} group."
  , ""
)

es_info$other_names = c(
  "Glass rank-biserial correlation; net benefit; Cliff's delta; simple difference formula. SD = one of the two values of Somers' Delta. |SD| = Freeman's theta. For binary variables, SD = risk difference (RD), also known as excess risk, attributable risk, absolute risk increase / reduction (ARI / ARD)."
  , "Goodman and Kruskal's gamma. For binary variables, ASD = Yule's Q, also known as Yule coefficient of association."
  , "Common language effect size (CLES); Grissom and Kim's probability of superiority; stress-strength model; and reliability."
  , "Vargha and Delaney's A; nonoverlap of all pairs; probabilistic index; probability of a superior outcome; concordance index c; proportion of similar responses; relative effect. Closely related to the area under a receiver operating characteristic curve."
  , "-"
  , "Agresti's generalized odds ratio for stochastic dominance; win ratio. For binary variables, GOR = odds ratio (OR). For continuous variables, GOR = odds."
  , "-"
  , "-"
  , "Cohen's d"
)

usethis::use_data(es_info, internal = TRUE, overwrite = TRUE)
