library(magrittr)
library(assertthat)

comparison_data = list()

# Style: American Psychological Association (APA) 7th edition

## Continuous
# known parameter values
aa = tibble::tribble(
  ~ group, ~mu, ~sigma
  #-----------
  , "Men", 175.0, 7.1
  , "Women", 161.6, 6.5
) %>% as.data.frame()
attr(aa, "meta") = data.frame(
  About = "Stature (cm)"
  , Source = "Sperrin, M., Marshall, A. D., Higgins, V., Renehan, A. G., & Buchan, I. E. (2016). Body mass index relates weight to height differently in women and older adults: Serial cross-sectional surveys in England (1992-2011). Journal of Public Health, 38(3), 607-613. https://doi.org/10.1093/pubmed/fdv067"
  , Notes = "Reported Cohen's d = 1.97"
)
comparison_data$stature1 = aa

aa = tibble::tribble(
  ~ group, ~xbar, ~sd, ~n
  #-----------
  , "Men", 69.7, 2.8, 988
  , "Women", 64.3, 2.6, 1066
) %>% as.data.frame()
attr(aa, "meta") = data.frame(
  About = "Stature (inches)"
  , Source = "McGraw, K. O., & Wong, S. P. (1992). A common language effect size statistic. Psychological Bulletin, 111(2), 361-365. https://doi.org/10.1037/0033-2909.111.2.361; and Ellis, P. D. (2010). The Essential Guide to Effect Sizes: Statistical Power, Meta-Analysis, and the Interpretation of Research Results. Cambridge University Press. https://doi.org/10.1017/CBO9780511761676 (Box 1.3)"
  , Notes = "Reported D = 92%"
)
comparison_data$stature2 = aa

# better = greater
aa = tibble::tribble(
  ~ group, ~xbar, ~sd, ~n
  #-----------
  , "Vitamin D2", 13.7, 11.4, 17
  , "Vitamin D3", 23.3, 15.7, 55
) %>% as.data.frame()
attr(aa, "meta") = data.frame(
  About = "Increases in 25-hydroxyvitamin D concentrations due to vitamin D supplementation (nmol/L)"
  , Source = "Trang, H. M., Cole, D. E., Rubin, L. A., Pierratos, A., Siu, S., & Vieth, R. (1998). Evidence that vitamin D3 increases serum 25-hydroxyvitamin D more efficiently than does vitamin D2. The American Journal of Clinical Nutrition, 68(4), 854-858. https://doi.org/10.1093/ajcn/68.4.854"
  , Notes = "Reported p-value: 0.03"
)
comparison_data$Vitamin.D = aa

# better = less
aa = tibble::tribble(
  ~ group, ~xbar, ~sd, ~n
  #-----------
  , "Oat milk", -0.17, 0.59, 29
  , "Control drink", 0.19, 0.54, 23
) %>% as.data.frame()
attr(aa, "meta") = data.frame(
  About = "Changes in total cholesterol due to an oat milk drink (mmol/l)"
  , Source = "Onning, G., Wallmark, A., Persson, M., Akesson, B., Elmstahl, S., & Oste, R. (1999). Consumption of oat milk for 5 weeks lowers serum cholesterol and LDL cholesterol in free-living men with moderate hypercholesterolemia. Annals of Nutrition & Metabolism, 43(5), 301-309. https://doi.org/10.1159/000012798"
  , Notes = "Reported p-value: 0.005"
)
comparison_data$cholesterol = aa

# SEM
aa = tibble::tribble(
  ~ group, ~xbar, ~sem, ~n
  #-----------
  , "Male", 25.7, 1.5, 43
  , "Female", 32.4, 2.4, 29
) %>% as.data.frame()
attr(aa, "meta") = data.frame(
  About = "Serotonin metabolite (5HIAA) concentration in cisternal CSF (ng/ml)"
  , Source = "Young, S. N., Gauthier, S., Anderson, G. M., & Purdy, W. C. (1980). Tryptophan, 5-hydroxyindoleacetic acid and indoleacetic acid in human cerebrospinal fluid: Interrelationships and the influence of age, sex, epilepsy and anticonvulsant drugs. Journal of Neurology, Neurosurgery, and Psychiatry, 43(5), 438-445. https://doi.org/10.1136/jnnp.43.5.438"
  , Notes = "Reported p-value < 0.05. Reported Cohen's d = 0.58."
)
comparison_data$serotonin = aa

## Continuous - one-sample and paired
aa = tibble::tribble(
  ~ group, ~xbar, ~sd, ~n
  #-----------
  , "Stroke survivors", 61, 20.4, 111
  , "Normative value", 66.5, NA, NA
) %>% as.data.frame()
attr(aa, "meta") = data.frame(
  About = "36-Item Short-Form Health Survey (SF-36), mental health (MH)"
  , Source = "Cerniauskaite, M., Quintas, R., Koutsogeorgou, E., Meucci, P., Sattin, D., Leonardi, M., & Raggi, A. (2012). Quality-of-life and disability in patients with stroke. American Journal of Physical Medicine & Rehabilitation, 91(13 Suppl 1), S39-47. https://doi.org/10.1097/PHM.0b013e31823d4df7 (Table 1)"
  , Notes = "Reported p-value: 0.005"
)
comparison_data$stroke = aa

aa = tibble::tribble(
  ~ group, ~xbar, ~sd, ~n
  #-----------
  , "Increase in IOP", 2.2, 3, 29
) %>% as.data.frame()
attr(aa, "meta") = data.frame(
  About = "Increase in intraocular pressure (IOP) (mm Hg) due to weightlifting (mode II)"
  , Source = "Vieira, G. M., Oliveira, H. B., de Andrade, D. T., Bottaro, M., & Ritch, R. (2006). Intraocular pressure variation during weight lifting. Archives of Ophthalmology (Chicago, Ill.: 1960), 124(9), 1251-1254. https://doi.org/10.1001/archopht.124.9.1251 (Figure 3)"
  , Notes = "Reported p-value: < .001. IOP increased in 62% of subjects. IOP increased by > 5.0 in 21%."
)
comparison_data$IOP = aa

## Binary
# better = greater
aa = tibble::tribble(
  ~ group, ~k, ~n
  #-----------
  , "Amox-clav", 11, 13
  , "Amoxicillin", 2, 8
) %>% as.data.frame()
attr(aa, "meta") = data.frame(
  About = "Amoxicillin / clavulanic acid (amox-clav) vs amoxicillin alone for urinary tract infection (UTI)"
  , Source = "Martinelli, R., Lopes, A. A., de Oliveira, M. M., & Rocha, H. (1981). Amoxicillin-clavulanic acid in treatment of urinary tract infection due to gram-negative bacteria resistant to penicillin. Antimicrobial Agents and Chemotherapy, 20(6), 800-802. https://www.ncbi.nlm.nih.gov/pmc/articles/PMC181801/"
  , Notes = "Reported p-value < 0.05"
)
comparison_data$UTI = aa

# better = less
aa = tibble::tribble(
  ~ group, ~k, ~n
  #-----------
  , "Heparin", 1, 20
  , "Placebo", 8, 20
) %>% as.data.frame()
attr(aa, "meta") = data.frame(
  About = "Heparin for the prevention of deep vein thrombosis (DVT)"
  , Source = "Dechavanne, M., Ville, D., Viala, J. J., Kher, A., Faivre, J., Pousset, M. B., & Dejour, H. (1975). Controlled trial of platelet anti-aggregating agents and subcutaneous heparin in prevention of postoperative deep vein thrombosis in high risk patients. Haemostasis, 4(2), 94-100. https://doi.org/10.1159/000214092"
  , Notes = "Reported p-value < 0.025"
)
comparison_data$DVT = aa

aa = tibble::tribble(
  ~ group, ~k, ~n
  #-----------
  , "Aspirin", 104, 11037
  , "Placebo", 189, 11034
) %>% as.data.frame()
attr(aa, "meta") = data.frame(
  About = "Aspirin for the prevention of myocardial infarction (MI)"
  , Source = "Rosnow, R. L., & Rosenthal, R. (1989). Statistical procedures and the justification of knowledge in psychological science. American Psychologist, 44(10), 1276-1284. https://doi.org/10.1037/0003-066X.44.10.1276 (Table 2); and Ellis, P. D. (2010). The Essential Guide to Effect Sizes: Statistical Power, Meta-Analysis, and the Interpretation of Research Results. Cambridge University Press. https://doi.org/10.1017/CBO9780511761676 (Table 1.4)"
  , Notes = "Reported Pearson correlation coefficient r = 0.034. Based on binomial effect size display (BESD), reported percentage point reduction in heart attacks: 3.4 pp"
)
comparison_data$MI = aa

# SE
aa = tibble::tribble(
  ~ group, ~prop, ~se
  , "12 to 17", 14.3 / 100, 0.45 / 100
  , "18 to 25", 40.9 / 100, 0.71 / 100
) %>% as.data.frame()
attr(aa, "meta") = data.frame(
  About = "Illicit drug use"
  , Source = "Substance Abuse and Mental Health Services Administration. (2023). Key substance use and mental health indicators in the United States: Results from the 2022 National Survey on Drug Use and Health (HHS Publication No. PEP23-07-01-006, NSDUH Series H-58). Center for Behavioral Health Statistics and Quality, Substance Abuse and Mental Health Services Administration. https://www.samhsa.gov/data/report/2022-nsduh-annual-national-report (Table A.5B)"
)
comparison_data$drug.use = aa

## Ordinal
#
aa = tibble::tribble(
  ~ group, ~`Considerable improvement`, ~`Moderate improvement`, ~`No change`, ~`Moderate deterioration`, ~`Considerable deterioration`, ~`Death`
  #-----------
  , "Streptomycin", 28, 10, 2, 5, 6, 4
  , "Bed rest", 4, 13, 3, 12, 6, 14
) %>% as.data.frame()
assert_that( all(rowSums(aa[,-1]) == c(55, 52)) )
attr(aa, "meta") = data.frame(
  About = "Streptomycin for tuberculosis (TB)"
  , Source = "Streptomycin in Tuberculosis Trials Committee. (1948). Streptomycin Treatment of Pulmonary Tuberculosis. British Medical Journal, 2(4582), 769-782. https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2091872/"
  , Notes = "Reported p-value for 'Considerable improvement' < 1/million. Reported p-value for 'Death' < 0.01."
)
comparison_data$TB = aa

#
# aa = tibble::tribble(
#   ~group, ~None, ~`A-`, ~`A`, ~`A+`
#   #-----------
#   , "WDGS-CON", 289, 67, 41, 136
#   , "WDGS+MT", 436, 32, 18, 45
# ) %>% as.data.frame()
# assert_that( all(rowSums(aa[,-1]) == c(533, 531)) )
# attr(aa, "meta") = data.frame(
#   About = "Liver abscess scores using the Elanco scoring system"
#   , Source = "Meyer, N. F., Erickson, G. E., Klopfenstein, T. J., Benton, J. R., Luebbe, M. K., & Laudert, S. B. (2013). Effects of monensin and tylosin in finishing diets containing corn wet distillers grains with solubles with differing corn processing methods. Journal of Animal Science, 91(5), 2219-2228. https://doi.org/10.2527/jas.2011-4168 (Table 7)"
#   , Notes = "Reported p-value for total abscesses < 0.05; reduction in total abscesses 57.6%"
# )
# comparison_data$cattle = aa

#
aa = tibble::tribble(
  ~group, ~`Not enlarged`, ~Enlarged, ~`Greatly enlarged`
  #-----------
  , "Noncarriers", 497, 560, 269
  , "Carriers", 19, 29, 24
) %>% as.data.frame()
assert_that( all(rowSums(aa[,-1]) == c(1326, 72)) )
attr(aa, "meta") = data.frame(
  About = "Size of tonsils of carriers and noncarriers of Streptococcus pyogenes"
  , Source = "Agresti, A. (1980). Generalized Odds Ratios for Ordinal Data. Biometrics, 36(1), 59. https://doi.org/10.2307/2530495 (Table 3); and Holmes, M. C., & Williams, R. E. (1954). The distribution of carriers of Streptococcus pyogenes among 2,413 healthy children. The Journal of Hygiene, 52(2), 165-179. https://doi.org/10.1017/s0022172400027376"
  , Notes = "Reported GOR: 1.69, with a 95% CI of 1.13-2.53."
)
comparison_data$tonsils = aa

##
usethis::use_data(comparison_data, overwrite = TRUE)
