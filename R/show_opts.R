#' @rdname set_opts
#' @export
show_opts = function() {

  #### !!! If making changes, update: .onLoad(), set_opts(), show_opts(), .check_options()

  conf.level = getOption("es4all.conf.level")
  assert_that(conf.level > 0.5, conf.level < 1)
  message(glue("* Level of confidence interval = {conf.level} (significance level = {1 - conf.level})."))
  if (!(conf.level %in% c(0.9, 0.95, 0.99))) {
    message("* Unusual confidence interval level.")
  }

  es = getOption("es4all.output_es")
  if (length(es) == 1 && is.na(es)) {
    message("* Effect size: print all effect sizes.")
  } else if (is.character(es)) {
    assert_that(all(es %in% c("SD", "ASD", "NNT", "D", "MSS", "AD", "GOR", "WO", "SMD"))
                , msg = "* Unknown effect size selected.")
    message(glue("* Effect size: print the following effect sizes: {glue_collapse(es, sep = ', ')}."))
  } else {
    stop("* Unknown value of `es`.")
  }

  xx = getOption("astra.print")
  assert_that(is.string(xx), nzchar(xx))
  .astra_print_info(print = xx)$message %>% message

  raw = getOption("es4all.output_raw")
  assert_that(is.flag(raw))
  message(glue("* Raw output: {raw}."))

  .check_options()
  invisible(NULL)
}
