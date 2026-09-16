#' Set certain options
#'
#' `set_opts()` sets certain package options. To view these options, use `show_opts()`.
#' For more advanced control and detailed customization, experienced  users can
#' also employ [options()] and [show_options()] (refer to [es4all-options]
#' for further information).
#'
#' If you are not setting a particular option, leave it as `NULL`.
#'
#' If `es = NA`, print all effect sizes. If `es` is a vector of
#' short effect size names, print just those effect sizes. Possible values are:
#' `"SD", "ASD", "NNT", "D", "MSS", "AD", "GOR", "WO", "SMD"`.
#'
#' `output` determines how the output is printed:
#'
#' * `"auto"` (default): automatically select the table-making package, depending on the
#' destination (such as screen, HTML, or PDF / LaTeX).
#' * `"huxtable"`, `"gt"`, `"kableExtra"`, `"flextable"`: use this table-making package. Be sure
#' that this package is installed.
#' * `"screen"`: print plain-text tables to the screen.
#' * `"Excel"`: print to an Excel workbook. Please specify the name of an Excel
#' file using the `file` argument. Before using Excel printing, please be sure to install
#' the `openxlsx2` package.
#' * `"Word"`: print to a Word document. Please specify the name of a Word
#' file using the `file` argument. Before using Word printing, please be sure to install these
#' packages: `flextable` and `officer`.
#' * `"CSV"`: print to a comma-separated values (CSV) file. Please specify the name of a
#' CSV file using the `file` argument.
#'
#' @param reset reset all options to their default values?
#' @param conf.level confidence level of the interval.
#' @param es which effect sizes to print? `NA` (the default, prints all), or a
#' vector of short effect size names. See details.
#' @param output specify how the output is printed: `"auto"` (default); `"huxtable"`, `"gt"`,
#' `"kableExtra"`, `"flextable"`; `"screen"`. For the following output types, please
#' also specify the `file` argument: `"Excel"`, `"Word"`, `"CSV"`.
#' @param file file name (see `output`).
#' @param .file_temp place `file` in a temporary folder?
#' @param raw if `TRUE`, `d_*()` functions return the raw intermediate
#' `data.frame` instead of the formatted output. Default: `FALSE`.
#'
#' @return (Nothing.)
#'
#' @family options
#' @family print
#'
#' @export
#'
#' @examples
#' set_opts(es = NA)
set_opts = function(
    reset = NULL
    , conf.level = NULL
    , es = NULL
    , output = NULL
    , file = NULL
    , .file_temp = NULL
    , raw = NULL
) {
  #### !!! If making changes, update: .onLoad(), set_opts(), show_opts(), .check_options()

  ## Reset has to go ahead of the other options
  if (!is.null(reset)) {
    assert_that(is.flag(reset), reset %in% c(TRUE, FALSE))
    if (reset) {
      message("* Resetting all options to their default values.")
      .onLoad()
    }
  }

  ##
  if (!is.null(conf.level)) {
    assert_that(conf.level > 0.5, conf.level < 1)
    message(glue("* Level of confidence interval = {conf.level} (significance level = {1 - conf.level})."))
    options(es4all.conf.level = conf.level)
    if (!(conf.level %in% c(0.9, 0.95, 0.99))) {
      message("* Unusual confidence interval level.")
    }
  }

  ##
  if (!is.null(es)) {
    if (length(es) == 1 && is.na(es)) {
      message("* Effect size: print all effect sizes.")
    } else if (is.character(es)) {
      assert_that(all(es %in% c("SD", "ASD", "NNT", "D", "MSS", "AD", "GOR", "WO", "SMD"))
                  , msg = "* Unknown effect size selected.")
      message(glue("* Effect size: print the following effect sizes: {glue_collapse(es, sep = ', ')}."))
    } else {
      stop("* Unknown value of `es`.")
    }
    options(es4all.output_es = es)
  }

  ##
  print_final = getOption("astra.print")
  if (is.null(print_final)) {
    print_final = ".print_auto"
  }
  if (!is.null(output)) {
    output %<>% .mymatch(c("huxtable", "gt", "kableExtra", "flextable"
                           , "auto", "screen"
                           , "excel", "word", "csv"))
    print_final = .astra_print_for_output(output)

    if (output == "auto") {
      message("* Printing with huxtable for screen, gt for HTML, or kableExtra for PDF.")
      options(astra.print = ".print_auto"
              , astra.file = "", astra.file_show = "")
    } else if (output %in% c("huxtable", "gt", "kableextra", "flextable") ) {
      message(glue("* Printing with {output}."))
      options(astra.print = .astra_print_for_output(output)
              , astra.file = "", astra.file_show = "")
    } else if (output == "screen") {
      message("* Printing to the screen.")
      options(astra.print = ".print_screen"
              , astra.file = "", astra.file_show = "")
    } else if (output %in% c("excel", "word", "csv")) {
      .set_output_file(output = output, file = file, .file_temp = .file_temp)
    }
  }

  ##
  if (!is.null(raw)) {
    assert_that(is.flag(raw), raw %in% c(TRUE, FALSE))
    message(glue("* Raw output: {raw}."))
    options(es4all.output_raw = raw)
  }

  .check_options()
  invisible(NULL)
}

.set_output_file = function(output, file, .file_temp) {
  assert_that(output %in% c("excel", "word", "csv"))
  type = switch(output
                , excel = "Excel"
                , word = "Word"
                , csv = "CSV")
  extension = switch(output
                     , excel = ".xlsx"
                     , word = ".docx"
                     , csv = ".csv")
  assert_that(is.string(file), nzchar(file)
              , msg = glue("For {type} printing, please specify a file name using the file argument."))
  if (!endsWith(tolower(file), extension)) {
    file = glue("{file}{extension}")
  }
  if (!isTRUE(.file_temp)) {
    file %<>% normalizePath(mustWork = FALSE)
    file_show = file
  } else {
    file = file.path(tempdir(), file)
    file_show = file %>% basename()
  }
  message(glue("* Printing to {type} file {file_show}."))
  if (file.exists(file)) {
    message("* NOTE: file already exists!")
  }
  options(astra.print = .astra_print_for_output(output)
          , astra.file = file, astra.file_show = file_show)
}
