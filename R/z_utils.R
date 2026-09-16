.my_set.seed = function() {
  seed = getOption("es4all.set.seed_seed")
  if (!is.na(seed)) {
    set.seed(seed)
  }
}

.mymatch = function(arg, table) {
  assert_that(is.string(arg), nzchar(arg), msg = "Must be a non-empty string.")
  table %<>% tolower
  idx = arg %>% tolower %>% pmatch(table)
  xx = glue_collapse(glue('"{table}"'), sep = ", ", last = ", or ")
  assert_that(noNA(idx), msg = glue('Unknown value: "{arg}". Must be one of: {xx}.'))
  table[idx]
}


# show_options() |> names() |> dput()
#### !!! If making changes, update: .onLoad(), set_opts(), show_opts(), .check_options()
.check_options = function() {
  c_need = c("astra.file", "astra.file_show", "astra.print", "es4all.conf.level",
             "es4all.flip", "es4all.max_ndraws_D", "es4all.n_D_tail", "es4all.np_max_nr",
             "es4all.np_nr", "es4all.np_signif_digits", "es4all.output_es",
             "es4all.output_raw", "es4all.set.seed_seed")
  c_have = show_options() %>% names()

  d_nh = setdiff(c_need, c_have)
  if (length(d_nh) > 0) {
    warning(glue("Certain package options have not been set. Try library(es4all). "
                 , "Missing options: ", glue_collapse(d_nh, sep = ", ")))
  }

  d_hn = setdiff(c_have, c_need)
  if (length(d_hn) > 0) {
    warning(glue("Certain unnecessary options have been set. "
                 , "You could be setting an option that is no longer required "
                 , "in the current version of es4all. "
                 , "Unnecessary options: ", glue_collapse(d_hn, sep = ", ")))
  }
}
