.onLoad = function(libname, pkgname) {
  #### !!! If making changes, update: .onLoad(), set_opts(), show_opts(), .check_options()
  options(
    es4all.flip = TRUE

    ## astra print options
    , astra.print = ".print_auto"
    , astra.file = ""
    , astra.file_show = ""

    ## CI
    , es4all.conf.level = 0.95
    , es4all.max_ndraws_D = 10e3
    , es4all.n_D_tail = 50

    ## NPE
    , es4all.np_signif_digits = 4
    , es4all.np_max_nr = 12e3
    , es4all.np_nr = 10e3

    ##
    , es4all.set.seed_seed = 42 # or NA

    ##
    , es4all.output_es = NA # NA or a vector of short ES names
    , es4all.output_raw = FALSE
  )
}

