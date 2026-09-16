.finalize_ep_tab = function(obj) {
  assert_that(is.list(obj)
              , names(obj) %>% setequal( c("rmse_mle", "rmse_npe", "coverage", "power", "fpr", "x") ))

  ##
  type = NULL
  str_param = if(setequal(names(obj$x), c("mu", "sigma", "n"))) {
    type = "cont"
    glue("$\\mu_{1:nrow(obj$x)} = {obj$x$mu}$, \\
      $\\sigma_{1:nrow(obj$x)} = {obj$x$sigma}$, \\
      $n_{1:nrow(obj$x)} = {obj$x$n}$") %>% paste(collapse = "; ")
  } else if (setequal(names(obj$x), c("p", "n"))) {
    type = "bin"
    glue("$\\pi_{1:nrow(obj$x)} = {obj$x$p}$, $n_{1:nrow(obj$x)} = {obj$x$n}$") %>% paste(collapse = "; ")
  } else {
    stop("Unknown parameters.")
  }
  out = list(Parameters = data.frame(Value = str_param))

  ##
  .get_rmse = function(rmse) {
    idx = which(rmse$name == "SD")
    assert_that(length(idx) == 1)
    r1 = rmse[idx,]
    r1[,-1] %<>% round(3)
    r1
  }
  rmse_mle = .get_rmse(obj$rmse_mle)
  rmse_npe = .get_rmse(obj$rmse_npe)
  out$`RMSE and coverage` = data.frame(Value =
      paste("| SD | RMSE (MLE) | RMSE (NPE) | Coverage (MLE) | Coverage (NPE) |"
      , "|---|---|---|---|---|"
      , "| {rmse_mle$value %>% .pp_r()} | {rmse_mle$rmse %>% .pp_r()} | {rmse_npe$rmse %>% .pp_r()} | {obj$coverage$mle %>% .pp_p1()} | {obj$coverage$npe %>% .pp_p1()} | " %>% glue()
      , sep = "\n") )

  ##
  if (type == "cont") {
    assert_that(all(obj$power %>% names == c("mle", "npe", "Welch's t-test", "Student's t-test", "WMW test") ))
    out$Power = data.frame(Value = paste("| MLE | NPE | Welch's t | Student's t | WMW |"
        , "|---|---|---|---|---|"
        , "|{obj$power %>% unlist %>% .pp_p1() %>% paste(collapse = '|')}|" %>% glue()
        , sep = "\n"))
    out$FPR = data.frame(Value = paste("| MLE | NPE | Welch's t | Student's t | WMW |"
        , "|---|---|---|---|---|"
        , "|{obj$fpr %>% unlist %>% .pp_p1() %>% paste(collapse = '|')}|" %>% glue()
        , sep = "\n"))
  } else if (type == "bin") {
    assert_that(all(obj$power %>% names == c("mle", "npe", "Fisher's exact test", "Pearson's chi-squared test")))
    out$Power = data.frame(Value = paste("| MLE | NPE | Fisher's exact | Pearson's chi-squared |"
        , "|---|---|---|---|"
        , "|{obj$power %>% unlist %>% .pp_p1() %>% paste(collapse = '|')}|" %>% glue()
        , sep = "\n"))
    out$FPR = data.frame(Value = paste("| MLE | NPE | Fisher's exact | Pearson's chi-squared |"
        , "|---|---|---|---|"
        , "|{obj$fpr %>% unlist %>% .pp_p1() %>% paste(collapse = '|')}|" %>% glue()
        , sep = "\n"))
  } else {
    stop("Unknown data.")
  }

  ##
  for (nn in names(out)) {
    class( out[[nn]] ) = c("astra_table", "data.frame")
    attr( out[[nn]], "title") = nn
  }
  class(out) = c("astra_list", "list")

  out
}
