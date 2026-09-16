#' Show package options
#'
#' See [es4all-options] for a discussion of some of the options.
#'
#' @param sw  starting characters
#'
#' @return List of options and their values.
#' @family options
#' @export
#'
#' @examples
#' show_options()
show_options = function(sw = c("es4all", "astra")) {
  op = options()
  nm = names(op)
  idx = rep(FALSE, length(nm))
  for (ii in sw) {
    idx = idx | startsWith(nm, ii)
  }
  op[idx]
}
