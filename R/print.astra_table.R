#' Print astra tables
#'
#' @param x an object of class `astra_table` or `astra_list`.
#' @param ... passed to helper functions.
#'
#' @return
#' Returns `x` invisibly.
#'
#' @family print
#'
#' @order 1
#' @export
#'
#' @examples
#' print( d_bin(comparison_data$UTI) )
print.astra_table = function(x, ...) {
  getOption("astra.print") %>% do.call( list(x, ...) )
  invisible(x)
}

#' @rdname print.astra_table
#' @order 2
#' @export
print.astra_list = print.astra_table
