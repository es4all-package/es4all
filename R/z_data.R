#' Comparison data
#'
#' A list of data frames, each describing data that can be used in comparisons.
#' See `attr(*, "meta")$Source` of each data frame for its source.
#'
#' @examples
#' # Metadata for one of the data frames
#' print( attr(comparison_data[[1]], "meta") )
#'
#' # Summary of all available data
#' ret = NULL
#' for (nn in names(comparison_data)) {
#'   r1 = data.frame(Label = nn
#'     , About = strtrim( attr(comparison_data[[nn]], "meta")$About, 25)
#'     , `Group 1` = comparison_data[[nn]]$group[1]
#'     , `Group 2` = comparison_data[[nn]]$group[2]
#'     , Source = strtrim( attr(comparison_data[[nn]], "meta")$Source, 25)
#'     , check.names = FALSE)
#'   ret = rbind(ret, r1)
#' }
#' print(ret)
"comparison_data"
