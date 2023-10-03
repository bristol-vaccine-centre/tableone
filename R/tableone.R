
# disable messages with option
.message = function(...) {
  if (!getOption("tableone.quiet",FALSE)) message(...)
}

#' Get footer text if available
#'
#' The functions in `tableone` will record the methods used for reporting in a
#' scientific paper. This is both for normality assumption tests and for
#' significance tests.
#'
#' @param df_output a data frame that is the output of a `tableone` function
#'
#' @return the footnotes if they exist as a list (NULL otherwise)
#' @export
#'
#' @examples
#' iris %>% describe_population(tidyselect::everything()) %>% get_footer_text()
#' iris %>% dplyr::group_by(Species) %>%
#'   compare_population(tidyselect::everything()) %>% get_footer_text()
get_footer_text = function(df_output) {
  tmp = df_output %>% attributes()
  tmp$methods
}




