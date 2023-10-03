#' Describe the data types and consistence
#'
#' The population description is a simple summary of the co-variates in a data set
#' with no reference to outcome, and not comparing intervention (although it might
#' contain intervention rates.) It will report summary statistics for continuous
#' and counts for categorical data,
#'
#' @inheritParams as_t1_shape
#' @inheritParams as_huxtable.t1_shape
#' @inheritParams compare_population
#'
#' @return a `huxtable` formatted table.
#' @export
#'
#' @examples
#' # Overriding the heuristics is possible:
#' iris %>% describe_data(tidyselect::everything())
#'
#' diamonds %>% dplyr::group_by(is_colored) %>% describe_data(tidyselect::everything())
#'
describe_data = function(
    df,
    ...,
    label_fn = label_extractor(df),
    units = extract_units(df),
    layout = "single",
    font_size = getOption("tableone.font_size",8),
    font = getOption("tableone.font","Arial"),
    footer_text = NULL,
    raw_output = FALSE
) {
  shape = as_t1_shape(df, ...,
                          label_fn = label_fn,
                          units=units
  )

  if (raw_output) return(shape)

  shape %>% as_huxtable.t1_shape(
    font_size = font_size,
    font = font,
    footer_text = footer_text
  )

}


#' Summarise a data set
#'
#' The data set description is a simple summary of the data formats, types and missingness
#'
#' @param df a dataframe of individual observations. Grouping, if present, is ignored.
#'  (n.b. if you wanted to construct multiple summary tables a [dplyr::group_map()] call
#'  could be used)
#' @param ... the columns of variables we wish to summarise. This can be given as
#'  a `tidyselect` specification (see \code{utils::vignette("syntax", package = "tidyselect")}),
#'  identifying the columns. Alternatively it can be given as a formula of the nature
#'
#'  `outcome ~ intervention + covariate_1 + covariate_2 + ...` .
#'
#'  which may be more convenient if you are going on to do a model fit. If the
#'  latter format the left hand side is ignored (outcomes are not usual in
#'  this kind of table).
#' @param label_fn (optional) a function for mapping a co-variate column name to
#'   printable label. This is by default a no-operation and the output table
#'   will contain the dataframe column names as labels. A simple alternative
#'   would be some form of [dplyr::case_when] lookup, or a string function such
#'   as [stringr::str_to_sentence]. (N.b. this function must be vectorised).
#'   Any value provided here will be overridden by the
#'   `options("tableone.labeller" = my_label_fn)` which allows global setting of
#'   the labeller.
#' @param units (optional) a named list of units, following a `c(<colname_1> =
#'   "<unit_1>", <colname_2> = "<unit_2>", ...)` format. columns not present in
#'   this list are assumed to have no units. Units may be involved in the
#'   formatting of the summary output.
#'
#' @return a `t1_shape` data frame.
#' @export
#' @examples
#' tmp = iris %>% as_t1_shape(
#'   tidyselect::everything()
#' )
as_t1_shape = function(
    df,
    ...,
    label_fn = label_extractor(df),
    units = extract_units(df)
) {

  cols = .parse_vars(df, ...)
  label_fn = purrr::as_mapper(label_fn)
  shape = .get_shape(df,cols,label_fn,units)
  return(shape)
}

#' Convert a `t1_summary` object to a `huxtable`
#'
#' @param x the `t1_summary` object as produced by describe_population
#' @param ... not used
#' @inheritParams as_huxtable.t1_signif
#'
#' @importFrom huxtable as_huxtable
#'
#' @return a formatted table as a `huxtable`
#' @export
as_huxtable.t1_shape = function(
    x,
    ...,
    # layout = "single",
    font_size = getOption("tableone.font_size",8),
    font = getOption("tableone.font","Arial"),
    footer_text = NULL,
    show_binary_value=NULL
) {
  # if (is.list(layout)) {
  #   format = layout
  # } else {
  #   format = getOption("tableone.format_list", default.format[[layout]])
  # }

  grps = x %>% dplyr::groups()

  variable_col = as.symbol(getOption("tableone.variable_column_name","Variable"))
  fmt = .format_shape(x) #, format=format)

  fmt = fmt %>% dplyr::rename(
    !!variable_col := variable
  )

  hux = fmt %>% .hux_tidy(
    rowGroupVars = dplyr::vars(!!variable_col, `Values / Units`),
    colGroupVars = dplyr::vars(),
    defaultFontSize= font_size,
    defaultFont = font,
    displayRedundantColumnNames=FALSE)

  tmp = get_footer_text(x)
  footer = footer_text
  if (any(x$.type == "continuous")) {
    footer = c(
      .describe_normality_test(),
      footer
    )
  }
  if (!getOption("tableone.hide_footer",isFALSE(footer_text))) {
    hux = hux %>%
      huxtable::insert_row(paste0(footer,collapse="\n"), after=nrow(hux), colspan = ncol(hux), fill="") %>%
      huxtable::set_bottom_border(row=huxtable::final(),value=0)
  }
  return(hux)
}




.format_shape = function(df_shape) { #, format) {
  # df_shape %>%
  df_shape %>% dplyr::transmute(
    variable = .label,
    `Values / Units` = .maybe(dplyr::if_else(
      is.na(.levels),
      .unit,
      purrr::map_chr(.level_names, ~ glue::glue_collapse(.x %||% "", sep = ", ", last=" or "))
    )),
    `Type` = .type,
    `Normally distributed` = .is_normal,
    `Unique (%)` = sprintf("%1.1f%%", (1-.p_ties)*100),
    `Missing (%)` = sprintf("%1.1f%%", .p_missing*100),
    `Default summary` = .summary_type,
    `Comparable groups (n)` = .comparisons,
    `Default test` = .comparison_method,
  )

}
