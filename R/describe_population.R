#' Describe the population in a summary table
#'
#' The population description is a simple summary of the co-variates in a data set
#' with no reference to outcome, and not comparing intervention (although it might
#' contain intervention rates.) It will report summary statistics for continuous
#' and counts for categorical data,
#'
#' @inheritParams as_t1_summary
#' @inheritParams as_huxtable.t1_signif
#' @inheritParams compare_population
#'
#' @return a `huxtable` formatted table.
#' @export
#'
#' @examples
#' # the heuristics detect that Petals in the iris data set are not normally
#' # distributed and hence report median and IQR:
#' iris %>% describe_population(tidyselect::everything())
#'
#' # Overriding the heuristics is possible:
#' iris %>% describe_population(
#'   tidyselect::everything(),
#'   override_type = c(Petal.Length = "mean_sd", Petal.Width = "mean_sd")
#' )
#'
#' # The counts sometimes seem redundant if there is no missing information:
#' diamonds %>% describe_population(tidyselect::everything())
#'
#' # however in a data set with missing values the denominators are important:
#' missing_diamonds %>% describe_population(tidyselect::everything())
#'
#' # for factor levels we can make the missing values more explicit
#' missing_diamonds %>% explicit_na() %>%
#'   describe_population(tidyselect::everything())
#'
#' # in the output above the price variable is not # presented the way we would
#' # like so here we override the number of decimal places shown for the price
#' # variable while we are at it we will use a mid point for the decimal point,
#' # and make the variable labels sentence case.
#'
#' old = options("tableone.dp"="\u00B7")
#' missing_diamonds %>%
#'   explicit_na() %>%
#'   describe_population(
#'     tidyselect::everything(),
#'     label_fn=stringr::str_to_sentence,
#'     override_real_dp=list(price=6)
#'   )
#' options(old)
describe_population = function(
    df,
    ...,
    label_fn = label_extractor(df),
    units = extract_units(df),
    override_type = list(),
    layout = "single",
    override_percent_dp = list(),
    override_real_dp = list(),
    font_size = getOption("tableone.font_size",8),
    font = getOption("tableone.font","Arial"),
    footer_text = NULL,
    show_binary_value=NULL,
    raw_output = FALSE
) {
  summary = as_t1_summary(df, ...,
                          label_fn = label_fn,
                          units=units,
                          override_type = override_type
  )

  if (raw_output) return(summary)

  summary %>% as_huxtable.t1_summary(
    layout = layout,
    override_percent_dp = override_percent_dp,
    override_real_dp = override_real_dp,
    font_size = font_size,
    font = font,
    footer_text = footer_text,
    show_binary_value=show_binary_value,
  )

}


#' Summarise a population
#'
#' The population description is a simple summary of the co-variates in a data set
#' with no reference to outcome, and not comparing intervention (although it might
#' contain intervention rates.) It will report summary statistics for continuous
#' and counts for categorical data,
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
#' @param override_type (optional) a named list of data summary types. The
#'   default type for a column in a data set are calculated using heurisitics
#'   depending on the nature of the data (categorical or continuous), and result
#'   of normality tests. if you want to override this the options are
#'   `r paste0("\"", names(tableone:::.summary_types), "\"", collapse= ",")` and you
#'   specify this on a column by column bases with a named list (e.g
#'   `c("Petal.Width"="mean_sd")`). Overriding the default does not check the
#'   type of data is correct for the summary type and will potentially cause
#'   errors if this is not done correctly.
#'
#' @return a `t1_summary` data frame.
#' @export
#' @examples
#' tmp = iris %>% as_t1_summary(
#'   tidyselect::everything(),
#'   override_type = c(Petal.Length = "mean_sd", Petal.Width = "mean_sd")
#' )
#'
as_t1_summary = function(
    df,
    ...,
    label_fn = label_extractor(df),
    units = extract_units(df),
    #TODO: implement override_type the same way as units have been done
    override_type = list()
) {

  cols = .parse_vars(df, ...)
  label_fn = purrr::as_mapper(label_fn)
  shape = .get_shape(df,cols,label_fn,units)
  summary = .summary_stats(shape, override_type)
  return(summary)
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
as_huxtable.t1_summary = function(
    x,
    ...,
    layout = "single",
    override_percent_dp = list(),
    override_real_dp = list(),
    font_size = getOption("tableone.font_size",8),
    font = getOption("tableone.font","Arial"),
    footer_text = NULL,
    show_binary_value=NULL
) {
  if (is.list(layout)) {
    format = layout
  } else {
    format = getOption("tableone.format_list", default.format[[layout]])
  }

  grps = x %>% dplyr::groups()

  variable_col = as.symbol(getOption("tableone.variable_column_name","Variable"))
  characteristic_col = as.symbol(getOption("tableone.characteristic_column_name","Characteristic"))
  fmt = .format_summary(x, format=format, override_percent_dp = override_percent_dp, override_real_dp=override_real_dp, show_binary_value=show_binary_value)

  fmt = fmt %>% dplyr::rename(
    !!variable_col := variable,
    !!characteristic_col := characteristic
  )

  hux = fmt %>% .hux_tidy(
    rowGroupVars = dplyr::vars(!!variable_col, !!characteristic_col),
    colGroupVars = dplyr::vars(!!!grps,.tbl_col_name),
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

# print the summary stats for each of the columns of a dataframe, into
# a prettified table with control for layout as defined in default.format
# this can be overwritten in "tableone.format_list" option
# df_shape = diamonds %>%  dplyr::mutate(is_clear = ifelse(clarity>"VS2","clear","less clear")) %>% dplyr::group_by(is_clear) %>% .get_shape()
# df_summary = df_shape %>% .summary_stats()
# df_summary %>% .format_summary()
.format_summary = function(df_summary, format, override_percent_dp = list(), override_real_dp = list(), show_binary_value=NULL ) {

  override_percent_dp = as.list(override_percent_dp)
  override_real_dp = as.list(override_real_dp)

  if (!".glue" %in% colnames(df_summary)) {
    df_summary = df_summary %>% dplyr::mutate(
      .glue = format[.summary_type]
    )
  }

  # Override decimal points
  if (length(override_percent_dp) > 0) {
    override = tibble::tibble(
      .name = names(override_percent_dp),
      .percent_dp = unlist(override_percent_dp)
    )
    jc = if(is.null(names(override_percent_dp))) character() else ".name"
    df_summary = df_summary %>% dplyr::left_join(override, by=jc)
  } else {
    df_summary = df_summary %>% dplyr::mutate(.percent_dp = NA)
  }

  if (length(override_real_dp) > 0) {
    override = tibble::tibble(
      .name = names(override_real_dp),
      .real_dp = unlist(override_real_dp)
    )
    jc = if(is.null(names(override_real_dp))) character() else ".name"
    df_summary = df_summary %>% dplyr::left_join(override, by=jc)
  } else {
    df_summary = df_summary %>% dplyr::mutate(.real_dp = NA)
  }

  # why does the R world have such a dim view of loops
  # This is basically much more tractable than the equivalent in
  # map / lapply madness and worked immediately.
  df_summary = df_summary %>% dplyr::mutate(.labelled_data = list(rep(tibble::tibble(),nrow(df_summary))))

  for (i in 1:nrow(df_summary)) {
    df_row = df_summary %>% dplyr::filter(dplyr::row_number() == i)

    grps = df_row$.summary_data[[1]] %>% dplyr::groups()
    data = df_row %>% tidyr::unnest(.summary_data)
    data = data %>% dplyr::group_by(!!!grps) %>%
      dplyr::rename(
        name = .name,
        label = .label,
        unit = .unit,
        type = .type
      )

    # at this point data should be one row per intervention group and level in the data
    # with all the stats on a given intervention / level combination for a specific
    # variable (defined by the row of the df_summary). The precise list of
    # things available to the glue spec is dependent on the nature of the variable
    glue = df_row$.glue[[1]]

    # firstly handle characteristic column. This is in every definition as is the
    # effective table row label. We might expect `{unit}` in here but mostly this
    # will be static text, or the `{level}` name for categorical data. This should
    # be the same across intervention groups but probably different between levels.
    thisglue = glue[["characteristic"]]
    # TODO: here is where "characteristic" gets given its name. Probably easier to rename
    # this later when printed.
    named_data = data %>% dplyr::mutate(characteristic = glue::glue(thisglue))
    outdata = named_data %>% dplyr::filter(FALSE)

    # for the remaining formatted columns in our definition we are after a long format:
    order3 = 1
    for (newcol in setdiff(names(glue),"characteristic")) {

      # newcol is potentially a glue spec. This is possibly going to be different
      # for different interventions (e.g. might reference `N` as total in each)
      # intervention, or even ({N}/{N_total}) for example. It should not reference
      # anything that changes on a variable level or the columns will be different
      # on a variable by variable basis (which is not what is generally wanted)
      # the intervention group itself will be a heading above this one.
      thisglue = glue[[newcol]]
      thisglue = .adjust_fmt(thisglue, percent = df_row$.percent_dp[[1]], real = df_row$.real_dp[[1]])

      # we want a long format tibble now with
      # .tbl_col_name and .tbl_col_value
      tmp = named_data %>% dplyr::mutate(
        .tbl_col_name = as.character(glue::glue_data(df_row, newcol)),
        .tbl_col_value = glue::glue(thisglue),
        .order3 = order3
      )
      outdata = dplyr::bind_rows(outdata, tmp)

      order3 = order3+1
    }

    # and we nest this back one item at a time. this col will now be a list of
    # tibbles grouped by intervention as before
    if (
      all(outdata$type == "categorical") &&
      length(levels(outdata$level)) == 2 &&
      any(show_binary_value %in% levels(outdata$level))
    ) {
      outdata = outdata %>% dplyr::filter(level %in% show_binary_value)
    }
    df_summary$.labelled_data[[i]] = outdata %>% dplyr::select(!!!grps, characteristic, .tbl_col_name, .tbl_col_value, .order2, .order3)
  }

  tmp = suppressMessages(
    df_summary %>%
      # TODO: here is where "variable" gets given its name. Probably easier to rename
      # this later when printed
      dplyr::select(variable = .label, .order, .labelled_data) %>%
      tidyr::unnest(.labelled_data) %>%
      dplyr::relocate(!!!grps) %>%
      dplyr::arrange(!!!grps, .order, .order2, .order3) %>%
      dplyr::select(-.order,-.order2,-.order3)
  )
  return(structure(tmp, methods = get_footer_text(df_summary)))

}

