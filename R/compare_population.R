#' Compares the population against an intervention in a summary table
#'
#' The population comparison is a summary of the co-variates in a data set with
#' no reference to outcome, but comparing intervention groups. It will report
#' summary statistics for continuous and counts for categorical data, for each
#' of the intervention groups, and reports on the significance of the
#' association in relation to the intervention groups. It gives a clear summary
#' of whether data is correlated to intervention.
#'
#' @inheritParams as_t1_signif
#' @inheritParams as_huxtable.t1_signif
#' @param raw_output return comparison as `t1_signif` dataframe rather than formatted table
#'
#' @return a `huxtable` formatted table.
#' @export
#'
#' @examples
#' # the heuristics detect that Petals in the iris data set are not normally
#' # distributed and hence report median and IQR:
#' iris %>% dplyr::group_by(Species) %>% compare_population(tidyselect::everything())
#'
#' # Missing data
#' old = options("tableone.show_pvalue_method"=FALSE)
#' missing_diamonds %>%
#'   dplyr::group_by(is_colored) %>%
#'   compare_population(-color, layout="relaxed")
#'
#' tmp = missing_diamonds %>% explicit_na() %>% dplyr::group_by(is_colored)
#' tmp %>% compare_population(-color,
#'     footer_text = c(
#'       "IQR: Interquartile range; CI: Confidence interval",
#'       "Line two")
#'     )
#'
#' options(old)
compare_population = function(
    df,
    ...,
    label_fn = label_extractor(df),
    units = extract_units(df),
    override_type = list(),
    override_method = list(),
    layout = "compact",
    override_percent_dp = list(),
    override_real_dp = list(),
    p_format = names(.pvalue.defaults),
    font_size = getOption("tableone.font_size",8),
    font = getOption("tableone.font","Arial"),
    footer_text = NULL,
    show_binary_value = NULL,
    raw_output = FALSE
) {
  sign = as_t1_signif(
    df, ...,
    label_fn = label_fn,
    units = units,
    override_type = override_type,
    override_method = override_method
  )
  if (raw_output) return(sign)
  as_huxtable.t1_signif(sign, layout = layout,
                        override_percent_dp = override_percent_dp,
                        override_real_dp=override_real_dp,
                        p_format = p_format,
                        font_size = font_size,
                        font = font,
                        footer_text = footer_text,
                        show_binary_value = show_binary_value
  )
}

#' Compares multiple outcomes against an intervention in a summary table
#'
#' The outcome table is a simple summary of a binary or categorical outcome
#' in a data set compared by intervention groups. The comparison is independent
#' of any covariates, and is a preliminary output prior to more formal statistical
#' analysis or model fitting.
#'
#' It reports summary counts for the outcomes and a measure of significance of
#' the relationship between outcome and intervention. Interpretation of
#' significance tests, should include Bonferroni adjustment.
#'
#' @inheritParams compare_population
#' @param ... the outcomes are specified either as a `tidyselect` specification,
#'   in which case the grouping of the `df` input determines the intervention
#'   and the output is the same as a `compare_population()` call with a tidyselect.
#'   Alternatively a set of formulae can be provided that specify the outcomes
#'   on the left hand side, e.g. `outcome1 ~ intervention + cov1, outcome2 ~
#'   intervention + cov1, ...` in this case the `intervention` must be the same
#'   for all formulae and used to determine the comparison groups.
#'
#' @return a `huxtable` formatted table.
#' @export
compare_outcomes = function(df,
                            ...,
                            label_fn = label_extractor(df),
                            units = extract_units(df),
                            override_type = list(),
                            layout = "compact",
                            override_percent_dp = list(),
                            override_real_dp = list(),
                            p_format = names(.pvalue.defaults),
                            font_size = getOption("tableone.font_size",8),
                            font = getOption("tableone.font","Arial"),
                            footer_text = NULL,
                            show_binary_value = NULL,
                            raw_output = FALSE
) {
  if (!.is_formula_interface(...)) {
    compare_population(df, ..., label_fn = label_fn, units = units,
                       override_type = override_type, layout = layout, override_percent_dp = override_percent_dp,
                       override_real_dp = override_real_dp, p_format = p_format, font_size = font_size,
                       font = font, footer_text = footer_text, show_binary_value = show_binary_value,
                       raw_output = raw_output
    )
  } else {
    rhs = .parse_formulae(df, ..., side="rhs")
    first = rhs %>% purrr::map(~ rlang::as_label(.x[[1]]))
    interv = unique(unlist(first))
    if (length(interv)>1) stop(
      "The formulae input have multiple different first terms in the model. ",
      "compare_outcomes() is expecting the same first term across all formulae ",
      "representing the intervention, e.g. (out1 ~ int+cov1+cov2, out2 ~ int+cov1+cov2). ",
      "Different covariates are ignored but the primary intervention must be the same. "
    )
    outcomes = .parse_formulae(df, ..., side="lhs")
    if (any(sapply(outcomes, length) > 1)) stop(
      "Some (or all of) the formulae input have more than one term on the left ",
      "hand side. compare_outcomes() expects each formula to have a single outcome."
    )
    new_form = sprintf("~ %s + %s", interv, paste0(unlist(outcomes),collapse = " + "))
    new_form = stats::as.formula(new_form)
    compare_population(df, new_form, label_fn = label_fn, units = units,
                       override_type = override_type, layout = layout, override_percent_dp = override_percent_dp,
                       override_real_dp = override_real_dp, p_format = p_format, font_size = font_size,
                       font = font, footer_text = footer_text, show_binary_value = show_binary_value,
                       raw_output = raw_output
    )
  }
}

#' Compares the population against an intervention
#'
#' The population comparison is a summary of the co-variates in a data set with
#' no reference to outcome, but comparing intervention groups. It will report
#' summary statistics for continuous and counts for categorical data, for each
#' of the intervention groups, and reports on the significance of the
#' association in relation to the intervention groups. It gives a clear summary
#' of whether data is correlated to intervention.
#'
#' @param df a dataframe of individual observations. If using the `tidyselect`
#'  syntax data grouping defines the intervention group and should be present. if the
#'  formula interface is used the first variable in the right hand side of the
#'  formula is used as the intervention, in which case grouping is ignored.
#' @param ... the columns of variables we wish to summarise. This can be given as
#'  a `tidyselect` specification (see \code{utils::vignette("syntax", package = "tidyselect")}),
#'  identifying the columns. Alternatively it can be given as a formula of the nature
#'
#'  `outcome ~ intervention + covariate_1 + covariate_2 + ...` .
#'
#'  which may be more convenient if you are going on to do a model fit later. If the
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
#' @param override_method if you want to override the comparison method for a
#'   particular variable the options are
#'   `r paste0("\"", names(tableone:::.comparison.fns), "\"", collapse= ",")` and you
#'   specify this on a column by column bases with a named list (e.g
#'   `c("Petal.Width"="t-test")`)
#'
#' @return a `t1_signif` dataframe.
#' @export
#' @examples
#' tmp = iris %>% dplyr::group_by(Species) %>% as_t1_signif(tidyselect::everything())
#' tmp = diamonds %>% dplyr::group_by(is_colored) %>% as_t1_signif(tidyselect::everything())
as_t1_signif = function(
    df,
    ...,
    label_fn = label_extractor(df),
    units = extract_units(df),
    override_type = list(),
    override_method = list()
) {
  cols = .parse_vars(df, ...)
  if (.is_formula_interface(...)) {
    intervention = cols[[1]]
    if (dplyr::is.grouped_df(df) && (df %>% dplyr::groups() != intervention))
      warning("compare_missing(...) ignores grouping when using the formula interface.")
    df = df %>% dplyr::group_by(!!intervention)
  } else {
    intervention = df %>% dplyr::groups()
    if (length(intervention) == 0) stop("If using the tidyselect interface, `df` must be grouped by the intervention")
    if (length(intervention) > 1) warning(
      "there are multiple columns defined in the intervention group.\n",
      "If you meant to do a nested comparison use a dplyr::group_modify().\n",
      "Otherwise this is likely a mistake."
    )
    cols = cols %>% setdiff(intervention)
  }
  label_fn = purrr::as_mapper(label_fn)

  if (df %>% dplyr::n_groups() > getOption("tableone.max_comparisons",10)) {
    stop("The number of groups being compared is ",df %>% dplyr::n_groups()," which is more than the maximum allowed by `options('tableone.max_comparisons'=...)`, which is currently ",getOption("tableone.max_comparisons",10))
  }

  shape = .get_shape(df,cols,label_fn,units)
  summary = .summary_stats(shape,override_type = override_type)
  sign = .significance_tests(summary,override_method = override_method)
  return(sign)
}


#' Convert a `t1_signif` S3 class to a huxtable
#'
#' This is responsible for printing the significance test results and comparison
#'
#' @param x the `t1_signif` result as calculated by `compare_population(...)`
#' @param ... not used
#' @param layout (optional) various layouts are defined as default. As of this
#'   version of `tableone` they are
#'   `r paste0("\"",names(tableone::default.format), "\"", collapse= ",")`. The layouts can be
#'   customised using the options `options("tableone.format_list"=list(...)")`,
#'   and this is described in more detail in the vignettes.
#' @param override_percent_dp (optional) a named list of overrides for the default
#'   precision of formatting percentages, following a `c(<colname_1> = 2,
#'   <colname_2> = 4, ...)` format. columns not present in this list
#'   will use the defaults defined in the layout. See the vignette on customisation.
#' @param override_real_dp (optional) a named list of overrides for the default
#'   precision of formatting real values, following a `c(<colname_1> = 2,
#'   <colname_2> = 4, ...)` format. columns not present in this list
#'   will use the defaults defined in the layout. See the
#'   \code{utils::vignette("customisation", package="tableone")}.
#' @param p_format the format of the p-values: one of
#'  `r paste0("\"",names(.pvalue.defaults),"\"",collapse=", ")` but any value
#'  here is overridden by the `option("tableone.pvalue_formatter"=function(...))`
#' @param font_size (optional) the font size for the table in points
#' @param font (optional) the font family for the table (which will be matched to
#'   closest on your system)
#' @param footer_text any text that needs to be added at the end of the table,
#'   setting this to FALSE dsables the whole footer (as does
#'   `options("tableone.hide_footer"=TRUE)`).
#' @param show_binary_value if set this will filter the display of covariates where the number of possibilities
#'   is exactly 2 to this value.
#'
#' @importFrom huxtable as_huxtable
#'
#' @return a formatted table as a `huxtable`
#' @export
#' @examples
#' library(tableone)
#' tmp = iris %>% dplyr::group_by(Species) %>%
#'   as_t1_signif(tidyselect::everything()) %>%
#'   huxtable::as_huxtable()
as_huxtable.t1_signif = function(
    x, ...,
    layout = "compact",
    override_percent_dp = list(),
    override_real_dp = list(),
    p_format = names(.pvalue.defaults),
    font_size = getOption("tableone.font_size",8),
    font = getOption("tableone.font","Arial"),
    footer_text = NULL,
    show_binary_value = NULL
) {
  p_format = match.arg(p_format)
  if (is.list(layout)) {
    format = layout
  } else {
    format = getOption("tableone.format_list", default.format[[layout]])
  }
  fmt = .format_summary(x, format=format, override_percent_dp = override_percent_dp, override_real_dp=override_real_dp, show_binary_value=show_binary_value)

  intervention = attr(x, "intervention")

  p_col = as.symbol(getOption("tableone.pvalue_column_name","P value"))
  variable_col = as.symbol(getOption("tableone.variable_column_name","Variable"))
  characteristic_col = as.symbol(getOption("tableone.characteristic_column_name","Characteristic"))


  fsign = x %>% .format_significance(p_format)
  fmt = fmt %>% dplyr::left_join(fsign, by="variable")

  fmt = fmt %>% dplyr::rename(
    !!variable_col := variable,
    !!characteristic_col := characteristic
  )

  hux = fmt %>% .hux_tidy(
    rowGroupVars = dplyr::vars(!!variable_col, !!p_col, !!characteristic_col),
    colGroupVars = c(intervention, dplyr::sym(".tbl_col_name")),
    defaultFontSize= font_size,
    defaultFont = font,
    displayRedundantColumnNames=FALSE
  ) %>%
    dplyr::relocate(2, .after = tidyselect::everything())

  tmp = get_footer_text(fsign)

  norm = NULL
  if (any(x$.type == "continuous")) norm = .describe_normality_test()

  method = getOption("tableone.show_pvalue_method",TRUE)
  if (method) {
    footer = c(
      sprintf("%s", tmp$table_key),
      norm,
      footer_text
    )
  } else {
    footer = c(
      sprintf("Significance determined using %s", tmp$significance_test),
      norm,
      footer_text
    )
  }

  signif_lvl = getOption("tableone.bonferroni_level",0.05)
  if (!isFALSE(signif_lvl)) {
    footer = c(footer,
               sprintf("An adjusted %s of %1.3g may be considered significant.",
                       rlang::as_label(p_col),
                       signif_lvl/nrow(x))
    )
  }

  if (!getOption("tableone.hide_footer",isFALSE(footer_text))) {
    hux = hux %>%
      huxtable::insert_row(paste0(footer,collapse="\n"), after=nrow(hux), colspan = ncol(hux), fill="") %>%
      huxtable::set_bottom_border(row=huxtable::final(),value=0)
  }
  # hux = structure(hux, methods = get_footer_text(summary))
  return(hux)
}

# TODO: update to follow guidelines
# https://support.jmir.org/hc/en-us/articles/360019690851-Guidelines-for-Reporting-Statistics

#' Extract one or more comparisons for inserting into text.
#'
#' At some point we need to take information from the tables produced by
#' `tableone` and place it into the main text of the document. It is annoying
#' if this cannot be done automatically. the `group_comparison()` function enables
#' extraction of one or more head to head comparisons and provides a fairly
#' flexible mechanism for building the precise format desired.
#'
#' @param t1_signif a `t1_signif` as produced by `as_t1_signif()` or
#'   `compare_population(..., raw_output = TRUE)`.
#' @param variable a variable or set of variables to compare. If missing a
#'   set of approriate values is displayed based on the columns of `t1_signif`
#' @param subgroup a subgroup or set of subgroups to compare.
#' @param intervention the side or sides of the intervention to select. N.b.
#'   using this effectively prevents any statistical comparison as only one side will be
#'   available.
#' @param percent_fmt a `sprintf` format string that is applied to probability
#'   fields in the summary data to convert to percentages.
#' @inheritParams as_huxtable.t1_signif
#' @param no_summary only extract significance test values
#' @param summary_glue a glue specification that maps the summary statistics to
#'   a readable string.
#' @param summary_arrange an expression by which to order the summary output
#' @param summary_sep a separator to combine the summary output (see `glue::glue_collapse()`)
#' @param summary_last a separator to combine the last 2 summary outputs (see `glue::glue_collapse()`)
#' @param no_signif do not try and include significance in the output. Sometimes
#'   this is the only option if there is not enough of the comparison to retained
#'   by the `variable`, `subgroup`, and `intervention` filters. (Specifically if
#'   there is only a comparison between different subgroups, as the p-values will
#'   be for the different comparison between intervention groups.)
#' @param signif_glue a glue specification that maps the combined summary output with
#'   the result of the significance tests, to given a complete comparison.
#' @param signif_sep a separator to combine complete comparisons (see `glue::glue_collapse()`)
#' @param signif_last a separator to combine the last 2 complete comparisons (see `glue::glue_collapse()`)
#'
#' @return ideally a single string but various things will be returned depending
#'   on hos much input is constrained, and sometimes will provide guidance about
#'   what next to do. The intention is the function to be used interactively
#'   until a satisfactory result is obtained.
#' @export
#'
#' @examples
#' tmp = diamonds %>%
#'   dplyr::group_by(is_colored) %>%
#'   set_units(price,units="£") %>%
#'   compare_population(-color, raw_output=TRUE)
#'
#' # The tabular output is retrieved by converting to a huxtable
#' # as_huxtable(tmp, layout="simple")
#'
#' # An unqualified group_comparison call gives informative messages
#' # about what can be compared:
#' tmp %>% group_comparison()
#'
#' # filtering down the data gets us to a specific comparison:
#' tmp %>% group_comparison(variable = "cut", subgroup="Fair") %>% dplyr::glimpse()
#'
#' # With further interactive exploration the
#' # data available for that comparison can be made into a glue string
#' tmp %>% group_comparison(variable = "cut", subgroup="Fair", intervention = "clear",
#'   summary_glue = "{is_colored}: {x}/{n} ({prob.0.5}%)",
#'   signif_glue = "{variable}={subgroup}; {text}; Overall p-value for '{variable}': {p.value}.")
#'
#' # group comparisons above using many individual subgroups are a bit confusing because
#' # the p-value is at the variable level. This is less of an issue for continuous
#' # or binary values.
#' tmp %>% group_comparison(
#'   variable = "price",
#'   summary_glue = "{is_colored}: {unit}{q.0.5}; IQR: {q.0.25} \u2014 {q.0.75} (n={n})",
#'   signif_glue = "{variable}: {text}; P-value {p.value}.")
#'
#' # Sometimes we only want to extract a p-value:
#' tmp %>%
#'   group_comparison(variable = "cut", subgroup="Fair", no_summary=TRUE) %>%
#'   dplyr::glimpse()
group_comparison = function(
    t1_signif,
    variable = NULL,
    subgroup = NULL,
    intervention = NULL,
    percent_fmt = "%1.1f%%",
    p_format = names(.pvalue.defaults),
    no_summary = FALSE,
    summary_glue = NULL,
    summary_arrange = NULL,
    summary_sep = ", ",
    summary_last = " versus ",
    no_signif = FALSE,
    signif_glue = NULL,
    signif_sep = NULL,
    signif_last = NULL
  ) {
  tmp = t1_signif
  p_format = match.arg(p_format)
  intervention_col = attr(tmp,"intervention")

  if (!is.null(variable)) {
    tmp = tmp %>% dplyr::filter(
      .name %in% as.character(variable) | .label %in% as.character(variable)
    )
  } else {
    message("* `variable` can be any of:\n",
            c(tmp %>% dplyr::pull(.name),tmp %>% dplyr::pull(.label)) %>% unique() %>% paste0("`",.,"`",collapse = ","))
  }

  if (!is.null(subgroup)) {
    # this is a categorical data item
    tmp = tmp %>% dplyr::mutate(
      .summary_data = purrr::map(.summary_data, ~ .x %>%
                                   dplyr::filter(level %in% subgroup))
    )
  } else {
    subgs = tmp %>% dplyr::pull(.level_names) %>% purrr::list_c() %>% unique()
    if (length(subgs) > 0)
    message("* `subgroup` can be any of:\n",
      subgs %>% paste0("`",.,"`",collapse = ",")
    )
  }

  if (!is.null(intervention)) {
    if (length(intervention_col) == 0) stop("cannot filter on intervention when this is not defined.")
    if (length(intervention_col) > 1) stop("cannot filter on intervention when this is over multiple columns.")
    tmp_intervention_col = intervention_col[[1]]
    tmp = tmp %>% dplyr::mutate(
      .summary_data = purrr::map(.summary_data, ~ .x %>%
                                   dplyr::filter(!!tmp_intervention_col %in% intervention))
    )
  } else {
    message("* `intervention` can be any of:\n", attr(tmp,"intervention_levels") %>% dplyr::pull() %>% levels() %>% paste0("`",.,"`",collapse = ","))
  }

  summarydf = tmp %>% dplyr::select(
    variable = .label, unit = .unit, .summary_data, N_total, N_present
  ) %>% tidyr::unnest(.summary_data) %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("prob"), ~ .sprintf_no_na(percent_fmt, .x*100)))

  if ("level" %in% colnames(summarydf)) {
    summarydf = summarydf %>% dplyr::rename(subgroup = level)
  } else {
    summarydf = summarydf %>% dplyr::mutate(subgroup = NA_character_)
  }

  if (nrow(summarydf) == 0) stop(sprintf(
    "The combination of `intervention=%s`, `variable=%s` and `subgroup=%s` are not found in the data",
    as.character(intervention %||% "<any>"), as.character(variable %||% "<any>"), as.character(subgroup %||% "<any>")
  ))

  summary_arrange = rlang::enexpr(summary_arrange)
  if (!is.null(summary_arrange)) {
    summarydf = summarydf %>% dplyr::arrange(!!summary_arrange)
  }

  summarydf = summarydf %>%
    dplyr::group_by(variable, subgroup, !!!intervention_col)

  no_signif = no_signif || (summarydf %>% dplyr::ungroup() %>%
    dplyr::select(!!!intervention_col) %>% dplyr::distinct() %>% nrow() <= 1)

  if (!no_signif) {
    fun = getOption("tableone.pvalue_formatter",.pvalue.defaults[[p_format]])
    signifdf = tmp %>% dplyr::select(
      variable = .label, .significance_test
    ) %>%
      tidyr::unnest(.significance_test) %>%
      dplyr::mutate(p.value = fun(p.value)) %>%
      dplyr::group_by(variable)
  } else {
    # in the case that the subgroups are being compared the signifance tests no
    # do not apply
    signifdf = tmp %>% dplyr::select(
      variable = .label
    ) %>% dplyr::distinct()
  }

  if (!no_summary) {
    if (is.null(summary_glue)) {
      message("* `summary_glue` can use any of the following variables:\n",
            paste0("`",colnames(summarydf),"`",collapse = ", "))
      # if (nrow(summarydf) < 3) return(invisible(dplyr::glimpse(summarydf)))
      return(invisible(summarydf))
    }

    summarytext = summarydf %>% dplyr::mutate(text = glue::glue(summary_glue)) %>%
      dplyr::select(variable, subgroup, !!!intervention_col, text) %>%
      dplyr::group_by(variable, subgroup)

    if (!is.null(summary_sep)) {
      summarytext = summarytext %>%
        dplyr::summarise(text =
                glue::glue_collapse(text, sep=summary_sep, last = summary_last)
        )
    }

  } else {

    summarytext = summarydf %>%
      dplyr::ungroup() %>%
      dplyr::select(variable) %>%
      dplyr::distinct() %>%
      dplyr::group_by(variable)

  }





  if (no_signif) {

    return(
      summarytext$text %>% glue::glue_collapse(sep=summary_sep, last = summary_last) %>% as.character()
    )

  } else {

    # incorporate significance
    summarytext = summarytext %>% dplyr::left_join(signifdf, by="variable")

    if (is.null(signif_glue)) {
      message("* `signif_glue` can use any of the following variables:\n",
              paste0("`",colnames(summarytext),"`",collapse = ", "))
      return(invisible(summarytext))
    }

    signiftext = summarytext %>%
      glue::glue_data(signif_glue)

    if (!is.null(signif_sep)) {
      signiftext = signiftext %>%
        glue::glue_collapse(sep=summary_sep, last = summary_last)
    }

    return(signiftext)
  }
}



## Helpers ----

# df_signif = df_shape %>% .significance_tests()
# df_signif %>% .format_significance()
.format_significance = function(df_signif, p_format = names(.pvalue.defaults), method = getOption("tableone.show_pvalue_method",TRUE)) {
  p_format = match.arg(p_format)
  p_col = as.symbol(getOption("tableone.pvalue_column_name","P value"))

  fun = getOption("tableone.pvalue_formatter",.pvalue.defaults[[p_format]])

  tmp = df_signif %>% dplyr::select(variable = .label, .type, .significance_test) %>%
    tidyr::unnest(.significance_test)

  methods = tmp %>% dplyr::select(.type,p.method) %>% dplyr::distinct()
  methods_description = methods %>% dplyr::group_by(.type) %>%
    dplyr::summarise(.methods = paste0(p.method,collapse = ", ")) %>%
    dplyr::summarise(t = paste0(sprintf("%s (%s variables)",.methods,.type), collapse=" or ")) %>%
    dplyr::pull(t)
  if (method) {
    # TODO: don;t really want to account for .type when p.method is "Not
    # calculated due to missing values"
    methods = methods %>% dplyr::mutate(daggers = lapply(1:nrow(methods), rep_len, x="\u2020") %>% lapply(paste0, collapse="") %>% unlist())
    tmp = tmp %>% dplyr::left_join(methods, by=c("p.method",".type")) %>%
      dplyr::mutate(!!p_col := paste0(fun(p.value)," ",daggers))

    methods_key = paste0(sprintf("%s, %s (%s)",methods$daggers,methods$p.method, methods$.type), collapse = "; ")
    tmp = structure(tmp %>% dplyr::select(variable, !!p_col),
                    methods = c(list(
                      table_key = methods_key,
                      significance_test = methods_description
                    ),
                    get_footer_text(df_signif))

    )
  } else {
    tmp = tmp %>%
      dplyr::mutate(
        !!p_col := fun(p.value)
      )
    tmp = structure(
      tmp %>% dplyr::select(variable, !!p_col),
      methods = c(list(
        significance_test = methods_description
      ), get_footer_text(df_signif))
    )
  }

  return(tmp)
}
