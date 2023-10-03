.comparison_printer = function(sign, intervention, p_fun) {
  # TODO:
  # list fo functions: significance(.variable) - includes p_fun formatting
  # statistics(.variable, .characteristic=NULL) - list of stats, checks it is single
  # checks characteristics match. exactuly one row.
  #
  function(.variable, .characteristic = NULL) {
    .summary_data = NULL
    .variable = rlang::ensym(.variable)
    tmp = sign %>% dplyr::filter(.name==rlang::as_label(.variable))
    tmp2 = tmp %>% dplyr::select(.summary_type, .summary_data) %>%
      tidyr::unnest(.summary_data) %>%
      dplyr::mutate(.group = as.character(!!intervention))
    if (!is.null(.characteristic)) tmp2 = tmp2 %>% dplyr::filter(level == .characteristic)
    simple = list(
      # "subtype_count" = "{.sprintf_na('%1.1f%% %s (%d/%d - %s)',prob.0.5*100,as.character(level),n,N,.group)}",
      "subtype_count" = "{.sprintf_na('%1.1f%% (%s) [N=%d],prob.0.5*100,.group,n)}",
      "median_iqr" = "{.sprintf_na('%1.3g (%s)',q.0.5, .group)}",
      "mean_sd" = "{.sprintf_na('%1.3g (%s)',mean, .group)}"
    )
    out = rep(NA_character_, nrow(tmp2))
    for (i in 1:nrow(tmp2)) {
      l = lapply(tmp2, `[[`, i)
      glue = simple[[l$.summary_type]]
      out[[i]] = glue::glue_data(l, glue)
    }
    s = out %>% glue::glue_collapse(sep = ", ", last = " and ")
    p = tmp %>% dplyr::select(.significance_test) %>% tidyr::unnest(.significance_test) %>%
      dplyr::pull(p.value) %>% p_fun()
    # q = tmp %>% dplyr::select(.significance_test) %>% tidyr::unnest(.significance_test) %>%
    #   dplyr::pull(p.method)
    p_col = getOption("tableone.pvalue_column_name","P value")
    return(sprintf("%s [%s: %s]",s,p_col,p))

  }
}

.data_filter = function(sign, intervention) {
  # TODO:
  # list fo functions: significance(.variable) - includes p_fun formatting
  # statistics(.variable, .characteristic=NULL) - list of stats, checks it is single
  # checks characteristics match. exactuly one row.

  get_summary = .summary_filter(sign, intervention)
  get_comparison = .comparison_filter(sign)

  function(.variable, .intervention = NULL, .characteristic = NULL) {

    summary = get_summary(.variable,.intervention,.characteristic)
    comparison = get_comparison(.variable)
    return(summary %>% dplyr::left_join(
      comparison %>% dplyr::select(-.label, -.order, -.unit, -.type, -N_total, -N_present),
      by = ".name",
      suffix = c("",".signif")
    ))

  }
}

# TODO: the level column here, where do I define it? It may be only in categorical data
# In which case the filter will fall over for non cateogorical data.
.summary_filter = function(sign, intervention) {
  function(.variable, .intervention = NULL, .characteristic = NULL) {
    .summary_data = NULL
    .variable = rlang::ensym(.variable)
    tmp = sign %>% dplyr::filter(.cols==.variable)
    tmp2 = tmp %>% dplyr::select(.name, .label, .order, .unit, .type, N_total, N_present, .summary_data) %>%
      tidyr::unnest(.summary_data) %>%
      dplyr::mutate(
        .group = as.character(!!intervention)
      )
    if (!is.null(.intervention)) tmp2 = tmp2 %>% dplyr::filter(.group == .intervention)
    if (!is.null(.characteristic)) tmp2 = tmp2 %>% dplyr::filter(level == .characteristic)
    return(tmp2)
  }
}

.comparison_filter = function(sign) {
  function(.variable) {
    .summary_data = NULL
    .variable = rlang::ensym(.variable)
    tmp = sign %>% dplyr::filter(.cols==.variable)
    p = tmp %>% dplyr::select(.name, .label, .order, .unit, .type, N_total, N_present, .significance_test) %>% tidyr::unnest(.significance_test)
    if (".power_analysis" %in% colnames(tmp)) {
      a = tmp %>% dplyr::select(.name,.power_analysis) %>% tidyr::unnest(.power_analysis)
      return(p %>% dplyr::left_join(a, by=".name", suffix=c("","power")))
    } else {
      return(p)
    }
  }
}


#' Get summary comparisons and statistics between variables as raw data.
#'
#' @inheritParams as_t1_signif
#' @inheritParams as_huxtable.t1_signif
#' @inheritParams compare_population
#' @param ... the outcomes are specified either as a `tidyselect` specification,
#'   in which case the grouping of the `df` input determines the intervention
#'   and the output is the same a `compare_population()` call with a tidyselect.
#'   Alternatively a set of formulae can be provided that specify the outcomes
#'   on the left hand side, e.g. `outcome1 ~ intervention + cov1, outcome2 ~
#'   intervention + cov1, ...` in this case the `intervention` must be the same
#'   for all formulae and used to determine the comparison groups.
#' @param power_analysis conduct sample size based power analysis.
#' @param override_power if you want to override the power calculation method for a
#'   particular variable the options are
#'   `r paste0("\"", names(tableone:::.power.fns), "\"", collapse= ",")` and you
#'   specify this on a column by column bases with a named list (e.g
#'   `c("Petal.Width"="t-test")`)
#'
#' @return a list of accessor functions for the summary data allowing granular
#'   access to the results of the analysis:
#'
#'   * `comparison$compare(.variable, .characteristic = NULL)` -
#'   prints a comparison between the different
#'   intervention groups for the specified variable (and optionally the given
#'   characteristic if it is a categorical variable).
#'   * `comparison$filter(.variable, .intervention = NULL, .characteristic = NULL)`
#'   extracts a given variable
#'   (e.g. `gender`), optionally for a given level of intervention (e.g.
#'   `control`) and if categorical a given characteristic (e.g. `male`). This
#'   will output a dataframe with all the calculated summary variables, for all
#'   qualifying intervention, variable and characteristic combinations,
#'   significance tests (and power analyses) for the qualifying variable
#'   (comparing intervention groups).
#'   * `comparison$signif_tests(.variable)` - extracts for
#'   a given variable (e.g. `gender`) the significance tests (and optionally
#'   power analyses) of the univariate comparison between different
#'   interventions and the variable.
#'   * `comparison$summary_stats(.variable, .intervention = NULL, .characteristic = NULL)`
#'   extracts a given variable (e.g. `gender`),
#'   optionally for a given level of intervention (e.g. `control`) and if
#'   categorical a given characteristic (e.g. `male`). This returns only the
#'   summary stats for all qualifying intervention, variable and characteristic
#'   combinations.
#' @export
extract_comparison = function(
    df,
    ...,
    label_fn = label_extractor(df),
    override_type = list(),
    p_format = names(.pvalue.defaults),
    override_method = list(),
    power_analysis = FALSE,
    override_power = list(),
    raw_output = FALSE
) {

  rlang::warn("extract_comparison is deprecated and wil be removed in future versions of tableone",
              .frequency = "once",.frequency_id = "dep_ext_comp")

  sign = as_t1_signif(
    df, ...,
    label_fn = label_fn,
    units = units,
    override_type = override_type,
    override_method = override_method
  )
  if (power_analysis) {
    sign = .power_analysis(sign, override_power = override_power)
  }
  if (raw_output) return(sign)

  p_format = match.arg(p_format)
  p_fun = getOption("tableone.pvalue_formatter",.pvalue.defaults[[p_format]])
  return(list(
    compare = .comparison_printer(sign, intervention, p_fun),
    filter = .data_filter(sign, intervention),
    summary_stats = .summary_filter(sign, intervention),
    signif_tests = .comparison_filter(sign)
  ))
}

