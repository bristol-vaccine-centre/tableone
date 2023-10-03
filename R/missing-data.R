# Missingness ----

# Change a dataset of values into a dataset of logical missing indicators
# missing_diamonds %>% data_missingness() %>% describe_population(tidyselect::everything())
.data_missingness = function(df) {
  df %>% dplyr::mutate(dplyr::across(tidyselect::everything(), ~ ifelse(is.na(.x) | is.infinite(.x) | is.nan(.x), "missing", "not missing") %>% factor(levels = c("missing","not missing"))))
}

#' Compares missing data against an intervention in a summary table
#'
#' The missing data summary is a simple summary of the missingness of co-variates
#' in a data set with no reference to outcome, but comparing intervention groups.
#' It reports summary counts for missingness in data and reports on the
#' significance of that missingness in relation to the intervention groups,
#' allowing a clear summary of whether data is missing at random compared to the
#' intervention.
#'
#' @inheritParams compare_population
#' @param significance_limit the limit at which we reject the hypothesis that the
#'   data is missing at random.
#' @param missingness_limit the limit at which too much data is missing
#'   to include the predictor.
#' @param raw_output return comparison as tidy dataframe rather than formatted table
#'
#' @return a `huxtable` formatted table.
#' @export
#'
#' @examples
#' # this option lets us change the column name for p value from its default
#' # "P value"
#' old = options("tableone.pvalue_column_name"="p-value")
#'
#' # missing at random
#' missing_diamonds %>% dplyr::group_by(is_colored) %>% compare_missing(tidyselect::everything())
#'
#' # nothing missing
#' iris %>% dplyr::group_by(Species) %>% compare_missing(tidyselect::everything())
#'
#' # MNAR: by design missingness is correlated with grouping
#' mnar_two_class_1000 %>% dplyr::group_by(grouping) %>% compare_missing(tidyselect::everything())
#'
#' options(old)
compare_missing = function(
    df,
    ...,
    label_fn = label_extractor(df),
    p_format = names(.pvalue.defaults),
    font_size = getOption("tableone.font_size",8),
    font = getOption("tableone.font","Arial"),
    significance_limit = 0.05,
    missingness_limit = 0.1,
    footer_text = NULL,
    raw_output = FALSE
) {
  cols = .parse_vars(df, ...)
  p_format = match.arg(p_format)
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

  if (df %>% dplyr::n_groups() > getOption("tableone.max_comparisons",10)) {
    stop("The number of groups being compared is ",df %>% dplyr::n_groups()," which is more than the maximum allowed by `options('tableone.max_comparisons'=...)`, which is currently ",getOption("tableone.max_comparisons",10))
  }

  footer = footer_text
  label_fn = purrr::as_mapper(label_fn)
  shape1 = .get_shape(df,cols,label_fn)
  too_missing = shape1 %>% dplyr::filter(.p_missing > missingness_limit) %>% dplyr::pull(.cols)

  if (length(too_missing)>0) {
    footer = c(footer, .missing_message(too_missing, missingness_limit, label_fn))
  }

  df_missing = .data_missingness(df)
  shape = .get_shape(df_missing,cols,label_fn)
  summary = .summary_stats(shape)
  fmt = .format_summary(summary, format = default.format$missing, show_binary_value="missing") %>%
    dplyr::select(-characteristic)
  p_col = as.symbol(getOption("tableone.pvalue_column_name","P value"))
  sign = .significance_tests(summary)

  comparisons = nrow(sign)

  mnar = sign %>% dplyr::select(.cols, .significance_test) %>%
    tidyr::unnest(.significance_test) %>%
    dplyr::filter(!is.na(p.value) & p.value < significance_limit/comparisons) %>%
    dplyr::pull(.cols)
  if (length(mnar)>0) {
    footer = c(footer, .mnar_message(mnar, intervention, significance_limit, comparisons, label_fn))
  }

  # don't show daggers in missing values table (always is fisher test)
  fsign = sign %>% .format_significance(p_format, method = FALSE)
  fmt = fmt %>% dplyr::left_join(fsign, by="variable")

  if (raw_output) return(fmt)

  hux = fmt %>% .hux_tidy(
    rowGroupVars = dplyr::vars(variable, !!p_col),
    colGroupVars = c(intervention, dplyr::sym(".tbl_col_name")),
    defaultFontSize= font_size,
    defaultFont = font,
    displayRedundantColumnNames=FALSE
  ) %>%
    dplyr::relocate(2, .after = tidyselect::everything())
  tmp = get_footer_text(sign)

  if (!getOption("tableone.hide_footer",isFALSE(footer_text))) {
    hux = hux %>%
      huxtable::insert_row(paste0(footer,collapse="\n"), after=nrow(hux), colspan = ncol(hux), fill="") %>%
      huxtable::set_bottom_border(row=huxtable::final(),value=0)
  }
  return(hux)
}

#' Remove variables that fail a missing data test from models
#'
#' Comparing missingness by looking at a table is good but we also want to update
#' models to exclude missing data from the predictors.
#'
#' @param ... a list of formulae that specify the models that we want to check
#' @inheritParams compare_missing

#'
#' @return a list of formulae with missing parameters removed
#' @export
#'
#' @examples
#' df = iris %>%
#'   dplyr::mutate(Petal.Width = ifelse(
#'     stats::runif(dplyr::n()) < dplyr::case_when(
#'       Species == "setosa" ~ 0.2,
#'       Species == "virginica" ~ 0.1,
#'       TRUE~0
#'     ),
#'     NA,
#'     Petal.Width
#'   ))
#' remove_missing(df, ~ Species + Petal.Width + Sepal.Width, ~ Species + Petal.Length + Sepal.Length)
remove_missing = function(df,
                          ...,
                          label_fn = label_extractor(df),
                          significance_limit = 0.05,
                          missingness_limit = 0.1
) {
  rhss = .parse_formulae(df, ...)
  # rhss = .parse_formulae(df, ~ Species + Petal.Width + Sepal.Width, ~ Species + Petal.Length + Sepal.Length)
  # forms = c(~ Species + Petal.Width + Sepal.Width, ~ Species + Petal.Length + Sepal.Length)
  label_fn = purrr::as_mapper(label_fn)
  cols = rhss %>% purrr::discard(~ is.null(.x) || length(.x)==0) %>% unlist()
  forms = c(...)
  intervention = cols[[1]]
  if (dplyr::is.grouped_df(df) && (df %>% dplyr::groups() != intervention))
    warning("compare_missing(...) ignores grouping when using the formula interface.")
  # missingness
  df = df %>% dplyr::group_by(!!intervention)
  shape1 = .get_shape(df,cols,label_fn)
  too_missing = shape1 %>%
    dplyr::filter(.p_missing > missingness_limit) %>% dplyr::pull(.cols)

  df_missing = .data_missingness(df)
  shape = .get_shape(df_missing,cols,label_fn)
  summary = .summary_stats(shape)
  sign = .significance_tests(summary)
  # boneferoni adjustment:
  comparisons = nrow(sign)

  mnar = sign %>%
    dplyr::select(.cols, .significance_test) %>%
    tidyr::unnest(.significance_test) %>%
    dplyr::filter(!is.na(p.value) & p.value < significance_limit/comparisons) %>%
    dplyr::pull(.cols)

  if (length(too_missing)>0) {
    message(.missing_message(too_missing, missingness_limit, label_fn))
    mod_forms = lapply(too_missing, function(x) stats::as.formula(paste0(". ~ . -",x)))
    for (i in  1:length(mod_forms)) {
      forms = lapply(forms, function(x) stats::update(x,mod_forms[[i]]))
    }
  }
  if (length(mnar)>0) {
    message(.mnar_message(mnar,intervention, significance_limit, comparisons, label_fn))
    mod_forms = lapply(mnar, function(x) stats::as.formula(paste0(". ~ . -",x)))
    for (i in  1:length(mod_forms)) {
      forms = lapply(forms, function(x) stats::update(x,mod_forms[[i]]))
    }
  }
  return(forms)

}

.missing_message = function(too_missing, missingness_limit, label_fn) {
  label_fn = getOption("tableone.labeller",label_fn)
  sprintf("More than %1.0f%% of data is missing for variables %s.",
          missingness_limit*100,
          paste0(
            lapply(sapply(too_missing, rlang::as_label),label_fn),
            collapse=", ")
  )
}

.mnar_message = function(mnar, intervention, significance_limit, comparisons, label_fn) {
  label_fn = getOption("tableone.labeller",label_fn)
  sprintf("Data is missing not at random (compared to %s) at a p-value<%1.3f (%1.2f over %d comparisons) for variables %s.",
          paste0(
            lapply(sapply(intervention, rlang::as_label),label_fn),
            collapse=", "),
          significance_limit / comparisons,
          significance_limit,
          comparisons,
          paste0(
            lapply(sapply(mnar, rlang::as_label),label_fn),
            collapse=", ")
  )
}
