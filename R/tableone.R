

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

#' Convert discrete data to factors
#'
#' It is simpler for presentation and sometimes more correct for discrete valued
#' data to be represented as factors. Such discrete valued data might be logical values,
#' character values, or numeric values with a limited number of levels (e.g. scores).
#' this function lets you convert (a subset of) data frame columns into factors
#' using
#'
#' @param df a data frame
#' @param ... either a `tidyselect` specification or a formula with the right
#'   hand side defining the columns to convert (left hand side is ignored)
#' @param .logical (optional) a length 2 vector defining the levels of TRUE, then
#'   FALSE.
#' @param .numeric (optional) if provided it must either be a named list e.g.
#'   `c(column_name = "{name}:{value}", ..., .default="{value}")` pairs which define the way in which
#'   numeric columns are converted to factor levels. If a single value is given
#'   then all numerics are converted in the same way (this is the default). If
#'   there are some values that you are not certain you want to convert setting
#'   a limit on the maximum number of levels in a generated factor may be a good
#'   idea (i.e. `options("tableone.max_discrete_levels"=16)`) otherwise all
#'   values are converted
#' @param .character in general character columns are converted into a factor with
#'   the default levels. To explicitly set levels a named list can be given here
#'   which `c(colname_1 = c("level_1", "level_2", ...), colname_2 = ...)`
#'
#' @return a dataframe with the columns converted to factors
#' @export
#'
#' @examples
#' iris %>%
#'   make_factors(tidyselect::ends_with("Length"), .numeric = "{name}={round(value)}") %>%
#'   dplyr::glimpse()
#'
#' # Convert everything in diamonds to be a factor, rounding all
#' # the numeric values and converting all the names to upper case
#' tmp = diamonds %>%
#'   dplyr::mutate(is_colored = color > "F") %>%
#'   make_factors(tidyselect::everything(), .numeric="{toupper(name)}={round(value)}")
#'
#' # as we included `price` which has very many levels one factor is unuseable with 11602 levels:
#' length(levels(tmp$price))
#'
#' # we could explicitly exclude it from the `tidyselect` syntax `...` parameter:
#' diamonds %>% dplyr::mutate(is_colored = color > "F") %>%
#'   make_factors(-price, .numeric="{toupper(name)}={round(value)}") %>%
#'   dplyr::glimpse()
#'
#' # or alternatively we set a limit on the maximum number of factors, which
#' # in this example picks up the `depth` and `table` columns as exceeding this
#' # new limit:
#'
#' old = options("tableone.max_discrete_levels"=16)
#' diamonds %>% dplyr::mutate(is_colored = color > "F") %>%
#'   make_factors(tidyselect::everything(), .numeric="{toupper(name)}={round(value)}") %>%
#'   dplyr::glimpse()
#'
#' options(old)
#'
#' # converting a character vector. Here we specify `.character` as a list giving the
#' # possible levels of `alpha2`. Values outside of this list are converted to `NA`
#'
#' set.seed(100)
#' eg_character = tibble::tibble(
#'   alpha1 =  sample(letters,50,replace=TRUE),
#'   alpha2 = sample(LETTERS,50,replace=TRUE)
#' )
#'
#' eg_character %>%
#'   make_factors(tidyselect::everything(), .character = list(alpha2 = LETTERS[3:20]))
#'
#'
make_factors = function(df, ..., .logical = c("yes","no"), .numeric = "{name}={value}", .character = NULL) {

  # In this context we generally know we want to convert everything
  max_levels = getOption("tableone.max_discrete_levels",Inf)
  cols = .parse_vars(df, ...)
  if(is.null(names(.numeric))) {
    if (length(.numeric) > 1) {
      stop("if more than one value is specified for `.numeric` it must be provided as a named list, where the names are column names from `df`.")
    } else {
      .numeric = list(.default = .numeric)
    }
  }

  character_cols_given = !is.null(.character)
  if (character_cols_given & is.null(names(.character))) stop("if `.character` is specified it must be a named list, containing the levels")

  for (col in cols) {
    tmp = df %>% dplyr::pull(!!col)
    if (is.logical(tmp)) {
      # the column is a logical which is definitely discrete
      df = df %>% dplyr::mutate(!!col := ifelse(tmp, .logical[[1]],.logical[[2]]) %>% factor(levels = .logical))
    } else {
      is_discrete = length(unique(tmp)) < max_levels
      col_label = rlang::as_label(col)
      if (is.character(tmp)) {
        if (character_cols_given & col_label %in% names(.character)) {
          # we are given an explicit set of levels for this column
          levels = .character[[col_label]]
          df = df %>% dplyr::mutate(!!col := factor(tmp, levels = levels))
        } else {
          # we are not given a set of levels, but we still want to encode it because
          # the function was called with this column.
          df = df %>% dplyr::mutate(!!col := factor(tmp))
        }
      } else if (is.numeric(tmp)) {
        if (col_label %in% names(.numeric)) {
          # The glue is given as a named list and this column is in the names
          # This means we definitely want to convert this one. and we will
          # not respect the max_levels if it gives too many levels
          numeric_glue = .numeric[[col_label]]
          tmp_max_levels = Inf
        } else {
          # We found a default and hence will respect the max_levels parameter
          numeric_glue = .numeric[[".default"]]
          tmp_max_levels = max_levels
        }
        if (is.null(numeric_glue)) {
          message("Skipping conversion of ",col_label," as `.numeric` does not contain an entry for this column or a `.default` value")
        } else {
          levels = unique(glue::glue(numeric_glue, value=sort(unique(tmp)), name = col_label))
          if (length(levels) < max_levels) {
            new_tmp = glue::glue(numeric_glue, value=tmp, name = col_label)
            df = df %>% dplyr::mutate(!!col := factor(new_tmp,levels = levels,ordered = TRUE))
          } else {
            message("Skipping factor conversion for ",col_label," because is has ",
                    length(levels)," levels and the maximum number allowed is set to ",max_levels)
          }
        }
      }
    }
  }
  return(df)
}

#' Make NA values in factor columns explicit
#'
#' Converts NA values in any factors in the dataframe into a new level -
#' This is a thin wrapper for [forcats::fct_explicit_na()]
#'
#' @param df the data frame
#' @param na_level a label for NA valued factors
#'
#' @return the dataframe with all factor columns containing explicit na values
#' @export
#'
#' @examples
#' # before
#' missing_diamonds %>% dplyr::group_by(cut) %>% dplyr::count()
#' # after
#' missing_diamonds %>% explicit_na() %>% dplyr::group_by(cut) %>% dplyr::count()
explicit_na = function(df, na_level = "<missing>") {
  df %>%
    dplyr::mutate(across(where(is.factor), ~ forcats::fct_explicit_na(.x, na_level)))
}

# missing_diamonds %>% data_missingness() %>% describe_population(tidyselect::everything())
.data_missingness = function(df) {
  df %>% dplyr::mutate(across(tidyselect::everything(), ~ ifelse(is.na(.x) | is.infinite(.x) | is.nan(.x), "missing", "not missing") %>% factor(levels = c("missing","not missing"))))
}

# where dots is either a function (in which case we only want rhs) or a tidyselect.
# .parse_vars(iris, tidyselect::everything())
# .parse_vars(iris, ~ Species + Petal.Width + Missing)
.parse_vars = function(df, ...) {
  expr = rlang::expr(c(...))
  vars = tryCatch(
    {
      pos = tidyselect::eval_select(expr, data = df)
      cols = (df %>% colnames() %>% sapply(as.symbol, USE.NAMES = FALSE))[pos]
      if (length(cols)==0) {
        stop("No columns given: please supply a formula or a tidyselect expression e.g. `tidyselect::everything()`")
      }
      return(cols)
    }, error = function(e) {
      dots = rlang::list2(...)
      if (length(dots) == 0)
        stop("No columns given: please supply a formula or a tidyselect expression e.g. `tidyselect::everything()`")
      if (rlang::is_formula(dots[[1]])) {
        vars = rlang::f_rhs(dots[[1]]) %>% all.vars()
        wronguns = vars %>% setdiff(colnames(df))
        if (length(wronguns) > 0) warning("Ignoring variables in formula but not in `df`: ", wronguns %>% paste0(collapse = ", "))
        vars = vars %>%
          intersect(colnames(df)) %>%
          sapply(as.symbol, USE.NAMES = FALSE)
      } else {
        stop("No columns given: please supply a formula or a tidyselect expression e.g. `tidyselect::everything()`")
      }
    }
  )
  return(vars)
}

# where dots is either a function (in which case we only want rhs) or a tidyselect.
# .is_formula_interface(iris, ~ Species + Petal.Width + Missing)
# .is_formula_interface(iris, tidyselect::everything())
.is_formula_interface = function(df, ...) {
  expr = rlang::expr(c(...))
  out = tryCatch({
      pos = tidyselect::eval_select(expr, data = df)
      cols = (df %>% colnames() %>% sapply(as.symbol, USE.NAMES = FALSE))[pos]
      if (length(cols)==0) {  return(NA)  }
      return(FALSE)
    }, error = function(e) rlang::is_formula(...))
  return(out)
}

#' Describe the population in a summary table
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
  label_fn = ~ .x,
  units = list(),
  override_type = list(),
  layout = "single",
  override_percent_dp = list(),
  override_real_dp = list(),
  font_size = 8,
  font = "Arial",
  footer_text = NULL
) {
  if (dplyr::is.grouped_df(df)) {
    message("describe_population(...) ignores grouping.")
    df = df %>% dplyr::ungroup()
  }
  cols = .parse_vars(df, ...)
  label_fn = purrr::as_mapper(label_fn)
  shape = .get_shape(df,cols,label_fn,units)
  summary = .summary_stats(shape, override_type)
  fmt = .format_summary(summary, layout, override_percent_dp, override_real_dp)
  hux = fmt %>% .hux_tidy(rowGroupVars = ggplot2::vars(variable, characteristic), colGroupVars = ggplot2::vars(), defaultFontSize= font_size, defaultFont = font)
  tmp = get_footer_text(summary)
  footer = c(
    sprintf("Normality of distributions determined using %s", tmp$normality_test),
    footer_text
  )
  hux = hux %>% huxtable::insert_row(paste0(footer,collapse="\n"), after=nrow(hux), colspan = ncol(hux), fill="",copy_cell_props = FALSE)
  return(hux)
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
  label_fn = ~ .x,
  p_format = names(.pvalue.defaults),
  font_size = 8,
  font = "Arial",
  footer_text = NULL
) {
  cols = .parse_vars(df, ...)
  p_format = match.arg(p_format)
  if (.is_formula_interface(df,...)) {
    intervention = cols[[1]]
    if (dplyr::is.grouped_df(df) & df %>% dplyr::groups() != intervention)
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

  df_missing = .data_missingness(df)
  shape = .get_shape(df_missing,cols,label_fn)
  summary = .summary_stats(shape)
  fmt = .format_summary(summary, layout = "missing") %>%
    dplyr::filter(type == "missing") %>%
    dplyr::select(-type)
  p_col = as.symbol(getOption("tableone.pvalue_column_name","P value"))
  sign = .significance_tests(summary) %>% .format_significance(p_format)
  fmt = fmt %>% dplyr::left_join(sign, by="variable")
  hux = fmt %>% .hux_tidy(rowGroupVars = ggplot2::vars(variable, !!p_col), colGroupVars = intervention, defaultFontSize= font_size, defaultFont = font) %>%
    dplyr::relocate(2, .after = tidyselect::everything())
  tmp = get_footer_text(sign)
  footer = c(
    sprintf("Signifiance determined using %s", tmp$significance_test),
    footer_text
  )
  hux = hux %>% huxtable::insert_row(paste0(footer,collapse="\n"), after=nrow(hux), colspan = ncol(hux), fill="",copy_cell_props = FALSE)
  # hux = structure(hux, methods = get_footer_text(summary))
  return(hux)
}

#' Compares the population against an intervention in a summary table
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
#'   as [stringr::str_to_sentence]. (N.b. this function must be vectorised)
#' @param units (optional) a named list of units, following a `c(<colname_1> =
#'   "<unit_1>", <colname_2> = "<unit_2>", ...)` format. columns not present in
#'   this list are assumed to have no units. Units may be involved in the
#'   formatting of the summary output.
#' @param override_type (optional) a named list of data summary types. The
#'   default type for a column in a data set are calculated using heurisitics
#'   depending on the nature of the data (categorical or continuous), and result
#'   of normality tests. if you want to override this the options are
#'   `r paste0("\"", names(tableone::.summary_types), "\"", collapse= ",")` and you
#'   specify this on a column by column bases with a named list (e.g
#'   `c("Petal.Width"="mean_sd")`). Overriding the default does not check the
#'   type of data is correct for the summary type and will potentially cause
#'   errors if this is not done correctly.
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
#' @param footer_text any text that needs to be added at the end of the table.
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
#' old = options("tableone.show_pvalue_method"=TRUE)
#' missing_diamonds %>% dplyr::group_by(is_colored) %>% compare_population(-color, layout="relaxed")
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
  label_fn = ~ .x,
  units = list(),
  override_type = list(),
  layout = "compact",
  override_percent_dp = list(),
  override_real_dp = list(),
  p_format = names(.pvalue.defaults),
  font_size = 8,
  font = "Arial",
  footer_text = NULL
) {
  cols = .parse_vars(df, ...)
  p_format = match.arg(p_format)
  if (.is_formula_interface(df,...)) {
    intervention = cols[[1]]
    if (dplyr::is.grouped_df(df) & df %>% dplyr::groups() != intervention)
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

  shape = .get_shape(df,cols,label_fn)
  summary = .summary_stats(shape)
  fmt = .format_summary(summary, layout = layout)

  p_col = as.symbol(getOption("tableone.pvalue_column_name","P value"))
  method = getOption("tableone.show_pvalue_method",FALSE)

  sign = .significance_tests(summary) %>% .format_significance(p_format)
  fmt = fmt %>% dplyr::left_join(sign, by="variable")
  hux = fmt %>% .hux_tidy(rowGroupVars = ggplot2::vars(variable, !!p_col, characteristic), colGroupVars = intervention, defaultFontSize= font_size, defaultFont = font) %>%
    dplyr::relocate(2, .after = tidyselect::everything())
  tmp = get_footer_text(sign)

  if (method) {
    footer = c(
      sprintf("%s", tmp$table_key),
      sprintf("Normality of distributions determined using %s", tmp$normality_test),
      footer_text
    )
  } else {
    footer = c(
      sprintf("Signifiance determined using %s", tmp$significance_test),
      sprintf("Normality of distributions determined using %s", tmp$normality_test),
      footer_text
    )
  }

  hux = hux %>% huxtable::insert_row(paste0(footer,collapse="\n"), after=nrow(hux), colspan = ncol(hux), fill="",copy_cell_props = FALSE)
  # hux = structure(hux, methods = get_footer_text(summary))
  return(hux)
}



# compare_outcomes
# grouping is intervention
# possible formula outcome_1 + outcome_2 + outcome_3 ~ intervention + covariates
# covariates ignored?
# or list of formulae maybe from update?
# would this need bonferroni adjustment?


