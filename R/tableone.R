
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
    if (is.logical(tmp) | (is.numeric(tmp) & all(tmp %in% c(0,1,NA)))) {
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
          .message("Skipping conversion of ",col_label," as `.numeric` does not contain an entry for this column or a `.default` value")
        } else {
          levels = unique(glue::glue(numeric_glue, value=sort(unique(tmp)), name = col_label))
          if (length(levels) < max_levels) {
            new_tmp = glue::glue(numeric_glue, value=tmp, name = col_label)
            df = df %>% dplyr::mutate(!!col := factor(new_tmp,levels = levels,ordered = TRUE))
          } else {
            .message("Skipping factor conversion for ",col_label," because is has ",
                    length(levels)," levels and the maximum number allowed is set to ",max_levels)
          }
        }
      }
    }
  }
  return(df)
}

# explicit NA regardless of whether the level exists
.fct_explicit_na = function(f, na_level = "<missing>") {
  if (is.character(f)) f= factor(f)
  if (!is.factor(f)) stop("f must be a factor or a character vector")
  is_missing <- is.na(f)
  f <- forcats::fct_expand(f, na_level)
  f[is_missing] <- na_level
  f
}

#' Make NA values in factor columns explicit
#'
#' Converts NA values in any factors in the dataframe into a new level -
#' This is a thin wrapper for [forcats::fct_explicit_na()] but with missing
#' value level added regardless of whether any values missing. This forces an
#' empty row in count tables.
#'
#' @param df the data frame
#' @param na_level a label for NA valued factors
#' @param hide_if_empty dont add a missing data category if no data is missing
#'
#' @return the dataframe with all factor columns containing explicit na values
#' @export
#'
#' @examples
#' # before
#' missing_diamonds %>% dplyr::group_by(cut) %>% dplyr::count()
#' # after
#' missing_diamonds %>% explicit_na() %>% dplyr::group_by(cut) %>% dplyr::count()
explicit_na = function(df, na_level = "<missing>", hide_if_empty = FALSE) {
  if (hide_if_empty) {
    df %>% dplyr::ungroup() %>%
      dplyr::mutate(dplyr::across(tidyselect::where(is.factor), ~ forcats::fct_explicit_na(.x, na_level)))
  } else {
    df %>% dplyr::ungroup() %>%
      dplyr::mutate(dplyr::across(tidyselect::where(is.factor), ~ .fct_explicit_na(.x, na_level)))
  }
}

# missing_diamonds %>% data_missingness() %>% describe_population(tidyselect::everything())
.data_missingness = function(df) {
  df %>% dplyr::mutate(dplyr::across(tidyselect::everything(), ~ ifelse(is.na(.x) | is.infinite(.x) | is.nan(.x), "missing", "not missing") %>% factor(levels = c("missing","not missing"))))
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
  font_size = getOption("tableone.font_size",8),
  font = getOption("tableone.font","Arial"),
  footer_text = NULL
) {
  if (dplyr::is.grouped_df(df)) {
    .message("describe_population(...) ignores grouping.")
    df = df %>% dplyr::ungroup()
  }
  cols = .parse_vars(df, ...)
  label_fn = purrr::as_mapper(label_fn)
  shape = .get_shape(df,cols,label_fn,units)
  summary = .summary_stats(shape, override_type)
  if (is.list(layout)) {
    format = layout
  } else {
    format = getOption("tableone.format_list", default.format[[layout]])
  }
  fmt = .format_summary(summary, format=format, override_percent_dp = override_percent_dp, override_real_dp=override_real_dp)
  hux = fmt %>% .hux_tidy(rowGroupVars = dplyr::vars(variable, characteristic), colGroupVars = dplyr::vars(), defaultFontSize= font_size, defaultFont = font)
  tmp = get_footer_text(summary)
  footer = c(
    sprintf("Normality of distributions determined using %s", tmp$normality_test),
    footer_text
  )
  if (!getOption("tableone.hide_footer",isFALSE(footer_text))) {
    hux = hux %>%
      huxtable::insert_row(paste0(footer,collapse="\n"), after=nrow(hux), colspan = ncol(hux), fill="") %>%
      huxtable::set_bottom_border(row=huxtable::final(),value=0)
  }
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
#'   `options("tableone.hide_footer"=TRUE)`) .
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
  font_size = getOption("tableone.font_size",8),
  font = getOption("tableone.font","Arial"),
  footer_text = NULL
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
  label_fn = purrr::as_mapper(label_fn)

  shape = .get_shape(df,cols,label_fn)
  summary = .summary_stats(shape,override_type = override_type)
  if (is.list(layout)) {
    format = layout
  } else {
    format = getOption("tableone.format_list", default.format[[layout]])
  }
  fmt = .format_summary(summary, format=format, override_percent_dp = override_percent_dp, override_real_dp=override_real_dp)

  p_col = as.symbol(getOption("tableone.pvalue_column_name","P value"))
  method = getOption("tableone.show_pvalue_method",FALSE)

  sign = .significance_tests(summary) %>% .format_significance(p_format)
  fmt = fmt %>% dplyr::left_join(sign, by="variable")
  # TODO: this is where characteristic gets put in as a row group. intervention
  # is the colgroup but we need another nesting level in cols which is e.g. "value_col_name" and then "value_col_value" actually has the value.
  hux = fmt %>% .hux_tidy(rowGroupVars = dplyr::vars(variable, !!p_col, characteristic), colGroupVars = intervention, defaultFontSize= font_size, defaultFont = font) %>%
    dplyr::relocate(2, .after = tidyselect::everything())
  tmp = get_footer_text(sign)

  norm = NULL
  if (any(shape$.type == "continuous")) norm = sprintf("Normality of distributions determined using %s", tmp$normality_test)

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

  if (!getOption("tableone.hide_footer",isFALSE(footer_text))) {
    hux = hux %>%
      huxtable::insert_row(paste0(footer,collapse="\n"), after=nrow(hux), colspan = ncol(hux), fill="") %>%
      huxtable::set_bottom_border(row=huxtable::final(),value=0)
  }
  # hux = structure(hux, methods = get_footer_text(summary))
  return(hux)
}

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
      "subtype_count" = "{.sprintf_na('%1.1f%% - %s (%d/%d)',prob.0.5*100,.group,n,N)}",
      "median_iqr" = "{.sprintf_na('%1.3g (%s)',q.0.5, .group)}",
      "mean_sd" = "{.sprintf_na('%1.3g (%s)',mean, .group)}"
    )
    out = rep(NA_character_, nrow(tmp2))
    for (i in 1:nrow(tmp2)) {
      l = lapply(tmp2, `[[`, i)
      glue = simple[[l$.summary_type]]
      out[[i]] = glue::glue_data(l, glue)
    }

    s = out %>% paste0(collapse = " versus ")
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
    return(summary %>% left_join(
      comparison %>% select(-.label, -.order, -.unit, -.type, -N_total, -N_present),
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
    tmp = sign
    p = tmp %>% dplyr::select(.name, .label, .order, .unit, .type, N_total, N_present, .significance_test) %>% tidyr::unnest(.significance_test)
    if (".power_analysis" %in% colnames(tmp)) {
      a = tmp %>% dplyr::select(.name,.power_analysis) %>% tidyr::unnest(.power_analysis)
      return(p %>% left_join(a, by=".name", suffix=c("","power")))
    } else {
      return(p)
    }
  }
}


#' Get summary comparisons and statistics between variables as raw data.
#'
#' @inheritParams compare_population
#' @param ... the outcomes are specified either as a `tidyselect` specification,
#'   in which case the grouping of the `df` input determines the intervention
#'   and the output is the same a `compare_population()` call with a tidyselect.
#'   Alternatively a set of formulae can be provided that specify the outcomes
#'   on the left hand side, e.g. `outcome1 ~ intervention + cov1, outcome2 ~
#'   intervention + cov1, ...` in this case the `intervention` must be the same
#'   for all formulae and used to determine the comparison groups.
#'
#' @return a list of accessor functions for the summary data allowing granular
#'   access to the results of the analysis: * `compare(.variable,
#'   .characteristic = NULL)` - prints a comparison between the different
#'   intervention groups for the specified variable (and optionally the given
#'   characteristic if it is a categorical variable). * `filter(.variable,
#'   .intervention = NULL, .characteristic = NULL)` - extracts a given variable
#'   (e.g. `gender`), optionally for a given level of intervention (e.g.
#'   `control`) and if categorical a given characteristic (e.g. `male`). This
#'   will output a dataframe with all the calculated summary variables, for all
#'   qualifying intervention, variable and characteristic combinations,
#'   significance tests (and power analyses) for the qualifying variable
#'   (comparing intervention groups). * `signif_tests(.variable)` - extracts for
#'   a given variable (e.g. `gender`) the significance tests (and optionally
#'   power analyses) of the univariate comparison between different
#'   interventions and the variable. * `summary_stats(.variable, .intervention =
#'   NULL, .characteristic = NULL)` - extracts a given variable (e.g. `gender`),
#'   optionally for a given level of intervention (e.g. `control`) and if
#'   categorical a given characteristic (e.g. `male`). This returns only the
#'   summary stats for all qualifying intervention, variable and characteristic
#'   combinations.
#' @export
extract_comparison = function(
    df,
    ...,
    label_fn = ~ .x,
    override_type = list(),
    p_format = names(.pvalue.defaults),
    override_method = list(),
    power_analysis = FALSE,
    override_power = list()
) {
  p_format = match.arg(p_format)
  p_fun = getOption("tableone.pvalue_formatter",.pvalue.defaults[[p_format]])
  cols = .parse_vars(df, ...)
  if (.is_formula_interface(...)) {
    intervention = cols[[1]]
    if (dplyr::is.grouped_df(df) && (df %>% dplyr::groups() != intervention))
      warning("compare_missing(...) ignores grouping when using the formula interface.")
    df = df %>% dplyr::group_by(!!intervention)
  } else {
    intervention = df %>% dplyr::groups()
    if (length(intervention) == 0) stop(
      "If using the tidyselect interface, `df` must be grouped by the intervention")
    if (length(intervention) > 1) warning(
      "there are multiple columns defined in the intervention group.\n",
      "If you meant to do a nested comparison use a dplyr::group_modify().\n",
      "Otherwise this is likely a mistake."
    )
    cols = cols %>% setdiff(intervention)
  }
  shape = .get_shape(df, cols = cols, label_fn = label_fn)
  summary = .summary_stats(shape,override_type = override_type)
  sign = .significance_tests(summary, override_method = override_method)
  if (power_analysis) {
    sign = .power_analysis(sign, override_power = override_power)
  }
  return(list(
    compare = .comparison_printer(sign, intervention, p_fun),
    filter = .data_filter(sign, intervention),
    summary_stats = .summary_filter(sign, intervention),
    signif_tests = .comparison_filter(sign)
  ))
}



#' Compares multiple outcomes against an intervention in a summary table
#'
#' The outcome table is a simple summary of a binary or categorical outcome
#' in a data set compared by intervention groups. The comparison is independent
#' of any covariates, and is a preliminary output prior to more formal statistical
#' analysis or model fitting
#' .
#' It reports summary counts for the outcomes and a measure of significance of
#' the relationship between outcome and intervention. Interpretation of
#' significance tests, should include Bonferroni adjustment.
#'
#' @inheritParams compare_population
#' @param ... the outcomes are specified either as a `tidyselect` specification,
#'   in which case the grouping of the `df` input determines the intervention
#'   and the output is the same a `compare_population()` call with a tidyselect.
#'   Alternatively a set of formulae can be provided that specify the outcomes
#'   on the left hand side, e.g. `outcome1 ~ intervention + cov1, outcome2 ~
#'   intervention + cov1, ...` in this case the `intervention` must be the same
#'   for all formulae and used to determine the comparison groups.
#'
#' @return a `huxtable` formatted table.
#' @export
compare_outcomes = function(df,
      ...,
      label_fn = ~ .x,
      units = list(),
      override_type = list(),
      layout = "compact",
      override_percent_dp = list(),
      override_real_dp = list(),
      p_format = names(.pvalue.defaults),
      font_size = getOption("tableone.font_size",8),
      font = getOption("tableone.font","Arial"),
      footer_text = NULL
) {
  if (!.is_formula_interface(...)) {
    compare_population(df, ..., label_fn = label_fn, units = units,
      override_type = override_type, layout = layout, override_percent_dp = override_percent_dp,
      override_real_dp = override_real_dp, p_format = p_format, font_size = font_size,
      font = font, footer_text = footer_text
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
       font = font, footer_text = footer_text
    )
  }
}

# Missingness ----

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
    font_size = getOption("tableone.font_size",8),
    font = getOption("tableone.font","Arial"),
    significance_limit = 0.05,
    missingness_limit = 0.1,
    footer_text = NULL
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
  fmt = .format_summary(summary, layout = "missing") %>%
    dplyr::filter(type == "missing") %>%
    dplyr::select(-type)
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

  fsign = sign %>% .format_significance(p_format)
  fmt = fmt %>% dplyr::left_join(fsign, by="variable")
  hux = fmt %>% .hux_tidy(rowGroupVars = dplyr::vars(variable, !!p_col), colGroupVars = intervention, defaultFontSize= font_size, defaultFont = font) %>%
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
      label_fn = ~ .x,
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
    message(.mnar_message(mnar,intervention, significance_limit, label_fn))
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
          label_fn(rlang::as_label(intervention)),
          significance_limit / comparisons,
          significance_limit,
          comparisons,
          paste0(
            lapply(sapply(mnar, rlang::as_label),label_fn),
            collapse=", ")
  )
}
