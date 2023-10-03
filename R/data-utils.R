
## FACTORS / DISCRETISATION ----

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


#' Cut and label an integer valued quantity
#'
#' Deals with some annoying issues classifying integer data sets, such as ages, into groups. where you want to
#' specify just the change over points as integers and clearly label the resulting ordered factor.
#'
#' @param x a vector of integer valued numbers, e.g. ages, counts
#' @param cut_points a vector of integer valued cut points which define the lower, inclusive boundary of each group
#' @param glue a glue spec that may be used to generate a label. It can use {low}, {high}, {next_low}, or {label} as values.
#' @param lower_limit the minimum value we should include (this is inclusive for the bottom category) (default -Inf)
#' @param upper_limit the maximum value we should include (this is also inclusive for the top category) (default Inf)
#' @param ... not used
#'
#' @return an ordered factor of the integer
#' @export
#'
#' @examples
#' cut_integer(stats::rbinom(20,20,0.5), c(5,10,15))
#' cut_integer(floor(stats::runif(100,-10,10)), cut_points = c(2,3,4,6), lower_limit=0, upper_limit=10)
#' cut_integer(1:10, cut_points = c(1,3,9))
cut_integer = function(x, cut_points, glue = "{label}", lower_limit = -Inf, upper_limit = Inf, ...) {

  next_low = NULL # remove global binding note

  if (!all(as.integer(x)==x,na.rm = TRUE)) warning("input to cut_integer(...) has been coerced to integer values")
  x = floor(x)
  if (!all(as.integer(cut_points)==cut_points)) stop("cut_points must be integer valued, and define the lower end of each category.")
  if (any(cut_points <= lower_limit | cut_points >= upper_limit)) warning("cut_point values should be between lower_limit (",lower_limit,") and upper_limit (",upper_limit,").")
  # make sure the limits are not included.
  cut_points = cut_points[cut_points > lower_limit & cut_points < upper_limit]
  # sort and uniquify
  #
  breaks = unique(sort(c(lower_limit,cut_points,upper_limit+1)))
  labels = tibble::tibble(
    low = utils::head(breaks,-1),
    next_low = utils::head(dplyr::lead(breaks,n = 1),-1),
    high = ifelse(next_low != upper_limit, next_low-1, upper_limit)
  ) %>% dplyr::mutate(
    label = dplyr::case_when(
      low == high ~ sprintf("%1.0f",low),
      low == -Inf ~ sprintf("<%1.0f",next_low),
      high == Inf ~ sprintf("\u2265%1.0f",low),
      TRUE ~ sprintf("%1.0f\u2012%1.0f",low, high)
    )
  ) %>% dplyr::mutate(
    label2 = glue::glue(glue)
  )

  return(
    cut(x,include.lowest = TRUE,breaks = breaks, labels=labels$label2, ordered_result = TRUE, right=FALSE)
  )
}


## MISSING VALUES ----

# explicit NA regardless of whether the level exists
.fct_explicit_na = function(f, na_level = "<missing>", force = TRUE) {
  if (is.character(f)) f= factor(f)
  if (!is.factor(f)) stop("f must be a factor or a character vector")
  is_missing <- is.na(f)
  if (!any(is_missing) && force == FALSE) {
    # nothing missing and level is optional
    return(f)
  } else {
    f <- forcats::fct_expand(f, na_level)
    f[is_missing] <- na_level
    return(factor(f,ordered=FALSE))
  }
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
  df %>% dplyr::ungroup() %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.factor), ~ .fct_explicit_na(.x, na_level, force=!hide_if_empty)))
}


## LABELS ----

#' Set a label attribute
#'
#' @param df a dataframe
#' @param labels a vector of labels, one for each column
#' @param attribute the name of the label attribute (defaults to `"label"`)
#'
#' @return the same dataframe with each column labelled
#' @export
#'
#' @examples
#' iris = set_labels(iris,
#'   c("Sepal Length", "Sepal Width",
#'     "Petal Length", "Petal Width",  "Species"
#'    ))
#' fn = label_extractor(iris,tolower)
#' fn(colnames(iris))
set_labels = function(df, labels, attribute="label") {
  if (length(labels) != ncol(df)) stop("Labels must be same length as dataframe columns")
  i = 0
  for(label in labels) {
    i = i+1
    attr(df[[i]],attribute) = label
  }
  return(df)
}

#' Extract labels from a dataframe column attributes
#'
#' Retrieve column labels are embedded as an attribute of each column.
#'
#' @param df a dataframe containing some labels
#' @param ... additional string manipulation functions to apply e.g. `tolower`
#' @param attribute the name of the label containing attribute (defaults to `"label"`)
#'
#' @return a labelling function. This is specific to the dataframe provided in `df`
#' @export
#'
#' @examples
#' iris = set_labels(iris, c(
#'     "Sepal Length", "Sepal Width",
#'     "Petal Length", "Petal Width",  "Species"
#'  ))
#' fn = label_extractor(iris,tolower)
#' fn(colnames(iris))
label_extractor = function(df, ..., attribute="label") {
  labels = unname(sapply(df, function(x) { tmp = attr(x,attribute); if (is.null(tmp)) return(NA_character_) else return(tmp) }))
  cols = ifelse(is.na(labels), colnames(df), labels)
  dots = rlang::list2(...)
  for (fn in dots) {
    if (is.function(fn)) cols = fn(cols)
  }
  names(cols) = colnames(df)
  return(
    function(x) {
      if (any(!x %in% names(cols))) stop("The `label_extractor` does not recognise the columns in this dataframe.\nWEach label_extractor is specific to one dataframe.")
      cols[x]
    }
  )
}

## Units ----

# Slightly different you can define the unit on a column by column basis.

# iris %>% set_units(-Species, units="mm")
#' Title
#'
#' @param df a dataframe
#' @param ... a tidyselect specification or a formula
#' @param units a list of unit as strings which must be either 1 or the same length as
#'  the columns matched by the tidyselect.
#'
#' @return the dataframe with the `unit` attribute updated
#' @export
#'
#' @examples
#' iris = iris %>% set_units(-Species, units="mm")
#' iris %>% extract_units()
set_units = function(df, ..., units) {
  cols = .parse_vars(df, ..., .side = "both")
  if (length(units)==1) units = rep(units,length(cols))
  if (length(cols) != length(units)) stop("`cols` and `units` must be the same length")
  for (i in seq_along(cols)) {
    col = rlang::as_label(cols[[i]])
    unit = units[[i]]
    attr(df[[col]],"unit") = unit
  }
  return(df)
}



#' Extracts units set as dataframe column attributes
#'
#' @param df the data frame from `set_units()`
#'
#' @return a named list of column / unit pairs.
#' @export
#'
#' @examples
#' iris = iris %>% set_units(-Species, units="mm")
#' iris %>% extract_units()
extract_units = function(df) {
  labels = unname(sapply(df, function(x) { attr(x,"unit") %||% NA_character_}))
  names(labels) = colnames(df)
  return(stats::na.omit(labels))
}
