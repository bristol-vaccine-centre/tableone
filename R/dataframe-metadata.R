## Get metadata from a dataframe ----

### setup normality tests ----
.name_of = function(fun) {
  return(fun(stats::rnorm(50))$method)
}

#' @importFrom nortest ad.test
.wrap = function(fun) {
  return(function(x) {
    tryCatch({fun(x)$p.value}, error=function(e) NA_real_)
  })
}

.normality.tests = list(
  "ad" = list(fun = .wrap(nortest::ad.test), name = .name_of(nortest::ad.test)),
  "cvm" = list(fun = .wrap(nortest::cvm.test), name = .name_of(nortest::cvm.test)),
  "lillie" = list(fun = .wrap(nortest::lillie.test), name = .name_of(nortest::lillie.test)),
  "pearson" = list(fun = .wrap(nortest::pearson.test), name = .name_of(nortest::pearson.test)),
  "sf" = list(fun = .wrap(nortest::sf.test), name = .name_of(nortest::sf.test))
)

### extract column names and types ----

# df column names as symbol list
# .col_symbols(one_class_test_100)
.col_symbols = function(df) {
  lapply(colnames(df),as.symbol)
}

# symbol list as character vector
# .col_symbols(one_class_test_100) %>% .col_names()
.col_names = function(cols) {
  unlist(lapply(cols,dplyr::as_label))
}

# grab df columns as nested list or vectors of data.
.pull_cols = function(cols, df) {
  lapply(cols, function(col) {
    dplyr::pull(df,!!col)
  }) %>% unname()
}

# select df columns as nested list of dataframes, with x as data and y as
# grouping
.select_content = function(cols, df, grps = df %>% dplyr::groups()) {
  lapply(cols, function(col) {
    col = rlang::ensym(col)
    dplyr::select(df,!!!grps, x=!!col) %>%
      dplyr::group_by(!!!grps) %>%
      dplyr::mutate(y = dplyr::cur_group_id())
  }) %>% unname()
}

# is column categorical or continuous?
.col_type = function(col, max_levels) {
  tmp = col
  if (is.factor(tmp)) return("categorical")
  if (is.logical(tmp)) return("categorical")
  if (is.numeric(tmp) & length(unique(tmp)) < max_levels) return("categorical")
  if (is.numeric(tmp)) return("continuous")
  return(NA_character_)
}

# how many levels does discrete data have (integer data with more than max_levels
# is treated as continuous)
.col_levels = function(col, max_levels) {
  tmp = col
  if (is.factor(tmp)) return(length(levels(tmp)))
  if (is.logical(tmp)) return(2)
  if (is.numeric(tmp) & length(unique(tmp)) < max_levels) return(length(unique(tmp)))
  return(NA_integer_)
}

# is the data normal according to a test
# test type is found from `.normality.tests`
.p_is_normal = function(col, method = c("ad","cvm","lillie","pearson","sf"), trans=function(x) x) {
  if (!is.numeric(col)) return(NA)
  method = match.arg(method)
  fun = .normality.tests[[method]]
  col2 = trans(col)
  return(fun$fun(col2))
}

# detect the probability that a data item is tied.
.p_ties = function(col) {
  if (!is.numeric(col)) return(1)
  return(1- length(unique(stats::na.omit(col))) / length(stats::na.omit(col)))
}

# what fraction of the col vector is missing?
.p_missing = function(col) {
  return(1 - length(stats::na.omit(col)) / length(col))
}

# gets metadata about the dataframe necessary to decide on comparison tests
# labeller takes a list of strings of column labels and converts it to a
# useful list of display label strings ( could be a simple case_when )
# labeller = avoncap::readable_label_mapping
# labeller = .col_names
# This will take grouped dataframes which are the then used as the main axis
# for any comparisons
# e.g. no comparison:
# diamonds %>% .get_shape(.col_names, ggplot2::vars(carat, cut, color, clarity, depth, price))
# diamonds %>% .get_shape()
# e.g. with binary comparison:
# df_shape = diamonds %>%  dplyr::mutate(is_clear = clarity>"VS2") %>% dplyr::group_by(is_clear) %>% .get_shape()
# e.g. with multi way comparison:
# df_shape = iris %>% dplyr::group_by(Species) %>% .get_shape()
.get_shape = function(df, cols = .col_symbols(df), label_fn = function(x) x, units = list()) {

  grps = df %>% dplyr::groups()
  if (dplyr::is.grouped_df(df)) {
    cols = setdiff(cols, grps)
    # return(df %>% dplyr::group_modify(function(d,g,...) .get_shape(d,labeller = labeller, cols=cols) ))
    grp_count = df %>% dplyr::group_data() %>% nrow()
  } else {
    grp_count = 1
  }
  units = as.list(units)

  df = df %>% dplyr::ungroup()

  label_fn = getOption("tableone.labeller",label_fn)
  max_levels = getOption("tableone.max_discrete_levels",0)
  normality_test = getOption("tableone.normality_test","ad")

  not_matched = setdiff(.col_names(cols), colnames(df))
  if (length(not_matched)>0) {
    stop("Columns found in `cols` parameter but not present in dataframe: ",paste0(not_matched, collapse = ", "))
  }

  tmp = tibble::tibble(
    .cols = cols,
    .source = .select_content(cols, df, grps),
    .content = .pull_cols(cols, df),
    .name = .col_names(cols),
    .label = label_fn(.col_names(cols)),
    .order = 1:length(cols),
    .comparisons = grp_count
  ) %>% dplyr::mutate(
    .unit = lapply(.name,function(n) units[[n]]) %>% sapply(function(x) ifelse(is.null(x),"",x)),
    .type = lapply(.content, .col_type, max_levels) %>% unlist(),
    .levels = lapply(.content, .col_levels, max_levels) %>% unlist(),
    .p_is_normal = lapply(.content, .p_is_normal, method=normality_test) %>% unlist(),
    # .p_is_log_normal = lapply(.content, .p_is_normal, method=normality_test, trans=log) %>% unlist(),
    .p_ties = lapply(.content, .p_ties) %>% unlist(),
    .p_missing = lapply(.content, .p_missing) %>% unlist()
  )

  .normality_test_name = .normality.tests[[normality_test]]$name
  structure(tmp, methods = list(normality_test = .normality_test_name))

}
