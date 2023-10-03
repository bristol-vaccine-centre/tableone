## text utilities ----

### P-values ----

.pvalue.defaults = list(
  sampl = function(p) dplyr::case_when(p<0.001 ~ "<0.001", TRUE ~ .sprintf_na("%1.2g",p)),
  nejm = function(p) dplyr::case_when(p<0.001 ~ "<0.001", TRUE ~ .sprintf_na("%1.1g",p)),
  jama = function(p) dplyr::case_when(p<0.001 ~ "<0.001", p>0.99~">0.99", TRUE ~ .sprintf_na("%1.2g",p)),
  lancet = function(p) dplyr::case_when(p<0.0001 ~ "<0.0001", TRUE ~ .sprintf_na("%1.2g",p)),
  aim = function(p) dplyr::case_when(p<0.001 ~ "<0.001", p>= 0.001 & p<0.2 ~ .sprintf_na("%1.3f",p), TRUE ~ .sprintf_na("%1.2f",p))
)

#' Format a p-value
#'
#' Uses the default formatter set globally in `options("tableone.pvalue_formatter")` in
#' preference the one defined by `p_format` which is only used if no default is set.
#'
#' @param p.value the p-value to be formatted
#' @param p_format a name of a p-value formatter (one of `r paste0(names(.pvalue.defaults),collapse=", ")`)
#'
#' @return a formatted P-value
#' @export
format_pvalue = function(p.value, p_format = names(.pvalue.defaults)) {
  p_format = match.arg(p_format)
  fun = getOption("tableone.pvalue_formatter",.pvalue.defaults[[p_format]])
  fun(p.value)
}

### Missing value sprintfs ----

# .sprintf_no_na("%1.0f %s",c(1.0,2.0),c(NA,"sdfsdf"))
.sprintf_no_na = function(fmt, ...) {
  .na.value = getOption("tableone.na","\u2014")
  dots = rlang::list2(...)
  # this is a listwise OR:
  any_na = lapply(dots, is.na) %>% purrr::reduce(.f = `|`)
  ifelse(any_na, .na.value, sprintf(fmt,...) %>% .replace_dp())
}

# options("sprintf.missing"="?")
# .sprintf_na("%1.0f %s",c(1.0,2.0),c(NA,"sdfsdf"))
# .sprintf_na("%1.0f %s",c(1.0,2.0,Inf,NA),c(NA,"sdfsdf","inf",NA))
.sprintf_na = function(fmt, ...) {
  .na.value = getOption("tableone.na","\u2014")
  .missing = getOption("tableone.missing","<?>")
  dots = rlang::list2(...)
  dots = dots %>% purrr::map( ~ ifelse(is.infinite(.x) | is.nan(.x), NA, .x))
  all_na = lapply(dots, is.na) %>% purrr::reduce(.f = `&`)
  tmp = rlang::exec(sprintf, fmt, !!!dots)
  tmp = tmp %>% stringr::str_replace_all("NA",.missing)
  tmp = .replace_dp(tmp)
  ifelse(all_na, .na.value, tmp)
}

### post hoc change sprintf dp ----

# .replace_dp(c("1.0",".3","1.2.3","a.3", "[1.2]"), sep="\u00B7")
.replace_dp = function(c, sep=getOption("tableone.dp",".")) {
  if (sep==".") return(c)
  c %>% stringr::str_replace_all("([^A-Za-z0-9.]|\\s|^)([0-9]*)\\.([0-9]+)(?!\\.)", paste0("\\1\\2",sep,"\\3"))
}

# .adjust_fmt(c("'%1.1f%% [%1.1f%%\u2014%1.1f%%]'","'%1.1f%% [%1.1f%%\u2014%1.1f%%]'","'%1.1f%% [%1.1f%%\u2014%1.1f%%]'"), percent=c(3,4,NA), real=c(2,NA,5))
# .adjust_fmt(c("'%1.3g %s [%1.3g\u2014%1.3g]'","'%1.3g %s [%1.3g\u2014%1.3g]'","'%1.3g %s [%1.3g\u2014%1.3g]'"), percent=c(3,4,NA), real=c(2,NA,5))
# .adjust_fmt(c("'%1.3g %s [%1.3g\u2014%1.3g]'","'%1.3g %s [%1.3g\u2014%1.3g]'","'%1.3g %s [%1.3g\u2014%1.3g]'"), percent=c("3g","4f",NA), real=c("2g",NA,"5f"))
.adjust_fmt = function(fmt, percent=1, real=3) {
  percent = rep_len(percent, length(fmt))
  real = rep_len(real, length(fmt))
  fmt = dplyr::case_when(
    is.na(percent) ~ fmt,
    is.numeric(percent) ~ fmt %>% stringr::str_replace_all("%([0-9]+)\\.([0-9]+)(f|g)%%", paste0("%\\1.",percent,"\\3%%")),
    is.character(percent) ~ fmt %>% stringr::str_replace_all("%([0-9]+)\\.([0-9]+)(f|g)%%", paste0("%\\1.",percent,"%%")),
    TRUE ~ fmt
  )
  fmt = dplyr::case_when(
    is.na(real) ~ fmt,
    is.numeric(real) ~ fmt %>% stringr::str_replace_all("%([0-9]+)\\.([0-9]+)(f|g)(?!%%)", paste0("%\\1.",real,"\\3")),
    is.character(real) ~ fmt %>% stringr::str_replace_all("%([0-9]+)\\.([0-9]+)(f|g)(?!%%)", paste0("%\\1.",real)),
    TRUE ~ fmt
  )
  return(fmt)
}

### optional text ----

.maybe = function(s) {
  return(ifelse(is.na(s),"",s))
}

`%||%` = function(x,y) {
  if (is.null(x)) return(y) else return(x)
}
