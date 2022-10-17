## text utilities ----

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

# .replace_dp(c("1.0",".3","1.2.3","a.3", "[1.2]"), sep="\u00B7")
.replace_dp = function(c, sep=getOption("tableone.dp",".")) {
  if (sep==".") return(c)
  c %>% stringr::str_replace_all("([^A-Za-z0-9.]|\\s|^)([0-9]*)\\.([0-9]+)(?!\\.)", paste0("\\1\\2",sep,"\\3"))
}

# .adjust_fmt(c("'%1.1f%% [%1.1f%%\u2014%1.1f%%]'","'%1.1f%% [%1.1f%%\u2014%1.1f%%]'","'%1.1f%% [%1.1f%%\u2014%1.1f%%]'"), percent=c(3,4,NA), real=c(2,NA,5))
# .adjust_fmt(c("'%1.3g %s [%1.3g\u2014%1.3g]'","'%1.3g %s [%1.3g\u2014%1.3g]'","'%1.3g %s [%1.3g\u2014%1.3g]'"), percent=c(3,4,NA), real=c(2,NA,5))
.adjust_fmt = function(fmt, percent=1, real=3) {
  percent = rep_len(percent, length(fmt))
  real = rep_len(real, length(fmt))
  fmt = ifelse(is.na(percent), fmt, fmt %>% stringr::str_replace_all("%([0-9]+)\\.([0-9]+)(f|g)%%", paste0("%\\1.",percent,"\\3%%")))
  fmt = ifelse(is.na(real), fmt, fmt %>% stringr::str_replace_all("%([0-9]+)\\.([0-9]+)(f|g)(?!%%)", paste0("%\\1.",real,"\\3")))
  return(fmt)
}

.maybe = function(s) {
  return(ifelse(is.na(s),"",s))
}
