

## significance tests ----

### some signifiance test wrapper functions ----

# twoclass = diamonds %>% dplyr::mutate(is_clear = ifelse(clarity>"VS2","clear","less clear"))
# xy_df = twoclass %>% dplyr::group_by(is_clear) %>% dplyr::mutate(y=dplyr::cur_group_id(), x=price) %>% dplyr::select(y,x)
# xy_df %>% .do_ks()
.do_ks = function(xy_df) {
  if (any(xy_df$y > 2)) stop("too many groups for a 2 sided ks test")
  side1 = xy_df %>% dplyr::filter(y==1) %>% dplyr::pull(x)
  side2 = xy_df %>% dplyr::filter(y==2) %>% dplyr::pull(x)
  pval = tryCatch({stats::ks.test(side1,side2) %>% broom::tidy() %>% dplyr::pull(p.value)}, error=function(e) NA) # not able to perform ks test - e.g. all zeros
  return(tibble::tibble(p.value = pval, p.method = "2 sample Kolmogorov-Smirnov test"))
}

# xy_df = twoclass %>% dplyr::group_by(is_clear) %>% dplyr::mutate(y=dplyr::cur_group_id(), x=price) %>% dplyr::select(y,x)
# xy_df %>% .do_wilcoxon()
.do_wilcoxon = function(xy_df) {
  if (any(xy_df$y > 2)) stop("too many groups for a 2 sided wilcoxon test")
  side1 = xy_df %>% dplyr::filter(y==1) %>% dplyr::pull(x)
  side2 = xy_df %>% dplyr::filter(y==2) %>% dplyr::pull(x)
  pval = tryCatch({stats::wilcox.test(side1,side2) %>% broom::tidy() %>% dplyr::pull(p.value)}, error=function(e) NA) # not able to perform ks test - e.g. all zeros
  return(tibble::tibble(p.value = pval, p.method = "2 sample Wilcoxon Rank Sum test"))
}

# categorical data
# xy_df = twoclass %>% dplyr::group_by(is_clear) %>% dplyr::mutate(y=dplyr::cur_group_id(), x=cut) %>% dplyr::select(y,x)
.do_fisher = function(xy_df) {
  chi.data = with(xy_df, table(x,y))
  # pval = suppressWarnings(stats::chisq.test(chi.data) %>% broom::tidy() %>% dplyr::pull(p.value))
  pval = tryCatch({
    suppressWarnings(stats::fisher.test(chi.data, simulate.p.value=TRUE,B=1e5) %>%
                       broom::tidy() %>% dplyr::pull(p.value))
  },error= function(e) NA_real_)
  return(tibble::tibble(p.value = pval, p.method = "Fisher's exact test"))
}

# ordered data
.do_cochran_armitage = function(xy_df) {
  if (any(xy_df$y > 2)) stop("too many groups for a Chi-squared Test for Trend in Proportions")

  tmp = xy_df %>%
    dplyr::group_by(x,y) %>% dplyr::count() %>%
    dplyr::group_by(x) %>% dplyr::mutate(N = sum(n)) %>%
    dplyr::filter(y==1)

  pval = tryCatch({stats::prop.trend.test(tmp$n, tmp$N) %>% broom::tidy() %>% dplyr::pull(p.value)}, error=function(e) NA)
  return(tibble::tibble(p.value = pval, p.method = "Chi-squared Test for Trend in Proportions"))

}

# xy_df = twoclass %>% dplyr::group_by(is_clear) %>% dplyr::mutate(y=dplyr::cur_group_id(), x=price) %>% dplyr::select(y,x)
# xy_df %>% .do_ttest()
.do_ttest = function(xy_df) {
  if (any(xy_df$y > 2)) stop("too many groups for a 2 sided t-test")
  side1 = xy_df %>% dplyr::filter(y==1) %>% dplyr::pull(x)
  side2 = xy_df %>% dplyr::filter(y==2) %>% dplyr::pull(x)
  pval = tryCatch({stats::t.test(side1,side2) %>% broom::tidy() %>% dplyr::pull(p.value)}, error=function(e) NA) # not able to perform ks test - e.g. all zeros
  return(tibble::tibble(p.value = pval, p.method = "2 sided student's t-test"))
}

# xy_df = iris %>% dplyr::group_by(Species) %>% dplyr::mutate(y=dplyr::cur_group_id(), x=Petal.Width)
# xy_df %>% .do_anova()
.do_anova = function(xy_df) {
  pval = tryCatch({
    model = stats::lm(x ~ y, xy_df)
    stats::anova(model) %>%  broom::tidy() %>% dplyr::pull(p.value) %>% `[`(1)
  }, error=function(e) NA)
  return(tibble::tibble(p.value = pval, p.method = "Analysis of variance (linear model)"))
}

# xy_df = iris %>% dplyr::group_by(Species) %>% dplyr::mutate(y=dplyr::cur_group_id(), x=Petal.Width)
# xy_df %>% .do_kruskal()
.do_kruskal = function(xy_df) {
  pval = tryCatch({stats::kruskal.test(x = xy_df$x, g= xy_df$y) %>% broom::tidy() %>% dplyr::pull(p.value)}, error=function(e) NA) # not able to perform ks test - e.g. all zeros
  return(tibble::tibble(p.value = pval, p.method = "Kruskal-Wallis rank sum test"))
}

.do_nothing = function(xy_df) {
  return(tibble::tibble(p.value = NA_real_, p.method = "\u2014"))
}

.do_missing = function(xy_df) {
  return(tibble::tibble(p.value = NA_real_, p.method = "Not calculated due to missing values"))
}

### mapping to comparion method ----
# these functions must accept a dataframe with y (cateogory) and x (data) columns.
# and output a single row tibble containing a p-value and a method column
.comparison.fns = list(
  "chi-sq trend" = .do_cochran_armitage,
  "fisher" = .do_fisher,
  "t-test" = .do_ttest,
  "2-sided wilcoxon" = .do_wilcoxon,
  "2-sided ks" = .do_ks,
  "anova" = .do_anova,
  "kruskal-wallis" = .do_kruskal,
  "no comparison" = .do_nothing
)

### do significance tests ----

# df_shape %>% glimpse() %>% .significance_tests()
.significance_tests = function(df_shape, override_method = list()) {

  # See if user wants to override comparison method?
  if (length(override_method) > 0) {
    if (any(!override_method %in% names(.comparison.fns)))
      stop("override method must be one of ", paste0(.comparison.fns,collapse=", "))
    # override_type = list("multinom_class"="mean_sd")
    override = tibble::tibble(
      .name = names(override_method),
      .override_method = unlist(override_method)
    )
    df_shape = df_shape %>% dplyr::left_join(override, by=".name") %>%
      dplyr::mutate(.comparison_method = ifelse(!is.na(.override_method), .override_method, .comparison_method)) %>%
      dplyr::select(-.override_method)
  }

  comp_grps = df_shape$.source %>% purrr::map(~ dplyr::groups(.x)) %>% unique()
  if (length(comp_grps) != 1) stop("multiple different interventions detected. please raise this as a big in tableone")
  intervention = purrr::list_c(comp_grps) %>% unique()
  if (length(intervention) > 1) warning("Intervention grouping is defined over multiple columns. Support for this is experimental.")
  intervention_levels = df_shape$.source %>%
    purrr::map(~ dplyr::select(.x,!!!intervention)) %>%
    dplyr::bind_rows() %>% dplyr::distinct()

  df_shape$.significance_test = list(rep(NA,nrow(df_shape)))
  for (i in 1:nrow(df_shape)) {
    # get dataframe row as a list
    tmp = df_shape %>% purrr::map(~ .x[[i]])
    .message(tmp$.comparison_method, " test on ",tmp$.label)
    fun = .comparison.fns[[tmp$.comparison_method]]
    d = tmp$.source
    if (any(is.na(d$x))) {
      .message("Significance testing skipped due to missing values: ", tmp$.label)
      fun = .do_missing
    }
    # this is where the significance test is done
    result = fun(d)
    df_shape$.significance_test[[i]] = result
  }

  return(structure(
    df_shape,
    intervention = intervention,
    intervention_levels = intervention_levels,
    class = c("t1_signif",class(df_shape))))
}



