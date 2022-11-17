

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



## power calculations ----

### some power calculation wrapper functions ----

# https://med.und.edu/daccota/_files/pdfs/berdc_resource_pdfs/sample_size_r_module.pdf
# https://www.frontiersin.org/articles/10.3389/fpsyg.2013.00863/full#:~:text=Cohen's%20d%20in%20between%2Dsubject,equals%20half%20a%20standard%20deviation.

# twoclass = diamonds %>% dplyr::mutate(is_clear = ifelse(clarity>"VS2","clear","less clear"))
# xy_df = twoclass %>% dplyr::group_by(is_clear) %>% dplyr::mutate(y=dplyr::cur_group_id(), x=price) %>% dplyr::select(y,x)
# xy_df %>% .do_ks()
.pwr_ks = function(xy_df, significance_limit, power) {
  return(.pwr_ttest(xy_df, significance_limit, power, 1.15))
}

# xy_df = twoclass %>% dplyr::group_by(is_clear) %>% dplyr::mutate(y=dplyr::cur_group_id(), x=price) %>% dplyr::select(y,x)
# xy_df %>% .do_wilcoxon()
.pwr_wilcoxon = function(xy_df, significance_limit, power) {
  return(.pwr_ttest(xy_df, significance_limit, power, 1.15))
}

# categorical data
# xy_df = two_class_test %>% dplyr::group_by(grouping) %>% dplyr::mutate(y=dplyr::cur_group_id(), x=multinom_class) %>% dplyr::select(y,x)
# xy_df %>% .do_fisher()
# xy_df %>% .pwr_fisher(0.05,0.8)
.pwr_fisher = function(xy_df, significance_limit, power) {
  chi.data = with(xy_df, table(x,y))
  # Chi-sqared testing fails if all of one marginal probability is zero as the
  # Expected value in the given cell is zero. We remove such rows or columns
  # (assuming they are sampling and not structural zeros) and calculate the chisq
  # statistic and hence power on the remaining data. This will give an answer but
  # it might not be the most exactly correct.
  x = apply(chi.data,MARGIN = 1, function(x) !all(x==0))
  y = apply(chi.data,MARGIN = 2, function(x) !all(x==0))
  tmp = suppressWarnings(chisq.test(chi.data[x,y]))
  chisq = tmp$statistic


  df = tmp$parameter
  n = nrow(xy_df)
  obs_w = sqrt(chisq/(n*df))

  tmp2 = tryCatch(pwr::pwr.chisq.test(N=n, df=df, sig.level=significance_limit, power=power ), error = function(e) NA)
  w = tmp2$w

  interp = case_when(
    tmp2$w < 0.1 ~ "small",
    tmp2$w < 0.3 ~ "small to medium",
    tmp2$w < 0.5 ~ "medium to large",
    TRUE ~ "large"
  )

  # est.min.detection.limit = detectable

  # # pval = suppressWarnings(stats::chisq.test(chi.data) %>% broom::tidy() %>% dplyr::pull(p.value))
  # pval = tryCatch({
  #   suppressWarnings(stats::fisher.test(chi.data, simulate.p.value=TRUE,B=1e5) %>%
  #                      broom::tidy() %>% dplyr::pull(p.value))
  # },error= function(e) NA_real_)
  return(tibble::tibble(
    power.method = "Chi-squared power analysis",
    effect.size.observed = obs_w,
    effect.size.min.detectable = tmp2$w,
    effect.size.type = "Cohen's w",
    effect.size.interpretation = interp,
    power.alpha = significance_limit,
    power.beta = power))
}

# xy_df = two_class_test %>% dplyr::group_by(grouping) %>% dplyr::mutate(y=dplyr::cur_group_id(), x=normal_variable) %>% dplyr::select(y,x)
# xy_df %>% .do_ttest()
# xy_df %>% .pwr_ttest(0.05, 0.8)
.pwr_ttest = function(xy_df, significance_limit, power, factor=1) {
  # significance_limit = 0.05
  if (any(xy_df$y > 2)) stop("too many groups for a 2 sided t-test")
  side1 = xy_df %>% dplyr::filter(y==1) %>% dplyr::pull(x)
  side2 = xy_df %>% dplyr::filter(y==2) %>% dplyr::pull(x)
  n1 = floor(length(side1) / factor)
  s1 = sd(side1, na.rm = TRUE)
  n2 = floor(length(side2) / factor)
  s2 = sd(side2, na.rm = TRUE)
  # Cohens D
  sd_pooled = sqrt( ((n1-1)*s1^2 + (n2-1)*s2^2 )/(n1+n2-2) )
  obs_d = abs( mean(side1,na.rm=TRUE)-mean(side2,na.rm=TRUE)) / sd_pooled
  # tmp = tryCatch(pwr::pwr.t2n.test(n1 = length(side1), n2 = length(side2), d=c(0.2,0.5,0.8), sig.level=significance_limit ), error = function(e) NA)
  tmp2 = tryCatch(pwr::pwr.t2n.test(n1 = length(side1), n2 = length(side2), sig.level=significance_limit, power=power ), error = function(e) NA)
  detectable = tmp2$d * sd_pooled
  interp = case_when(
    tmp2$d < 0.2 ~ "small",
    tmp2$d < 0.5 ~ "small to medium",
    tmp2$d < 0.8 ~ "medium to large",
    TRUE ~ "large"
  )
  method = "t-test power analysis"
  if (factor!=1) method = sprintf("%s (+%1.0f%%)",method, (1-factor)*100)
  return(tibble::tibble(
    power.method = method,
    effect.size.observed = obs_d,
    effect.size.min.detectable = tmp2$d,
    # est.min.detection.limit = detectable,
    effect.size.type = "Cohen's d",
    effect.size.interpretation = interp,
    power.alpha = significance_limit,
    power.beta = power))
}

# xy_df = iris %>% dplyr::group_by(Species) %>% dplyr::mutate(y=dplyr::cur_group_id(), x=Petal.Width)
# xy_df %>% .do_anova()
.pwr_anova = function(xy_df, significance_limit, power, factor=1) {
  # number of groups
  k = length(unique(xy_df$y))
  # we are taking an minimum group size as we do not enforce equality
  n = xy_df %>% group_by(y) %>% count() %>% pull(n) %>% min()
  n = n / factor
  tmp2 = tryCatch(pwr::pwr.anova.test(k=k, n=n, sig.level=significance_limit, power=power ), error = function(e) NA)
  obs_f = .cohens_f(xy_df)

  method = "One-way ANOVA power analysis"
  if (factor!=1) method = sprintf("%s (+%1.0f%%)",method, (1-factor)*100)

  interp = case_when(
    tmp2$f < 0.1 ~ "small",
    tmp2$f < 0.25 ~ "small to medium",
    tmp2$f < 0.4 ~ "medium to large",
    TRUE ~ "large"
  )

  return(tibble::tibble(
    power.method = tmp2$method,
    effect.size.observed = obs_f,
    effect.size.min.detectable = tmp2$f,
    effect.size.type = "Cohen's f",
    effect.size.interpretation = interp,
    power.alpha = significance_limit,
    power.beta = power))

  #For f-tests:
  #  0.1=small, 0.25=medium, and 0.4 large effect sizes
  # pval = tryCatch({
  #   model = stats::lm(x ~ y, xy_df)
  #   stats::anova(model) %>%  broom::tidy() %>% dplyr::pull(p.value) %>% `[`(1)
  # }, error=function(e) NA)
  # return(tibble::tibble(p.value = pval, p.method = "Analysis of variance (linear model)"))
}

# aovstat2 = rstatix::eta_squared(lm(x ~ y, xy_df))
# .cohens_f(xy_df) == sqrt(aovstat2/(1 - aovstat2))
.cohens_f = function(xy_df) {
  # adapted from https://github.com/kassambara/rstatix/blob/master/R/eta_squared.R
  model = stats::lm(x ~ y, xy_df)
  model = stats::anova(model)
  aov.sum = broom::tidy(model)
  meansq.resid = aov.sum[["meansq"]][nrow(aov.sum)]
  ss.total = sum(aov.sum[["sumsq"]])
  ss.resid = aov.sum[["sumsq"]][nrow(aov.sum)]
  aovstat = aov.sum[["sumsq"]][1]/(aov.sum[["sumsq"]][1] + ss.resid)
  aovstat = sqrt(aovstat/(1 - aovstat))
  aovstat
}

# xy_df = iris %>% dplyr::group_by(Species) %>% dplyr::mutate(y=dplyr::cur_group_id(), x=Petal.Width)
# xy_df %>% .do_kruskal()
.pwr_kruskal = function(xy_df, significance_limit, power) {
  return(.pwr_anova(xy_df, significance_limit, power, 1.15))
}

.pwr_nothing = function(xy_df, significance_limit, power) {
  return(tibble::tibble(
    power.method = "\u2014",
    effect.size.observed = NA_real_,
    effect.size.min.detectable = NA_real_,
    effect.size.type = "\u2014",
    effect.size.interpretation = NA_character_,
    power.alpha = significance_limit,
    power.beta = power))
}

.pwr_missing = function(xy_df, significance_limit, power) {
  # TODO: consider if this is the right thing.
  return(tibble::tibble(
    power.method = "Not calculated due to missing values",
    effect.size.observed = NA_real_,
    effect.size.min.detectable = NA_real_,
    effect.size.type = "\u2014",
    effect.size.interpretation = NA_character_,
    power.alpha = significance_limit,
    power.beta = power))
}

### mapping to comparion method ----
# these functions must accept a dataframe with y (cateogory) and x (data) columns.
# and output a single row tibble containing a p-value and a method column
.comparison.fns = list(
  "fisher" = .do_fisher,
  "t-test" = .do_ttest,
  "2-sided wilcoxon" = .do_wilcoxon,
  "2-sided ks" = .do_ks,
  "anova" = .do_anova,
  "kruskal-wallis" = .do_kruskal,
  "no comparison" = .do_nothing
)

### mapping to power calculatation method ----
# these functions must accept a dataframe with y (cateogory) and x (data) columns.
# and output a single row tibble containing a p-value and a method column
.power.fns = list(
  "fisher" = .pwr_fisher,
  "t-test" = .pwr_ttest,
  "2-sided wilcoxon" = .pwr_wilcoxon, # consider .pwr_nothing
  "2-sided ks" = .pwr_ks, # consider .pwr_nothing
  "anova" = .pwr_anova,
  "kruskal-wallis" = .pwr_kruskal, # consider .pwr_nothing
  "no comparison" = .pwr_nothing
)


### do significance tests ----

.determine_comparison_method = function(df_shape) {
  normality_signif = getOption("tableone.normality_significance",0.005)
  ties_cutoff = getOption("tableone.tolerance_to_ties",0.25)

  if (!".comparison_method" %in% colnames(df_shape)) {
    # what kind of summary stats?
    df_shape = df_shape %>% dplyr::mutate(
      .comparison_method = dplyr::case_when(
        # categorical:
        .type == "categorical" ~ "fisher",
        # 2 sided continuous:
        # continuous normally distributed
        .comparisons == 2 & .type == "continuous" & .p_is_normal >= normality_signif ~ "t-test",
        # continuous not normally distributed with ties
        .comparisons == 2 & .type == "continuous" & .p_is_normal < normality_signif & .p_ties >= ties_cutoff ~ "2-sided wilcoxon",
        # continuous not normally distributed without ties
        .comparisons == 2 & .type == "continuous" & .p_is_normal < normality_signif & .p_ties < ties_cutoff ~ "2-sided ks",
        # multi-way continuous:
        # continuous normally distributed
        .comparisons > 2 & .type == "continuous" & .p_is_normal >= normality_signif ~ "anova",
        # continuous not normally distributed
        .comparisons > 2 & .type == "continuous" & .p_is_normal < normality_signif ~ "kruskal-wallis",
        TRUE ~ "no comparison"
      )
    )
  }
  return(df_shape)
}

# df_shape %>% glimpse() %>% .significance_tests()
.significance_tests = function(df_shape, override_method = list()) {

  df_shape = df_shape %>% .determine_comparison_method()

  # See if user wants to override?
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

  return(df_shape)

}



.power_analysis = function(df_shape, override_power = list()) {

  significance_limit = getOption("tableone.significance_limit",0.05)
  power = getOption("tableone.power",0.8)

  df_shape = df_shape %>% .determine_comparison_method()

  # See if user wants to override?
  if (length(override_power) > 0) {
    if (any(!override_power %in% names(.power.fns)))
      stop("override method must be one of ", paste0(.power.fns,collapse=", "))
    override = tibble::tibble(
      .name = names(override_power),
      .override_power = unlist(override_power)
    )
    df_shape = df_shape %>% dplyr::left_join(override, by=".name") %>%
      dplyr::mutate(.power_method = ifelse(!is.na(.override_power), .override_power, .power_method)) %>%
      dplyr::select(-.override_power)
  }

  df_shape$.power_analysis = list(rep(NA,nrow(df_shape)))
  for (i in 1:nrow(df_shape)) {
    # get dataframe row as a list
    tmp = df_shape %>% purrr::map(~ .x[[i]])

    # TODO: consider there being seperate comparison and power methods.
    fun = .power.fns[[tmp$.comparison_method]]
    d = tmp$.source
    # Can we do power analysis with missing values. I think we mostly can as
    # it is really a function of size and number of groups.
    # if (any(is.na(d$x))) {
    #   .message("Power analysis skipped due to missing values: ", tmp$.label)
    #   result = .pwr_missing(d, significance_limit, power)
    # } else {
      # this is where the significance test is done
      result = fun(d, significance_limit, power)
      .message(result$power.method, " on ",tmp$.label)
    # }
    df_shape$.power_analysis[[i]] = result
  }

  return(df_shape)

}

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

# df_signif = df_shape %>% .significance_tests()
# df_signif %>% .format_significance()
.format_significance = function(df_signif, p_format = names(.pvalue.defaults)) {
  p_format = match.arg(p_format)
  p_col = as.symbol(getOption("tableone.pvalue_column_name","P value"))

  fun = getOption("tableone.pvalue_formatter",.pvalue.defaults[[p_format]])
  method = getOption("tableone.show_pvalue_method",FALSE)

  tmp = df_signif %>% dplyr::select(variable = .label, .type, .significance_test) %>%
    tidyr::unnest(.significance_test)

  methods = tmp %>% dplyr::select(.type,p.method) %>% dplyr::distinct()
  methods_description = methods %>% dplyr::group_by(.type) %>%
    dplyr::summarise(.methods = paste0(p.method,collapse = ", ")) %>%
    dplyr::summarise(t = paste0(sprintf("%s (%s variables)",.methods,.type), collapse=" or ")) %>%
    dplyr::pull(t)
  if (method) {
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

