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
  tmp = suppressWarnings(stats::chisq.test(chi.data[x,y]))
  chisq = tmp$statistic


  df = tmp$parameter
  n = nrow(xy_df)
  obs_w = sqrt(chisq/(n*df))

  tmp2 = tryCatch(pwr::pwr.chisq.test(N=n, df=df, sig.level=significance_limit, power=power ), error = function(e) NA)
  w = tmp2$w

  interp = dplyr::case_when(
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
  s1 = stats::sd(side1, na.rm = TRUE)
  n2 = floor(length(side2) / factor)
  s2 = stats::sd(side2, na.rm = TRUE)
  # Cohens D
  sd_pooled = sqrt( ((n1-1)*s1^2 + (n2-1)*s2^2 )/(n1+n2-2) )
  obs_d = abs( mean(side1,na.rm=TRUE)-mean(side2,na.rm=TRUE)) / sd_pooled
  # tmp = tryCatch(pwr::pwr.t2n.test(n1 = length(side1), n2 = length(side2), d=c(0.2,0.5,0.8), sig.level=significance_limit ), error = function(e) NA)
  tmp2 = tryCatch(pwr::pwr.t2n.test(n1 = length(side1), n2 = length(side2), sig.level=significance_limit, power=power ), error = function(e) NA)
  detectable = tmp2$d * sd_pooled
  interp = dplyr::case_when(
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
  n = xy_df %>% dplyr::group_by(y) %>% dplyr::count() %>% dplyr::pull(n) %>% min()
  n = n / factor
  tmp2 = tryCatch(pwr::pwr.anova.test(k=k, n=n, sig.level=significance_limit, power=power ), error = function(e) NA)
  obs_f = .cohens_f(xy_df)

  method = "One-way ANOVA power analysis"
  if (factor!=1) method = sprintf("%s (+%1.0f%%)",method, (1-factor)*100)

  interp = dplyr::case_when(
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

# aovstat2 = rstatix::eta_squared(stats::lm(x ~ y, xy_df))
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


.power_analysis = function(df_shape, override_power = list()) {

  significance_limit = getOption("tableone.significance_limit",0.05)
  power = getOption("tableone.power",0.8)

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

  return(structure(
    df_shape,
    class = c("t1_power",class(df_shape))))

}
