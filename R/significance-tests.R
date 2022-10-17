

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

### do significance tests ----
# df_shape %>% glimpse() %>% .significance_tests()
.significance_tests = function(df_shape, override_method = list()) {

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
    message(tmp$.comparison_method, " test on ",tmp$.label)
    fun = .comparison.fns[[tmp$.comparison_method]]
    d = tmp$.source
    result = fun(d)
    df_shape$.significance_test[[i]] = result
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
    tmp = tmp %>% dplyr::left_join(methods, by="p.method") %>%
      dplyr::mutate(!!p_col := paste0(fun(p.value)," ",daggers))
    methods_key = paste0(methods$daggers,", ",methods$p.method, collapse = "; ")
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

