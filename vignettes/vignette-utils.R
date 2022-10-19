
# pretty print R list
# .f(tableone::default.format$relaxed)
.f <- function(x) {
  text_con <- textConnection("output", "w")
  dput(x, text_con)
  close(text_con)
  tmp = formatR::tidy_source(text = output, args.newline = TRUE, width = 40,output = FALSE)
  tmp$text.tidy
}


# multinomial class random generator
# .mclass(100, c("one"=0.1,"two"=0.2,"three"=0.3))
.mclass = function(n, prob) {
  factor(apply(rmultinom(n,1,prob=prob), MARGIN = 2, FUN = function(x) names(prob)[x==1]), levels=names(prob))
}

# randomly selectively remove data
.make_missing = function(df, p_missing=0.1) {
  df %>%
    mutate(across(where(is.factor), .fns = ~ ifelse(runif(length(.x)) < p_missing, NA, as.character(.x)) %>% factor(levels = as.character(levels(.x)), ordered = is.ordered(.x)))) %>%
    mutate(across(where(is.numeric), .fns = ~ ifelse(runif(length(.x)) < p_missing, NA, .x))) %>%
    mutate(across(where(is.logical), .fns = ~ ifelse(runif(length(.x)) < p_missing, NA, .x)))
}

# make test data
.one_class = function(
    n = 100, median = 2, IQR = 1, mean = 0, sd=2, binom_p = 0.3,
    multinom_p = c("one"=0.1,"two"=0.2,"three"=0.7),
    discrete = ~ floor(runif(.x,0,10))
) {
  argg <- as.list(environment())
  discrete = purrr::as_mapper(discrete)(n)
  tmp = tibble::tibble(
    uniform_variable = runif(n,median-IQR,median+IQR),
    normal_variable = rnorm(n,mean,sd),
    binomial_class = rbinom(n,1,binom_p)==1,
    multinom_class = .mclass(n, multinom_p),
    discrete = discrete,
    ignore_me = NA
  )

  structure(
    tmp,
    input = argg)
}


.show_in_rstudio = function(hux) {
  hux %>% huxtable::to_html() %>% htmltools::HTML()
}
