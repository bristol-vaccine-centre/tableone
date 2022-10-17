## code to prepare `one_class_test` dataset goes here

library(tidyverse)

# .mclass(100, c("one"=0.1,"two"=0.2,"three"=0.3))
.mclass = function(n, prob) {
  factor(apply(rmultinom(n,1,prob=prob), MARGIN = 2, FUN = function(x) names(prob)[x==1]), levels=names(prob))
}

set.seed(100)

test_cols = vars(uniform_variable, normal_variable, binomial_class, multinom_class, discrete)
usethis::use_data(test_cols, overwrite = TRUE)

bad_test_cols = vars(uniform_variable, normal_variable, non_existant_class, multinom_class, discrete)
usethis::use_data(bad_test_cols, overwrite = TRUE)

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

one_class_test_100 = .one_class()
one_class_test_1000 = .one_class(n=1000, mean=0.1, binom_p = 0.6,
                                 multinom_p = c("one"=0.2,"two"=0.4,"three"=0.4),
                                 discrete = ~floor(scales::squish(rnorm(.x,5,2),c(0,9))))

usethis::use_data(one_class_test_100, overwrite = TRUE)
usethis::use_data(one_class_test_1000, overwrite = TRUE)

two_class_test = bind_rows(
  one_class_test_100 %>% mutate(grouping = "first group"),
  one_class_test_1000 %>% mutate(grouping = "second group")
) %>% group_by(grouping)

usethis::use_data(two_class_test, overwrite = TRUE)

multi_class_negative = bind_rows(
  .one_class(1000) %>% mutate(grouping = "first"),
  .one_class(1000) %>% mutate(grouping = "second"),
  .one_class(1000) %>% mutate(grouping = "third"),
  .one_class(1000) %>% mutate(grouping = "fourth"),
) %>% group_by(grouping)

usethis::use_data(multi_class_negative, overwrite = TRUE)

# Missing values test data ----

.make_missing = function(df, p_missing=0.1) {
  df %>%
    mutate(across(where(is.factor), .fns = ~ ifelse(runif(length(.x)) < p_missing, NA, .x) %>% factor(labels=as.character(levels(.x)), ordered = is.ordered(.x)))) %>%
    mutate(across(where(is.numeric), .fns = ~ ifelse(runif(length(.x)) < p_missing, NA, .x))) %>%
    mutate(across(where(is.logical), .fns = ~ ifelse(runif(length(.x)) < p_missing, NA, .x)))
}

set.seed(100)
missing_diamonds = diamonds %>% mutate(is_colored = ifelse(color < "F", "clear","colored")) %>%
  .make_missing() %>% mutate(is_colored = factor(is_colored))
usethis::use_data(missing_diamonds, overwrite = TRUE)

mnar_two_class_1000 = bind_rows(
  .one_class(1000) %>% mutate(grouping = "10% missing") %>% .make_missing(0.1),
  .one_class(1000) %>% mutate(grouping = "20% missing") %>% .make_missing(0.2),
) %>% group_by(grouping)

usethis::use_data(mnar_two_class_1000, overwrite = TRUE)

diamonds = ggplot2::diamonds %>% mutate(is_colored = ifelse(color < "F", "clear","colored"))
usethis::use_data(diamonds, overwrite = TRUE)
