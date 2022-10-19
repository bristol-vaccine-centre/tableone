## code to prepare `one_class_test` dataset goes here
here::i_am("data-raw/test-data.R")
library(tidyverse)
source(here::here("vignettes/vignette-utils.R"))

set.seed(100)

# Cols ----
test_cols = dplyr::vars(uniform_variable, normal_variable, binomial_class, multinom_class, discrete)
usethis::use_data(test_cols, overwrite = TRUE)

bad_test_cols = dplyr::vars(uniform_variable, normal_variable, non_existant_class, multinom_class, discrete)
usethis::use_data(bad_test_cols, overwrite = TRUE)

# One class ----
one_class_test_100 = .one_class()
usethis::use_data(one_class_test_1000, overwrite = TRUE)

one_class_test_1000 = .one_class(n=1000, mean=0.1, binom_p = 0.6,
                                 multinom_p = c("one"=0.2,"two"=0.4,"three"=0.4),
                                 discrete = ~floor(scales::squish(rnorm(.x,5,2),c(0,9))))
usethis::use_data(one_class_test_100, overwrite = TRUE)

# Multi class -----
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

set.seed(100)
missing_diamonds = diamonds %>% mutate(is_colored = ifelse(color < "F", "clear","colored")) %>%
  .make_missing() %>% mutate(is_colored = factor(is_colored))
usethis::use_data(missing_diamonds, overwrite = TRUE)

mnar_two_class_1000 = bind_rows(
  .one_class(1000) %>% mutate(grouping = "10% missing") %>% .make_missing(0.1),
  .one_class(1000) %>% mutate(grouping = "20% missing") %>% .make_missing(0.2),
) %>% group_by(grouping)
usethis::use_data(mnar_two_class_1000, overwrite = TRUE)

diamonds = ggplot2::diamonds %>%
  mutate(is_colored = ifelse(color < "F", "clear","colored")) %>%
  mutate(is_colored = factor(is_colored))
usethis::use_data(diamonds, overwrite = TRUE)
