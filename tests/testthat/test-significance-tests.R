
.df_shape2 = function() {
  two_class_test %>% .get_shape(test_cols)
}


testthat::test_that("ks works", {
  # not normally distributes
  # these should be non significant
  testthat::expect_true(.df_shape2()$.source[[1]] %>% .do_ks() %>% dplyr::pull(p.value) > 0.05)
})


testthat::test_that("wilcoxon works", {
  # not normally distributes
  # these should be non significant
  testthat::expect_true(.df_shape2()$.source[[1]] %>% .do_wilcoxon() %>% dplyr::pull(p.value) > 0.05)
})

testthat::test_that("fisher works", {
  # categorical / binary
  # these should be non significant
  testthat::expect_true(.df_shape2()$.source[[3]] %>% .do_fisher() %>% dplyr::pull(p.value) < 0.05)
  testthat::expect_true(.df_shape2()$.source[[4]] %>% .do_fisher() %>% dplyr::pull(p.value) < 0.05)
  testthat::expect_true(.df_shape2()$.source[[5]] %>% .do_fisher() %>% dplyr::pull(p.value) < 0.05)

  # multi-way
  tmp = multi_class_negative %>% .get_shape(test_cols)
  tmp$.source[[4]] %>% .do_fisher() %>% dplyr::pull(p.value) > 0.05
})

testthat::test_that("t-test works", {
  # not normally distributes
  # these should be non significant as difference is quite small
  testthat::expect_true(.df_shape2()$.source[[2]] %>% .do_ttest() %>% dplyr::pull(p.value) > 0.05)
})

testthat::test_that("multiclass comparison works", {
  tmp = multi_class_negative %>% .get_shape(test_cols)
  # differences should be non significant
  # normal data
  testthat::expect_true(tmp$.source[[2]] %>% .do_anova() %>% dplyr::pull(p.value) > 0.05)
  testthat::expect_true(tmp$.source[[2]] %>% .do_kruskal() %>% dplyr::pull(p.value) > 0.05)

  # the non normal data
  testthat::expect_true(tmp$.source[[1]] %>% .do_anova() %>% dplyr::pull(p.value) > 0.05)
  testthat::expect_true(tmp$.source[[1]] %>% .do_kruskal() %>% dplyr::pull(p.value) > 0.05)
})


testthat::test_that("signifincance tests",{
  tmp = multi_class_negative %>% .get_shape(test_cols)
  st = tmp %>% .significance_tests()
  # options("avoncap.show_pvalue_method"=TRUE)
  fmt = st %>% .format_significance()
  testthat::expect_true(fmt %>% get_footer_text() %>% stringr::str_detect("Anderson-Darling normality test") %>% any())
  testthat::expect_true(fmt %>% get_footer_text() %>% stringr::str_detect("Kruskal-Wallis") %>% any())
})
