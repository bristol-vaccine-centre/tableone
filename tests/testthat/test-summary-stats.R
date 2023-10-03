.df_shape1 = function() {
  one_class_test_1000 %>% .get_shape(test_cols)
}

.df_shape = function() {
  two_class_test %>% .get_shape(cols = dplyr::vars(multinom_class))
}

.df_shape2 = function() {
  two_class_test %>% .get_shape(test_cols)
}

testthat::test_that("subgroup summary is OK", {
  grp_df = .df_shape()$.source[[1]]
  testthat::expect_equal(
    # n.b. x is not subtype count, n is group count (non missing)
    .subtype_count(grp_df) %>% dplyr::pull(x),
    two_class_test %>% dplyr::group_by(grouping, multinom_class) %>% dplyr::count() %>% dplyr::pull(n)
  )
})


testthat::test_that("median iqr is OK", {
  input = attributes(one_class_test_1000)$input
  tmp = .median_iqr(.df_shape1()$.source[[1]])
  testthat::expect_true(
    abs(input$median - tmp$q.0.5) < 0.1,
    abs((tmp$q.0.75 - tmp$q.0.25)-input$IQR) < 0.1,

  )
})


testthat::test_that("mean sd is OK", {
  input = attributes(one_class_test_1000)$input
  tmp = .mean_sd(.df_shape1()$.source[[2]])
  testthat::expect_true(
    # mean should be 0.1
    abs(input$mean - tmp$mean) < 0.1,
    # sd should be 2
    abs(input$sd - tmp$sd) < 0.1,
    tmp$N == input$n
  )
})

# TODO: test overridden summary stat types

testthat::test_that("summary stat is high level OK", {
  ss = .df_shape1() %>% .summary_stats()
  testthat::expect_equal(
    ss$.summary_type,
    c("median_iqr","mean_sd","subtype_count","subtype_count","subtype_count","median_iqr")
  )
})

testthat::test_that("format summary is high level OK", {
  ss = .df_shape1() %>% .summary_stats()
  fs = ss %>% .format_summary(format = default.format$single)

  ss2 = .df_shape2() %>% .summary_stats()
  fs2 = ss2 %>% .format_summary(format = default.format$single)
  testthat::expect_equal(
    fs %>% dplyr::filter(.tbl_col_name=="Value") %>% dplyr::pull(.tbl_col_value),
    fs2 %>% dplyr::filter(grouping == "second group") %>%
      dplyr::filter(.tbl_col_name=="Value") %>% dplyr::pull(.tbl_col_value)
  )

  ss = .df_shape1() %>% .summary_stats()
  # TODO: some error checking for bad formats woudl be helpful here
  fs = ss %>% .format_summary(format = default.format$single, override_percent_dp = list(binomial_class=6), override_real_dp = list(normal_variable="5g"))
  # difficult to test. ?vignette
})
