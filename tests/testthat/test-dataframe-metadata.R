# library(testthat)

testthat::test_that("column symbol name extraction", {
  testthat::expect_equal(
    .col_symbols(one_class_test_100) %>% .col_names(),
    colnames(one_class_test_100)
  )
})

testthat::test_that("columns are ignored", {
  testthat::expect_equal(
    .pull_cols(test_cols, one_class_test_100) %>% length(),
    length(test_cols)
  )
})

testthat::test_that("columns extract", {
  df1 = .select_content(test_cols, one_class_test_100)[[1]]
  testthat::expect_equal(df1$x, one_class_test_100$uniform_variable)

  # grouping columns are retained
  df2 = .select_content(test_cols, two_class_test)[[1]]
  testthat::expect_equal(length(df2), 3)
})

testthat::test_that("col types correct", {
  col = .pull_cols(test_cols, one_class_test_100)
  testthat::expect_true( .col_type(col[[1]],100) == "continuous")
  testthat::expect_true( .col_type(col[[3]],100) == "categorical")
  testthat::expect_true( .col_type(col[[6]],4) == "continuous")
  testthat::expect_true( .col_type(col[[5]],100) == "ordered")
})

testthat::test_that("col levels", {
  col = .pull_cols(test_cols, one_class_test_100)
  testthat::expect_true( .col_levels(col[[3]],100) == 2)
  testthat::expect_true( .col_levels(col[[4]],100) == 3)
  testthat::expect_true( .col_levels(col[[6]],100) == 10)
})

testthat::test_that("normality check works",{
  col = .pull_cols(test_cols, one_class_test_100)
  testthat::expect_true( .p_is_normal(col[[1]],"ad") < 0.05)

  testthat::expect_true( .p_is_normal(col[[2]],"ad") > 0.05)
  testthat::expect_true( .p_is_normal(col[[2]],"cvm") > 0.05)
  testthat::expect_true( .p_is_normal(col[[2]],"lillie") > 0.05)
  testthat::expect_true( .p_is_normal(col[[2]],"pearson") > 0.05)
  testthat::expect_true( .p_is_normal(col[[2]],"sf") > 0.05)

  # not continuous
  testthat::expect_true( is.na(.p_is_normal(col[[3]],"sf")) )

})

testthat::test_that("ties detected", {
  col = rep(stats::runif(50,1,2),2)
  testthat::expect_equal(.p_ties(col),0.5)
})

testthat::test_that("missing detected", {
  col = c(stats::runif(50,1,2),rep(NA,50))
  testthat::expect_equal(.p_missing(col),0.5)
})

testthat::test_that("shape dataframe approx correct", {
  shape = .get_shape(one_class_test_100, cols=test_cols)
  testthat::expect_equal(nrow(shape), length(test_cols))


  shape2 = .get_shape(two_class_test, cols=test_cols)
  testthat::expect_equal(nrow(shape2), length(test_cols))

  testthat::expect_equal(shape$.type, shape2$.type)
})

testthat::test_that("shape units are assigned", {
  shape = .get_shape(one_class_test_100, cols=test_cols, units=list("uniform_variable"="varUnit"))
  testthat::expect_true("varUnit" %in% shape$.unit)
})


