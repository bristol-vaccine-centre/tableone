
.s = function(...) {
  tryCatch({
    unname(dplyr::ensyms(...))
  }, error = function(e) {
    lapply(
      ...,
      as.symbol
    )
  })
}

# .n = function(l) {
#   lapply(l, lapply, as_label)
# }

testthat::test_that("multiple formula parsing", {

  testthat::expect_warning(
    testthat::expect_warning(
      testthat::expect_equal(
        .parse_formulae(iris, ~ Species + Petal.Width + Missing, a ~ b+Sepal.Width),
        list(.s(Species,Petal.Width),.s(Sepal.Width))
      )
    )
  )


  testthat::expect_warning(
    testthat::expect_equal(
      .parse_formulae(iris, Species ~ Petal.Width + Missing, a ~ b+Sepal.Width, side="lhs"),
      list(.s(Species),.s())
    )
  )

  testthat::expect_warning(
    testthat::expect_equal(
      .parse_formulae(iris, c(Species ~ Petal.Width + Missing, a ~ b+Sepal.Width), side="lhs"),
      list(.s(Species),.s())
    )
  )

  testthat::expect_warning(
    testthat::expect_equal(
      .parse_formulae(iris, list(Species ~ Petal.Width + Missing, a ~ b+Sepal.Width), side="lhs"),
      list(.s(Species),.s())
    )
  )

  testthat::expect_equal(
    .parse_formulae(iris, Species ~ .), # everything except species
    list(.s(colnames(iris)[colnames(iris) != "Species"]))
  )
})


testthat::test_that("tidyselect parsing", {
  testthat::expect_equal(
    .parse_tidyselect(iris,tidyselect::everything()),
    .s(colnames(iris))
  )
})

testthat::test_that("formula detection", {
  testthat::expect_true(.is_formula_interface(~ Species + Petal.Width + Missing))
  testthat::expect_true(.is_formula_interface(~ Species + Petal.Width + Missing, a ~ b+c))
  testthat::expect_true(.is_formula_interface(c(~ Species + Petal.Width + Missing, a ~ b+c)))
  testthat::expect_true(.is_formula_interface(list(~ Species + Petal.Width + Missing, a ~ b+c)))
  testthat::expect_false(.is_formula_interface(tidyselect::everything()))
  testthat::expect_true(.is_formula_interface())
})

testthat::test_that("vars parsing (single)", {
  # where dots is either a function (in which case we only want rhs) or a tidyselect.

  testthat::expect_equal(
    .parse_vars(iris, tidyselect::everything()),
    .s(colnames(iris))
  )

  testthat::expect_equal(
    .parse_vars(iris, ~ Species + Petal.Width),
    .s(Species,Petal.Width)
  )

  testthat::expect_warning(
    testthat::expect_equal(
      .parse_vars(iris, ~ Species + Petal.Width + Missing),
      .s(Species,Petal.Width)
    )
  )

  form =  ~ Species + Petal.Width + Missing
  testthat::expect_warning(
    testthat::expect_equal(
      .parse_vars(iris, form),
      .s(Species,Petal.Width)
    )
  )



})
