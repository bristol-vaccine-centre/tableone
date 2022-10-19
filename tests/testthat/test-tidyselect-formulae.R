
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

test_that("multiple formula parsing", {

  expect_warning(
    expect_warning(
      expect_equal(
        .parse_formulae(iris, ~ Species + Petal.Width + Missing, a ~ b+Sepal.Width),
        list(.s(Species,Petal.Width),.s(Sepal.Width))
      )
    )
  )


  expect_warning(
    expect_equal(
      .parse_formulae(iris, Species ~ Petal.Width + Missing, a ~ b+Sepal.Width, side="lhs"),
      list(.s(Species),.s())
    )
  )

  expect_equal(
    .parse_formulae(iris, Species ~ .), # everything except species
    list(.s(colnames(iris)[colnames(iris) != "Species"]))
  )
})


test_that("tidyselect parsing", {
  expect_equal(
    .parse_tidyselect(iris,tidyselect::everything()),
    .s(colnames(iris))
  )
})

test_that("formula detection", {
  expect_true(.is_formula_interface(~ Species + Petal.Width + Missing))
  expect_true(.is_formula_interface(~ Species + Petal.Width + Missing, a ~ b+c))
  expect_true(.is_formula_interface(c(~ Species + Petal.Width + Missing, a ~ b+c)))
  expect_true(.is_formula_interface(list(~ Species + Petal.Width + Missing, a ~ b+c)))
  expect_false(.is_formula_interface(tidyselect::everything()))
  expect_true(.is_formula_interface())
})

test_that("vars parsing (single)", {
  # where dots is either a function (in which case we only want rhs) or a tidyselect.

  expect_equal(
    .parse_vars(iris, tidyselect::everything()),
    .s(colnames(iris))
  )

  expect_equal(
    .parse_vars(iris, ~ Species + Petal.Width),
    .s(Species,Petal.Width)
  )

  expect_warning(
    expect_equal(
      .parse_vars(iris, ~ Species + Petal.Width + Missing),
      .s(Species,Petal.Width)
    )
  )

  form =  ~ Species + Petal.Width + Missing
  expect_warning(
    expect_equal(
      .parse_vars(iris, form),
      .s(Species,Petal.Width)
    )
  )



})
