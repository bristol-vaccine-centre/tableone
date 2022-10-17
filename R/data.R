#' Default table layout functions
#'
#' Customisation of output can use one of these entries as a starting point.
#' A custom layout should look like one of the entries in level 2 of this
#' nested list, containing 4 named entries, one for each type of table summary.
#'
#' @format ## `default.format`
#' A names list of lists:
#' \describe{
#'   \item{level one}{The name of the table layout}
#'   \item{level two}{The name of the summary type required. one of `subtype_count`,
#'   `median_iqr`,`mean_sd`,`skipped`}
#'   \item{level three}{a named list of `column`=`glue specification` pairs. The
#'   `column` (itself a glue spec) might reference `N_total`, `N_present` or `.unit` but
#'   typically will be a fixed string- it defines the name of the table column
#'   to generate. The `glue specification` defines the layout of that column,
#'   and can use summary statistics as below}
#'   \item{subtype_count}{can use `level`, `prob.0.5`, `prob.0.025`,
#'     `prob.0.975`, `unit`, `n`, `N`. `n` is subgroup count, `N` is data count.}
#'   \item{median_iqr}{can use `q.0.5`, `q.0.25`, ..., `unit`, `n`, `N` - `n` excludes
#'     missing, `N` does not.}
#'   \item{mean_sd}{can use `mean`, `sd`, `unit`, `n`, `N` - `n` excludes
#'     missing, `N` does not.}
#'   \item{skipped}{can use `n`, `N` - `n` excludes
#'     missing, `N` does not.}
#' }
"default.format"

#' A list of columns for a test case
#'
#' @format ## `bad_test_cols`
#' \describe{Test data}
"bad_test_cols"

#' A list of columns for a test case
#'
#' @format ## `test_cols`
#' \describe{Test data}
"test_cols"

#' A copy of the diamonds dataset
#'
#' with 10% of entries replaced by NA
#' and a binary class is_coloured based on the color column
#'
#' @format ## `missing_diamonds`
#' \describe{Test data}
"missing_diamonds"

#' A copy of the diamonds dataset
#'
#' with a binary class is_coloured based on the color column
#'
#' @format ## `diamonds`
#' \describe{Test data}
"diamonds"

#' Missing not at random 2 class 1000 items
#'
#' A random data test dataset with 2 classes (groupings column)
#' one of which has 10% missing data and the other has 20%
#'
#' @format ## `mnar_two_class_1000`
#' \describe{Test data}
"mnar_two_class_1000"

#' A multi-class dataset with equal random samples in each class
#'
#' @format ## `multi_class_negative`
#' \describe{Test data}
"multi_class_negative"


#' A single-class dataset with 100 items of random data
#'
#' columns contain a set of random data of different types e.g.
#' uniform continuous, normal, binomial, multinomial.
#'
#' @format ## `one_class_test_100`
#' \describe{Test data}
"one_class_test_100"

#' A single-class dataset with 1000 items of random data
#'
#' columns contain a set of random data of different types e.g.
#' uniform continuous, normal, binomial, multinomial.
#'
#' @format ## `one_class_test_1000`
#' \describe{Test data}
"one_class_test_1000"



#' A two-class dataset with random data
#'
#' columns contain a set of random data of different types e.g.
#' uniform continuous, normal, binomial, multinomial. in grouping 1
#' there is 100 items in grouping 2 there are 1000 items
#'
#' @format ## `one_class_test_100`
#' \describe{Test data}
"two_class_test"
