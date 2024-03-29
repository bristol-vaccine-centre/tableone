% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data-utils.R
\name{set_labels}
\alias{set_labels}
\title{Set a label attribute}
\usage{
set_labels(df, labels, attribute = "label")
}
\arguments{
\item{df}{a dataframe}

\item{labels}{a vector of labels, one for each column}

\item{attribute}{the name of the label attribute (defaults to \code{"label"})}
}
\value{
the same dataframe with each column labelled
}
\description{
Set a label attribute
}
\examples{
iris = set_labels(iris,
  c("Sepal Length", "Sepal Width",
    "Petal Length", "Petal Width",  "Species"
   ))
fn = label_extractor(iris,tolower)
fn(colnames(iris))
}
