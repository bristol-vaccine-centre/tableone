% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/text-utils.R
\name{format_pvalue}
\alias{format_pvalue}
\title{Format a p-value}
\usage{
format_pvalue(p.value, p_format = names(.pvalue.defaults))
}
\arguments{
\item{p.value}{the p-value to be formatted}

\item{p_format}{a name of a p-value formatter (one of sampl, nejm, jama, lancet, aim)}
}
\value{
a formatted P-value
}
\description{
Uses the default formatter set globally in \code{options("tableone.pvalue_formatter")} in
preference the one defined by \code{p_format} which is only used if no default is set.
}
