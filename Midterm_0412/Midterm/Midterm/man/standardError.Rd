% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/standardError.R
\name{standardError}
\alias{standardError}
\title{Standard Error}
\usage{
standardError(y, SEtype)
}
\arguments{
\item{y}{numeric. The observed data}

\item{SEtype}{character. Two values: basic or bootstrap}
}
\value{

}
\description{
Calculate the Standard Error. This function can take two values: basic or bootstrap
}
\examples{
y=c(1,2,3,4)
standardError(y, "basic")
}
\author{
Berta Diaz
}
