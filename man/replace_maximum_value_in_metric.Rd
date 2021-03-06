% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/replace_maximum_value_in_metric.R
\name{replace_maximum_value_in_metric}
\alias{replace_maximum_value_in_metric}
\title{Replace tidycyte metric values above threshold}
\usage{
replace_maximum_value_in_metric(.df, .max, .replace, ...)
}
\arguments{
\item{.df}{A data frame containing tidycyte data.}

\item{.max}{A numeric value specifying the maximum. Values are replaced if they are over this value, but not if they are equal to this value.}

\item{.replace}{A numeric replacement value}

\item{...}{<\code{\link[dplyr:dplyr_data_masking]{data-masking}}> The masked names of the metrics to which replacement is applied.}
}
\value{
A dataframe with adjusted metrics
}
\description{
Replaces all values of a tidycyte metric above a specified threshold with a replacement value.
}
\examples{
df \%>\% replace_maximum_value_in_metric(50,50,Confluence,mAzalea,`h2-3`)
}
\keyword{max}
\keyword{tidycyte}
