% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/presentation_convenience.R
\name{formatDate}
\alias{formatDate}
\title{Format date with locale}
\usage{
formatDate(x, format = "", locale = Sys.getlocale("LC_TIME"), ...)
}
\arguments{
\item{x}{date}

\item{format}{format to use}

\item{locale}{locale to use}

\item{...}{passed to strftime}
}
\value{
formatted date string
}
\description{
Format date with locale
}
\author{
Christoffer Moesgaard Albertsen
}
