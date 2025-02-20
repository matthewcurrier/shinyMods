% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{gt_create_table_bld}
\alias{gt_create_table_bld}
\title{Create a gt table}
\usage{
gt_create_table_bld(gt_object, tbl_title = "Untitled", ...)
}
\arguments{
\item{tbl_title}{a character string specifying the title of the table}

\item{...}{additional arguments to pass to \code{\link{summarise}}}

\item{df}{a dataframe with ad campaign data}
}
\value{
a gt table object
}
\description{
Create a gt table
}
\examples{
\donttest{
my_ad_stats_tbl <- gt_create_table_bld(df, tbl_title = "Ad Campaign Summary")
}
}
