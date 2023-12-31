% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hd.R
\name{hd}
\alias{hd}
\title{hd}
\usage{
hd(obj, x = 5, y = NULL)
}
\arguments{
\item{obj}{data object.}

\item{x}{Number to print of horizontal direction, defaut 5.}

\item{y}{Number to print of vertical direction, defaut 5.}
}
\value{
A part of input data.
}
\description{
Enhanced version of 'head'.
Default only print 5x5 field for two-dimensional data
or first 5 element for one-dimension data.
And print dimension of data in first line.
It also support specified range.
Don't worry about going out of range, it can fix it!
}
\examples{
hd(LETTERS, 10) 
hd(LETTERS, 24:100) 
df = get(data(package = "ggplot2", "diamonds")) 
rownames(df) = paste0("row_", 1:nrow(df)) 
colnames(df) = paste0("col_", 1:ncol(df)) 
hd(df) 
hd(df, 3, 5) 
hd(df, 3:5, 7:9) 
hd(df, -10:-nrow(df), -3:-5) 
hd(df, (nrow(df) - 2):10^6, (ncol(df) - 2):10^6) 
}
\author{
Jiahao Wang
}
