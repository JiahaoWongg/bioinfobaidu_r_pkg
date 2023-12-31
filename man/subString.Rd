% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/subString.R
\name{subString}
\alias{subString}
\title{subString}
\usage{
subString(strings, idx, sep = NULL, rev = FALSE, collapse = NULL)
}
\arguments{
\item{strings}{One or more strings.}

\item{idx}{Positions of the fields to extract, one or more numbers.}

\item{sep}{Specified delimiter.}

\item{rev}{Whether to extract in reverse order.}

\item{collapse}{Delimiter used to reassemble the strings after splitting.}
}
\value{
One or more strings.
}
\description{
Multifunctional String Processing.
String extraction, string splitting, string concatenation, reverse manipulation.
}
\examples{
strings = c("TCGA-A1-A0SB-10B-01D-A141-01", "TCGA-A7-A26E-01A-11D-A272-09") 
strings = c(strings, "TCGA-E2-A3DX-01A-21D-A20S-09") 
subString(strings, 14:15) 
subString(strings, 20) 
subString(strings, 1:2, rev = TRUE) 
subString(strings, 2, sep = "-") 
subString(strings, 2, sep = "-", rev = TRUE) 
string2 = c("GENE1 ENSG0000000001", "GENE2 ENSG0000000002", "GENE3 ENSG0000000003") 
subString(string2, 1, " ") 
subString(string2, 2, " ") 
subString(strings, 1:3, sep = "-", collapse = "-") 
subString(strings, c(1, 3, 5), sep = "-", collapse = "-") 
subString(strings, c(2, 4, 6), sep = "-", collapse = "-nb-", rev = TRUE) 

}
\author{
Jiahao Wang
}
