% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hudsonFst.R
\name{hudsonFst}
\alias{hudsonFst}
\title{Calculates Hudson's Fst}
\usage{
hudsonFst(locus = NA, p1 = NA, p2 = NA)
}
\arguments{
\item{locus}{Locus for Fst Calculation}

\item{p1}{Population 1 allele frequency vector}

\item{p2}{Population 2 allele frequency vector}
}
\value{
Value for Hudson's Fst
}
\description{
This function calculates Hudson's Fst
}
\examples{
fst.HVAxHVC<-hudsonFst(locus=pops.af$locus, p1=pops.af$afHVA,p2=pops.af$afHVC)
}
