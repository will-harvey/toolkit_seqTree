% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cpg_ratio.R
\name{cpg_ratio}
\alias{cpg_ratio}
\title{CpG ratio calculator}
\usage{
cpg_ratio(sequence = NA, method = 1, res_expand = F)
}
\arguments{
\item{sequence}{String representation of nucleotide sequence}

\item{method}{1 or 2 representing 1) Saxonov, Berg & Brutlag or 2) Gardiner-Garden & Frommers}

\item{res_expand}{Expanded results includes observed and expected CpG}
}
\value{
CpG ratio or alternatively data frame with ratio, observed and expected
}
\description{
Calculate the ratio of observed CpG to expected CpG in a nucleotide sequence.
}
\examples{
sequence <- 'CATGCTGTGGTCGCGTAGGGTTATTCGTCGTAATTGCAGCT'
cpg_ratio(sequence, method = 2, res_expand = T)
}
