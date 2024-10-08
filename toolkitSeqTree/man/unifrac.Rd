% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/unifrac.R
\name{unifrac}
\alias{unifrac}
\title{UniFrac calculation}
\usage{
unifrac(
  tree_dat = NA,
  trait_var = "trait",
  per_level = FALSE,
  return_df = FALSE
)
}
\arguments{
\item{tree_dat}{Data frame describing tree structure}

\item{trait_var}{Name of column with discrete trait}

\item{per_level}{Logical on whether to calculate a level-specific metric rather than the overall UniFrac metric for the trait}

\item{return_df}{Logical - should dataframe with inferred internal nodes be returned}
}
\value{
UniFrac metric
}
\description{
Calculate the UniFrac metric - a measure of the phylogenetic signal in a
discrete trait. Nodes are inferred to as being unambiguously associated with
a single trait level or as being ambiguous. A higher fraction of unambiguous
branches as a proportion of the total branch length in the tree indicates
greater phylogenetic signal.
}
