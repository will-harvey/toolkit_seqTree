# toolkit_seqTree

This repository hosts the R package *toolkitSeqTree* which includes functions to perform a variety of analyses on sequence alignments and phylogenetic trees.

### Vignettes

To get a flavour for some of the package's functionality, see the vignettes which run a variety of functions to examine correlation between phylogeny and traits (phylogenetic signal), temporal signal in phylogenies and to define relationships between nodes of the phylogeny such as identifying all nodes which fall on a path between two taxa.


### Install

The simplest way to install this package is to do so within R using the following commands:

> install.packages('remotes')

> remotes::install_github('will-harvey/toolkit_seqTree/toolkitSeqTree')

or 

> install.packages('devtools')

> devtools::install_github('will-harvey/toolkit_seqTree/toolkitSeqTree')

Typically installation should complete in <1 minute. Alternatively, this repository can be cloned and the compiled locally. To do so, clone, navigate to the file toolkit_seqTree/toolkitSeqTree/toolkitSeqTree.Rproj and open with RStudio. Then click 'Build' and 'Install'.

This package should be compatible with most versions of R and Rstudio and has been tested with R v4.3.2 (2023-10-31) and RStudio v2025.09.1+401. Installation has been tested on Mac, Windows and Ubuntu. No non-standard hardware is required for use.

