\name{windex-package}
\alias{windex-package}
\docType{package}
\title{
windex: Analysing Convergent Evolution using the Wheatsheaf Index
}
\description{
Functions to calculate the Wheatsheaf index of the strength of convergent evolution, testing whether this represents stronger convergence than expected, and some other related (and not so related) functions.
}
\details{
\tabular{ll}{
Package: \tab windex\cr
Type: \tab Package\cr
Version: \tab 2.0\cr
Date: \tab 2020-10-15\cr
License: \tab GPL2.0\cr
}
The core function of the package is windex(), which takes a phylogenetic tree of class 'phylo' and a dataset and calculates the Wheatsheaf index of convergent evolution along with jackknived 95\% confidence intervals. Other functions provide plotting and test functions to support the use of windex for data analysis, and several other functions are provided that are not related to convergent evolution but may be useful for some users in other capacities ("put some windex on it"). The datasets required for some of these functions (particularly windex(), test.windex(), and windex.sim.test()) must have the first column called 'species' and containing a list of species names that match those in the phylogenetic tree. One other column must be a vector denoting which species are part of the focal group (1) and which are in the non-focal group (0). Remaining columns contain values of traits that can be quantified (often continuous traits but see Arbuckle et al., 2014 for other examples).
}
\author{
Kevin Arbuckle and Amanda Minter

Maintainer: Kevin Arbuckle <kevin.arbuckle@swansea.ac.uk>
}
\references{
Arbuckle, K. and Minter, A. 2015. windex: analyzing convergent evolution using the Wheatsheaf index in R. Evolutionary Bioinformatics 11:11 - 14.

Arbuckle, K., Bennett, C.M. and Speed, M.P. 2014. A simple measure of the strength of convergent evolution. Methods in Ecology and Evolution 5:685 - 693.
}
\keyword{ package }

