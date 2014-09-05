\name{windex-package}
\alias{windex-package}
\docType{package}
\title{
windex: Analysing convergent evolution using the Wheatsheaf index
}
\description{
Functions to calculating the Wheatsheaf index of the strength of convergent evolution and testing whether this represents stronger convergence than expected.
}
\details{
\tabular{ll}{
Package: \tab windex\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-07-02\cr
License: \tab GPL2.0\cr
}
There are three main functions in the package. The first, plot.trait, gives a visual representation of the data although the plot does not account for phylogeny and so is only intended for (very rough-scale) visual data exploration. The second, windex, is the core function of the package. This takes a phylogenetic tree of class 'phylo' and a dataset and calculates the Wheatsheaf index of convergent evoltuion along with jackknived 95\% confidence intervals. The third function, test.windex, runs the windex function but also performs bootstrapped randomisations of the tips in the tree to generate a null distribution. This distribution is then used to test whether the calculated value of the Wheatsheaf index represents stronger convergence that is expected given the topological constraints of the tree. The datasets required for these functions must have the first column called 'species' and containing a list of species names that match those in the phylogenetic tree. One other column must be a vector denoting which species are part of the focal group (1) and which are in the non-focal group (0). Remaining columns contain values of traits that can be quantified (often continuous traits but see Arbuckle et al., 2014 for other examples).
}
\author{
Kevin Arbuckle and Amanda Minter

Maintainer: Kevin Arbuckle <k.arbuckle@liverpool.ac.uk>
}
\references{
Arbuckle, K., Bennett, C.M. and Speed, M.P. 2014. A simple measure of the strength of convergent evolution. Methods in Ecology and Evolution 5:685 - 693.
This citation is for the method, there is currently a manuscript being prepared to describe this package.
}
\keyword{ package }

