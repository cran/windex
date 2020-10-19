\name{sisterRichness}
\alias{sisterRichness}
\title{
One of two functions to generate the inputs necessary to run richness.yule.test() in ape (see sisterDivTime for preparing other input)
}
\description{
The function richness.yule.test() in the package ape requires two inputs - a dataframe with species richness of pairs of sister lineages which differ in the presence of a binary trait of interest, and a vector of divergence times of each of those sister group pairs. The sisterRichness function generates the first of these input files, returning a dataframe containing two columns (species richness in sister lineages with and without the trait of interest) and rows representing different sister pairs.
}
\usage{
sisterRichness(tree, x)
}
\arguments{
  \item{tree}{
Phylogenetic tree of class 'phylo' containing node labels representing divergence times of each node.
}
  \item{x}{
 Named vector representing the binary trait (labelled as 0 and 1 for absence and presence respectively).
}
}

\author{
Kevin Arbuckle
}

\seealso{
\code{\link{sisterDivTime}}
\code{\link{richness.yule.test}}
}
