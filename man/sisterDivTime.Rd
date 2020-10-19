\name{sisterDivTime}
\alias{sisterDivTime}
\title{
One of two functions to generate the inputs necessary to run richness.yule.test() in ape (see sisterRichness for preparing other input)
}
\description{
The function richness.yule.test() in the package ape requires two inputs - a dataframe with species richness of pairs of sister lineages which differ in the presence of a binary trait of interest, and a vector of divergence times of each of those sister group pairs. The sisterDivTime function generates the second of these input files, returning a vector of divergence times of sister pairs differing in possession of a trait.
}
\usage{
sisterDivTime(tree, x)
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
\code{\link{sisterRichness}}
\code{\link{richness.yule.test}}
}
