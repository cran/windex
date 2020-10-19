\name{treedatacheck}
\alias{treedatacheck}
\title{
Checks a set of phylogenetic trees to confirm they are ultrametric and binary and that tip labels match a list of names.
}
\description{
Takes a set of phylogenetic trees as a multiPhylo object (or a single tree as a phylo object) and a vector (e.g. of species names) and reports which trees (if any) are not binary, not ultrametric, or have tip labels that don't match the vector of names.
}
\usage{
treedatacheck(trees,species)
}
\arguments{
  \item{trees}{
Set of phylogenetic trees of class 'multiPhylo' or 'phylo' containing branch lengths.
}
  \item{species}{
Vector of names to be matched against tip labels of the tree(s).
}
}

\value{
  Either confirms that all trees are binary, ultrametric, and have tip labels matching the list of names, or prints warnings stating which trees do not meet those criteria.
}

\author{
Kevin Arbuckle
}