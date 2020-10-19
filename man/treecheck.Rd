\name{treecheck}
\alias{treecheck}
\title{
Checks a set of phylogenetic trees to confirm they are ultrametric and binary.
}
\description{
Takes a set of phylogenetic trees as a multiPhylo object (or a single tree as a phylo object) and reports which (if any) are not binary or ultrametric.
}
\usage{
treecheck(trees)
}
\arguments{
  \item{trees}{
Set of phylogenetic trees of class 'multiPhylo' or 'phylo' containing branch lengths.
}
}

\value{
  Either confirms that all trees are binary and ultrametric or prints warnings stating which trees do not meet those criteria.
}

\author{
Kevin Arbuckle
}