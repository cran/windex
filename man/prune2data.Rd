\name{prune2data}
\alias{prune2data}
\title{
Prunes a phylogenetic tree to match a vector (e.g. of species names in a dataset)
}
\description{
Takes a phylo object and vector of names to be matched to tip labels and returns a pruned phylogeny containing only tip labels that match those in the vector.
}
\usage{
prune2data(tree, species)
}
\arguments{
  \item{tree}{
Phylogenetic tree of class 'phylo'.
}
  \item{species}{
Vector of names to be matched against tip labels of the tree.
}
}

\value{
Returns a phylogenetic tree of the class 'phylo' containing only tips whose labels match the input vector (species)
}

\author{
Kevin Arbuckle
}

\examples{
data(sample.data)
data(sample.tree)
tree<-prune2data(sample.tree,sample.data$species[1:10])
plot(tree)
}
