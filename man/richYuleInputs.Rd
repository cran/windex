\name{richYuleInputs}
\alias{richYuleInputs}
\title{
Generates the inputs necessary to run richness.yule.test() in ape
}
\description{
The function richness.yule.test() in the package ape requires two inputs - a dataframe with species richness of pairs of sister lineages which differ in the presence of a binary trait of interest, and a vector of divergence times of each of those sister group pairs. The richYuleInputs function generates these in a format which can be entered as the two required arguments.
}
\usage{
richYuleInputs(tree, x, rich=NULL)
}
\arguments{
  \item{tree}{
Phylogenetic tree of class 'phylo' with branch lengths in units of time.
}
  \item{x}{
 Named vector representing the binary trait (labelled as 0 and 1 for absence and presence respectively).
}
  \item{rich}{
 Optional named vector of species richness for each tip (for instance to account for incomplete sampling or when tips represent more than one species).
}
}

\value{
  \item{sisRich}{Dataframe containing two columns (species richness in sister lineages with and without the trait of interest) and rows representing different sister pairs}
  \item{divTimes}{Vector of divergence times of sister pairs (corresponding to rows of sisRich) differing in possession of a trait.}
}

\author{
Kevin Arbuckle
}

\seealso{
\code{\link[ape]{richness.yule.test}}
}

\examples{
data(sample.data)
data(sample.tree)
trait<-sample.data$focals
names(trait)<-sample.data$species

ryi<-richYuleInputs(sample.tree,trait)
richness.yule.test(ryi$sisRich,ryi$divTimes)
}
