\name{pir}
\alias{pir}
\title{
Calculates phylogenetic imbalance ratio (PIR)
}
\description{
Calculates the PIR to assess suitability of categorical traits for modelling approaches, following Gardner and Organ (2021).
}
\usage{
pir(tree,trait1,trait2=NULL)
}
\arguments{
  \item{tree}{
Phylogenetic tree of class 'phylo'.
}
  \item{trait1}{
Named vector containing states of a categorical trait. Must be a character or a factor and names must match tip labels of the tree.
}
  \item{trait2}{
An optional second trait when the intention is to test suitability of modelling a correlation between two categorical traits. Argument requirements are the same as trait1.
}
}

\value{
  \item{CI}{Consistency index}
  \item{NIR}{Normalised imbalance ratio (a measure of class imbalance across states or, if there are two traits, state combinations)}
  \item{PIR}{Phylogenetic imbalance ratio}
}

\note{
This function implements the phylogenetic imbalance ratio recommended in concert with its component parts (Consistency index and Normalised imbalance ratio) by Gardner and Organ (2021) to assess the suitability of categorical trait data for modelling in phylogenetic comparative methods. Each of these three indices ranges from 0 to 1. Low values of CI indicate high levels of homoplasy, which is linked to higher evolutionary sample sizes, whereas low values of NIR indicate a balanced distribution of traits (similar proportion of species in each state) and this often enables better and more data-driven parameter estimation from models. PIR is the product of CI and NIR, with lower values again preferred for phylogenetic comparative models. Gardner and Organ(2021) recommended a rule of thumb of PIR<0.1 as indicative that the categorical trait data are suitable for model-based analysis, but see that paper for more detailed discussion.
}

\references{
Gardner, J.D. and Organ, C.L. 2021. Evolutionary sample size and consilience in phylogenetic comparative analysis. Systematic Biology 70:1061 - 1075.
}
\author{
Kevin Arbuckle
}


\examples{
data(sample.tree)

# Single trait (perhaps intended for estimating transition rates)
t1<-sample(c("brown","blue","green"),length(sample.tree$tip.label),replace=TRUE)
names(t1)<-sample.tree$tip.label
pir(sample.tree,trait1=t1)

# Two traits (perhaps intended for testing correlations)
t2<-sample(c("0","1"),length(sample.tree$tip.label),replace=TRUE)
names(t2)<-sample.tree$tip.label
pir(sample.tree,trait1=t1,trait2=t2)
}
