\name{nodeDist}
\alias{nodeDist}
\title{
Plotting distribution of divergence times of a pair of species from a set of phylogenetic trees.
}
\description{
Plots the distribution of age estimates of the most recent common ancestor of a specified pair of species across trees (for instance a posterior distribution).
}
\usage{
nodeDist(trees,sp1,sp2,relTime=F,fillcol="blue",xlabel="Age (mya)",main="",add=F,...)
}
\arguments{
  \item{trees}{
An object of class multiPhylo containing a set of time-calibrated trees.
}
  \item{sp1}{
Name of one of the two species for which the divergence time is of interest.
}
  \item{sp2}{
Name of the other of the two species for which the divergence time is of interest.
}
  \item{relTime}{
Logical whether to plot relative (to age of root) divergence times or absolute times (defaults to absolute times).
}
  \item{fillcol}{
Colour to plot distribution.
}
  \item{xlabel}{
Label for the x-axis.
}
  \item{main}{
Title for plot (if desired, defaults to no title).
}
  \item{add}{
Logical whether to overlay distribution on an existing plot (make sure xlim and ylim on original call are set to accomodate both distributions).
}
  \item{\dots}{
Additional arguments to customise output, passed to plot.
}
}

\value{
Density plot of the distribution of relative or absolute divergence times across trees for the specified pair of species.
}

\author{
Kevin Arbuckle
}

\examples{
\dontrun{
# Density plot of absolute divergence times
nodeDist(trees,"Naja_haje","Naja_nivea")

# Density plot of relative divergence times (root age for each tree set to 1)
nodeDist(trees,"Naja_haje","Naja_nivea",relTime=T,xlabel="Relative time")}
}