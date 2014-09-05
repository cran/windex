\name{plotTrait}
\alias{plotTrait}
\title{
Provides a visualisation of up to three-dimensional trait space (not accounting for phylogeny) for data exploration
}
\description{
Plots the trait space occupied by up to 3 traits with focals highlighted in red.
}
\usage{
plotTrait(dat, traits, focal = dat[, 2])
}
\arguments{
  \item{dat}{
A dataframe containing a column of 0s and 1s to denote non-focal and focal taxa respectively, and columns of trait data which you which to plot.
}
  \item{traits}{
Column numbers (or names) for 1-3 traits which you want to plot.
}
  \item{focal}{
Column in the dataframe containing the focal designations.
}
}

\references{
Arbuckle, K., Bennett, C.M. and Speed, M.P. 2014. A simple measure of the strength of convergent evolution. Methods in Ecology and Evolution 5:685 - 693.
This citation is for the method, there is currently a manuscript being prepared to describe this package.
}
\author{
Kevin Arbuckle and Amanda Minter
}

\examples{
data(sample.data)
plotTrait(sample.data,c("ou1","ou2"),focal=sample.data[,2])
}