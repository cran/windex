\name{plotTrait}
\alias{plotTrait}
\title{
Provides a visualisation of up to three-dimensional trait space (not accounting for phylogeny) for data exploration
}
\description{
Plots the trait space occupied by up to 3 traits with focals highlighted in red.
}
\usage{
plotTrait(dat, traits, focal = dat[, 2], ...)
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
  \item{\dots}{
Arguments to be passed to plot (or scatterplot3d for 3 traits) to customise output.
}
}

\author{
Kevin Arbuckle and Amanda Minter
}

\examples{
data(sample.data)
plotTrait(sample.data,c("ou1","ou2"),focal=sample.data[,2])
}