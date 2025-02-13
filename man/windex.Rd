\name{windex}
\alias{windex}
\title{
Calculates Wheatsheaf index with 95\% confidence intervals
}
\description{
Takes a phylo object and trait data and returns the Wheatsheaf index for the traits on the tree along with 95\% confidence intervals obtained from jackkniving.
}
\usage{
windex(dat, tree, traits, focal = dat[, 2], SE = TRUE, fossil = FALSE,
species.col="species")
}
\arguments{
  \item{dat}{
A dataframe containing a column of 0s and 1s to denote non-focal and focal taxa respectively, and columns of trait data which you which to plot.
}
  \item{tree}{
Phylogenetic tree of class 'phylo' containing branch lengths. The tree should also be ultrametric.
}
  \item{traits}{
Column numbers (or names) for the traits for which you want to calculate a Wheatsheaf index.
}
  \item{focal}{
Column in the dataframe containing the focal designations.
}
  \item{SE}{
A logical specifying whether to standardise the traits by their standard error across species, default is \code{SE=TRUE}.
}
  \item{fossil}{
A logical specifying whether the tree contains fossil tips (i.e. is not ultrametric), in which case a different phylogenetic distance penalty based on shared branch length rather than shared time before divergence will be used instead. Note that this alternative penalty for trees containing fossils hasn't yet been rigorously tested so use cautiously, but it seems to behave as expected.
}
  \item{species.col}{
Name of column in the dataset containing species names corresponding to tip labels in the tree, default assumes the column is named "species".
}
}

\value{
  \item{w}{Calculated Wheatsheaf index}
  \item{low95}{Lower bound of 95\% confidence interval for the Wheatsheaf index obtained by jackkniving}
  \item{up95}{Upper bound of 95\% confidence interval for the Wheatsheaf index obtained by jackkniving}
}
\references{
Arbuckle, K., Bennett, C.M. and Speed, M.P. 2014. A simple measure of the strength of convergent evolution. Methods in Ecology and Evolution 5:685 - 693.
}
\author{
Kevin Arbuckle and Amanda Minter
}

\seealso{
\code{\link{test.windex}}
\code{\link{windex.sim.test}}
}
\examples{
data(sample.data)
data(sample.tree)
windex(sample.data,sample.tree,traits=c("ou1","ou2"),focal=sample.data[,2],
SE=TRUE)

}
