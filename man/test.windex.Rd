\name{test.windex}
\alias{test.windex}
\title{
Tests for particularly strong convergent evolution given topological constraints
}
\description{
The P-value returned is for the null hypothesis that the calculated Wheatsheaf index is no higher than expected by chance given the topology of the phylogenetic tree. Note that this is not a test for convergence per se, but of whether the convergence is unexpectedly strong.
}
\usage{
test.windex(dat, tree, traits, focal = dat[, 2], SE = TRUE, reps,
plot = TRUE, fossil = FALSE, species.col="species", main = "",
line = 2.5, ...)
}
\arguments{
  \item{dat}{
A dataframe containing a column of 0s and 1s to denote non-focal and focal taxa respectively, and columns of trait data which you which to plot.
}
  \item{tree}{
Phylogenetic tree of class 'phylo' containing branch lengths. The tree should also be ultrametric.
}
  \item{traits}{
Column numbers (or names) of the traits for which you want to calculate a Wheatsheaf index.
}
  \item{focal}{
Column in the dataframe containing the focal designations.
}
\item{SE}{
A logical specifying whether to standardise the traits by their standard error across species, default is \code{SE=TRUE}.
}
  \item{reps}{
Number of bootstrap replicates on which to base the P-value.
}
  \item{plot}{
A logical indicating whether to plot the bootstrap distribution. If TRUE, a histogram is plotted with the calculated Wheatsheaf index and its 95\% confidence interval overlayed on the histogram as a solid and dashed lines (respectively).
}
  \item{fossil}{
A logical specifying whether the tree contains fossil tips (i.e. is not ultrametric), in which case a different phylogenetic distance penalty based on shared branch length rather than shared time before divergence will be used instead. Note that this alternative penalty for trees containing fossils hasn't yet been rigorously tested so use cautiously, but it seems to behave as expected.
}
  \item{species.col}{
Name of column in the dataset containing species names corresponding to tip labels in the tree, default assumes the column is named "species".
}
  \item{main}{
Main title for plot (defaults to no title).
}
  \item{line}{
Adjusts position of main title (if one is given), with lower values moving it down and higher values moving it up.
}
  \item{\dots}{
Additional arguments passed to hist() to customise the histogram (when plot=TRUE).
}
}

\value{
  \item{w}{Calculated Wheatsheaf index}
  \item{low95}{Lower bound of 95\% confidence interval for the Wheatsheaf index obtained by jackkniving}
  \item{up95}{Upper bound of 95\% confidence interval for the Wheatsheaf index obtained by jackkniving}
  \item{P}{P-value from bootstrapping the tips of the phylogenetic tree}
  \item{boot.dist}{Bootstrap sample of Wheatsheaf index used to calculate P-value}
}
\references{
Arbuckle, K., Bennett, C.M. and Speed, M.P. 2014. A simple measure of the strength of convergent evolution. Methods in Ecology and Evolution 5:685 - 693.
}
\author{
Kevin Arbuckle and Amanda Minter
}

\seealso{
\code{\link{windex}}
\code{\link{windex.sim.test}}
}
\examples{
\dontrun{
data(sample.data)
data(sample.tree)
test.windex(sample.data,sample.tree,traits=c("bm1","bm2"),focal=sample.data[,2],
reps=1000,plot=TRUE,col="light grey")
}
}
