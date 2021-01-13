\name{error.bars}
\alias{error.bars}
\title{
Plots error bars on a barplot
}
\description{
Adds error bars to a barplot.
}
\usage{
error.bars(x,y,upper,lower=upper,length=0.1,...)
}
\arguments{
  \item{x}{
Command to produce a boxplot (saved as an object).
}
  \item{y}{
Vector of heights of bars (same as vector given as height argument to barplot).
}
  \item{upper}{
Vector of upper confidence interval to plot as error bars.
}
  \item{lower}{
Vector of lower confidence interval to plot as error bars (defaults to symmetrical error bars but can be specified separately for lower and upper confidence intervals to enable asymmetrical error bars to be plotted).
}
  \item{length}{
Width of floor and ceiling of error bars in inches (not the length of the confidence interval, just an aesthetic choice), passed to arrows().
}
  \item{\dots}{
Arguments to be passed to arrows() to customise appearance of error bars.
}
}

\author{
Kevin Arbuckle
}

\examples{
means<-c(4,5,7,11)
ci<-c(0.5,1.2,0.7,1)
x<-barplot(means,main="",ylim=c(0,15),ylab="Mean number of things",xlab="Colours",
names.arg=c("Blue","Red","Yellow","Black"),cex.lab=1.5,col=c("blue","red","yellow","black"))
error.bars(x,y=means,upper=ci,col="grey30")
}