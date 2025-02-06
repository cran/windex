\name{mark.dist}
\alias{mark.dist}
\title{
Summarising distributions of (exam/coursework/etc.) marks for (UK) university teaching
}
\description{
Provides a breakdown of marks including summary statistics, plotted histogram with test of Normality, and distribution of grades (for UK system, i.e. 1st, 2.1, 2.2, 3rd, fail). Note that grading system is assumed to be out of 100 and grade boundaries are assumed to be 40 (from fail to 3rd), 50 (from 3rd to 2.2), 60 (from 2.2 to 2.1), and 70 (from 2.1 to 1st).
}
\usage{
mark.dist(marks,plot=TRUE,col="light grey",main=NULL,xlab="Marks",xlim=c(0,100),
showBounds=FALSE,y=20,digits=2,...)
}
\arguments{
  \item{marks}{
Numerical vector containing the marks being summarised.
}
  \item{plot}{
A logical indicating whether to plot a histogram of mark distribution.
}
  \item{col}{
Fill colour for histogram, passed to hist().
}
  \item{main}{
Title for histogram, passed to hist().
}
  \item{xlab}{
Text for x-axis label of histogram, passed to hist().
}
  \item{xlim}{
Numerical vector of length 2 giving start and end points of x-axis of histogram, passed to hist().
}
  \item{showBounds}{
A logical which, if TRUE, adds dashed lines and text to the histogram illustrating grade boundaries.
}
  \item{y}{
If showBounds=TRUE, y gives the height to plot the text on the histogram (this will likely need tweaked for each case).
}
  \item{digits}{
Number of digits to round values in grade breakdown table, default gives proportions to a precision of 1 percent (0.01).
}
  \item{\dots}{
Additional arguments passed to hist() to customise the histogram.
}
}

\value{
  \item{Summary}{Summary statistics of the mark distribution}
  \item{NormalityTest}{Results of Shapiro-Wilk normality test}
  \item{GradeBreakdown}{Proportion of marks falling into each grade}
}

\author{
Kevin Arbuckle
}

\examples{
testscores<-rnorm(n=85,mean=60,sd=15)
mark.dist(testscores)
}