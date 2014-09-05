\name{sample.data}
\alias{sample.data}
\docType{data}
\title{
Sample data for windex
}
\description{
Simulated data in a format suitable for use with the functions in the windex package.
}
\usage{data(sample.data)}
\format{
  A data frame with 100 observations on the following 9 variables.
  \describe{
    \item{\code{species}}{a factor}
    \item{\code{focals}}{a numeric vector}
    \item{\code{bm1}}{a numeric vector}
    \item{\code{bm2}}{a numeric vector}
    \item{\code{bm3}}{a numeric vector}
    \item{\code{ou1}}{a numeric vector}
    \item{\code{ou2}}{a numeric vector}
    \item{\code{ou3}}{a numeric vector}
    \item{\code{bin}}{a numeric vector}
  }
}
\details{
The three 'bm' columns are values for three trait that have not evolved convergently. The three 'ou' columns are values for three traits that have evolved convergently with respect to the focal designation. bin is a column that was only for utility when creating the dataset.
}

\examples{
dat<-data(sample.data)
summary(dat)
}
\keyword{datasets}
