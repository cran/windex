\name{lrTest}
\alias{lrTest}
\title{
Calculates likelihood ratio test
}
\description{
Takes raw log-likelihood values plus degrees of freedom and performs a likelihood ratio test.
}
\usage{
lrTest(small,big,df)
}
\arguments{
  \item{small}{
Numerical value giving the smaller of the two log-likelihoods being compared.
}
  \item{big}{
Numerical value giving the larger of the two log-likelihoods being compared.
}
  \item{df}{
A numerical value giving the degrees of freedom for the test.
}
}

\value{
  \item{LR}{Likelihood ratio statistic}
  \item{P}{P-value from likelihood ratio test}
}

\author{
Kevin Arbuckle
}