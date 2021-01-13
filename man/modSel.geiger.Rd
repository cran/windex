\name{modSel.geiger}
\alias{modSel.geiger}
\title{
Model selection table for phenotypic evolution models fit in the package geiger, based on information theoretical measures.
}
\description{
Creates a model selection table based on either AIC or AICc for phenotypic evolution models fit using the fitContinuous() and fitDiscrete() functions in the geiger package.
}
\usage{
modSel.geiger(...,type="AICc")
}
\arguments{
  \item{\dots}{
A set of phenotypic evolution models fitted with either fitContinuous() or fitDiscrete() in the package geiger, which you want to compare.
}
  \item{type}{
Type of information theoretical measure you want to use (AICc or AIC are allowed), defaults to AICc.
}
}

\value{
Model selection table with rownames corresponding to input model names and columns for K (number of parameters), logLik (log-likelihood), AICc (or AIC), deltaAICc (or deltaAIC, the difference between each model and the best model), Weight (Akaike weights, aka model probabilities), and Evidence ratio (the amount of evidence for the best model relative to each model such that, for instance, 4 would mean that model has 4x less evidence supporting it than the best model).
}

\author{
Kevin Arbuckle
}

\examples{
\dontrun{
# Two models initially run in geiger using fitContinuous (see help file for that package to fit
# these models) and saved as objects named 'bm' and 'ou'.

# Model selection table using AICc
modSel.geiger(bm,ou)

# Model selection table using AIC
modSel.geiger(bm,ou,type="AIC")}
}