\name{modSel.rayDISC}
\alias{modSel.rayDISC}
\title{
Model selection table for evolutionary pathway models fit in the package corHMM, based on informational theoretical measures.
}
\description{
Creates a model selection table based on either AIC or AICc for evolutioanry pathway models fit using the rayDISC() function in the corHMM package.
}
\usage{
modSel.rayDISC(...,type="AICc")
}
\arguments{
  \item{\dots}{
A set of evolutionary pathway models fitted with rayDISC() in the package corHMM, which you want to compare.
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
# Two models initially run in corHMM using rayDISC (see help file for that package to fit
# these models) and saved as objects named 'rev' (for reversible) and 'non' (for non-reversible).

# Model selection table using AICc
modSel.rayDISC(non,rev)

# Model selection table using AIC
modSel.rayDISC(non,rev,type="AIC")}
}