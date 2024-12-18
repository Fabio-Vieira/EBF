% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/EBF.R
\name{EBF}
\alias{EBF}
\title{EBF: A function to compute the Empirical Bayes Factor from model estimates}
\usage{
EBF(theta, sig, tau, logarithm = T)
}
\arguments{
\item{theta}{a matrix with the random effects estimates (name the columns, so the vector of EBFs will also be named)}

\item{sig}{a list containing the covariance matrix of the random effect, the list should have the same length as the number of random effects}

\item{tau}{a vector with the estimates of the variance of the random effects}

\item{logarithm}{a logical parameter that indicates whether to compute the EBF in the log scale or not}
}
\value{
a vector with the EBFs
}
\description{
EBF: A function to compute the Empirical Bayes Factor from model estimates
}
\examples{
# Install rstanarm if needed
# install.packages("rstanarm")

# Load required libraries
library(rstanarm)
library(dplyr)
library(EBF)

# Check the structure of the mtcars dataset
str(mtcars)

# Convert 'cyl' to a factor since it's a grouping variable
mtcars$cyl <- as.factor(mtcars$cyl)

# Fit a mixed-effects model with random intercepts for 'cyl'
model <- stan_glmer(mpg ~ wt + hp + (1 | cyl),
                    data = mtcars,
                    prior_covariance = decov(1),
                    chains = 1)

# Summary of the model
summary(model)

# Extract posterior samples as an array
posterior_samples_array <- as.array(model)

# View the structure of the posterior samples array
str(posterior_samples_array)

# Optionally, inspect the first few posterior samples for a specific parameter
posterior_samples_array[1:5, , ]

#Getting the parameters to run the EBF
b <- matrix(colMeans(posterior_samples_array[,,4:6]), ncol = 1)
colnames(b) <- "cyl"
sig <- list(cov(posterior_samples_array[,,4:6]))
tau <- matrix(posterior_samples_array[,,8])

#Computing the EBF
EBF(theta = b, sig = sig, tau = tau)


}
