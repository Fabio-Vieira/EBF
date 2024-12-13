# EBF

This package contains a function to compute the Empirical Bayes Factor for testing for the presence of random effects.

## Instalation

```R
library(devtools)
install_github("Fabio-Vieira/EBF")

#load the package
library(EBF)
```

## Example

Run the code below for an example of how to prepare the estimates to pass to the function.

Don't forget to name the columns in the matrix with the random effects, those names will be used to tell you which values of the EBF correspond to which variables in the model.

```R
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
```

## Citation

If you use this package in your research, please cite the paper below.

```R
@article{vieira2024vary,
  title={To Vary or Not To Vary: A Simple Empirical Bayes Factor for Testing Variance Components},
  author={Vieira, Fabio and Zhao, Hongwei and Mulder, Joris},
  journal={arXiv preprint arXiv:2410.14459},
  year={2024}
}
```

## Issues

This is a first version of this package, so it might have some bugs. If you find any, please feel free to report.

## Acknowledgements

The development of this package was supported by a Vidi Grant (452-17-006) awarded by the Netherlands Organization for Scientific Research (NWO) Grant.
