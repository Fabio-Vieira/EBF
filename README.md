# EBF

This package contains a function to compute the Empirical Bayes Factor for testing for the presence of random effects.

## Instalation

``` r
library(devtools)
install_github("Fabio-Vieira/EBF")

#load the package
library(EBF)
```

## Example

Run the code below for an example of how to prepare the estimates to pass to the function.

Don't forget to name the columns in the matrix with the random effects, those names will be used to tell you which values of the EBF correspond to which variables in the model.

We simulated data from the model below. To see the parameters used in this simulation, check the file **/data-raw/example.R**.

$$
{\large
\begin{gathered}
y_{ij} = \beta_0 + u_{i} + v_{j} + \epsilon_{ij}, \\
u_{i} \sim \mathcal{N}(0, \sigma_{u}^2), \\
v_{j} \sim \mathcal{N}(0, \sigma_{v}^2), \\
\epsilon_{ij} \sim \mathcal{N}(0, \sigma_\epsilon^2)
\end{gathered}
}
$$

Where ${\large y_{ij}}$ is the response variable, ${\large \beta_0}$ the fixed intercept, ${\large u_{i}}$ is the random intercept for group A, ${\large v_{j}}$ the random intercept for group B and ${\large \epsilon_{ij}}$ is the error term.

We fit the model using a fully Bayesian approach and extract the parameters needed to conduct the test. The goal of this example is to show the user how the parameters should be defined in the EBF function so the test is properly calculated.

In this example, the random intercept for **group A** should display evidence of being a true random effect, whereas the random intercept for **group B** should not.

``` r
#Loading the libraries
library(EBF)
library(rstanarm)
#Loading the data
data(example)

#Fitting the model
model <- stan_lmer(y ~ 1 + (1 | groupA) + (1 | groupB), data = example,
                   chains = 1)

#Extracting the posterior samples
posterior_samples_array <- as.data.frame(model)

#Extracting the parameters to compute the test
b <- data.frame(groupA = colMeans(posterior_samples_array[,paste0("b[(Intercept) groupA:", 1:10, "]")]), #group A
                groupB = colMeans(posterior_samples_array[,paste0("b[(Intercept) groupB:", 1:10, "]")]) #group B
            )

covb <- list(groupA = cov(posterior_samples_array[,paste0("b[(Intercept) groupA:", 1:10, "]")]),
             groupB = cov(posterior_samples_array[,paste0("b[(Intercept) groupB:", 1:10, "]")]))

sig <- colMeans(posterior_samples_array[,c("Sigma[groupA:(Intercept),(Intercept)]","Sigma[groupB:(Intercept),(Intercept)]")])

#Computing the EBF
EBF(b, covb, sig)
```

## Citation

If you use this package in your research, please cite the paper below.

``` r
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
