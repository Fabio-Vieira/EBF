#' @title EBF: A function to compute the Empirical Bayes Factor from model estimates
#'
#' @param theta a matrix with the random effects estimates (name the columns, so the vector of EBFs will also be named)
#' @param sig a list containing the covariance matrix of the random effect, the list should have the same length as the number of random effects
#' @param tau a vector with the estimates of the variance of the random effects
#' @param logarithm a logical parameter that indicates whether to compute the EBF in the log scale or not
#'
#' @return a vector with the EBFs
#'
#' @examples
#' # Install rstanarm if needed
#' # install.packages("rstanarm")
#'
#' # Load required libraries
#' library(rstanarm)
#' library(dplyr)
#'
#' # Check the structure of the mtcars dataset
#' str(mtcars)
#'
#' # Convert 'cyl' to a factor since it's a grouping variable
#' mtcars$cyl <- as.factor(mtcars$cyl)
#'
#' # Fit a mixed-effects model with random intercepts for 'cyl'
#' model <- stan_glmer(mpg ~ wt + hp + (1 | cyl),
#'                     data = mtcars,
#'                     prior_covariance = decov(1),
#'                     chains = 1)
#'
#' # Summary of the model
#' summary(model)
#'
#' # Extract posterior samples as an array
#' posterior_samples_array <- as.array(model)
#'
#' # View the structure of the posterior samples array
#' str(posterior_samples_array)
#'
#' # Optionally, inspect the first few posterior samples for a specific parameter
#' posterior_samples_array[1:5, , ]
#'
#' #Getting the parameters to run the EBF
#' b <- matrix(colMeans(posterior_samples_array[,,4:6]), ncol = 1)
#' colnames(b) <- "cyl"
#' sig <- list(cov(posterior_samples_array[,,4:6]))
#' tau <- posterior_samples_array[,,8]
#'
#' #Computing the EBF
#' EBF(theta = b, sig = sig, tau = tau)
#'
#'
#' @export
EBF <- function(theta, sig, tau, logarithm = T){
  #Creating output
  out <- vector("list")
  #Number of random effects
  q <- ncol(theta)
  if(is.null(q)){ # we only have one random effect
    q <- 1
    theta <- matrix(theta, ncol = 1)
    #Number of groups
    j <- length(theta)
  } else {
    #Number of groups
    j <- nrow(theta)
  }
  random_effects <- colnames(theta)
  #Prior
  prior <- numeric(q)
  #Posterior
  posterior <- numeric(q)
  #Naming the vectors of prior and posterior
  names(prior) <- names(posterior) <- random_effects
  for(i in 1:q){
    if(is.matrix(tau)){ # here we assume the user wants to approximate the integral in the denominator of the EBF
      p <- as.numeric(nrow(tau))
      out$samples <- nrow(tau)
      for(h in 1:nrow(tau)){
        p[h] <- mvtnorm::dmvnorm(x = rep(0, j),
                                 mean = rep(0, j),
                                 sigma = diag(tau[h,i], j),
                                 log = logarithm)
      }
      out$integral <- T
      prior[i] <- mean(p)
    } else {
      out$integral <- F
      prior[i] <- mvtnorm::dmvnorm(x = rep(0, j),
                                   mean = rep(0, j),
                                   sigma = diag(tau[i], j),
                                   log = logarithm)
    }
    posterior[i] <- mvtnorm::dmvnorm(x = rep(0, j),
                                     mean = theta[,i],
                                     sigma = sig[[i]],
                                     log = logarithm)
  }
  out$posterior <- posterior
  out$prior <- prior
  out$params <- list(theta = theta,
                     sigma = sig,
                     tau = tau)
  out$q <- q
  out$j <- j
  if(logarithm){
    out$EBF <- matrix(posterior - prior, ncol = 1)
    colnames(out$EBF) <- "Log of EBF"
  } else {
    out$EBF <- matrix(posterior/prior, ncol = 1) #Savage-Dickey density ratio
    colnames(out$EBF) <- "EBF"
  }
  #Giving the name to the EBFs based on the names of the columns of theta
  row.names(out$EBF) <- random_effects
  #Defining class
  class(out) <- "EBF"
  return(out)
}

