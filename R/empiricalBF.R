#' @title empiricalBF: A function to compute the Empirical Bayes Factor from model estimates
#'
#' @param theta a matrix with the random effects estimates (name the columns, so the vector of EBFs will also be named)
#' @param sig a list containing the covariance matrix of the random effect, the list should have the same length as the number of random effects
#' @param tau a vector with the estimates of the variance of the random effects
#'
#' @return a vector with the EBFs
#'
#' @export
empiricalBF <- function(theta, sig, tau, log = T){
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
      for(h in 1:nrow(tau)){
        p[h] <- mvtnorm::dmvnorm(x = rep(0, j),
                                 mean = rep(0, j),
                                 sigma = diag(tau[h,i], j),
                                 log = log)
      }
      prior[i] <- mean(p)
    } else {
      prior[i] <- mvtnorm::dmvnorm(x = rep(0, j),
                                   mean = rep(0, j),
                                   sigma = diag(tau[i], j),
                                   log = log)
    }
    posterior[i] <- mvtnorm::dmvnorm(x = rep(0, j),
                                     mean = theta[,i],
                                     sigma = sig[[i]],
                                     log = log)
  }
  if(log){
    return(posterior - prior)
  } else {
    return(posterior/prior) #Savage-Dickey density ratio
  }
}

