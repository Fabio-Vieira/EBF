#' @title EBF: A function to compute Empirical Bayes Factors.
#'
#' @description
#' A function to compute the Empirical Bayes Factor (Vieira et al., 2024a, 2024b) from the random effects estimates, the random effects error (posterior) covariance matrix, and the estimated random effects variance.
#'
#'
#' @param theta a vector of random effects estimates, or a matrix with multiple the random effects estimates (name the columns, so the vector of EBFs will also be named).
#' @param var	 the error (posterior) covariance matrix of the random effects, or a list of covariance matrices of multiple random effects.
#' @param Sigma an estimate of a variance of the random effects, or a vector with the estimates of the variance of the random effects.
#' @param logarithm a logical parameter that indicates whether to compute the EBF in the log scale or not
#'
#' @return a vector with the EBFs
#'
#' @references
#' Vieira, F., Zhao, H., & Mulder, J. (2024). To Vary or Not To Vary: A Simple Empirical Bayes Factor for Testing Variance Components. arXiv preprint arXiv:2410.14459.
#'
#' Vieira, F., Leenders, R., McFarland, D., & Mulder, J. (2024). A Bayesian actor-oriented multilevel relational event model with hypothesis testing procedures. Behaviormetrika, 51(1), 37-74.
#'
#' @examples
#'
#' library(EBF)
#' #Fitting the model
#' library(rstanarm)
#'
#' data(example)
#'
#' model <- stan_lmer(y ~ 1 + (1 | groupA) + (1 | groupB), data = example,
#'                    chains = 1)
#'
#'posterior_samples_array <- as.data.frame(model)
#'
#' b <- data.frame(groupA = colMeans(posterior_samples_array[,paste0("b[(Intercept) groupA:", 1:10, "]")]), #group A
#'                 groupB = colMeans(posterior_samples_array[,paste0("b[(Intercept) groupB:", 1:10, "]")]) #group B
#' )
#'
#' covb <- list(groupA = cov(posterior_samples_array[,paste0("b[(Intercept) groupA:", 1:10, "]")]),
#'              groupB = cov(posterior_samples_array[,paste0("b[(Intercept) groupB:", 1:10, "]")]))
#'
#'sig <- colMeans(posterior_samples_array[,c("Sigma[groupA:(Intercept),(Intercept)]","Sigma[groupB:(Intercept),(Intercept)]")])
#'
#'EBF(b, covb, sig)
#'
#' @export
EBF <- function(theta, var, Sigma, logarithm = T){
  #Creating a conditional to check if we have one random effect
  if(dim(theta)[2] != 1){
    if(is.matrix(var)){
      stop("There's more than one random effect. Their error matrix should be a list.")
    }
  }
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
    if(is.matrix(Sigma)){ # here we assume the user wants to approximate the integral in the denominator of the EBF
      p <- sapply(1:nrow(Sigma), function(x) mvtnorm::dmvnorm(x = rep(0, j),
                                                            mean = rep(0, j),
                                                            sigma = diag(Sigma[x,i], j),
                                                            log = logarithm))
      out$samples <- nrow(Sigma)
      out$integral <- T
      prior[i] <- mean(p)
    } else {
      out$integral <- F
      prior[i] <- mvtnorm::dmvnorm(x = rep(0, j),
                                   mean = rep(0, j),
                                   sigma = diag(Sigma[i], j),
                                   log = logarithm)
    }
    if(is.matrix(var)){
      posterior[i] <- mvtnorm::dmvnorm(x = rep(0, j),
                                       mean = theta[,i],
                                       sigma = var,
                                       log = logarithm)
    } else {
      posterior[i] <- mvtnorm::dmvnorm(x = rep(0, j),
                                       mean = theta[,i],
                                       sigma = var[[i]],
                                       log = logarithm)
    }
  }
  out$posterior <- posterior
  out$prior <- prior
  out$params <- list(theta = theta,
                     var = var,
                     Sigma = Sigma)
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

