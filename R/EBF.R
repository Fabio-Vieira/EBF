#' @title EBF: A function to compute the Empirical Bayes Factor from model estimates
#'
#' @param theta a matrix with the random effects estimates (name the columns, so the vector of EBFs will also be named)
#' @param sig a list containing the covariance matrix of the random effect, the list should have the same length as the number of random effects
#' @param tau a vector with the estimates of the variance of the random effects
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
      p <- sapply(1:nrow(tau), function(x) mvtnorm::dmvnorm(x = rep(0, j),
                                                            mean = rep(0, j),
                                                            sigma = diag(tau[x,i], j),
                                                            log = logarithm))
      out$samples <- nrow(tau)
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

