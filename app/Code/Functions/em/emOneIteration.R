function(D,n,k,E,pi,mu,sigma,piFixed=F,muFixed=F,sigmaFixed=F) {
  # E STEP
  for(i in 1:nrow(E)) {
    denominator <- 0
    for(j in 1:k) {
      denominator <- denominator + pi[j] * dnorm(D[i],mu[j],sigma[j])
    }
    for(j in 1:k) {
      numerator <- pi[j] * dnorm(D[i],mu[j],sigma[j])
      E[i,j] <- numerator / denominator
    }
  }
  
  # M STEP
  for(j in 1:k) {
    S <- sum(E[,j])
    if(!piFixed) {
      pi[j] <- S/n
    }
    if(!muFixed) {
      mu[j] <- (E[,j] %*% D)/S
    }
    if(!sigmaFixed) {
      sigma[j] <- sqrt((E[,j] %*% (D - mu[j])^2)/S)
    }
  }
  
  list(
    k = k,
    E = E,
    pi = pi,
    mu = mu,
    sigma = sigma,
    loglike = Functions$logLikelihood(D,pi,mu,sigma)
  )
}