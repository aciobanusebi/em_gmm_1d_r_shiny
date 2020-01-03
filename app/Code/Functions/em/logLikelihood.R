function(D,pi,mu,sigma) {
  result <- 0
  for(i in 1:length(D)) {
    pSum <- 0
    for(j in 1:length(pi)) {
      pSum <- pSum + pi[j] * dnorm(D[i],mu[j],sigma[j])
    }
    result <- result + log(pSum)
  }
  result
}