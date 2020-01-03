function(D,zeros,xlim,ylim,k,pi,mu,sigma) {
  # PLOT
  f <- getPdfFromParameters(pi[1],mu[1],sigma[1])
  plot(f,xlim=xlim,ylim=ylim,col=2)
  
  clustering <- rep(0,length(D))
  for(i in 1:length(D)) {
    values <- rep(0,k)
    for(j in 1:k) {
      values[j] <- pi[j] * dnorm(D[i],mu[j],sigma[j])
    }
    myMax <- max(values)
    if(is.infinite(myMax)) {
      clustering[i] <- 0
    } else {
      indexes <- which(values == myMax)
      if(length(indexes) != 1) {
        clustering[i] <- 0
      } else {
        clustering[i] <- indexes[1]
      }
    }
  }
  
  points(D,zeros,pch=16,lwd=2,col=clustering + 1,cex=2)
  points(mu,rep(0,length(mu)),pch=4,lwd=2,col=(1:length(mu))+1,cex=2)
  for(j in 2:k) {
    f <- getPdfFromParameters(pi[j],mu[j],sigma[j])
    plot(f,add=TRUE,xlim=xlim,col=j+1)
  }
}