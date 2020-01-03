getPdfFromParameters <- function(pi,mu,sigma) {
  f <- function(x) {
    pi * dnorm(x,mu,sigma)
  }
  f
}
