function(data) {
  ncol(data) == 3 && is.numeric(data[[1]]) && 
    is.numeric(data[[2]]) && is.numeric(data[[3]]) &&
    sum(data[[1]]) == 1 && all(data[[3]] > 0)
}