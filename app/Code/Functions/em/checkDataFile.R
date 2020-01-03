function(data) {
  ncol(data) == 1 && is.numeric(data[[1]])
}