simpsons_uno <- function(x,f,limits) {
  if (is.function(f) == FALSE) {
    stop('f must be a function with a parameter')
  }
  if (limits[1]>min(x) | max(x)>limits[2]) {
    stop('the limits need to be between a and b')
  }
  n <-length(x)-1
  h <- (limits[2] - limits[1])/n
  x <- sort(x)
  y <- f(x)
  s <- (h / 3) * y %*% c(1, rep(c(2,4),(n-1)/2), 1)

  return(s)
}


simpsons_dos <- function(x,f,limits) {
  if (is.function(f) == FALSE) {
    stop('f must be a function with a parameter')
  }

  n <-length(x)-1
  h <- (limits[2] - limits[1])/n
  y <- f(x)

  s <- (h / 3) * y %*% c(1, 2 ^ (1:(n-1) %% 2 + 1), 1)

  return(s)
}

simpsons_dos(x,f,lim)
simpsons_uno(x,f,lim)
