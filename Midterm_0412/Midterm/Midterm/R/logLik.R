#' Log likelihood
#'
#' Calculate the log likelihood for the observed data
#'
#' @param y numeric. The vector of observed data
#' @param lambda numeric. The assume value of lambda
#'
#'
#' @author Berta Diaz
#'
#'
#' @author Berta Diaz
#' @return Log likelihood
#' @export
#'
#' @examples
#' y = c(1,2,3,4)
#' lambda = 4
#' logLik(y,lambda)
setGeneric( #Create a generic
  name = "logLik",
  def = function(y, lambda)
  {standardGeneric("logLik")}
)

#' @export
setMethod( #Create the function to calculate the log likelihood for the observed data
f = "logLik",
definition = function(y, lambda){ #Start function
  n <- length(y) #n is the number of elements in 'y' vector
  ll <- -n*lambda- sum(lfactorial(y))+ log(lambda)*sum(y) #Calculate the loglikelihood for the observed data conditioned in lambda

  return(ll) #Return the value of ll

}
)
