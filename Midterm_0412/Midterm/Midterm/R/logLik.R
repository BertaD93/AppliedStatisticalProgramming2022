#' Title
#'
#' @param y
#' @param lambda
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
setGeneric( #Create a generic
  name = "logLik",
  def = function(y, lambda, ...)
  {standardGeneric("logLik")}
)

#' @export
setMethod( #Create the function to calculate a log likelihood for the observed data
f = "logLik",
definition = function(y, lambda, ...){
  n <- length(y)
  ll <- -n*lambda- sum(lfactorial(y))+ log(lambda)*sum(y)

  return(ll)

}
)
