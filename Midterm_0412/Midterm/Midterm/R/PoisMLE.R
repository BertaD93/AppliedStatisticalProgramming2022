#' PoisMLE
#'
#' Create an object of class PoisMLE and a plot tgat show the MLE and standard errors (1.96)
#'
#' @slot y numeric.The original data
#' @slot mle numeric. The maximum likelihood estimator for this dataset
#' @slot ll numeric. The log likelihood calculated from the observed data
#' @slot se numeric. Standard error for the MLE
#' @slot SEtype character. The method used to calculate the standard error: basic or bootstrap
#'
#' @author Berta Diaz
#' @return
#' @export
#'
#' @examples
setClass(#We need to setClass to create the structure
  Class = "PoisMLE",
  representation = representation(
    y = "numeric",
    mle = "numeric",
    ll = "numeric",
    se= "numeric",
    SEtype="character"
  ),
  prototype = prototype(
    y=c(),
    mle = c(),
    ll = c(),
    se= c(),
    SEtype=c()

  )
)

#' @export
setValidity( #Create a validator, we can add more validation as we want
  "PoisMLE",
  function(object){
    if(!is.numeric(object@y)) stop("y is not  numeric") #If y it is not numeric, then stop
    if(!all(object@y >= 0)) stop("y must be positive") #If the vector y has negative number, then stop

  }
)

#' @export
setMethod( #Initialize Method
  "initialize", #It is a new class
  "PoisMLE",
  function(.Object, ...) {
    value = callNextMethod() #Call the method PoisMLE
    validObject(value)
    return(value)
  }
)
