#' Title
#'
#' @slot y numeric.
#' @slot mle numeric.
#' @slot ll numeric.
#' @slot se numeric.
#' @slot SEtype character.
#'
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
    if(!is.numeric(object@y)) stop("y is not  numeric")
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
