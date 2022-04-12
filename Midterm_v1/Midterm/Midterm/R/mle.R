#' Title
#'
#' @param y
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
setGeneric( #Create a generic
  name = "mle",
  def = function(y, ...)
  {standardGeneric("mle")}
)

#' @export
setMethod( #Create the function to calculate the standard error
  f = "mle",
  definition = function(y, ...){
    n <- length(y)
    mle <- sum(y)/n

    return(mle)

  }
)
mle(y)
