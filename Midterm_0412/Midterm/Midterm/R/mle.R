#' Maximum likelihood
#'
#' Calculates the maximum likelihood estimator for lambda
#'
#' @param y numeric. The vector of observed data
#'
#' @author Berta Diaz
#'
#' @return
#' @export
#'
#' @examples
#' y=c(1,2,3,4)
#' mle(y)
setGeneric( #Create a generic
  name = "mle",
  def = function(y)
  {standardGeneric("mle")}
)

#' @export
setMethod( #Create the function to calculate the Maximum likelihood
  f = "mle",
  definition = function(y){#Number of observations
    n <- length(y) #Number of observations
    mle <- sum(y)/n #Sum of y over n

    return(mle) #Return the mle

  }
)

