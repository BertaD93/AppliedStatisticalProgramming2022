#' estimatePois
#'
#' Returns an object of class PoisMLE
#'
#' @param y numeric. A vector of observed data
#' @param SEtype character. The method used to calculate the standard error: basic or bootstrap
#' @param ...
#'
#' @author Berta Diaz
#' @return
#' @export
#'
#' @examples
#' y_test = rpois(100,4)
#' estimatePois(y_test, SEtype = "bootstrap")
#'
setGeneric( #Create a generic
  name = "estimatePois",
  def = function(y, SEtype, ...)
  {standardGeneric("estimatePois")}
)

#' @export
setMethod( #Create a function that return an object of class PoisMLE
  f = "estimatePois",
  definition = function(y, SEtype, ...){ #Here we are using functions that we already built.
    MLE <- mle(y) #Maximum likelihood
    LL <- logLik(y, MLE) #Log likelihood
    SE <- standardError(y, SEtype) #Standard errors
    return(new("PoisMLE", y = y,mle=MLE, ll=LL, se=SE, SEtype=SEtype)) #Return an object of class PoisMLE
  }
)


