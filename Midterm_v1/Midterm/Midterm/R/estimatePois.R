#' Title
#'
#' @param y
#' @param lambda
#' @param SEtype
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
setGeneric( #Create a generic
  name = "estimatePois",
  def = function(y,lambda, SEtype, ...)
  {standardGeneric("estimatePois")}
)

#' @export
setMethod(
  f = "estimatePois",
  definition = function(y,lambda, SEtype, ...){
    MLE <- mle(y)
    LL <- logLik(y, lambda)
    SE <- standardError(y, SEtype)
    #return(new("PoisMLE", MLE=MLE, LL=LL, SE=SE, SEtype=SEtype))


    }
)


