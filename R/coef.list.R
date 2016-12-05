#' Extract Model Coefficients
#' 
#' @param model A \code{list} as computed by \code{\link{reduction}}.
#' 
#' @export

coef.list = function( model ) {
  print( model$coef[ , 1 ] )
}