#' Confidence intervals for model covariates
#'
#' Computes confidence intervals for one or more covariates in a fitted model.
#' 
#' @param model A fitted model.
#' @param covariates A character vector giving the names of the covariates in 
#' the model. When missing, the confidence intervals are computed for all the
#' covariates.
#' @param level The confidence level required. Default is 95\%.
#' 
#' @details This function computes confidence intervals without calling 
#' \code{\link[stats]{vcov}}, but it still assumes normality. This brings slightly
#' different results with respect to the standar 
#' \code{\link[stats]{confint}} method. Either a linear 
#' model given by \code{\link[stats]{lm}} or a reduced model given by 
#' \code{\link{reduction}} can be provided.
#' 
#' @return A matrix (or vector) with columns giving the estimated coefficients,
#' the lower and upper confidence limits for each parameter. 
#' 
#' @seealso \code{\link[stats]{lm}} \code{\link[statutils]{reduction}} 
#' \code{\link[stats]{confint}}
#' @author Francesco Grossetti \email{francesco.grossetti@@gmail.com}.
#' 
#' @examples 
#' model = lm( mpg ~ hp + wt, data = mtcars )
#' 
#' # compute confidence intervals for all covariates
#' conf_int( model )
#' 
#' # specify for which covariate a confidence intervals must be computed
#' conf_int( model, "wt" )
#' @export

conf_int = function( model, ... ) {
  UseMethod( "conf_int", model )
}

#' @rdname conf_int
#' @aliases conf_int
#' @export

conf_int.lm = function( model, covariates, level = 0.95 ) {
  
  alpha = 1 - level
  qz = qnorm( alpha / 2, lower.tail = FALSE )
  model_out = summary( model )$coefficients
  
  if ( missing( covariates ) ) {
    # all covariates
    lwr = model_out[ , 1 ] - qz * model_out[ , 2 ]
    upr = model_out[ , 1 ] + qz * model_out[ , 2 ]
    out = cbind( model_out[ , 1 ], lwr, upr )
    colnames( out )[ 1 ] = colnames( model_out )[ 1 ]
    return( out )
    
  } else {
    # selected covariates
    id.cov = as.numeric( sapply( covariates, function( x ) 
      grep( x, rownames( model_out ) ) ) )
    if ( any( is.na( id.cov ), TRUE ) ) {
      stop( "check the names of the covariates you have provided" )
    }
    lwr = model_out[ c( id.cov ), 1 ] - qz * model_out[ c( id.cov ), 2 ]
    upr = model_out[ c( id.cov ), 1 ] + qz * model_out[ c( id.cov ), 2 ]
    out = cbind( model_out[ c( id.cov ), 1 ], lwr, upr )
    colnames( out )[ 1 ] = colnames( model_out )[ 1 ]
    rownames( out ) = rownames( model_out )[ id.cov ]
    return( out )
    
  }
  
}

#' @rdname conf_int
#' @aliases conf_int
#' @export

conf_int.list = function( model, covariates, level = 0.95 ) {
  
  alpha = 1 - level
  qz = qnorm( alpha / 2, lower.tail = FALSE )
  model_out = model$coef
  
  if ( missing( covariates ) ) {
    # all covariates
    lwr = model_out[ , 1 ] - qz * model_out[ , 2 ]
    upr = model_out[ , 1 ] + qz * model_out[ , 2 ]
    out = cbind( model_out[ , 1 ], lwr, upr )
    colnames( out )[ 1 ] = colnames( model_out )[ 1 ]
    return( out )
    
  } else {
    # selected covariates
    id.cov = as.numeric( sapply( covariates, function( x ) 
      grep( x, rownames( model_out ) ) ) )
    if ( any( is.na( id.cov ), TRUE ) ) {
      stop( "check the names of the covariates you have provided" )
    }
    lwr = model_out[ c( id.cov ), 1 ] - qz * model_out[ c( id.cov ), 2 ]
    upr = model_out[ c( id.cov ), 1 ] + qz * model_out[ c( id.cov ), 2 ]
    out = cbind( model_out[ c( id.cov ), 1 ], lwr, upr )
    colnames( out )[ 1 ] = colnames( model_out )[ 1 ]
    rownames( out ) = rownames( model_out )[ id.cov ]
    return( out )
    
  }
  
}

#' @rdname conf_int
#' @aliases conf_int
#' @export

