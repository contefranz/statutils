#' Reduce a linear model
#'
#' The procedure takes a linear model of class \code{\link[stats]{lm}} 
#' (including the output from a stepwise procedure given by 
#' \code{\link[stats]{step}}) and reduces it by iteratively deleting predictors 
#' based on pvalues of the t-tests (see 'Details'). 
#'
#' @param model An object of class "lm".
#' @param data A \code{data.frame} or \code{data.table} with all components
#' of \code{model}.
#' @param alpha The significance level at which to cut off predictors. Default is 5\%.
#' @param intercept Specify whether the reduction procedure has to consider an 
#' intercept or not. Default is \code{TRUE}.
#' 
#' @details The procedure makes use of the workhorse function of \code{\link[stats]{lm}}: 
#' \code{\link[stats]{lm.fit}}. This allows to explicitly specify the design matrix which enables
#' to split main effects from interactions. In particular, conversely to what the 
#' function \code{\link[stats]{lm}} currently does, it allows the inclusion of just 
#' high order effects. For instance, it can fit just interaction terms given by 
#' x1*x2 or x1:x2 passed with the usual \code{R} formula notation.
#' 
#' @return \code{reduce_model} returns an object of class "lm" with the same
#' components (see \code{\link[stats]{lm}} for a complete list).
#' 
#' @examples
#' data_factor = mtcars
#' data_factor$am = as.factor( data_factor$am )
#' levels( data_factor$am ) = c( "no", "yes" )
#' 
#' complete_model = lm( hp ~ qsec + cyl + mpg + disp + drat + wt + qsec + vs +
#'                      am + gear + carb + am:qsec, data = data_factor )
#' summary( complete_model )
#' 
#' reduced_model = reduce_model( complete_model, data = data_factors )
#' summary( reduced_model )
#' 
#' @seealso \code{\link[stats]{lm.fit}} \code{\link[stats]{lm}} \code{\link[stats]{step}}
#' @author Francesco Grossetti \email{francesco.grossetti@@gmail.com}.
#' @export

reduce_model = function( model, ... ) {
  UseMethod( "reduce_model", model )
}

#' @rdname reduce_model
#' @aliases reduce_model
#' 
#' @importFrom stats lm.fit
#' 
#' @export

reduce_model.lm = function( model, data, alpha = 0.05, intercept = TRUE ) {
  
  cl = mf = model$call
  m = match( c( "formula", "data", "subset", 
                "weights", "na.action", "offset" ), names( mf ), 0L )
  mf = mf[ c( 1L, m ) ]
  mf$drop.unused.levels = TRUE
  mf[[ 1L ]] <- quote( stats::model.frame )
  mf = eval( mf, parent.frame() )
  mt = attr( mf, "terms" )
  y = model.response( mf, "numeric" )
  w = as.vector( model.weights( mf ) )
  if ( !is.null( w ) && !is.numeric( w ) ) 
    stop( "'weights' must be a numeric vector" )
  offset <- as.vector( model.offset( mf ) )
  
  if ( !intercept ) {
    cat( "---\n" )
    message( "fitting a model without intercept" )
    X_all = model.matrix( model )[ , 2:model$rank ]
  } else {
    X_all = model.matrix( model )
  }
  fit = lm.fit( x = X_all, y = model.extract( model.frame( model ), "response") )
  
  max = .compute_pval_max( fit, alpha )
  max_name = max[[ 2 ]]
  X_red = X_all[ , !colnames( X_all ) %in% max_name ]
  fit = lm.fit( x = X_red, y = model$model[[ 1 ]] )
  
  cat( "---\n" )
  repeat {
    max = .compute_pval_max( fit, alpha, X_red )
    max_name = max[[ 2 ]]
    threshold = max[[ 1 ]][ max[[ 2 ]] ]
    if ( threshold <= alpha ) {
      break
    }
    cat( "deleting", max_name, "with a pval of:", threshold, "\n" )
    X_red = X_red[ , !colnames( X_red ) %in% max_name ]
    fit = lm.fit( x = X_red, y = model$model[[ 1 ]] )
  }
  
  class( fit ) = c( if ( is.matrix( y ) ) "mlm", "lm" )
  fit$na.action = attr( mf, "na.action" )
  fit$offset = offset
  fit$xlevels = .getXlevels( mt, mf )
  fit$call = cl
  fit$terms = mt
  fit
  
}

.compute_pval_max = function( obj, alpha, X ) {

  z = obj
  r = z$residuals
  n = length( r )
  rss = sum( r^2 )
  rdf = z$df.residual
  resvar = rss / rdf
  sigma = sqrt( resvar )
  Qr = z$qr
  p = z$rank
  p1 = 1L:p
  R = chol2inv( Qr$qr[ p1, p1, drop = FALSE ] )
  se = sqrt( diag( R ) * resvar )
  est = z$coefficients[ Qr$pivot[ p1 ] ]
  tval = est/se
  pval = 2 * pt( abs( tval ), rdf, lower.tail = FALSE )
  if ( any( names( z$coefficients ) %in% "(Intercept)" ) ) { 
    pval_max = names( which.max( pval[ !names( pval ) %in% "(Intercept)" ] ) )
  } else {
    pval_max = names( which.max( pval ) )
  }
  if ( alpha > pval[ pval_max ] && pval_max != "(Intercept)" && missing( X ) ) {
    cat( "---\n" )
    stop( "alpha is too high to start the reduction. Please lower the significance value" )
    cat( "---\n" )
  } else if ( alpha > pval[ pval_max ] && pval_max != "(Intercept)" && !missing( X ) ) {
    cat( "---\n" )
    message( "reduction procedure completed with alpha = ", alpha )
    cat( "---\n" )
  }
  
  return( list( pval, pval_max ) )
}