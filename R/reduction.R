#' Reduce a linear model by iteratively checking the signficance of each predictor.
#'
#' The procedure takes a linear model of class \code{lm} (including the output from
#' a stepwise procedure) and reduces it by iteratively deleting predictors 
#' based on pvalues of the t-tests. In particular, conversely to what the 
#' function \code{\link[stats]{lm}} currently does, it allows the inclusion of just 
#' high order effects. For instance, it can fit just interaction terms given by 
#' x1*x2 or x1:x2 passed with the usual \code{R} notation.
#'
#' @param model An object of class \code{lm}.
#' @param data A \code{data.frame} or \code{data.table} on which \code{model} has run.
#' @param alpha The significance level at which cut off predictors. Default is 5\%.
#' @param intercept Specify whether the reduction procedure has to consider an 
#' intercept or not. Default is \code{TRUE}.
#' 
#' @details The procedure makes use of the workhorse function of \code{lm}: 
#' \code{lm.fit}. This allows to explicitly specify the design matrix which enables
#' to split main effects from interactions. 
#' 
#' @return A list with the following elements:\cr
#' \enumerate{
#' \item \code{coef}: a matrix of estimated coefficients, standard errors, t-value and
#' the relative p-values
#' \item \code{sigma}: the residual standard error and the associated degrees of freedom
#' \item \code{r.squared}: the R^2 of the model
#' \item \code{adj.r.squared}: the adjusted R^2 of the model
#' \item \code{fstatistic}: the F-statistic of the model and the associated degrees of freedom
#' \item \code{rdf}: the explicit value of the degrees of freedom of the model.
#' }
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
#' reduced_model = reduction( complete_model, data = data_factors )
#' summary( reduced_model )
#' 
#' @seealso \code{\link[stats]{lm.fit}} \code{\link[stats]{lm}} \code{\link[stats]{step}}
#' @author Francesco Grossetti \email{francesco.grossetti@@gmail.com}.
#' @export

reduction = function( model, ... ) {
  UseMethod( "reduction", model )
}

#' @rdname reduction
#' @aliases reduction
#' 
#' @importFrom stats lm.fit

reduction.lm = function( model, data, alpha = 0.05, intercept = TRUE ) {
  
  compute_pval_max = function( obj ) {
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
    if ( alpha > pval[ pval_max ] && pval_max != "(Intercept)" && !exists( "X_red" ) ) {
      cat( "---\n" )
      message( "alpha is too high to start the reduction. Please lower the significance value" )
      cat( "---\n" )
    } else if ( alpha > pval[ pval_max ] && pval_max != "(Intercept)" && exists( "X_red" ) ) {
      cat( "---\n" )
      message( "reduction procedure completed with alpha = ", alpha )
      cat( "---\n" )
    }
    lm.results = matrix( 0, nrow = z$rank, ncol = 4, byrow = TRUE ) 
    lm.results = cbind( est, se, tval, pval )
    colnames( lm.results ) = c( "Estimate", "Std. Error", "t value", "Pr(>|t|)" ) 
    
    return( list( pval, pval_max ) )
  }
  
  final_fit = function( obj ) {
    digits = 5
    z = obj
    r = z$residuals
    n = length( r )
    p = z$rank
    rss = sum( r^2 )
    rdf = z$df.residual
    resvar = rss / rdf
    sigma = sqrt( resvar )
    f = z$fitted.values
    if( any( names( z$coefficients ) %in% "(Intercept)" ) ) {
      mss = sum( ( f - mean( f ) )^2 ) 
      df.int = 1L
    } else {
      mss = sum( f^2 )
      df.int = 0L
    }
    r.squared = mss / ( mss + rss )
    adj.r.squared = 1 - ( 1 - r.squared ) * ( ( n - df.int ) / rdf )
    fstatistic = c( value = ( mss / ( p - df.int ) ) / resvar, 
                    numdf = p - df.int, 
                    dendf = rdf )
    Qr = z$qr
    p = z$rank
    p1 = 1L:p
    R = chol2inv( Qr$qr[ p1, p1, drop = FALSE ] )
    se = sqrt( diag( R ) * resvar )
    est = z$coefficients[ Qr$pivot[ p1 ] ]
    tval = est/se
    pval = 2 * pt( abs( tval ), rdf, lower.tail = FALSE )
    lm.results = matrix( 0, nrow = z$rank, ncol = 4, byrow = TRUE ) 
    lm.results = cbind( est, se, tval, pval )
    colnames( lm.results ) = c( "Estimate", "Std. Error", "t value", "Pr(>|t|)" ) 
    
    return( invisible( list( coef = lm.results, sigma = sigma, 
                             r.squared = r.squared, 
                             adj.r.squared = adj.r.squared, 
                             fstatistic = fstatistic,
                             rdf = rdf ) ) )
  }
  if ( !intercept ) {
    cat( "---\n" )
    message( "fitting a model without intercept" )
    X_all = model.matrix( model )[ , 2:model$rank ]
  } else {
    X_all = model.matrix( model )
  }
  fit = lm.fit( x = X_all, y = model$model[[ 1 ]] )
  max = compute_pval_max( fit )
  max_name = max[[ 2 ]]
  X_red = X_all[ , !colnames( X_all ) %in% max_name ]
  fit = lm.fit( x = X_red, y = model$model[[ 1 ]] )
  
  cat( "---\n" )
  repeat {
    max = compute_pval_max( fit )
    max_name = max[[ 2 ]]
    threshold = max[[ 1 ]][ max[[ 2 ]] ]
    if ( threshold <= alpha ) {
      break
    }
    cat( "deleting", max_name, "with a pval of:", threshold, "\n" )
    X_red = X_red[ , !colnames( X_red ) %in% max_name ]
    fit = lm.fit( x = X_red, y = model$model[[ 1 ]] )
  }
  mod_final = final_fit( fit )
  return( mod_final )
}
