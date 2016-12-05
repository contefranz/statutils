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
  Qr = z$qr
  p1 = 1L:p
  R = chol2inv( Qr$qr[ p1, p1, drop = FALSE ] )
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
  se = sqrt( diag( R ) * resvar )
  est = z$coefficients[ Qr$pivot[ p1 ] ]
  tval = est/se
  pval = 2 * pt( abs( tval ), rdf, lower.tail = FALSE )
  lm.results = matrix( 0, nrow = z$rank, ncol = 4, byrow = TRUE ) 
  lm.results = cbind( est, se, tval, pval )
  colnames( lm.results ) = c( "Estimate", "Std. Error", "t value", "Pr(>|t|)" ) 
  
  return( invisible( list( coef = lm.results, residuals = r, 
                           fitted.values = f, sigma = sigma, 
                           r.squared = r.squared, 
                           adj.r.squared = adj.r.squared, 
                           fstatistic = fstatistic,
                           rdf = rdf ) ) )
}
