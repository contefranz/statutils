count_na = function( x ) {
  round( sum( is.na( x ) ) / length( x ) * 100, 2 )
}

# after computed the complete dataset, compute its average over all vars
# this should guarantee a consistent result given by pool()
def_complete = function( data, data_key ) {
  
  if ( !inherits( data, "data.table" ) ) {
    stop( "data must be a data.table" )
  }
  
  data_key = as.character( substitute( list( data_key ) )[ -1L ] )
  numeric_vars = sapply( data, is.numeric )
  average = data[ , lapply( .SD, mean ), by = eval( data_key ), 
                  .SDcols = numeric_vars ]
  factor_vars = sapply( data, is.factor )
  factor_data = data[ .imp == 1, .SD, .SDcols = factor_vars ]
  
  setkeyv( average, data_key )
  setkeyv( factor_data, data_key )
  finale = average[ factor_data ]
  finale[ , .id := NULL ]
  finale[ , .imp := NULL ]
  
  return( invisible( finale ) )
  
}



