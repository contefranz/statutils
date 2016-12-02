#' Utility to count missing data in a \code{matrix}, \code{data.frame} or 
#' \code{data.table}.
#'
#' The function returns the sum of missing data detected in each column.
#'
#' @param data A \code{matrix}, \code{data.frame} or \code{data.table}.
#' 
#' @param prop Compute the column proportion of missing data.
#' 
#' @return Print the results in console.
#' 
#' @author Francesco Grossetti \email{francesco.grossetti@@gmail.com}.
#' @export

count_na = function( data, prop = TRUE ) {
  print( apply( data, 2, function( x ) sum( is.na( x ) ) ) )
  if ( prop ) {
    print( apply( data, 2, 
                  function( x ) round( sum( is.na( x ) ) / length( x ) * 100, 2 ) ) )
  }
}

#' Complete a dataset after \code{\link[mice]{mice}} has been run.
#' 
#' Utility to define a non-repeated dataset after a missing imputation algorithm 
#' with the function \code{\link[mice]{mice}} has been run and the associated 
#' complete procedure through \code{\link[mice]{complete}} has been carried out.
#'
#' @param data A \code{data.table} or a \code{data.frame} (see 'Details').
#' @param data_key The keying variable in the \code{data.table}.
#' 
#' @details The function acts on numeric variables only and takes a column mean
#' by considering all the simulations detected given by \code{.imp}. This should
#' ensure consistent results as given by \code{\link[mice]{pool}}
#' When a \code{data.frame} is passed, \code{def_complete} internally
#' converts it to a \code{data.table} but still returns a \code{data.frame}.
#' 
#' @return Either a \code{data.table} or a \code{data.frame}.
#' 
#' @author Francesco Grossetti \email{francesco.grossetti@@gmail.com}.
#' @export
#' @importFrom data.table data.table
#' @importFrom data.table setkeyv
#' @importFrom data.table setDT
#' @importFrom data.table setDF

def_complete = function( data, data_key ) {

  if ( !inherits( data, "data.table" ) ) {
    message( "converting ", data, " to a data.table" )
    setDT( data )
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
  
  if ( !inherits( data, "data.table" ) ) {
    message( "converting output to a data.frame" )
    setDF( finale )
  }
  return( invisible( finale ) )
}



