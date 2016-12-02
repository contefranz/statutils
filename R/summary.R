#' Summary method for \code{\link{reduction}}.
#'
#' This function adds a method for \code{\link[base]{summary}} when the object is a list.
#' In particular, here the list corresponds to a bunch of information computed
#' through \code{\link[stats]{lm.fit}}.
#'
#' @param model bla bla
#' 
#' @details Bla bla
#' 
#' @return Bla bla
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

summary = function( model, ... ) {
  UseMethod( "summary", model )
}

#' @rdname summary
#' @aliases summary

summary.list = function( model, digits = NULL ) {
  
  if ( is.null( digits ) ) {
    digits = max( 3,  getOption( "digits" ) - 2 )
  } else {
    digits = digits
  }
  
  printCoefmat( model$coef, digits = digits )
  cat( "\n---------------------------------------------------------------\n" )
  cat( "Residual standard error:", format( signif( model$sigma, digits ) ), 
       "on", model$rdf, "degrees of freedom" )
  cat( "\n" )
  cat( "Multiple R-squared:", formatC( model$r.squared, digits = digits ) )
  cat( ", Adjusted R-squared:", formatC( model$adj.r.squared, digits = digits ),
       "\nF-statistic:", formatC( model$fstatistic[ 1L ], digits = digits ), "on",
       model$fstatistic[ 2L ], "and", model$fstatistic[ 3L ], "DF, p-value:",
       format.pval( pf( model$fstatistic[ 1L ], model$fstatistic[ 2L ],
                        model$fstatistic[ 3L ], lower.tail = FALSE ),
                    digits = digits ) )
  cat( "\n" )
  cat( "\n" )
  
} 
