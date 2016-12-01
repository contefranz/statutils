# SUMMARY OF OLS WITH LM.FIT() --------------------------------------------
# Author: Francesco Grossetti
# email: francesco.grossetti@gmail.com
# version: 1.0
# date: 12/01/2016
# DESCRIPTION
# This function adds a method for summary() in case the object is a list.
# In this particular case, the list corresponds to a bunch of information
# computed through the lm.fit() function. 


summary = function( model, ... ) {
  UseMethod( "summary", model )
}

summary.list = function( model, digits = NULL ) {
  
  if ( is.null( digits ) ) {
    digits = max( 3,  getOption( "digits" ) - 2 )
  } else {
    digits = digits
  }
  
  printCoefmat( model$coef, digits = digits )
  cat( "\nResidual standard error:", format( signif( model$sigma, digits ) ), 
       "on", rdf, "degrees of freedom" )
  cat( "\n" )
  cat( "Multiple R-squared: ", formatC( model$r.squared, digits = digits ) )
  cat( ",\tAdjusted R-squared: ", formatC( model$adj.r.squared, digits = digits ),
       "\nF-statistic:", formatC( model$fstatistic[ 1L ], digits = digits ), "on",
       model$fstatistic[ 2L ], "and", model$fstatistic[ 3L ], "DF,  p-value:",
       format.pval( pf( model$fstatistic[ 1L ], model$fstatistic[ 2L ],
                        model$fstatistic[ 3L ], lower.tail = FALSE ),
                    digits = digits ) )
  cat( "\n" )
  cat( "\n" )
  
} 
