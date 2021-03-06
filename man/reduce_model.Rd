% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reduce_model.R
\name{reduce_model}
\alias{reduce_model}
\alias{reduce_model.lm}
\alias{reduce_model}
\title{Reduce a linear model}
\usage{
reduce_model(model, ...)

\method{reduce_model}{lm}(model, data, alpha = 0.05, intercept = TRUE)
}
\arguments{
\item{model}{An object of class "lm".}

\item{data}{A \code{data.frame} or \code{data.table} with all components
of \code{model}.}

\item{alpha}{The significance level at which to cut off predictors. Default is 5\%.}

\item{intercept}{Specify whether the reduction procedure has to consider an 
intercept or not. Default is \code{TRUE}.}
}
\value{
\code{reduce_model} returns an object of class "lm" with the same
components (see \code{\link[stats]{lm}} for a complete list).
}
\description{
The procedure takes a linear model of class \code{\link[stats]{lm}} 
(including the output from a stepwise procedure given by 
\code{\link[stats]{step}}) and reduces it by iteratively deleting predictors 
based on pvalues of the t-tests (see 'Details').
}
\details{
The procedure makes use of the workhorse function of \code{\link[stats]{lm}}: 
\code{\link[stats]{lm.fit}}. This allows to explicitly specify the design matrix which enables
to split main effects from interactions. In particular, conversely to what the 
function \code{\link[stats]{lm}} currently does, it allows the inclusion of just 
high order effects. For instance, it can fit just interaction terms given by 
x1*x2 or x1:x2 passed with the usual \code{R} formula notation.
}
\examples{
data_factor = mtcars
data_factor$am = as.factor( data_factor$am )
levels( data_factor$am ) = c( "no", "yes" )

complete_model = lm( hp ~ qsec + cyl + mpg + disp + drat + wt + qsec + vs +
                     am + gear + carb + am:qsec, data = data_factor )
summary( complete_model )

reduced_model = reduce_model( complete_model, data = data_factors )
summary( reduced_model )

}
\seealso{
\code{\link[stats]{lm.fit}} \code{\link[stats]{lm}} \code{\link[stats]{step}}
}
\author{
Francesco Grossetti \email{francesco.grossetti@gmail.com}.
}
