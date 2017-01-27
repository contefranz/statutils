#' @importFrom utils packageVersion
.onAttach = function( libname, pkgname ) {
  # Runs when attached to search() path such as by library() or require()
  if ( interactive() ) {
    packageStartupMessage( 'Welcome to statutils v', as.character( packageVersion( "statutils" ) ), "!" )
    packageStartupMessage( 'If you find bugs, please report them at https://github.com/contefranz/statutils/issues' )
  }
}
