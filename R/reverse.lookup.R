#' Perform a reverse lookup on searchables 
#' 
#' This function causes the pattern search to be performed against an object's
#' values instead of its names
#' 
#' @param string pattern for which to match against an objects values
#' 
#' \code{reverse.lookup} sets and toggles the logical attribute with name 
#' \code{reverse.lookup}. Actual implementation of the reverse lookup is 
#' performed in the Extract methods. 
#' 
#' 
#' In order to perform a reverse lookup, values must be converted to character 
#' names.
#' 
#' @return 
#'   a string object with the \code{reversse.lookup} attribute set. 
#' 
#' @seealso 
#'   The \code{invert} function in the \code{hash} package \cr
#'   
#' @examples 
#'   reverse.lookup("string")
#'   
#' @note
#'   What happens if there are two reverse.lookups
#' @rdname reverse.lookup
#' @export 

reverse.lookup <- 
  function(string) {
    orig <- attr(string, 'reverse.lookup', exact=TRUE )
    if( is.null(orig) )
      structure( string, reverse.lookup = TRUE ) else 
      structure( string, reverse.lookup = ! orig ) 
  }   
