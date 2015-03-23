.match.modifiers <- c( 'ignore.case', 'fixed', 'perl', 'exact', 'partial' )

#' Match modifiers 
#' 
#' Functions affecting how matching occurs. The modifiers can be applied to 
#' either the 
#' 
#' @param object string or target object to apply match modifiers to
#' 
#' Match modifiers control how matching is performed against a search target. 
#' They can be applied to either the search target or search string 
#' Modifiers to the search string take priority.   
#' 
#' 
#' @section exact:
#' 
#' Match modifier \code{exact}, set matches to performs an exact match against 
#' the target's object names. This is the default behavior of base. It is 
#' incompatible with all other match modifiers. Setting \code{exact} sets the 
#' other match modifiers to \code{FALSE}.
#' 
#' @section partial: 
#' 
#' This is a synonym for \code{\link[stringr]{fixed}}
#' 
#' @seealso 
#'   \code{\link{extract}} \cr
#'   \code{\link[stringr]{fixed}} \cr
#'   \code{\link[stringr]{ignore.case}} \cr
#'   \code{\link[stringr]{perl}} \cr
#'   
#' @examples 
#'   exact( "string" )
#'       
#' @rdname match.modifiers   
#' @export

  exact <- function(object) { 
    attr( object, 'exact' ) <- TRUE
    return(object)
  }
  
#' @rdname match.modifiers 
#' @export
  partial <- function(object) stringr::fixed(object)
