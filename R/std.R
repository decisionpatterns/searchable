#' Use/revert to standard matching
#' 
#' Creates or modifies the search type to use default R matching 
#' 
#' @param object to make specification 
#' @param ... additional arguments passes to \code{\link{pattern}}
#' 
#' \code{std} causes match behavior to revert to R's default. It is a non-op
#' stringr-style match modifier that indicates that no special matching 
#' behavior should be used. 
#' 
#' The function mainly exist to revert clear previos modifiers. 
#' 
#' @seealso 
#'   \code{\link{pattern}}
#' 
#' @examples
#'   ptrn <- std("a") 
#'   detect( c('alpha','beta'), ptrn )
#'   

#' @rdname std
#' @export
   std <- function( object, ... )  UseMethod('std')
  

#' @rdname std
#' @export
   std.default  <- function( object, ... ) pattern( as.character(object), 'std', ... )


#' @rdname std
#' @export
   std.character  <- function( object, ... ) pattern( object, 'std', ... )


#' @rdname std
#' @export
#  Note: an alternative is to clobber the options
   std.SearchableOrPattern  <- function( object, ... ) {
     object@type = 'std'
     object@options = list(...)
     return(object)
   }
