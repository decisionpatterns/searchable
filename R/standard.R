#' standard 
#' 
#' Creates or modifies the search type to use default R matching 
#' 
#' @param object to make specification 
#' @param ... additional arguments passes to \code{\link{pattern}}
#' 
#' \code{standard}
#' 
#' @seealso 
#'   \code{\link{pattern}}
#' 
#' @examples
#'   pat <- standard("a") 
#'   detect( c('alpha','beta'), pat )
#'   

#' @rdname standard
#' @export
   standard <- function( object, ... )  UseMethod('standard')
  

#' @rdname standard
#' @export
   standard.default  <- function( object, ... ) pattern( as.character(object), 'standard', ... )


#' @rdname standard
#' @export
   standard.character  <- function( object, ... ) pattern( object, 'standard', ... )


#' @rdname standard
#' @export
#  Note: an alternative is to clobber the options
   standard.SearchableOrPattern  <- function( object, ... ) {
     object@type = 'standard'
     object@options = list(...)
     return(object)
   }
