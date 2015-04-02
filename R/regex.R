#' regex 
#' 
#' Create a pattern for regular expression matching 
#' 
#' @param object to make specification 
#' @param ... additional arguments passes to \code{\link{pattern}}
#' 
#' \code{regex}
#' 
#' @seealso 
#'   \code{\link{pattern}}
#' 
#' @examples
#'   # -tk       
#'

#' @rdname regex
#' @export
   regex <- function( object, ... )  UseMethod('regex')
  

#' @rdname regex
#' @export
   regex.default  <- function( object, ... ) pattern( as.character(object), 'regex', ... )


#' @rdname regex
#' @export
   regex.character  <- function( object, ... ) pattern( object, 'regex', ... )


#' @rdname regex
#' @export
#  Note: an alternative is to clobber the options
   regex.pattern  <- function( object, ... ) {
     object@type = 'regex'
     object@options = list(...)
     return(object)
   }


#' @rdname regex
#' @export
   regex.searchable  <- function( object, ... ) {
     object@pattern = regex( object@pattern, ... ) 
     return(object)
   }  
    


# regex <- function( object, ... ) {
#     
#   if( object %>% is('pattern') ) { 
#     object@type = 'regex'
#     object@options = stri_opts_regex(...)
#     
#   } else if( object  %>% is('searchable' ) ) { 
#      object@pattern@type = 'regex'
#      object@pattern@options = stri_opts_regex(...)
#      
#   } else { 
#     object <- pattern(object, 'regex', ...) 
#     
#   }
# 
#   return(object)
#   
# }  
