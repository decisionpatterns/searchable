.match.modifiers <- c( 'ignore.case', 'fixed', 'perl', 'exact', 'partial' )

#' Match modifiers 
#' 
#' Functions affecting how matching occurs. The modifiers can be applied to 
#' either the search pattern or the search target.  
#' 
#' @param object string or target object to apply match modifiers to
#' 
#' Match modifiers control how a search pattern is matched against the search 
#' target. They can be applied to either the pattern or target. Generally, 
#' modifers applied to the target are applied when the object is created. 
#' Modifiers applied to the search string take priority, are applied after and 
#' override those of applied to the target.   
#' 
#' Modifiers \code{ignore.case}, \code{perl} and \code{fixed} follow closely the
#' implementations by the \strong{stringr} package.
#' 
#' 
#' @section ignore.case: 
#' 
#' Match modifier \code{ignore.case} performs case insensitive matching against
#' the names of the target. \code{ignore.case} is incompatible with 
#' \code{fixed}. Application of \code{ignore.case} overrides previous calls to
#' \code{fixed}.
#' 
#' @section perl: 
#' 
#' Match modifier \code{perl} performs matches against the targets names using
#' PCREs. \code{perl} is incompatible with 
#' \code{fixed}. Application of \code{perl} overrides previous calls to 
#' \code{fixed}.
#' 
#' 
#' @section fixed:
#' 
#' Match modifier \code{fixed} performs a fixed string match instead of using a
#' regular expression. This can yield substantial speed ups, if regular 
#' expression matching is not needed.  Fixed mathcing requires that the search 
#' target \strong{contains} the search string. For exact matching of the search 
#' string see \code{exact}. 
#' 
#' \code{fixed} is incompatible with 
#' \code{perl} and \code{ignore.case}.  Application of \code{fixed} overrides 
#' previous calls to the others. 
#' 
#' 
#' @section partial: 
#' 
#' This is a synonym for \code{\link[stringr]{fixed}}
#' 
#' 
#' @section exact:
#' 
#' Match modifier \code{exact}, set matches to performs an exact match against 
#' the target's object names. This is the default behavior of base. It is 
#' incompatible with all other match modifiers. Setting \code{exact} sets the 
#' other match modifiers to \code{FALSE}.
#' 
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

perl <- function (object) {
  
    fixed <- attr(object, 'fixed')
    if( ! is.null(fixed) && fixed ) { 
       message("perl overriding fixed matching")
       object <- structure( object, fixed = FALSE )
    } 
    
    object <- structure( object, perl = TRUE  )
    return(object)
}



#' @rdname match.modifiers 
#' @export
ignore.case <- function (object) {
  
    fixed <- attr(object, 'fixed')
    if( ! is.null(fixed) && fixed ) { 
       message("ignore.case overriding fixed matching")
       object <- structure( object, fixed = FALSE )
    } 
    
    object <- structure( object, ignore.case = TRUE  )
    return(object)
}


#' @rdname match.modifiers 
#' @export
fixed <- function (object) {
  
    perl <- attr(object, 'perl')
    if( ! is.null(perl) && perl ) { 
       message("fixed overriding perl matching")
       object <- structure( object, perl = FALSE )
    } 
    
    ignore.case <- attr(object, 'ignore.case')
    if( ! is.null(ignore.case) && ignore.case ) { 
       message("fixed overridding ignore.case matching")
       object <- structure( object, ignore.case = FALSE )
    }
    
    object <- structure( object, fixed = TRUE  )
    return(object)
}


#' @rdname match.modifiers 
#' @export
  partial <- function(object) fixed(object)



#' @rdname match.modifiers 
#' @export
  exact <- function(object) { 
    
    object <- structure( object, 'exact' ) <- TRUE
    return(object)
  }
  


