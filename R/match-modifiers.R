# List of active modifiers, exact/default is not a match modifier, it resets ther others
.match.modifiers <- c( 'ignore.case', 'fixed', 'perl' ) #, 'exact', 'default', 'partial' 

# Get active modifier(s) 
# @return list of match.modifiers or NULL if non-exist.
.get.modifiers <- function(object) {
  li <- attributes(object)[ .match.modifiers ] 
  li <- li[ ! sapply(li, is.null) ]
  
  # if( ! is.null(li) && length(li) == 0 ) return(NULL)
  return(li)
}
  
#' Match modifiers 
#' 
#' Functions affecting how matching occurs. The modifiers can be applied to 
#' either the search pattern or the search target.  
#' 
#' @param object string or target object to apply match modifiers to
#' 
#' Match modifiers control how a search pattern is matched against the search 
#' target. They can be applied to either the pattern or target.  
#' Modifiers applied to the search pattern take priority. \strong{If any match modifiers 
#' are set on the search pattern, these are used instead of those defined on the 
#' target.}
#' 
#' Modifiers \code{ignore.case}, \code{perl} and \code{fixed} follow closely the
#' implementations by the \strong{stringr} package. \code{exact} is added that 
#' reverts the match to R's default exact matching behavior. These are slightly 
#' enhanced from stringr version: See 
#' \url{https://github.com/hadley/stringr/issues/60} for details
#' 
#' 
#' @section ignore.case: 
#' 
#' Match modifier \code{ignore.case} performs case insensitive matching against
#' the names of the target. \code{ignore.case} is incompatible with 
#' \code{fixed}. Application of \code{ignore.case} overrides previous calls to
#' \code{fixed}.
#' 
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
#' Match modifier \code{exact}, clears all match modifiers and resets the 
#' R's normal exact matching to the target's names. 
#' 
#' It is incompatible with all other match modifiers. Setting \code{exact} 
#' clears other match modifiers to \code{FALSE}.
#' 
#' See \url{https://github.com/hadley/stringr/issues/55}
#' 
#' @references 
#'   \url{https://github.com/hadley/stringr/issues/55} \cr
#'   \url{https://github.com/hadley/stringr/issues/60} \cr
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
    
    object <- structure( object, perl = TRUE, exact = NULL  )
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
    
    object <- structure( object, ignore.case = TRUE, exact = NULL   )
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
    
    object <- structure( object, fixed = TRUE, exact = NULL  )
    return(object)
}


#' @rdname match.modifiers 
#' @export
  partial <- function(object) fixed(object)



#' @rdname match.modifiers 
#' @export
  exact <- function(object) { 
    
    object <- structure( object, ignore.case = NULL, fixed = NULL, perl = NULL )
    return(object)
    
  }

# #' @rdname match.modifiers 
# #' @export
#   default <- function(object) exact(object)