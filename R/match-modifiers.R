# List of active modifiers, exact/default is not a match modifier, it resets ther others
# .match.modifiers <- c( 'ignore.case', 'fixed', 'perl' ) #, 'exact', 'default', 'partial' #
.match.modifiers <- c( 'ignore.case', 'fixed', 'perl', 'exact', 'default', 'partial', 'case.sensitive', 'case.insensitive', 'use.case' )


# Turn OFF attribute by toggling attribute to NULL if TRUE  
.off <- function( object, which ) { 

  for( wh in which ) { 
    
    attr <- object  %>% attr(wh)
    if( ! attr  %>% is.null && attr  %>% is.logical && attr )
      attr(object,wh) <- NULL
      
  }
  
  return(object)
}




# Toggle ON attribute 
.on <- function(object, which ) { 
  
  for( wh in which ) { 
    attr <- object  %>% attr(wh)
    if( ! attr  %>% is.null && ! attr  %>% is.logical ) warning( wh, " is exists and is not logical") 
    attr(object, wh) <- TRUE
  }
  return(object)
}



.is.on <- function(object, which)
  ! object %>% attr(which) %>% is.null && object %>% attr(which)
  


# Get active modifier(s) 
# @return list of match.modifiers or NULL if non-exist.
.get.modifiers <- function(object) {
  li <- attributes(object)[ .match.modifiers ] 
  li <- li[ ! sapply(li, is.null) ]
  
  # if( ! is.null(li) && length(li) == 0 ) return(NULL)
  return(li)
}
  

# Unmodifed do not have any .match.modifiers set.

# Identify whether a search (object and pattern) are unmodified.
.is.unmodified <- function( object, pattern )  {
  
  mods.object  <- .get.modifiers(object)
  mods.pattern <- .get.modifiers(pattern)
  
  ( is.null( mods.object)  || length(mods.object)  == 0 ) && 
  ( is.null( mods.pattern) || length(mods.pattern) == 0 ) 
  
}


# resolve modifiers applied to both object and patterns.
.collect.modifiers <- function( object, pattern ) { 
   ret <- pattern 
   
   obj.mods <- .get.modifiers(object)
   if( ! is.null(obj.mods) ) 

      
   # RESOLVE CONFLICTS ...
   # If there are any modifiers to pattern use those instead.
   pat.mods <- .get.modifiers(pattern)
   if( ! is.null(pat.mods) ) 
     attributes(ret) <- pat.mods else
     attributes(ret) <- obj.mods

   return(ret)
   
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
#' Modifiers applied to the search pattern take priority. \strong{If any match 
#' modifiers are set on the search pattern, these are used instead of those 
#' defined on the target.}
#' 
#' Modifiers \code{ignore.case}, \code{perl} and \code{fixed} follow closely the
#' implementations by the \strong{stringr} package. \code{exact} is added that 
#' reverts the match to R's default exact matching behavior. These are slightly 
#' enhanced from stringr version: See 
#' \url{https://github.com/hadley/stringr/issues/60} for details
#' 
#' 
#' @section reset, default:
#' 
#' Clears all match modifiers and performs a search as base R.
#' 
#' @section ignore.case: 
#' 
#' Match modifier \code{ignore.case} performs case insensitive matching against
#' the names of the target. \code{ignore.case} is incompatible and overrides  
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
#' @section partial, fixed:
#' 
#' Match modifier \code{fixed} performs a fixed string match instead of using a
#' regular expression. This can yield substantial speed ups, if regular 
#' expression matching is not needed.  Fixed mathcing requires that the search 
#' target \strong{contains} the search string. For exact matching of the search 
#' string see \code{exact}. 
#' 
#' \code{fixed} is incompatible with \code{perl}, \code{default},.  
#' Application of \code{fixed} overrides previous calls to the others. 
#' 
#' 
#' @section reset, default:
#' 
#' Match modifiers \code{exact} or \code{default} clear all modification and 
#' resets to R's normal matching against the target's names. 
#' 
#' See \url{https://github.com/hadley/stringr/issues/55}
#' 
#' 
#' @section full: 
#' 
#' Match modifier \code{full} requires the whole pattern to match as opposted to 
#' \code{partial} or fixed that match a substring.   
#' 
#' If existing match modifiers 
#' 
#' 
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
#' @rdname match_modifiers   
#' @export

perl <- function (object) {
  
    if( object  %>% .is.on('fixed') ) { 
       message("perl overriding fixed matching")
       object  %<>% .off('fixed')
    } 
    
    object %<>%  .on('perl')
    return(object)
}



#' @rdname match_modifiers 
#' @export
fixed <- function (object) {
  
  if( object  %>% .is.on('perl') ) { 
     message("fixed overriding perl matching")
     object %<>%  .off('perl')
  } 
    
  if( object  %>% .is.on('ignore.case') ) { 
    message("fixed overridding ignore.case")
    object %<>% .off('ignore.case')
  }
  
  if( object  %>% .is.on('full') ) { 
    message("fixed overridding partial")
    object %<>% partial
  }
  
  object %<>% .on('fixed')
  return(object)
  
}


#' @rdname match_modifiers 
#' @export
partial <- function(object) { 

  object  %<>% .off("full")
  object  %<>% .on("partial")
  
  return(object)

}



full <- function(object) { 
 
  object %<>% .off('partial')
  
  # THERE IS NO FIXED, FULL SINCE BY DEFIN
  # fixed <- attr(object, 'fixed')
  if( .is.on('fixed') ) { 
    message("full overriding fixed matching")
    object %<>% perl
  } 
  
  if( .is.on)
  object %<>%  .on('full')
  
  return(object)
  
}  



#' @rdname match_modifiers 
#' @export

ignore.case <- function (object) {
  object %<>%  .off('use.case')
  object %<>%  .on('ignore.case')
  return(object)
}

#' @rdname match_modifiers 
#' @export

case.insensitive <- ignore.case


#' @rdname match_modifiers 
#' @export

use.case <- function(object) { 
 object %<>% .off('ignore.case')
 object %<>% .on('use.case')
   
 return(object)
}


#' @rdname match_modifiers 
#' @export

case.sensitive <- use.case 
     
     

#' @rdname match_modifiers 
#' @export

reset <- function(object) { 
  attributes(object) <- NULL 
  return(object)
}


#' @rdname match_modifiers 
#' @export

default <- reset

