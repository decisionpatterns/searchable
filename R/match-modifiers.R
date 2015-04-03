#' Match modifiers 
#' 
#' Functions affecting how matching occurs. The modifiers can be applied to 
#' either the search pattern or the search target.  
#' 
#' @param object string or target object to apply match modifiers to
#' @param ... additional arguments passed to \code{stri_opts_*}.
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
#' Clears all match attributes and performa a name search as base R.
#' 
#' @section regex, fixed, coll: 
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
#'   \code{\link{boundary}} for setting boundary matching \cr
#'   \code{\link{case}} for setting case (in)sensitive matching \cr
#'   \code{\link{extract}} \cr
#'   \code{\link[stringr]{fixed}} \cr
#'   \code{\link[stringr]{ignore.case}} \cr
#'   \code{\link[stringr]{perl}} \cr
#'   
#' @examples 
#'   "pattern" %>% regex %>% reset

# @rdname match_modifiers
# @export


# regex <- function( object, ... ) UseMethod('regex')
# 
# 
# regex.pattern <- function(object, ... ) { 
#   object@type
# 
# 
# }
#   
#   
# regex <- function( object, ... ) {
#   opts(object) <- stringi::stri_opts_regex(...)
#   type(object) <- "fixed"  
#   return(object)
# }


# #' @rdname match_modifiers
# #' @export
#  
# fixed <- function( object, ... ) { 
#   opts(object) <- stringi::stri_opts_fixed(...)
#   type(object) <- "fixed"  
#   return(object)
# }



# #' @rdname match_modifiers
# #' @export
#  
# coll <- function( object, ... ) { 
#   opts(object) <- stringi::stri_opts_collator(...)
#   type(object) <- "coll"  
#   return(object)
# }



# #' @rdname match_modifiers
# #' @export
# 
# reset <- function(object) { 
#   attributes(object) <- NULL 
#   return(object)
# }
# 
# 
# #' @rdname match_modifiers 
# #' @export
#    default <- reset
# 
# #' @rdname match_modifiers 
# #' @export
#    base <- reset 



# # GET AND SET TYPE OF SEARCH 
# type <- function(object) attr( object, 'type' ) 
# 
# `type<-` <- function( object, value = c('regex','coll','fixed') ) { 
#   attr(object,'type') <- match.arg(value)
#   object
# }

# # GET AND SET OPTIONS
# opts <- function(object) attr( object, 'opts' ) 
# 
# `opts<-` <- function( object, value ) { 
#   attr(object,'opts') <- value
#   return(object)
# }

