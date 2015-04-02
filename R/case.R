#' Turn on/off case sensitivity for searchable targets and patterns 
#' 
#' Functions for affecting the case sensitivity of matching.  
#' 
#' @param object search pattern or target
#' 
#' \code{ignore.case} and \code{use.case} control the case sensitivity of the 
#' matching
#' 
#' The default is to preform case sensitive matching. 
#' 
#' @seealso 
#'   \code{\link{match.modifiers}} \cr 
#'   
#' @examples 
#'   "pattern" %>% use.case 
#'   "pattern" %>% ignore.case 


#' @rdname case
#' @export
  ignore.case <- function(object) UseMethod('ignore.case')

#' @rdname case
#' @export
  ignore.case.pattern <- function(object) { 
    object@options$case_insensitivity = TRUE 
    return(object)
  }

#' @rdname case
#' @export
  ignore.case.searchable <- function(object) { 
    object@pattern@options$case_insensitivity = TRUE 
    return(object)
  }

#' @rdname case
#' @export
  ignore.case.character <- function(object) object %>% standard() 
  
#' @rdname case
#' @export
  ignore.case.default <- function(object) object %>% as.character %>% ignore.case 
  



#' @rdname case
#' @export
  use.case <- function(object) UseMethod('use.case')

#' @rdname case
#' @export
  use.case.pattern <- function(object) { 
    object@options$case_insensitivity = FALSE 
    return(object)
  }

#' @rdname case
#' @export
  use.case.searchable <- function(object) { 
    object@pattern@options$case_insensitivity = FALSE 
    return(object)
  }

#' @rdname case
#' @export
  use.case.character <- function(object) object %>% standard() 
  
#' @rdname case
#' @export
  use.case.default <- function(object) object %>% as.character %>% use.case 
  

