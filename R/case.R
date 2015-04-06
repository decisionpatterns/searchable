#' Turn on/off case sensitivity for searchable targets and patterns 
#' 
#' Functions for affecting the case sensitivity of matching.  
#' 
#' @param object search pattern or target
#' @param ... additional arguments
#' 
#' \code{ignore.case} and \code{use.case} control the case sensitivity of the 
#' matching
#' 
#' The default is to preform case sensitive matching. 
#' 
#' @seealso 
#'   \code{stri_detect_*} from the \code{stringi} package
#'   
#' @examples 
#'   "pattern" %>% use.case 
#'   "pattern" %>% ignore.case 
#'   
#' @aliases ignore.case use.case

#' @rdname case
#' @export

  ignore.case <- function(object) UseMethod('ignore.case')

#' @rdname case
#' @export
  ignore.case.SearchableOrPattern <- function(object, ...) { 
    object@options$case_insensitive = TRUE 
    return(object)
  }

#' @rdname case
#' @export
  ignore.case.character <- function(object) object %>% std( case_insensitive = TRUE ) 
  
#' @rdname case
#' @export
  ignore.case.default <- function(object) object %>% as.character %>% ignore.case 
  


# --------------------------------------------------------------

#' @rdname case
#' @export
  use.case <- function(object) UseMethod('use.case')

#' @rdname case
#' @export
  use.case.SearchableOrPattern <- function(object) { 
    object@options$case_insensitive = FALSE 
    return(object)
  }

#' @rdname case
#' @export
  use.case.character <- function(object) object %>% std( case_insensitive = FALSE )
  
#' @rdname case
#' @export
  use.case.default <- function(object) object %>% as.character %>% use.case 
  

