#' Use standard matching 
#' 
#' Functions for matching using standard, defaut matching 
#' 
#' @param str search target
#' @param pattern pattern to attempt
#' @param ... supplementary arguments passed to the underlying functions, 
#'        including additional settings for \code{stri_opts_standard}
#' @param case_insensitive logical; enable simple case insensitive matching
#'
#' \code{stri_detect_standard} is equivalent to \code{str %in% pattern} and is 
#' created to provide a parallel to other search methods.
#'  
#' \code{stri_opts_standard}
#' @return 
#'   logical indicating the matching elements in \code{str}
#'  
#' @seealso 
#'   \code{\link[stringi]{stri_detect}}
#' 
#' @examples 
#'   stri_detect_standard( letters[1:5], letters[1:2] )
#'   stri_detect_standard( letters[1:5], LETTERS[1:2] )
#'   stri_detect_standard( letters[1:5], LETTERS[1:2], opts_standard = list(case_insensitive = TRUE ) )
#'   
#' @rdname stri_standard            
#' @export

stri_detect_standard <- function( str, pattern, ..., opts_standard = NULL ) {
 
  if( ! missing(...) )
    opts_standard = as.list(opts_standards, ... ) 
  
  if( ! is.null(opts_standard$case_insensitive) && opts_standard$case_insensitive ) 
    toupper(str) %in% toupper(pattern) else 
    str %in% pattern 
  
}
   
  

#' @rdname stri_standard            
#' @export
stri_opts_standard <- function( case_insensitive = FALSE, ...) { 

  opts <- list(...)
  if ( !missing(case_insensitive) ) opts["case_insensitive"] <- case_insensitive
  opts

}
  