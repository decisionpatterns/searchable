# setClassUnion( "LogicalOrCharacter", c("logical","character") )
  setClassUnion( "searchables", c('vector', 'list') )
  
  # TO make hash searchable as well, it seems a second Class Union is required.
  # library(hash)
  # setClassUnion( "searchables2", c('searchables','hash'))


#' Superclass of searchable and pattern
#' 
#' Defines a superclas of classes \code{\link{searchable}} and 
#' \code{\link{pattern}}  
#' 
#' @slot type character; type of search performed; one of "std" (default), 
#'       "regex", "fixed", "coll", or "charclass". See details. 
#' @slot options list; name = value pairs for search options used.
#'
#' The \code{SearchaleOrPattern} class defines the attributes common to both 
#' searchables and patterns: \code{type} and \code{...}. Mainly this is just a
#' bookkeeping convention to simplify writing methods that change search 
#' behavior, e.g. \code{regex}. This allows writing a single functions that 
#' changes the search behavior at either the search target or search pattern or 
#' \code{pattern}
#' 
#' The constructor function is not meant to be used directly and is not 
#' exported.
#' 
#' @seealso 
#'   \code{\link{searchable}} \cr
#'   \code{\link{pattern}} 
#'   
#' @rdname SearchableOrPattern
#' @exportClass pattern
#' @export

   SearchableOrPattern <- setClass( 'SearchableOrPattern' 
     , representation = representation( 'searchables', type='character', options='list')  
     , prototype( type = 'std', options=list() ) # , ignore.case = FALSE, perl = FALSE, fixed = FALSE ) 
     , contains = 'searchables'  
   )

