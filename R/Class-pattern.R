setClassUnion( 'PatternOrCharacter', c('pattern','character'))

#' Defines or extract a search pattern 
#' 
#' Patterns defines how searches are conducted against a searchable target
#' 
#' @slot type character; type of search performed; one of "std" (default), "regex", 
#'      "fixed", "coll", or "charclass". See details. 
#' @slot options list; name = value pairs for search options used.
#' 
#' @param object character or pattern; 
#' @param type character; the type of match: std (default), regex, coll, 
#'        fixed.
#' @param .... additional arguments to be passed to \code{stri_opts_*} functions. 
#'        See details.
#' 
#' 
#' The \strong{pattern} class defines how the search is conducted.
#' 
#' The function \code{pattern} is the constructor for the class. It takes a 
#' \code{'string'} can be used to define a pattern that controls matching 
#' against a searchable target. Most often the user will want to use the type 
#' specific functions: \code{regex}, \code{coll}, \code{fixed} or \code{basic}. 
#' Eacb is described below.
#' 
#' These are closely related to the 
#'
#' @section std:
#' 
#' The default is \code{std} matching which performs matching 
#' as base R would. This is equivalent to \code{fixed} and 
#' \code{case_insensitive = FALSE}. Though the internal matching is sed.
#'   
#' @section regex:
#' 
#' \code{regex} matching takes a regular expression for matching using the 
#' \code{stri_*_regex} functions. 
#' 
#' @section coll:
#' ...
#' 
#' @section fixed:
#' ...
#' 
#' @examples
#' 
#'   pattern('hello')
#'   pattern('hello', type="regex", boundary="starts_with", )
#' 
#' 
#' @rdname pattern
#' @exportClass pattern
#' @export

   setClass( 'SearchableOrPattern' 
     , representation = representation( 'searchables', type='character', options='list')  
     , prototype( type = 'std', options=list() ) # , ignore.case = FALSE, perl = FALSE, fixed = FALSE ) 
     , contains = 'searchables'  
   )

#' @rdname pattern
#' @exportClass pattern
#' @export

setClass( 
  'pattern'
  # , representation( 'SearchableOrPattern', type = 'character', options='list') 
  # , prototype( type='regex', type = 'std', options=list() )
  , contains = 'SearchableOrPattern'
)



#  CONSTRUTOR
#  NB. compare with searchable
#' @rdname pattern
#' @export
  pattern <- function( object, type, ... ) UseMethod('pattern')


#' @rdname pattern
#' @export
  pattern.default <- function( object=NULL, type = 'std', ... ) {
    pattern.character( as.character(object), type=type, ... ) 
  }


#' @rdname pattern
#' @export
  pattern.character <- function( object, type = 'std', ... ) {
     if( object %>% is('SearchableOrPattern' ) ) NextMethod('pattern')
     new('pattern', object, type=type, options=list(...) ) # %>% return   
  }
  
#' @rdname pattern
#' @export
  pattern.pattern <- function( object, type = object@type, ... ) { 
  
    if( missing(type)                &&        # type not supplied 
        length( list(...) ) == 0               # no ... 
    ) return( object %>%  pattern )  
  
    new('pattern', object, type=type, options=list(...) ) # %>% return   
  
  }


#' @rdname pattern
#' @export
  pattern.searchable <- function( object, type = object@type, ... ) { 
    new('pattern', NA_character_, type=type, options=if( missing(...) ) object@options else list(...) )  # %>% return   
  }




#' @rdname pattern
#' @export

setMethod('show', 'pattern',
          
  function(object) {
    
    object  %>% .describe_pattern %>% cat
    show( object@.Data )  
    
    invisible(NULL)
    
  }
  
)


.describe_pattern <- function(object) { 
  
    msg <- character() 
    if( length(object) < 2 ) msg %<>% append(  "a " )
    
    if( ! is.null(object@options$case_insensitive) && 
          object@options$case_insensitive 
    ) msg  %<>% append( 'case-insensitive, ')
    
    msg %<>% append( c("'",  object@type, "' matching ") )  
    msg %<>% append( 'pattern ' )
    msg %<>% append( ":\n")
  
    msg %>% paste0( collapse = '' )  %>% return 
      
}


# #' @rdname pattern
# #' @export
# 
# coll <- function( object, ... ) { 
# 
#   if( object %>% is('pattern') ) { 
#     object@type = 'coll'
#     object@options = stri_opts_collator(...)
#     
#   } else if( object  %>% is('searchable' ) ) { 
#      object@type = 'coll'
#      object@options = stri_opts_collator(...)
#      
#   } else { 
#     object <- pattern(object, 'coll', ...) 
#     
#   }
# 
#   return(object)
#   
# }  
# 
# 
# 
# #' @rdname pattern
# #' @export
# 
# fixed <- function( object, ... ) { 
# 
#   if( object %>% is('pattern') ) { 
#     object@type = 'fixed'
#     object@options = stri_opts_fixed(...)
#     
#   } else if( object  %>% is('searchable' ) ) { 
#      object@type = 'fixed'
#      object@options = stri_opts_fixed(...)
#      
#   } else { 
#     object <- pattern(object, 'fixed', ...) 
#     
#   }
# 
#   return(object)
#   
# }  

