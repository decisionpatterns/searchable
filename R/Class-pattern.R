#' Defines a search pattern 
#' 
#' Defines how searches are conducted against a searchable target
#' 
#' @param object character or pattern; 
#' @param type character; the type of match: standard (default), regex, coll, 
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
#' @section standard:
#' 
#' The default is \code{standard} matching which performs matching 
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

# setClassUnion('CharacterOrNull', members = c('character','NULL'))

setClass( 
  'pattern'
  , representation( 'character', type = 'character', options='list') 
  , prototype( type='regex', type = 'standard', options=list() )
)



#  CONSTRUTOR
#  NB. compare with searchable
#' @rdname pattern
#' @export
  pattern <- function( object, type, ... ) UseMethod('pattern')


#' @rdname pattern
#' @export
  pattern.character <- function( object=NULL, type = NULL, ... ) { 
    
    if( is.null(type) ) type <- 'standard'
    opts <- list(...)
  
    new('pattern', object, type=type, options=opts ) %>% return   
  
  } 
  

#' @rdname pattern
#' @export
  pattern.pattern <- function( object, type, ... ) { 
    
    if( is.null(type)                &&       # type not supplied 
        length( list(...) ) == 0              # no ... 
    ) return(object)  
  
    # TYPE
    if( is.null(type) ) 
      if( object  %>% is('searchable') ) type <- object@type 
    
    opts <- list(...)
  
    new('pattern', object, type=type, options=opts ) # %>% return   
  
  }


#' @rdname pattern
#' @export
  pattern.searchable <- function( object, type, ... ) { 
  
    if( is.null(type)                &&       # type not supplied 
        length( list(...) ) == 0              # no ... 
    ) return(object)  
  
    # TYPE
    if( is.null(type) ) 
      if( object  %>% is('searchable') ) type <- object@type 
    
    opts <- list(...)
  
    new('pattern', object, type=type, options=opts ) # %>% return   
  
  }


#' @rdname pattern
#' @export
  pattern.default <- function( object=NULL, type = NULL, ... ) {
    pattern.character( as.character(object), type=type, ... ) 
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




#' @rdname pattern
#' @export

coll <- function( object, ... ) { 

  if( object %>% is('pattern') ) { 
    object@type = 'coll'
    object@options = stri_opts_collator(...)
    
  } else if( object  %>% is('searchable' ) ) { 
     object@pattern@type = 'coll'
     object@pattern@options = stri_opts_collator(...)
     
  } else { 
    object <- pattern(object, 'coll', ...) 
    
  }

  return(object)
  
}  



#' @rdname pattern
#' @export

fixed <- function( object, ... ) { 

  if( object %>% is('pattern') ) { 
    object@type = 'fixed'
    object@options = stri_opts_fixed(...)
    
  } else if( object  %>% is('searchable' ) ) { 
     object@pattern@type = 'fixed'
     object@pattern@options = stri_opts_fixed(...)
     
  } else { 
    object <- pattern(object, 'fixed', ...) 
    
  }

  return(object)
  
}  


#' @rdname pattern
#' @export

standard <- function( object, ... ) { 

  if( object %>% is('pattern') ) { 
    object@type = 'standard'
    object@options = list()
    
  } else if( object  %>% is('searchable' ) ) { 
     object@pattern@type = 'standard'
     object@pattern@options = list()
     
  } else { 
    object %<>%  pattern('standard', ...) 
    
  }

  return(object)

  
}


