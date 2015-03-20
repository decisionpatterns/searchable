# setClassUnion( "LogicalOrCharacter", c("logical","character") )
  setClassUnion( "searchables", c('vector', 'list') )
  modifiers <- c('ignore.case', 'fixed', 'perl', 'reverse.lookup' )

# TO make hash searchable as well, it seems a second Class Union is required.
# library(hash)
# setClassUnion( "searchables2", c('searchables','hash'))


#' searchable 
#' 
#' Creates a searchable class that allows for modification for searches.
#' 
#' @param x object to be marked as searchable
#' @param modifiers functions used to modify the search  
#' 
#' @slot modifiers zero or more function that are used to modify the search 
#' function before performing the search. 
#' 
#'  
# @slot ignore.case logical; whether case should be ignored
# @slot regex logical or character; if \code{TRUE}, R's regular expression 
# syntax is used; if \code{"perl"}, PCRE's are used. The default is to not use
# regular expressions. 
# 
#' The searchable class allows for non-standard, stringr-like searches when 
#' extracting (or replacing) on object based on the objects names. The following 
#' modification are (at present) supported: \code{fixed}, 
#' \code{ignore.case}, \code{perl} and 
#' \code{reverse.lookup}.
#' 
#' Search modification can be applied either to the object being searched or 
#' the search pattern(s). To apply them to the searched object add the modifiers
#' to the \code{modifiers} slot of the object. These are applied after any user
#' supplied modifiers.
#' 
#' \code{searchable} is designed to be minimally invase. No modification to the
#' object or its names are made when classing an object as searcable and if no 
#' search modifiers are enabled, the object behaves as a regular, 
#' "non-searchable" object.
#' 
#' When the target object search modifiers Search modifiers can be applied to either target object (e.g. \code{x} ) 
#' or to the name being sought.
#' name being searched for 
#' the object \code{x} can have a search modification    
#' wraps \code{x} in class \code{searchable} which have overloaded 
#' operators that allow better searching of names.
#' 
#' @return 
#'   By default, the extraction from a searchable objects is not a class 
#'   searchable. It is assumed that in most cases, the developer will not want 
#'   another searchable object.  
#'   
#' @note 
#'   - Environments cannot be (easily) be made "searchable" due to the way the 
#'   objects are implemented
#'   - The extraction methods for searchable objects are (at present) limited to
#'     only one pattern. This  
#'   
#' @seealso 
#'   \code{\link[stringr]{ignore.case}} \cr
#'   \code{\link[stringr]{perl}}        \cr
#'   \code{\link{reverse.lookup}}       \cr
#'   \code{\link[base]{Extract}}        \cr
#'   
#' @examples 
#' 
#'   # ATOMIC VECTORS: 
#'   v <- c( a=1, b=2, c=3, c2=4 )
#'   sv <- searchable(v)
#'   
#'   # EXTRACT:
#'     sv[["b"]]
#'     sv[[ ignore.case("B") ]] 
#'     sv[[ "B"  %>% ignore.case ]]
#'     
#'     sv[ c('a','b') ]
#'     sv[ perl('c.?') ]
#'     sv[ 'x' ]               # NA
#'     
#'     
#'   # WITH MARGRITTR:   
#'     "b" %>% sv[[.]]
#'     "B" %>% ignore.case %>% sv[[ . ]]
#'     "c." %>% perl %>% sv[[.]]
#'     
#'     "c.?" %>% perl %>% sv[.]
#'     
#'   
#'   # REPLACEMENT: 
#'     sv[['a']] <- "first"  
#'     sv[[ perl('^[c|d]$') ]] <- "third"
#'   
#'   # RECURSIVE LISTS:
#'   l <- list( a=1, b=2, c=3 )
#'   sl <- searchable(li)                
#'   sl[["b"]]
#'   sl[[ ignore.case("B") ]] 
#'   sl[[ "B"  %>% ignore.case ]]
#'   "b" %>% sl[[.]]
#'   "B" %>% ignore.case %>% sl[[ . ]]
#'   
#'   se <- searchable(e)                
#'   se[["b"]]
#'   se[[ ignore.case("B") ]] 
#'   se[[ "B"  %>% ignore.case ]]
#'   "b" %>% se[[.]]
#'   "B" %>% ignore.case %>% se[[ . ]]
#'   
#' @import stringr   
#' @rdname searchable
#' @export 


# CLASS: searchable

  searchable <- setClass( 'searchable' 
    , representation = representation( 'searchables', modifiers = 'list' ) # ignore.case = 'logical', regex = 'LogicalOrCharacter' )    # Fix 
    , prototype = prototype( vector(), modifiers = list()  ) 
    , contains = 'searchables'  #searchables2
  )


#' is.searchable 
#' @rdname searchable
#' @export 
  is.searchable <- function(x) is(x,'searchable')
  
  
# METHOD: show
#' @rdname searchable
#' @export 
  setMethod('show', 'searchable', 
    function(object) {
      cat( class(object), " ", class(object@.Data), ":\n", sep = "" )
      show(object@.Data)
      invisible(NULL)
    }
  )
  

  
# searchable( v, modifiers=c(ignore.case, perl, reverse.lookup) )
# sv <- v %>% searchable( modifiers = list(ignore.case, perl, reverse.lookup) )
# v %>% searchable( modifiers = list(ignore.case, perl, reverse.lookup ))

  
# EXTRACT 
#' @note extraction results in 

  setMethod( '[[', c(x='searchable', i='character'), 
     function(x,i) { 
       
     # ESCAPE HATCH
     # No modifiers specified? Do the normal thing.
       if( length(x@modifiers) == 0 && is.null( attributes(i) ) ) 
         return( x@.Data[[i]] )
         
       if( length(i) != 1 ) 
          stop("pattern string is not a one-element character vector")
         
     # ADD DEFAULT MODIFIERS
       for( mod in x@modifiers ) i <- mod(i)
       
     # APPLY REVERSE LOOKUP BY INVERTING x
       if( ! is.null( attr(x, "reverse.lookup" ) ) &&
           attr(x, "reverse.lookup") == TRUE 
       ) x <- invert(x)
       
     # 
       wh <- which( str_detect( string=names(x), pattern=i ))
       if( length(wh) > 1 ) stop('attempt to select more than one element')
       if( length(wh) < 1 ) stop('subscript not found')
       x[[wh]]
       
     }   
  )

  
  setMethod( '[', c(x='searchable', i='character', j='missing'), 
    function(x,i,j,...) {
      
     # ESCAPE HATCH
     # No modifiers specified? Do the normal thing.
       if( length(x@modifiers) == 0 && is.null( attributes(i) ) ) 
         return( x@.Data[i] )      

     # 
       if( ! is.string(i) ) 
         stop( "When search patterns are used on searchable objects, ",  
               "only one pattern is allowed."
         )   
     
     # ADD DEFAULT MODIFIERS
       for( mod in x@modifiers ) i <- mod(i)   
     
     # APPLY REVERSE LOOKUP BY INVERTING x
       if( ! is.null( attr(x, "reverse.lookup" ) ) &&
           attr(x, "reverse.lookup") == TRUE 
       ) x <- invert(x)
     
      wh <- which( str_detect( string=names(x), pattern=i ))
      x[wh]
     
    }           
  )
  
 

# ----------------------------------------------------------------
# REPLACE  
  
#' @param x searchable object
#' @param i pattern with modifiers
#' @param j missing; never spacified
#' @param value; set value 
  
setReplaceMethod( '[[', c(x="searchable", i="character", j="missing", value="ANY") ,
  function(x,i,value) {
    
     wh <- which( str_detect( string=names(x), pattern=i ))
       if( length(wh) > 1 ) stop('attempt to select more than one element')
       if( length(wh) < 1 ) stop('subscript not found')
     
     x@.Data[[wh]] <- value
     
     return(x)
  }
)


# sv[["a"]] <- "herman"  
# sv
#' sv <- searchable(v)
#' sv <- searchable(v, ignore.case=TRUE )

# setMethod( '[[<-', c('searchable', 'character', 'ANY'),
#     function(x,i,value) "hw"
# )
  
# setReplaceMethod( '[[', c(x='searchable', 'character', 'ANY'),
#     function(x,i,value) "hw"
# )  

  
  

  
  
  
  

# ---------------------------------------------------------------------------
# Appendix:
#
# #' OPTION 1: Use S3 classes and S4 methods
# 
# searchable <- function( x, ... ) { 
#   
#   if( is.null(attr(x, 'names')) ) 
#     stop( "Only objects with a 'names' attribute can be made searchable.") 
#     
#   class(x) <- unique( c( "searchable", class(x) ) )
#   
#   # Add how specification 
#   
#   return(x)
#   
# }
# 
# 
# 
# sv <- v
# class(sv) <- c('searchable','integer')
# setOldClass( 'searchable')
# setMethod( '[[', c(x='searchable', i='character'), function(x,i) "works" )
# sv[["a"]]  # Fails
# 
# 
# #'  OPTION 2: Use S4 classes with contains .... 
# TODO: get modifiers from ... by defining initialize method
# setMethod( 
#     'initialize'
#   , 'searchable'
#   , function( .Object, ... ) {
#       browser()
#       .Object %>% class %>% message
#       .Object@.Data = list(...)[[1]]
#       .Object
#   }
#     
# )