# setClassUnion( "LogicalOrCharacter", c("logical","character") )
  setClassUnion( "searchables", c('vector', 'list') )

# A searchable object is a vector or list and has a names attribute  
  is.searchables <- function(object) 
    ( is.vector(object) || is.list(object) ) && ! is.null( attr(object, "names"))
  
#   validSearchableObject <- function(object) { 
#     
#     if( object@perl = TRUE && object)
#   
#     return(TRUE)
#   }
  
  
  # TO make hash searchable as well, it seems a second Class Union is required.
  # library(hash)
  # setClassUnion( "searchables2", c('searchables','hash'))

#' @title searchable 
#' 
#' @description
#' Creates a searchable class that allows for modification for searches.
#'
# @slot modifiers zero or more function that are used to modify the search 
# function before performing the search. 
#  
# @param x,object object to be made or searchable
# @param modifiers functions used to modify the search  
#'  
#' @param object searchable object or object to be made searchable
#' @param ... one or more stringr-style match modifying sunctions, See 
#'   \code{?match.modifiers} for details.
#'  
# @slot ignore.case logical; whether case should be ignored
# @slot regex logical or character; if \code{TRUE}, R's regular expression 
# syntax is used; if \code{"perl"}, PCRE's are used. The default is to not use
# regular expressions. 
# 
#' @details 
#' 
#' The searchable class allows for non-standard, stringr-like searches when 
#' extracting (or replacing) objects. The following 
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
#' 
#' @section reverse.lookup:
#' 
#' When performing a reverse lookup, values (not names) are searched. The 
#' corresponding names are returned.  NOTE: this is highly experimental and only
#' works for atomic vectors. It is uncertain how this might be applied to 
#' recursive structures. 
#' 
#' @section replacement:
#' 
#' \code{searchable} can be used to replace objects as well. See \code{?extract} 
#' for additional exemples.
#' 
#' @section multiple dimension objects:
#' 
#' Multiple dimension ojects such as data.frames, data.tables, matrices and 
#' arrays are not supported at this time.
#' 
#' @section Adding new modifiers:
#' 
#' It is possible to add additional search modifiers ... (vignette)  
#' 
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
#'   \code{\link{extract}}              \cr 
#'   \code{\link[stringr]{ignore.case}} \cr
#'   \code{\link[stringr]{perl}}        \cr
#'   \code{\link{reverse.lookup}}       \cr
#' 
#'     
#' @examples 
#' 
#'   # ATOMIC VECTORS: 
#'     v <- c( a=1, b=2, B=3, c=4, c2=5 )
#'     sv <- searchable(v)
#'  
#'   # EXTRACT:
#'     sv$a
#'      
#'     sv[['a']]
#'     sv[[ ignore.case('A') ]]
#'     
#'     sv[ ignore.case('b') ]     
#'     sv[ perl('c') ]
#'     sv[ fixed('c') ]
#'            
#'                                       
#'   # REPLACEMENT: 
#'     sv$a               <- "first" 
#'     sv[['a']]          <- "1st"  
#'     sv[[ perl('c.') ]] <- "third"
#'     
#'     sv[ perl('c.?') ]   <- "3rd"
#'   
#'   
#'   # MODIFIERS TO SEARCH TARGET/OBJECT
#'     sv <- searchable(v, ignore.case )         
#'     sv$A
#'     sv['b']
#'     sv['B']
#'   
#'   
#'   # RECURSIVE LISTS:
#'     l <- list( a=1, b=2, c=3 )
#'     sl <- searchable(l)                
#'     sl[["b"]]
#'     sl[[ ignore.case("B") ]] 
#'     
#'   # USE WITH MAGRITTR   
#'    \dontrun{
#'     sl[[ "B"  %>% ignore.case ]]
#'     "b" %>% sl[[.]]
#'     "B" %>% ignore.case %>% sl[[ . ]]
#'    }
#'    
#'      
#' @rdname searchable
#' @exportClass searchable
#' @import methods
#' @export searchable 

  setClass( 'searchable' 
    , representation = representation( 
        'searchables'
        # , modifiers = 'list'
        # , ignore.case = 'logical'
        # , perl        = 'logical'
        #, fixed       = 'logical'
        # , exact       = 'logical'  # This is the default
      )  
    , prototype = prototype( vector() ) # , ignore.case = FALSE, perl = FALSE, fixed = FALSE ) 
    , contains = 'searchables'  #searchables2
  )


#' @rdname searchable
#' @export
  
searchable <- function( object, ...) { 

  ret <- new( 'searchable', object )
  
  li <- list(...)
  if( length(li) > 0 ) 
    for( elem in li )  
      if( is.function(elem) ) 
        ret <- elem(ret) else
        stop( "argument is not a match modifier." )
   
  return(ret)
  
}
   
  
# METHOD: show
#' @rdname searchable
  setMethod('show', 'searchable', 
    function(object) {
      
      # CREATE LIST OF MATCH MODIFIERS USED
      mods <- unlist( .get.modifiers(object) )
      if ( is.null(mods) )
        mods <- "exact" else 
        mods <- paste( names( which( mods  ) ), collapse = ", " )
      
      cat( class(object), class(object@.Data), "using",  mods, ":\n", sep = " " )
   
      val <- object@.Data 
      
      show( object@.Data[ 1:length(object@.Data) ] )  # REMOVE attributes
      invisible(NULL)
      
    }
  )
  

#' is.searchable 
#' @rdname searchable
#' @export 
  is.searchable <- function(object) is(object,'searchable')
  
  
