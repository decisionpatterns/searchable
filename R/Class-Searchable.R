#' @title searchable 
#' 
#' @description
#' Marks an objects as a \code{searchable} target, optionally specifying the 
#' default search options
#'   
#' @param object searchable object or object to be made searchable
#' @param type character; the type of search to perform 
#' @param ... additional arguments defining the search pattern. See 
#'   \code{?pattern} for details.
#'  
#' @details 
#' 
#' The searchable class allows 'stringr/i'-like searches when 
#' extracting (or replacing) objects. The following search types are supported: 
#' 
#' \itemize{ 
#'   \item \code{std} standard R matching,
#'   \item \code{regex} for regular expression matching, 
#'   \item \code{fixed} for fixed string matching, 
#'   \item \code{coll} for collation matching, 
#' }
#' 
#' Class \code{searchable} allows customization of how R's standard accessors -- 
#'  \code{\[}, \code{\$}, \code{\[\[} -- match objects' names.
#' 
#' @section Diffences from stringr:
#' 
#' \code{stringr/i} is a general purpose string manipulations library; 
#' amoung its functions is the ability to search/pattern match against strings. 
#' \code{searchable} applies this type of matching  to allow specifying that 
#' objects' names will be searched using standard R accessors: \code{\[}, 
#' \code{\$}, \code{\[\[}. Thus, \code{ searchable(sv)[ regex('b') ]} returns objects 
#'  
#' Unlike \code{stringr} which allows search modifiers to apply to only the 
#' search pattern, \code{searchable} also modifiers to be applied to 
#' the search target. Unless overridden, all subsequent searches of searchable 
#' objects will use the predefinced pattern. 
#'   
#' This search used can be specified at initialization of the searchable object 
#' or any subsequent time any of the match-modifying functions, e.g. 
#' \code{fixed}, \code{perl}, \code{ignore.case}, etc. See examples. 
#' 
#' When modifiers are applied to both target and pattern, \strong{modifers 
#' applied to the pattern take precedence} and the target's modifiers are disabled. 
#' 
#' 
#' @section Differences from base R:
#' 
#' \code{searchable} is designed to be minimally invase. When no search types 
#' or options are specified, searchable objects default to R's use of standard 
#' accessors. 
#' 
#' When a search type or options are specified, R's accessors work as 
#' expected with a few, small changes to accomodate an indeterminate number 
#' of search results. These are:
#' 
#' \itemize{
#'  
#'   \item \code{\$} works with atomic, searchable objects. This is not true of
#'         R's atomic objects. This should probably be fixed in Base R.
#'         
#'   \item Searches using \code{\[\[]} or \code{\$} may have more than one match.
#'         In order to be consistent with base R, searches with multiple matches
#'         produce an error: \code{attempt to select more than one element}. 
#'         
#'   \item Searches using \code{\[} accepts a one element search 
#'         pattern. (This may change). Attempts to provide multiple search terms
#'         results in an error: 
#'            \code{pattern string should be a one-element character vector}.
#'            
#'         In base R, there is output value every element of input argument, 
#'         \code{i}. Input elements that do not match a named element of 
#'         \code{x} return \code{NA}. Because of the indeterminant number of 
#'         matches given a pattern search against a \code{searchable} object, 
#'         there is no guarantee that a search pattern have a match. If no 
#'         matches are found, a zero-length object is returned. (This may change
#'         to \code{NA} to be more consisitent.) 
#'         
#'   \item Searches do not produce a searchable class but the superclass that
#'         the searchable class wraps.
#'          
#' }  
#' 
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
#' 
#' @return 
#'   By default, extraction from a searchable objects does not produce a subset 
#'   that is also searchable. It is assumed that in most cases, the developer 
#'   will not want another searchable object and only wish to have the subclass.  
#'   
#' @note 
#'   - Environments cannot be (easily) be made "searchable" due to the way the 
#'     they are implemented.
#'     
#'   - The extraction methods for searchable objects are (at present) limited to
#'     only one pattern. This may change in the future.  
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
#'     sv <- searchable(v, case_insensitive = TRUE )         
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
#' @exportClass Searchable
#' @export
#' @include Class-Searchables.R

   Searchable <- setClass( 'Searchable'
     , contains = 'Searchables' 
     , representation = representation( 'Searchables', type='character', options='list')
     , prototype( NA_character_, type = 'std', options=list() ) 
   )

   # setClass( 'Searchable', contains = 'SearchableOrPattern' )
    

# CONSTRUCTOR
# NB. compare with pattern 
#' @rdname searchable
#' @export
 
  searchable <- function( object, type='std', ... ) { 
    
    # TRAP NON-NAMED OBJECTS
    if( object  %>% attr('names')  %>% is.null ) 
      stop( 'Only objects with a names attribute can be made searchable.')
    
    return( 
      new( 'searchable', object, type=type, options=list(...) )  
    )
  }  

  

# METHOD: show
#' @rdname searchable

  setMethod('show', 'Searchable', 
    function(object) {
      
      cat( 'searchable object using', .describe_pattern(object) )       
      show( object@.Data[ 1:length(object@.Data) ] )  # REMOVE attributes
      invisible(NULL)
      
    }
  )
  