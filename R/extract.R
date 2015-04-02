#' Extraction operators for searchable object
#' 
#' Defines  \code{[}, \code{[[}, and \code{$} for searchable objects
#' 
#' @param x searchable object
#' @param i character; pattern with potential match modifiers applied,
#' @param name character; a name to be extracted, used with \code{$}, so no 
#'   match modification can be applied to the name.
#' @param j missing; never specified
#' @param drop For matrices and arrays. If TRUE the result is coerced to the 
#'    lowest possible dimension (see the examples). This only works for 
#'    extracting elements, not for the replacement. See \code{\link[base]{drop}}
#'    for further details.
#' @param ... additional arguments. See \code{\link[base]{Extract}}
#' 
#' @param value replacement value for replacement functions 
#'
#' The methods for searching respect the modifiers applied to both \code{x} and
#' \code{i}.
#' 
#' @section \code{[}, \code{[<-} :
#' 
#' \code{[} and \code{[<-} are used for getting and setting 
#' \strong{zero or more} elemenxts of \code{x}. This is probably the most 
#' useful of the search modifiers. This search returns elements of 
#' the target that matches \strong{ANY} of the search patterns. Unlike the 
#' its normal behavior, \code{\[} does not guarantee the output to have as many
#' elements as elements to \code{pattern}.
#'  
#' \code{[} does not return a searchable object. It is thought that 
#' the return valuable will not be subsequently searched. It is easy to turn 
#' the results into a searchable object using \code{searchable} however. 
#'
#'
#' @section \code{[[}, \code{[[<-} and \code{$}, \code{$<-} :
#' 
#' These operators are used for getting and setting at \strong{zero or one} 
#' element of \code{x}. Matches to more than one element result in an error.  
#' 
#' 
#' @section repeated-names:
#' 
#' Unlike for environments and hashes, no constraints exist for ensuring 
#' uniqueness for names in vectors and lists. These structures may contain 
#' multiple elements with the same name. Normal attempts to extract by name 
#' yield only the first element that matches the name. Using a \code{searchable}
#' pattern match yields all matching elements.
#' 
#' 
#' @return 
#'   The values after the extracting methods have been applied:\cr
#'   \code{\[} returns a subset of \code{x}, but which is not searchable.  \cr
#'   \code{\[\[} and \code{\$} return a sinlge element of \code{x}  \cr
#'   
#'  @seealso
#'    \code{\link{searchable}}           \cr
#'    \code{\link[base]{Extract}}        \cr
#'    \code{\link[stringr]{ignore.case}} \cr
#'    \code{\link[stringr]{perl}}        \cr
#'    \code{\link{reverse.lookup}}       \cr
#'    
#' @examples
#' 
#'   # ATOMIC VECTORS: 
#'     v <- c( a=1, b=2, B=3, c=4, c2=5 )
#'     sv <- searchable(v)
#'   
#'   # EXTRACT:
#'     sv[ c('a','b') ]        # Normal
#'     sv[ regex('c.?') ]      
#'     sv[ fixed('c') ] 
#'     sv[ 'x' ]                # NA
#'     
#'     sv[["a"]]
#'     sv[[ ignore.case("a") ]] 
#'     sv[[ ignore.case("A") ]] 
#'
#'     sv$a   
#'     sv$b
#'     sv$B 
#'  
#'        
#'   # WITH MARGRITTR:   
#'   \dontrun{
#'     "b" %>% sv[[.]]
#'     "B" %>% ignore.case %>% sv[.]
#'     "c." %>% perl %>% sv[[.]]     
#'     "c.?" %>% perl %>% sv[.]
#'    } 
#'   
#'   # REPLACEMENT: 
#'     sv[['a']] <- "first"  
#'     sv[[ perl('c.') ]] <- "third"
#'     # sv[[ perl('c.?') ]] <- "third"
#'   
#'   
#'   # RECURSIVE LISTS:
#'     l <- list( a=1, b=2, c=3 )
#'     sl <- searchable(l)                
#'     sl[["b"]]
#'     sl[[ ignore.case("B") ]] 
#'    \dontrun{
#'     sl[[ "B"  %>% ignore.case ]]
#'     "b" %>% sl[[.]]
#'     "B" %>% ignore.case %>% sl[[ . ]]
#'    }
#' 
#' @aliases extract

# ----------------------------------------------------------------
# EXTRACT 
# ----------------------------------------------------------------
  
#' @rdname extract
#' @export     
  setMethod( '[', c(x='searchable', i='character', j='missing'), 
    function(x,i,j,...) {
       browser()
     # ESCAPE HATCH FOR  'standard' matching
       if( .is.standard(x,i) ) return( x@.Data[i] ) 
       
       return( x[ .matches(x,i) ] )
     
    }           
  )
  
  
  
#' @rdname extract
#' @export   
  setMethod( '[[', c(x='searchable', i='character'), 
     function(x,i) { 
       
     # ESCAPE HATCH FOR  'standard' matching
       if( .is.standard(x,i) ) return( x@.Data[[i]] ) 
       
    #   pattern <- .resolve.patterns(x,i)
    #   if( pattern@type == 'standard' ) return( x@.Data[[i]] )
  
     # FIND MATCHES    
       wh <- which( .matches(x,i) )
    
     # [[ should match only one element
       if( length(wh) > 1 ) stop('attempt to select more than one element')
       if( length(wh) < 1 ) stop('subscript not found')
     
       return( x@.Data[[wh]] )
       
     }   
  )

  
#' @rdname extract
#' @export   
  setMethod( '$', c(x='searchable'), 
    function(x,name) `[[`(x,name)
  )

  
 
# ----------------------------------------------------------------
# REPLACE  
# ----------------------------------------------------------------
  
#' @rdname extract
#' @export  
  setReplaceMethod( '[', c(x='searchable', i='character', j='missing', value='ANY'), 
    function(x,i,value) {
      
      # ESCAPE HATCH
        if( .is.standard(x,i) ) return( `[<-`(x,i,value) )
      
        wh <- which( .matches(x,i) )
      
      x@.Data[wh] <- value
      return(x)
      
    }                
  )
  
  

#' @rdname extract
#' @export    
  setReplaceMethod( '[[', c(x="searchable", i="character", j="missing", value="ANY") ,
    function(x,i,value) {
      
       # ESCAPE HATCH 
         if( .is.standard(x,i) ) return( `[[<-`(x,i,value) )
       
       # BASE R WILL ONLY ALLOW [[<- TO MODIFY ONE ELEMENT THE FIRST,
       # WARN IF THE MODIFIER RETURNS MORE THAN ONE MATCH, ONLY THE FIRST WILL
       # BE MODIFIED
         wh <- which( .matches(x,i) )
       
         if( length(wh) > 1 ) 
           stop( call.=FALSE 
             , "[[<-,searchable,character - multiple matches for, '"
             , substitute(i) # deparse( substitute(i) )
             , "'. Use `[<-` to replace/modify multiple elements."
            )
       
          if( length(wh) == 0 ) { 
            warning( call.=FALSE
              , "No matches for, '", substitute(i), "'. "
              , "No replacements made or CREATED.\n"
              , "To make additions to '", substitute(x), "' "
              , "remove modifiers or use 'standard' modifier."
            )
            return(x)
          } 
       
       # MODI
       x@.Data[[ wh[[1]] ]] <- value
       return(x)
    }
  )

  

#' @rdname extract
#' @export   
  setReplaceMethod( '$', c( x="searchable", value="ANY"),
    function(x, name, value) {

      if( x@pattern@type == 'standard' ) { 
        x@.Data[[name]] <- value
        return(x)
      } 
      
      x[[ name ]] <- value
      return(x)
      
    }
  )
    




# --------------------------------------------------------------
# UTILITIES
# --------------------------------------------------------------

# Resolve patterns 
# 
# Resolves the pattern to use from the search 
#
# @param object searchable object 
# @param pattern object to use as a pattern 
#
# A pattern can be associated with either argument or both; this ensures
# returns the correct pattern to use
# 

.resolve.patterns <- function( object, pattern ) {
   
    if( ! pattern  %>% is('pattern') ) { 
      str <- pattern 
      pattern <- object@pattern 
      pattern@.Data <- str
    }
    
    return(pattern)
    
}

# Is this a standard R search 
.is.standard <- function(object, pattern) 
  ! is( pattern, "pattern" ) && object@pattern@type == 'standard' ||
    is( pattern, "pattern" ) && pattern@type == 'standard'

# Return logical indication matching elements for name search
#
# Returns indices for use with [, [<- allowing multiple patterns
#
# @return integer 

.matches <- function( object, pattern ) { 
  
  # Determine pattern to use ...
    pattern <- .resolve.patterns( object, pattern )
  
  # APPLY REVERSE LOOKUP BY INVERTING OBJECT
  #  This does not work for recursive, list-like  objects
    # object <- if( .reverse.lookup(pattern) ) invert(object) else object  
        
  return( 
    .detect( str=names(object), pattern=pattern ) 
  )
  
}  


# detect 
#
# which elements matching the search pattern 
#
# @param str seachable target
# @param pattern method for searching 
#
# @return logical; vector indicating which elements match
# 
# @seealso 
#  .matches

.detect  <- function(str, pattern) { 

  type <- if( ! pattern %>% is('pattern') ) "standard" else pattern@type
  
  switch( type
    , regex = stri_detect_regex( str, pattern@.Data, opts_regex = stri_opts_regex( pattern@options ) )      
    , fixed = stri_detect_fixed( str, pattern@.Data, opts_fixed = stri_opts_fixed( pattern@options ) )    
    , coll  = stri_detect_coll(  str, pattern@.Data, opts_coll  = stri_opts_coll(  pattern@options ) )
    , standard = stop('A pattern should have been specified by now.')
    , stop( "Unknown search type : ", type )
  )

}

