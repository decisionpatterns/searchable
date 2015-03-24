# ----------------------------------------------------------------
# HELPER FUNCTIONS 
 

# Unmodifed do not have any .match.modifiers set.


.is.unmodified <- function( object, pattern )  {
  
  mods.object <- .get.modifiers(object)
  mods.pattern <- .get.modifiers(pattern)
  
  
  ( is.null( mods.object)  || length(mods.object)  == 0 ) && 
  ( is.null( mods.pattern) || length(mods.pattern) == 0 ) 
  
}

.collect.modifiers <- function( object, pattern ) { 
   ret <- pattern 
   
   obj.mods <- .get.modifiers(object)
   if( ! is.null(obj.mods) ) 

      
   # RESOLVE CONFLICTS ...
   # If there are any modifiers to pattern use those instead.
   pat.mods <- .get.modifiers(pattern)
   if( ! is.null(pat.mods) ) 
     attributes(ret) <- pat.mods else
     attributes(ret) <- obj.mods

   return(ret)
   
}
  
# # Applies invert lookup, if set   
# .inverse.if.reverse.lookup <- function(object, pattern)
#    if( ! is.null( attr(pattern, "reverse.lookup" ) ) &&
#          attr(pattern, "reverse.lookup" ) == TRUE 
#    ) invert(object) else object  
#   
    
  
# Returns indices of matching elements
.which.matches <- function( object, pattern ) { 
  
  # TRAP ERRORS     
    if( ! .is.unmodified(object,pattern) & ! is.string(pattern) ) 
      stop("pattern string should be a one-element character vector")
     
  # ADD DEFAULT MODIFIERS
    pattern <- .collect.modifiers(object,pattern)
     
  # APPLY REVERSE LOOKUP BY INVERTING 
  #  This does not work for recursive, list-like  object
   object <- 
     if( ! is.null( attr(pattern, "reverse.lookup" ) ) &&
         attr(pattern, "reverse.lookup" ) == TRUE 
     ) invert(object) else object  
    
    
    return( 
      which( stringr::str_detect( string=names(object), pattern=pattern ) )
    )
  
}  


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
#' \strong{zero or more} elemenxts of \code{x}. When searching using a pattern, 
#' this is generally the search type that is wanted. 
#'  
#' \code{[} does not return a searchable object. It is generally thought that 
#' the returned object should not be a returnable object.   
#'  
#' @section \code{[[}, \code{[[<-} and \code{$}, \code{$<-} :
#' 
#' These operators are used for getting and setting at \strong{zero or one} 
#' element of \code{x}. Matches to more than one element result in an error.  
#' 
#' 
#' @section repeated-names:
#' 
#' Unlike for environments or hashes, there are no constraints ensuring 
#' uniqueness for names in vectors and lists. These structures may contain 
#' multiple elements with the same name. Attempts to extract by this name yield 
#' the first occurence of the name.
#' 
#' 
#' @return 
#'   The values after the extracting methods have been applied:\cr
#'   \code{[} returns a subset of \code{x}, but which is not searchable.  \cr
#'   \code{[[} returns an element of \code{x}  \cr
#'   \code{$} retutrns an element of \code{x} \cr
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
#'     sv[ perl('c.?') ]      
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
#' @include Class-searchable.R
#' @import stringr

# ----------------------------------------------------------------
# EXTRACT 
# ----------------------------------------------------------------
  
#' @rdname extract
#' @export     
  setMethod( '[', c(x='searchable', i='character', j='missing'), 
    function(x,i,j,...) {
       
     # ESCAPE HATCH
       if( .is.unmodified(x,i) ) return( x@.Data[i] )      

     # FIND MATCHES    
       wh <- .which.matches(x,i)

       return( x[wh] )
     
    }           
  )
  
  
  
#' @rdname extract
#' @export   
  setMethod( '[[', c(x='searchable', i='character'), 
     function(x,i) { 
       
     # ESCAPE HATCH
       if( .is.unmodified(x,i) ) return( x@.Data[[i]] )
  
     # FIND MATCHES    
       wh <- .which.matches(x,i)

     # [[ should match only one element
       if( length(wh) > 1 ) stop('attempt to select more than one element')
       if( length(wh) < 1 ) stop('subscript not found')
     
       return( x[[wh]] )
       
     }   
  )

  
#' @rdname extract
#' @export   
  setMethod( '$', c(x='searchable'), 
    function(x,name) { 
      # ESCAPE HATCH 
        if( .is.unmodified(x,name) ) return( x@.Data[[name]] )
        
        x[[name]]
    }         
  )
  
 
# ----------------------------------------------------------------
# REPLACE  
# ----------------------------------------------------------------
  
#' @rdname extract
#' @export  
  setReplaceMethod( '[', c(x='searchable', i='character', j='missing', value='ANY'), 
    function(x,i,value) {
      
      # ESCAPE HATCH
        if( .is.unmodified(x,i) ) return( `[<-`(x,i,value) )
      
      wh <- .which.matches(x,i)
      
      x@.Data[wh] <- value
      return(x)
      
    }                
  )
  
  

#' @rdname extract
#' @export    
  setReplaceMethod( '[[', c(x="searchable", i="character", j="missing", value="ANY") ,
    function(x,i,value) {
      
       # ESCAPE HATCH 
         if( .is.unmodified(x,i) ) return( `[[<-`(x,i,value) )
       
       # BASE R WILL ONLY ALLOW [[<- TO MODIFY ONE ELEMENT THE FIRST,
       # WARN IF THE MODIFIER RETURNS MORE THAN ONE MATCH, ONLY THE FIRST WILL
       # BE MODIFIED
         wh <- .which.matches(x,i)
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
              , "remove modifiers or use 'exact' modifier."
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

      if( .is.unmodified(x,name) ) { 
        x@.Data[[name]] <- value
        return(x)
      } 
      
      x[[ name ]] <- value
      return(x)
      
    }
  )
    
