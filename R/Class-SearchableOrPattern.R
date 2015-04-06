#' Class: SearchableOrPattern
#' 
#' This is a class used internally to specify an object as either a searchable 
#' or pattern. Both of these classes inherit from this class 
#' 
#' @slot .Data Searchables; an object that can be made searchable, currently 
#'       a vector or list.
#' @slot type character the  
#' @slot options list; list of stringi-type options
#' 
#' \code{SearchableOrPattern} wraps one of the classes that a \code{Searchables}
#' object.  
#' 
#' @include Class-Searchables.R
#' @exportClass SearchableOrPattern
#' @export

  setClass( 'SearchableOrPattern' 
     , representation = representation( 'Searchables', type='character', options='list')  
     , prototype( type = 'std', options=list() ) 
     , contains = 'Searchables'  
   )
