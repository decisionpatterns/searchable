# Introduction

The 'searchable' package provides flexibile methods for subseting  
object by matching objects names using case (in)sensitivity, regular or 
fixed expressions. Searches uses the standard '['
operator and allows specification of default search behavior to either the
search target (named object) and/or the search pattern.

It was designed to make flexible, high performance dictionary and 
thesaurus structures.  

Features of this package are:

* 'stringr/i'-style match modifiers for matching names by case (in)sensitivity, 
regular expressions or fixed expression. 

* Match modification applied to either/both the pattern and target.

* The `[` operatore overloaded to provide R-like functionality. 

* Search behavior defaults to base R behaviors.



# Installation

    library(devtools)
    install_github( "decisionpatterns/searchable" )
    
    # OR 
    install.packages('searchable')


# Examples

```R
  library(searchable)
  library(magrittr)
 
  # ATOMIC VECTORS: 
    v <- c( a=1, b=2, B=3, c=4, c2=5 )
    sv <- searchable(v)
    
  # EXTRACT:
    sv$a
     
    sv[['a']]
    sv[[ ignore.case('A') ]]
    
    sv[ ignore.case('b') ]     
    sv[ perl('c') ]
    sv[ fixed('c') ]
           
                                      
  # REPLACEMENT: 
    sv$a               <- "first" 
    sv[['a']]          <- "1st"  
    sv[[ perl('c.') ]] <- "third"
    
    sv[ perl('c.?') ]   <- "3rd"
  
  
  # MODIFIERS TO SEARCH TARGET/OBJECT
    sv <- searchable(v, ignore.case )         
    sv$A
    sv['b']
    sv['B']
  
  
  # RECURSIVE LISTS:
    l <- list( a=1, b=2, c=3 )
    sl <- searchable(l)                
    sl[["b"]]
    sl[[ ignore.case("B") ]] 
    
  # USE WITH MAGRITTR   

    sl[ "B"  %>% ignore.case ]
    "b" %>% sl[[.]]
    "B" %>% ignore.case %>% sl[[ . ]]

     
```
