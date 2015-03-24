# Introduction

The `searchable` packages provides functionality for searching named vectors and
lists more configurable. The packages uses stringr-style match modifiers to 
allow for matching by case (in)sensitivity, regular expressions 
or fixed expression. It also allows searching through values rather than names. 
This functionality facilitates creating dictionary and thesaurus like structures. 

# Installation

    library(devtools)
    install_github( "decisionpatterns/searchable" )
    
    # OR 
    install.packages('searchable')


# Examples

```R
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
     \dontrun{
      sl[[ "B"  %>% ignore.case ]]
      "b" %>% sl[[.]]
      "B" %>% ignore.case %>% sl[[ . ]]
     }
     
```
