# searchable

[![CRAN](http://www.r-pkg.org/badges/version/searchable)](https://cran.rstudio.com/web/packages/searchable/index.html) 
[![License](http://img.shields.io/badge/license-GPL%20%28%3E=%202%29-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-2.0.html) 
[![Downloads](http://cranlogs.r-pkg.org/badges/lettercase?color=brightgreen)](http://www.r-pkg.org/pkg/searchable)
 
 
Have you wanted more flexibility in retrieving list and vector elements than matching on the exact name? Have you wanted to ignore case? Or subset using a regular expression? Have you wanted to change default behavior? The `searchable` package is your answer.  

The *searchable* package provides flexibile methods for searching and subsetting list, vectors and other objects by matching names using case (in)sensitivity, regular expressions, fixed expressions or other user defined patterns.  This is accomplished by overloading the standard `[` operator to allow `string[r|i]` style match modifiers. These can used to change the default search semantics on an object or change search behavior on the fly. When no match modifiers are used, the default is standard R behavior, so this functions as a drop-in replacement.  

`searchable` was designed to be extensible. Developers can use `searchable` to build thier own flexible, high performance dictionary and thesaurus data structures or their own match modifiers


## Features

Features of this package are:

- Built-in search patterns:
-- exact (default) same as base-R.
-- fixed
-- regex
-- reverse lookup (match on values instead of keys)
- Easy to use: make look-ups flexible by calling `searchable` on search target. 
- `[` and `{<-` operators are overloaded so syntax is very R-like. 
- `stringr/i`-style match modifiers: matching by case (in)sensitivity, regular expressions or fixed expression, etc.
- Drop-in: default matching behavior is R's exact match behavior, so it is safe to use everywhere.
- Adjust search behavior on a **default** or **ad hoc** or **global** basis.
-- **Default** - match modifier applied to searched object
-- **Ad hoc** - match modifier applied to search term
-- **Global** - all searches become `searchable` searches
- Extensible: 
-- Create custom match modifiers
-- Allow new objects classes to be '`searchable`'


## Installation

Stable Version:

    install.packages('searchable')
    library(devtools)
    
    
Development Version: 

    library(devtools)
    install_github( "decisionpatterns/searchable" )
    

## Examples

```R
  library(searchable)
  library(magrittr)
 
  # ATOMIC VECTORS: 
    v <- c( a=1, b=2, B=3, c=4, c2=5 )
    sv <- searchable(v)
    
  # EXTRACT:
    sv$a
     
    sv[['a']]
    sv[[ ignore.case('A') ]]           # Ad-hoc case-insensitive matching 
    
    sv[ ignore.case('b') ]     
    sv[ perl('c') ]
    sv[ fixed('c') ]
           
                                      
  # REPLACEMENT: 
    sv$a               <- "first" 
    sv[['a']]          <- "1st"  
    sv[[ perl('c.') ]] <- "third"
    
    sv[ perl('c.?') ]   <- "3rd"
  
  
  # MODIFIERS TO SEARCH TARGET/OBJECT
    sv <- searchable(v, ignore.case )   # Defines default case-insensitive matching         
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


## Notes

- 