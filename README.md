# searchable

![](https://img.shields.io/cran/v/searchable.svg) <!-- Version --> 
![](https://img.shields.io/cran/l/searchable.svg) <!-- License --> 
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![Downloads](https://cranlogs.r-pkg.org/badges/searchable?color=brightgreen)](https://www.r-pkg.org/pkg/searchable)
[![](http://cranlogs.r-pkg.org/badges/grand-total/searchable)](https://cran.rstudio.com/web/packages/searchable/index.html)
<!-- [![Research software impact](http://depsy.org/api/package/cran/searchable/badge.svg)](http://depsy.org/package/r/searchable) -->

The *searchable* package provides flexibile methods for searching and subsetting list, vectors and other objects by matching names using case (in)sensitivity, regular expressions, fixed expressions or other user defined patterns.  This is accomplished by overloading the standard `[` operator to allow `string[r|i]` style match modifiers. These can used to change the default search semantics on an object or change search behavior on the fly. When no match modifiers are used, the default is standard R behavior, so this functions as a drop-in replacement.  

`searchable` was designed to be extensible. Developers can use `searchable` to build thier own flexible, high performance dictionary and thesaurus data structures or their own match modifiers


## Features

Features of this package are:

* Easy to make name look-ups searchable -- simply by declaring objects searchable.
* Drop-in functionality -- the `[` operator is overload but defaults to base-R functionality. 
* stringr/i*-style match modifiers for matching by case (in)sensitivity, regular expressions or fixed expression. 
* Match modification applied to pattern and/or target
** Define default behavior for target (dictionary)
** Overridable, per-search change of search behavior


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
