# searchable

![](https://img.shields.io/cran/v/optigrab.svg) <!-- Version --> 
![](https://img.shields.io/cran/l/optigrab.svg) <!-- License --> 
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![Downloads](https://cranlogs.r-pkg.org/badges/optigrab?color=brightgreen)](https://www.r-pkg.org/pkg/optigrab)
[![](http://cranlogs.r-pkg.org/badges/grand-total/optigrab)](https://cran.rstudio.com/web/packages/optigrab/index.html)
<!-- [![Research software impact](http://depsy.org/api/package/cran/optigrab/badge.svg)](http://depsy.org/package/r/optigrab) -->

The *searchable* package provides flexibile methods for subseting objects by matching their names using case (in)sensitivity, regular or fixed expressions. Searches uses the standard `[` operator and allows specification of a default (applied to the target) or ad hoc search behavior (applied to the search pattern).

It was designed to make object flexible, high performance dictionary and thesaurus data structures.  

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
