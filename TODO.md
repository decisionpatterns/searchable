TODO:
 
# IMMEDIATE 
 
## ? Support options for missing hits from search.  

     For example: 
        v[[MISS]] -> NA rather than 'named vector()'
    

## Test to support 
   - None, 0, 1, 1 of multiple, multiple match hits
   - Each match modifier
   - Each (upcoming) return modifier
   - Extract, Replace
 

## Support data.frames 
   
   This should be straight-foward and implement a method of 
   seachable-ANY-character that dispatches to data.frame-ANY-character after 
   appropriate resolutions.


## Support data.tables
   
    This might be tricky as j is interpreted within x. There might not be a 
    good way to do this unless with = FALSE.  Although there might not be 
    anything different from data.frames
 

## Support Search Modifiers

   - search.modifiers: 
       - reverse.lookup - look in values  
       - recursive (unlist) - descend into recursive structures

## Support return modifiers, e.g. 

   - multiple pattern matches reslovers:
       - any 
       - all
       - first, last, second, third, nth(string,n) 
       - collect
       
   - return.modifeirs: 
       - key, value
       x searchable (returns a searchable object): pipe result to searchable fn

   - setting modifiers:
       - uniqueness
       - count 



## Write fn to collect values sharing the same keys into a list or vector.

   - cf. base.tools/collect_values_by_name
   
    
# LONG TERM 

## Make it so names do not have to be quoted using qw, qcc or something


## Determine if we need subclasses: 
   - searchable.atomic and searchable.recursive
     There are no classes for atomic and recursive)
   - searchable.vector, searchable.list, searchable.hash

## Make environments searchable 
   - Environments cannot be (easily) be made "searchable" due to the way the 
     they are implemented.
     

# Dirrences with Base 

## Replacement behavior of $<- and [[<- 

   Potentially inconsistent behavior depending on whether one or more elements
   are matched. If one element is matched, this works as expected.  If multiple 
   matches, errors:
   
       multiple matches for, 'i'. Use `[<-` to replace/modify multiple elements.

   instructing the user to use [<-
   
   Option 1: This is the correct behavior
   Option 2: no patterns applied to $<- [[<- 
   Option 3: settable option (this is inane, since this can be overridden by 
             the pattern match-modifier)


   
###   Issue 1: Multiple returns for each search pattern
   
   In order to be more like base R, it might be better to support multiple 
   patterns. The problems is that the matching is indeterminant. 
   This would mean that either, 
     - OKAY: the returned object would be a different length than the patterns.
     
     - Or, a list would be returned. Breaking the rule that [ is a slice 
       operator but retaining the expectation that there is one output 
       for every input.
       
     - Or, elements matching ANY of the patterns would be acceptale. This might
       be implemented as adding a search-modifier `Or`/`Any`, `All` 
   
   This could be handled by a return modifier such as `collect` or `unlist`.
   OR 
   `atomic` : force results to be atomic 
   `recursive`, `allow.recursive`/`allow.list`
   
   
   
### Issue 2: Interpretation of multiple patterns

   Must the match occur for ALL patterns or ANY. Is this specifiable? 
   
   The most sensible thing is to match any of the patterns.  
   
   
### Alternatives:   
   
   The alternative something ugly like:      
     c('ay','bee','dee')  %>% sapply( function(x) sv[x] )  %>% unlist( use.names = FALSE )
     
     - Use a return-modifier that ensures a single return value.



# COMPLETED 

## Better modifier collision in extract.R/.collect.modifiers 

    - ? match modifiers should be resolved without necessarily affecting search etc.  
    - Its possible to apply the modifiers differently to searchable vs nt searchable
      objects
    
    Any modifiers to search pattern override the target. 

## remove dependency on stringr in favor of stringi?
    
## Test and support lists
   
   It is unclear how lists are supported.
   - On [ with search miss should yield NULL (?), Cf. tests
 
## Supporting multiple patterns for [ and [<- (version 0.2)
  
   STATUS: 
   * When matching a pattern 0,1 or more results will be given.  
     If no results are given a zero-lenght 
 
   BACKGROND
   Currently these operators support only one pattern, unlike there base 
   counterparts. Thus, this is an error:
   
     sv[ c('ay','bee','dee') ]
 
   The way to seach would be:
   
     "(ay|bee|dee)" %>% perl %>% sv[.] 


## Can there be perl and ignore.case together? OR fixed and ignore.case? 
   No. match.modifiers are mutually exclusive.

## target.object modification
   - X Should be able to apply modifiers to target object:
        v %>% searchable %>% ignore.case  
  
## What is the behavior of $ ?
 
   X Option 1: Not overridden, behaves as base R. This is rationale because $
     does not generally interpret the argument.
     
   - Option 2: As [[, and interprets 
     There is no way to directly add to the structure if the 
     Since it is object$name there can be no modifiers applied to name.    
   
   - $<- ?
     If there is no


## Support Match modifiers
   - match.modifiers: 
       x fixed, partial 
       x perl (regex)
       x ignore.case 
       x exact



# INTERFACE OPTIONS
-------------------------------------------------

## OPTION 1:

  Follow closely stringr and use modifiers to change attributes, but have a lot of
  slots.

PROs: Existing interface
CONs: many attributes, non-expandable, resolve at match time, object validity checking


## OPTION 2: 

Existing @modifiers list, overload fixed, perl, ignore,case (modifier) functions

PROs: easily extensible, write additional call-backs
CONs: conflict at load, though these are compatible with stringr

Rewrite fixed, perl and ignore.case to resolve match modification at call time.
Github( stringr#60 ).

- Can warn on incompatible modifiers, within the functions
- How to find incompatibles within list of modifiers? 
  - Digest?
  X Don't care...these are quick functions. 


## OPTION 3: 

Hybrid approach. 
- Refactor searchable with the appropriate slots.


## OPTION 3:

  Expand to match, search and return modifiers. Match follows closely stringr, searc

*This can be implemented later*
