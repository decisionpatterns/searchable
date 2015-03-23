TODO:
 - remove dependency on stringr in favor of stringi?
 
 - Refactor to 
   - match.modifiers: 
       x fixed, partial 
       x perl (regex)
       x ignore.case 
       x exact

   - search.modifiers: 
       reverse.lookup - look in values  
       
   - ????
       any, all
       recursive (unlist)

   - return.modifeirs: 
       key, value
       searchable (returns a searchable object), 
       recursive: first, last, second, third, nth(string,n)

   - setting modifiers:
       uniqueness
       
       
 - Determine if we need subclasses: 
   - searchable.atomic and searchable.recursive
     There are no classes for atomic and recursive(?)
   - searchable.vector, searchable.list, searchable.hash


- Should be able to apply modifiers to object:
  v %>% searchable %>% ignore.case for example. 
  This could be done by changing the S4 slot modifiers to the match mofiers



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