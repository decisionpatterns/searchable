library(testthat)
library(searchable)
library(magrittr)

v <- c( ay=1, bee=2, cee=3 )


context('vector-default')
  sv <- searchable(v)

  sv$ay        %>% expect_equal(1)
                   expect_error( sv$ee )
  
  sv[['bee']]  %>% expect_equal(2)  
                   expect_error( sv[['ee']] )
  
  sv['cee']    %>% expect_equivalent(3)
  sv['ee']     %>% is.na %>% expect_true
  sv[ c('ay','bee','dee') ]  %>% expect_equivalent( c(1,2,NA) ) 


context('vector-fixed')
  sv <- searchable(v, fixed )

  sv$ay        %>% expect_equal(1)
                   expect_error( sv$ee )
  
  sv[['bee']]  %>% expect_equal(2)  
                   expect_error( sv[['ee']] )
  
  sv['cee']    %>% expect_equivalent(3)
  sv['ee']     %>% expect_equivalent(2:3)

