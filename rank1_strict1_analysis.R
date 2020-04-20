library("tidyverse")
library("reshape2")
options(digits = 3)

rank1_strict1 <- read.csv("rank1_strict1.csv", check.names=FALSE)
( rank1_strict1 <- rank1_strict1[, -1] )


(rank1_strict1_marg <- rank1_strict1 %>% group_by(`P(A)_P(¬A)_P(B)_P(¬B)`) %>% count() ) ###8 rows
### there are 8 permutations in P(A)_P(¬A)_P(B)_P(¬B)

#################################################################################################################################
#################################################################################################################################
#################################################################################################################################

### given a permutation in P(A)_P(¬A)_P(B)_P(¬B), count the frequency of each permutation in `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`

( rank1_strict1_marg_conj <- rank1_strict1 %>%
   group_by(`P(A)_P(¬A)_P(B)_P(¬B)`, `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`) )

table_marg_conj_agg <- dcast(data=rank1_strict1_marg_conj,
      `P(A)_P(¬A)_P(B)_P(¬B)` ~ `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`,
      fun.aggregate = length,
      value.var = "P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)")

table_marg_conj <- function (x) {
  x <- x[, colSums(x != 0) > 0]
  
  x <- x %>% 
    pivot_longer(-1 , names_to = "P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)", values_to = "number")
  
  x$freq <- x$number / sum(x$number)
  
  x <- tibble::rowid_to_column(x, "Count")
  x
}

######### marginal: 1 4 2 3, count the conj ##################
table_marg_conj_1 <- table_marg_conj_agg %>% 
  filter( `P(A)_P(¬A)_P(B)_P(¬B)`  ==  "1 4 2 3" )

table_marg_conj_1 <- table_marg_conj(table_marg_conj_1 ) 


######### marginal: 1 4 3 2, count the conj ##################
table_marg_conj_2 <- table_marg_conj_agg %>% 
  filter( `P(A)_P(¬A)_P(B)_P(¬B)`  ==  "1 4 3 2" )

table_marg_conj_2 <- table_marg_conj(table_marg_conj_2 )

######### marginal: 2 3 1 4, count the conj ##################
table_marg_conj_3 <- table_marg_conj_agg %>% 
  filter( `P(A)_P(¬A)_P(B)_P(¬B)`  ==  "2 3 1 4" )

table_marg_conj_3 <- table_marg_conj(table_marg_conj_3 )


######### marginal: 2 3 4 1, count the conj ##################
table_marg_conj_4 <- table_marg_conj_agg %>% 
  filter( `P(A)_P(¬A)_P(B)_P(¬B)`  ==  "2 3 4 1" )

table_marg_conj_4 <- table_marg_conj(table_marg_conj_4 )


######### marginal: 3 2 1 4 , count the conj ##################
table_marg_conj_5 <- table_marg_conj_agg %>% 
  filter( `P(A)_P(¬A)_P(B)_P(¬B)`  ==  "3 2 1 4")

table_marg_conj_5 <- table_marg_conj(table_marg_conj_5 )



######### marginal:  3 2 4 1  , count the conj ##################
table_marg_conj_6 <- table_marg_conj_agg %>% 
  filter( `P(A)_P(¬A)_P(B)_P(¬B)`  ==  "3 2 4 1")

table_marg_conj_6 <- table_marg_conj(table_marg_conj_6 )


######### marginal:   4 1 2 3   , count the conj ##################
table_marg_conj_7 <- table_marg_conj_agg %>% 
  filter( `P(A)_P(¬A)_P(B)_P(¬B)`  ==  "4 1 2 3")

table_marg_conj_7 <- table_marg_conj(table_marg_conj_7 )


######### marginal:   4 1 3 2   , count the conj ##################
table_marg_conj_8 <- table_marg_conj_agg %>% 
  filter( `P(A)_P(¬A)_P(B)_P(¬B)`  ==  "4 1 3 2")

table_marg_conj_8 <- table_marg_conj(table_marg_conj_8 )


rank1_strict1_table_marg_conj <- rbind(table_marg_conj_1, table_marg_conj_2, table_marg_conj_3, table_marg_conj_4,
      table_marg_conj_5, table_marg_conj_6, table_marg_conj_7, table_marg_conj_8)
write.csv(rank1_strict1_table_marg_conj, file = 'rank1_strict1_table_marg_conj.csv')
