library("tidyverse")
library("reshape2")
options(digits = 3)

rank2_strict2_strict <- read.csv("rank2_strict2_strict.csv", check.names=FALSE)
( rank2_strict2_strict <- rank2_strict2_strict[, -1] )


(rank2_strict2_strict_conj <- rank2_strict2_strict %>% group_by(`P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`) %>% count() ) ###24 rows
### there are 24 permutations in P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)

#################################################################################################################################
#################################################################################################################################
#################################################################################################################################

### given a permutation in P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A), count the frequency of each permutation in `P(A)_P(¬A)_P(B)_P(¬B)`

( rank2_strict2_strict_conj_marg <- rank2_strict2_strict %>%
    group_by(`P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`, `P(A)_P(¬A)_P(B)_P(¬B)`) )

table_conj_marg_agg <- dcast(data=rank2_strict2_strict_conj_marg,
                             `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)` ~ `P(A)_P(¬A)_P(B)_P(¬B)`,
                             fun.aggregate = length,
                             value.var = "P(A)_P(¬A)_P(B)_P(¬B)")

table_conj_marg <- function (x) {
  x <- x[, colSums(x != 0) > 0]
  
  x <- x %>% 
    pivot_longer(-1 , names_to = "P(A)_P(¬A)_P(B)_P(¬B)", values_to = "number")
  
  x$freq <- x$number / sum(x$number)
  
  x <- tibble::rowid_to_column(x, "Count")
  x
}

######### conjunction 1 2 3 4, count the marg ##################
table_conj_marg_1 <- table_conj_marg_agg %>% 
  filter( `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`  ==  "1 2 3 4" )

table_conj_marg_1 <- table_conj_marg(table_conj_marg_1 ) 


######### conjunction 1 2 4 3	, count the marg ##################
table_conj_marg_2 <- table_conj_marg_agg %>% 
  filter( `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`  ==  "1 2 4 3" )

table_conj_marg_2 <- table_conj_marg(table_conj_marg_2 )

######### conjunction 1 3 2 4, count the marg ##################
table_conj_marg_3 <- table_conj_marg_agg %>% 
  filter( `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`  ==  "1 3 2 4" )

table_conj_marg_3 <- table_conj_marg(table_conj_marg_3 )


######### conjunction 1 3 4 2, count the marg ##################
table_conj_marg_4 <- table_conj_marg_agg %>% 
  filter( `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`  ==  "1 3 4 2" )

table_conj_marg_4 <- table_conj_marg(table_conj_marg_4 )


######### conjunction 1 4 2 3, count the marg ##################
table_conj_marg_5 <- table_conj_marg_agg %>% 
  filter( `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`  ==  "1 4 2 3")

table_conj_marg_5 <- table_conj_marg(table_conj_marg_5 )



######### conjunction  1 4 3 2, count the marg ##################
table_conj_marg_6 <- table_conj_marg_agg %>% 
  filter( `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`  ==  "1 4 3 2")

table_conj_marg_6 <- table_conj_marg(table_conj_marg_6 )


######### conjunction   2 1 3 4, count the marg ##################
table_conj_marg_7 <- table_conj_marg_agg %>% 
  filter( `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`  ==  "2 1 3 4")

table_conj_marg_7 <- table_conj_marg(table_conj_marg_7 )


######### conjunction   2 1 4 3, count the marg ##################
table_conj_marg_8 <- table_conj_marg_agg %>% 
  filter( `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`  ==  "2 1 4 3")

table_conj_marg_8 <- table_conj_marg(table_conj_marg_8 )

######### conjunction   2 3 1 4, count the marg ##################
table_conj_marg_9 <- table_conj_marg_agg %>% 
  filter( `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`  ==  "2 3 1 4")

table_conj_marg_9 <- table_conj_marg(table_conj_marg_9 )

######### conjunction   2 3 4 1, count the marg ##################
table_conj_marg_10 <- table_conj_marg_agg %>% 
  filter( `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`  ==  "2 3 4 1")

table_conj_marg_10 <- table_conj_marg(table_conj_marg_10 )

######### conjunction   2 4 1 3, count the marg ##################
table_conj_marg_11 <- table_conj_marg_agg %>% 
  filter( `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`  ==  "2 4 1 3")

table_conj_marg_11 <- table_conj_marg(table_conj_marg_11 )

######### conjunction  2 4 3 1, count the marg ##################
table_conj_marg_12 <- table_conj_marg_agg %>% 
  filter( `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`  ==  "2 4 3 1")

table_conj_marg_12 <- table_conj_marg(table_conj_marg_12 )

######### conjunction  3 1 2 4, count the marg ##################
table_conj_marg_13 <- table_conj_marg_agg %>% 
  filter( `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`  ==  "3 1 2 4")

table_conj_marg_13 <- table_conj_marg(table_conj_marg_13 )

######### conjunction  3 1 4 2, count the marg ##################
table_conj_marg_14 <- table_conj_marg_agg %>% 
  filter( `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`  ==  "3 1 4 2")

table_conj_marg_14 <- table_conj_marg(table_conj_marg_14 )

######### conjunction  3 2 1 4, count the marg ##################
table_conj_marg_15 <- table_conj_marg_agg %>% 
  filter( `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`  ==  "3 2 1 4")

table_conj_marg_15 <- table_conj_marg(table_conj_marg_15 )

######### conjunction  3 2 4 1, count the marg ##################
table_conj_marg_16 <- table_conj_marg_agg %>% 
  filter( `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`  ==  "3 2 4 1")

table_conj_marg_16 <- table_conj_marg(table_conj_marg_16 )

######### conjunction  3 4 1 2, count the marg ##################
table_conj_marg_17 <- table_conj_marg_agg %>% 
  filter( `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`  ==  "3 4 1 2")

table_conj_marg_17 <- table_conj_marg(table_conj_marg_17 )

######### conjunction  3 4 2 1, count the marg ##################
table_conj_marg_18 <- table_conj_marg_agg %>% 
  filter( `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`  ==  "3 4 2 1")

table_conj_marg_18 <- table_conj_marg(table_conj_marg_18 )

######### conjunction  4 1 2 3, count the marg ##################
table_conj_marg_19 <- table_conj_marg_agg %>% 
  filter( `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`  ==  "4 1 2 3")

table_conj_marg_19 <- table_conj_marg(table_conj_marg_19 )

######### conjunction  4 1 3 2, count the marg ##################
table_conj_marg_20 <- table_conj_marg_agg %>% 
  filter( `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`  ==  "4 1 3 2")

table_conj_marg_20 <- table_conj_marg(table_conj_marg_20 )

######### conjunction  4 2 1 3, count the marg ##################
table_conj_marg_21 <- table_conj_marg_agg %>% 
  filter( `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`  ==  "4 2 1 3")

table_conj_marg_21 <- table_conj_marg(table_conj_marg_21 )

######### conjunction  4 2 3 1, count the marg ##################
table_conj_marg_22 <- table_conj_marg_agg %>% 
  filter( `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`  ==  "4 2 3 1")

table_conj_marg_22 <- table_conj_marg(table_conj_marg_22 )

######### conjunction  4 3 1 2, count the marg ##################
table_conj_marg_23 <- table_conj_marg_agg %>% 
  filter( `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`  ==  "4 3 1 2")

table_conj_marg_23 <- table_conj_marg(table_conj_marg_23 )

######### conjunction  4 3 2 1, count the marg ##################
table_conj_marg_24 <- table_conj_marg_agg %>% 
  filter( `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`  ==  "4 3 2 1")

table_conj_marg_24 <- table_conj_marg(table_conj_marg_24)

rank2_strict2_strict_table_conj_marg <- rbind(table_conj_marg_1, table_conj_marg_2, table_conj_marg_3, table_conj_marg_4,
                                              table_conj_marg_5, table_conj_marg_6, table_conj_marg_7, table_conj_marg_8,
                                              table_conj_marg_9, table_conj_marg_10, table_conj_marg_11, table_conj_marg_12, 
                                              table_conj_marg_13, table_conj_marg_14, table_conj_marg_15, table_conj_marg_16,
                                              table_conj_marg_17, table_conj_marg_18, table_conj_marg_19, table_conj_marg_20,
                                              table_conj_marg_21, table_conj_marg_22, table_conj_marg_23, table_conj_marg_24)

sum(rank2_strict2_strict_table_conj_marg$number) ### check 

write.csv(rank2_strict2_strict_table_conj_marg, file = 'rank2_strict2_strict_table_conj_marg.csv', row.names = F)
