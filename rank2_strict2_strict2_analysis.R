library("tidyverse")
library("reshape2")
options(digits = 3)

rank2_strict2_strict2 <- read.csv("rank2_strict2_strict.csv", check.names=FALSE)
( rank2_strict2_strict2 <- rank2_strict2_strict2[, -1] )


(rank2_strict2_strict2_conj <- rank2_strict2_strict2 %>% group_by(`P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`) %>% count() ) ###24 rows
### there are 24 permutations in P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)

#################################################################################################################################
#################################################################################################################################
#################################################################################################################################

### given a permutation in P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A), count the frequency of each permutation in `P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`

( rank2_strict2_strict2_conj_disj <- rank2_strict2_strict2 %>%
    group_by(`P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`, `P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`) )

table_conj_disj_agg <- dcast(data=rank2_strict2_strict2_conj_disj,
                             `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)` ~ `P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`,
                             fun.aggregate = length,
                             value.var = "P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)")

table_conj_disj <- function (x) {
  x <- x[, colSums(x != 0) > 0]
  
  x <- x %>% 
    pivot_longer(-1 , names_to = "P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)", values_to = "number")
  
  x$freq <- x$number / sum(x$number)
  
  x <- tibble::rowid_to_column(x, "Count")
  x
}

######### conjunction 1 2 3 4, count the marg ##################
table_conj_disj_1 <- table_conj_disj_agg %>% 
  filter( `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`  ==  "1 2 3 4" )

table_conj_disj_1 <- table_conj_disj(table_conj_disj_1 ) 


######### conjunction 1 2 4 3	, count the marg ##################
table_conj_disj_2 <- table_conj_disj_agg %>% 
  filter( `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`  ==  "1 2 4 3" )

table_conj_disj_2 <- table_conj_disj(table_conj_disj_2 )

######### conjunction 1 3 2 4, count the marg ##################
table_conj_disj_3 <- table_conj_disj_agg %>% 
  filter( `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`  ==  "1 3 2 4" )

table_conj_disj_3 <- table_conj_disj(table_conj_disj_3 )


######### conjunction 1 3 4 2, count the marg ##################
table_conj_disj_4 <- table_conj_disj_agg %>% 
  filter( `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`  ==  "1 3 4 2" )

table_conj_disj_4 <- table_conj_disj(table_conj_disj_4 )


######### conjunction 1 4 2 3, count the marg ##################
table_conj_disj_5 <- table_conj_disj_agg %>% 
  filter( `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`  ==  "1 4 2 3")

table_conj_disj_5 <- table_conj_disj(table_conj_disj_5 )



######### conjunction  1 4 3 2, count the marg ##################
table_conj_disj_6 <- table_conj_disj_agg %>% 
  filter( `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`  ==  "1 4 3 2")

table_conj_disj_6 <- table_conj_disj(table_conj_disj_6 )


######### conjunction   2 1 3 4, count the marg ##################
table_conj_disj_7 <- table_conj_disj_agg %>% 
  filter( `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`  ==  "2 1 3 4")

table_conj_disj_7 <- table_conj_disj(table_conj_disj_7 )


######### conjunction   2 1 4 3, count the marg ##################
table_conj_disj_8 <- table_conj_disj_agg %>% 
  filter( `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`  ==  "2 1 4 3")

table_conj_disj_8 <- table_conj_disj(table_conj_disj_8 )

######### conjunction   2 3 1 4, count the marg ##################
table_conj_disj_9 <- table_conj_disj_agg %>% 
  filter( `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`  ==  "2 3 1 4")

table_conj_disj_9 <- table_conj_disj(table_conj_disj_9 )

######### conjunction   2 3 4 1, count the marg ##################
table_conj_disj_10 <- table_conj_disj_agg %>% 
  filter( `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`  ==  "2 3 4 1")

table_conj_disj_10 <- table_conj_disj(table_conj_disj_10 )

######### conjunction   2 4 1 3, count the marg ##################
table_conj_disj_11 <- table_conj_disj_agg %>% 
  filter( `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`  ==  "2 4 1 3")

table_conj_disj_11 <- table_conj_disj(table_conj_disj_11 )

######### conjunction  2 4 3 1, count the marg ##################
table_conj_disj_12 <- table_conj_disj_agg %>% 
  filter( `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`  ==  "2 4 3 1")

table_conj_disj_12 <- table_conj_disj(table_conj_disj_12 )

######### conjunction  3 1 2 4, count the marg ##################
table_conj_disj_13 <- table_conj_disj_agg %>% 
  filter( `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`  ==  "3 1 2 4")

table_conj_disj_13 <- table_conj_disj(table_conj_disj_13 )

######### conjunction  3 1 4 2, count the marg ##################
table_conj_disj_14 <- table_conj_disj_agg %>% 
  filter( `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`  ==  "3 1 4 2")

table_conj_disj_14 <- table_conj_disj(table_conj_disj_14 )

######### conjunction  3 2 1 4, count the marg ##################
table_conj_disj_15 <- table_conj_disj_agg %>% 
  filter( `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`  ==  "3 2 1 4")

table_conj_disj_15 <- table_conj_disj(table_conj_disj_15 )

######### conjunction  3 2 4 1, count the marg ##################
table_conj_disj_16 <- table_conj_disj_agg %>% 
  filter( `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`  ==  "3 2 4 1")

table_conj_disj_16 <- table_conj_disj(table_conj_disj_16 )

######### conjunction  3 4 1 2, count the marg ##################
table_conj_disj_17 <- table_conj_disj_agg %>% 
  filter( `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`  ==  "3 4 1 2")

table_conj_disj_17 <- table_conj_disj(table_conj_disj_17 )

######### conjunction  3 4 2 1, count the marg ##################
table_conj_disj_18 <- table_conj_disj_agg %>% 
  filter( `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`  ==  "3 4 2 1")

table_conj_disj_18 <- table_conj_disj(table_conj_disj_18 )

######### conjunction  4 1 2 3, count the marg ##################
table_conj_disj_19 <- table_conj_disj_agg %>% 
  filter( `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`  ==  "4 1 2 3")

table_conj_disj_19 <- table_conj_disj(table_conj_disj_19 )

######### conjunction  4 1 3 2, count the marg ##################
table_conj_disj_20 <- table_conj_disj_agg %>% 
  filter( `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`  ==  "4 1 3 2")

table_conj_disj_20 <- table_conj_disj(table_conj_disj_20 )

######### conjunction  4 2 1 3, count the marg ##################
table_conj_disj_21 <- table_conj_disj_agg %>% 
  filter( `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`  ==  "4 2 1 3")

table_conj_disj_21 <- table_conj_disj(table_conj_disj_21 )

######### conjunction  4 2 3 1, count the marg ##################
table_conj_disj_22 <- table_conj_disj_agg %>% 
  filter( `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`  ==  "4 2 3 1")

table_conj_disj_22 <- table_conj_disj(table_conj_disj_22 )

######### conjunction  4 3 1 2, count the marg ##################
table_conj_disj_23 <- table_conj_disj_agg %>% 
  filter( `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`  ==  "4 3 1 2")

table_conj_disj_23 <- table_conj_disj(table_conj_disj_23 )

######### conjunction  4 3 2 1, count the marg ##################
table_conj_disj_24 <- table_conj_disj_agg %>% 
  filter( `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`  ==  "4 3 2 1")

table_conj_disj_24 <- table_conj_disj(table_conj_disj_24)

rank2_strict2_strict2_table_conj_disj <- rbind(table_conj_disj_1, table_conj_disj_2, table_conj_disj_3, table_conj_disj_4,
                                              table_conj_disj_5, table_conj_disj_6, table_conj_disj_7, table_conj_disj_8,
                                              table_conj_disj_9, table_conj_disj_10, table_conj_disj_11, table_conj_disj_12, 
                                              table_conj_disj_13, table_conj_disj_14, table_conj_disj_15, table_conj_disj_16,
                                              table_conj_disj_17, table_conj_disj_18, table_conj_disj_19, table_conj_disj_20,
                                              table_conj_disj_21, table_conj_disj_22, table_conj_disj_23, table_conj_disj_24)

sum(rank2_strict2_strict2_table_conj_disj$number) ### check 

write.csv(rank2_strict2_strict2_table_conj_disj, file = 'rank2_strict2_strict2_table_conj_disj.csv')
