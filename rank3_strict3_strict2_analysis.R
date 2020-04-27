library("tidyverse")
library("reshape2")
options(digits = 3)

rank3_strict3_strict2 <- read.csv("rank3_strict3_strict.csv", check.names=FALSE)
( rank3_strict3_strict2 <- rank3_strict3_strict2[, -1] )


(rank3_strict3_strict2_disj <- rank3_strict3_strict2 %>% group_by(`P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`) %>% count() ) ###24 rows
### there are 24 permutations in P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)

#################################################################################################################################
#################################################################################################################################
#################################################################################################################################

### given a permutation in P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A), count the frequency of each permutation in `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`

( rank3_strict3_strict2_disj_conj <- rank3_strict3_strict2 %>%
    group_by(`P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`, `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`) )

table_disj_conj_agg <- dcast(data=rank3_strict3_strict2_disj_conj,
                             `P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)` ~ `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`,
                             fun.aggregate = length,
                             value.var = "P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)")

table_disj_conj <- function (x) {
  x <- x[, colSums(x != 0) > 0]
  
  x <- x %>% 
    pivot_longer(-1 , names_to = "P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)", values_to = "number")
  
  x$freq <- x$number / sum(x$number)
  
  x <- tibble::rowid_to_column(x, "Count")
  x
}

######### conjunction 1 2 3 4, count the marg ##################
table_disj_conj_1 <- table_disj_conj_agg %>% 
  filter( `P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`  ==  "1 2 3 4" )

table_disj_conj_1 <- table_disj_conj(table_disj_conj_1 ) 


######### conjunction 1 2 4 3	, count the marg ##################
table_disj_conj_2 <- table_disj_conj_agg %>% 
  filter( `P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`  ==  "1 2 4 3" )

table_disj_conj_2 <- table_disj_conj(table_disj_conj_2 )

######### conjunction 1 3 2 4, count the marg ##################
table_disj_conj_3 <- table_disj_conj_agg %>% 
  filter( `P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`  ==  "1 3 2 4" )

table_disj_conj_3 <- table_disj_conj(table_disj_conj_3 )


######### conjunction 1 3 4 2, count the marg ##################
table_disj_conj_4 <- table_disj_conj_agg %>% 
  filter( `P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`  ==  "1 3 4 2" )

table_disj_conj_4 <- table_disj_conj(table_disj_conj_4 )


######### conjunction 1 4 2 3, count the marg ##################
table_disj_conj_5 <- table_disj_conj_agg %>% 
  filter( `P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`  ==  "1 4 2 3")

table_disj_conj_5 <- table_disj_conj(table_disj_conj_5 )



######### conjunction  1 4 3 2, count the marg ##################
table_disj_conj_6 <- table_disj_conj_agg %>% 
  filter( `P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`  ==  "1 4 3 2")

table_disj_conj_6 <- table_disj_conj(table_disj_conj_6 )


######### conjunction   2 1 3 4, count the marg ##################
table_disj_conj_7 <- table_disj_conj_agg %>% 
  filter( `P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`  ==  "2 1 3 4")

table_disj_conj_7 <- table_disj_conj(table_disj_conj_7 )


######### conjunction   2 1 4 3, count the marg ##################
table_disj_conj_8 <- table_disj_conj_agg %>% 
  filter( `P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`  ==  "2 1 4 3")

table_disj_conj_8 <- table_disj_conj(table_disj_conj_8 )

######### conjunction   2 3 1 4, count the marg ##################
table_disj_conj_9 <- table_disj_conj_agg %>% 
  filter( `P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`  ==  "2 3 1 4")

table_disj_conj_9 <- table_disj_conj(table_disj_conj_9 )

######### conjunction   2 3 4 1, count the marg ##################
table_disj_conj_10 <- table_disj_conj_agg %>% 
  filter( `P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`  ==  "2 3 4 1")

table_disj_conj_10 <- table_disj_conj(table_disj_conj_10 )

######### conjunction   2 4 1 3, count the marg ##################
table_disj_conj_11 <- table_disj_conj_agg %>% 
  filter( `P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`  ==  "2 4 1 3")

table_disj_conj_11 <- table_disj_conj(table_disj_conj_11 )

######### conjunction  2 4 3 1, count the marg ##################
table_disj_conj_12 <- table_disj_conj_agg %>% 
  filter( `P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`  ==  "2 4 3 1")

table_disj_conj_12 <- table_disj_conj(table_disj_conj_12 )

######### conjunction  3 1 2 4, count the marg ##################
table_disj_conj_13 <- table_disj_conj_agg %>% 
  filter( `P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`  ==  "3 1 2 4")

table_disj_conj_13 <- table_disj_conj(table_disj_conj_13 )

######### conjunction  3 1 4 2, count the marg ##################
table_disj_conj_14 <- table_disj_conj_agg %>% 
  filter( `P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`  ==  "3 1 4 2")

table_disj_conj_14 <- table_disj_conj(table_disj_conj_14 )

######### conjunction  3 2 1 4, count the marg ##################
table_disj_conj_15 <- table_disj_conj_agg %>% 
  filter( `P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`  ==  "3 2 1 4")

table_disj_conj_15 <- table_disj_conj(table_disj_conj_15 )

######### conjunction  3 2 4 1, count the marg ##################
table_disj_conj_16 <- table_disj_conj_agg %>% 
  filter( `P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`  ==  "3 2 4 1")

table_disj_conj_16 <- table_disj_conj(table_disj_conj_16 )

######### conjunction  3 4 1 2, count the marg ##################
table_disj_conj_17 <- table_disj_conj_agg %>% 
  filter( `P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`  ==  "3 4 1 2")

table_disj_conj_17 <- table_disj_conj(table_disj_conj_17 )

######### conjunction  3 4 2 1, count the marg ##################
table_disj_conj_18 <- table_disj_conj_agg %>% 
  filter( `P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`  ==  "3 4 2 1")

table_disj_conj_18 <- table_disj_conj(table_disj_conj_18 )

######### conjunction  4 1 2 3, count the marg ##################
table_disj_conj_19 <- table_disj_conj_agg %>% 
  filter( `P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`  ==  "4 1 2 3")

table_disj_conj_19 <- table_disj_conj(table_disj_conj_19 )

######### conjunction  4 1 3 2, count the marg ##################
table_disj_conj_20 <- table_disj_conj_agg %>% 
  filter( `P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`  ==  "4 1 3 2")

table_disj_conj_20 <- table_disj_conj(table_disj_conj_20 )

######### conjunction  4 2 1 3, count the marg ##################
table_disj_conj_21 <- table_disj_conj_agg %>% 
  filter( `P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`  ==  "4 2 1 3")

table_disj_conj_21 <- table_disj_conj(table_disj_conj_21 )

######### conjunction  4 2 3 1, count the marg ##################
table_disj_conj_22 <- table_disj_conj_agg %>% 
  filter( `P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`  ==  "4 2 3 1")

table_disj_conj_22 <- table_disj_conj(table_disj_conj_22 )

######### conjunction  4 3 1 2, count the marg ##################
table_disj_conj_23 <- table_disj_conj_agg %>% 
  filter( `P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`  ==  "4 3 1 2")

table_disj_conj_23 <- table_disj_conj(table_disj_conj_23 )

######### conjunction  4 3 2 1, count the marg ##################
table_disj_conj_24 <- table_disj_conj_agg %>% 
  filter( `P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`  ==  "4 3 2 1")

table_disj_conj_24 <- table_disj_conj(table_disj_conj_24)

rank3_strict3_strict2_table_disj_conj <- rbind(table_disj_conj_1, table_disj_conj_2, table_disj_conj_3, table_disj_conj_4,
                                              table_disj_conj_5, table_disj_conj_6, table_disj_conj_7, table_disj_conj_8,
                                              table_disj_conj_9, table_disj_conj_10, table_disj_conj_11, table_disj_conj_12, 
                                              table_disj_conj_13, table_disj_conj_14, table_disj_conj_15, table_disj_conj_16,
                                              table_disj_conj_17, table_disj_conj_18, table_disj_conj_19, table_disj_conj_20,
                                              table_disj_conj_21, table_disj_conj_22, table_disj_conj_23, table_disj_conj_24)

sum(rank3_strict3_strict2_table_disj_conj$number) ### check 

write.csv(rank3_strict3_strict2_table_disj_conj, file = 'rank3_strict3_strict2_table_disj_conj.csv', row.names = F)
