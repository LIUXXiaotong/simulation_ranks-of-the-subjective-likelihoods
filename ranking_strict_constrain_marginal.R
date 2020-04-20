library("tidyverse")

unique_rank <- read.csv("unique_rank.csv", check.names=FALSE)
( unique_rank <- unique_rank[, -1] )

###########################################################################################################################
### we ##1 constrain the marginal events
### strict ranking (without ties)

### filter the condition where there are ties
unique_rank_ties1 <- unique_rank %>% 
  filter(`P(A)` == `P(¬A)` | `P(A)` == `P(B)` | `P(A)` == `P(¬B)` 
         |`P(B)` == `P(¬A)`  | `P(B)` == `P(¬B)` | `P(¬A)` == `P(¬B)` )  

### derive a new dataframe  by substracting ties in marginal from unique_rank
rank1_strict1 <- setdiff(unique_rank, unique_rank_ties1) ###217 ties
  
### if there are no tied rankings among marginal events in the initial place, 
### how about conjunctions and disjunctions?
strict1_con_equal <- rank1_strict1 %>% 
  filter(`P(A^B)` == `P(A^¬B)` | `P(A^B)` == `P(¬A^B)` 
         | `P(A^B)` == `P(¬B^¬A)` |`P(¬A^B)` == `P(A^¬B)`  | `P(¬A^B)` == `P(¬B^¬A)` | `P(A^¬B)` == `P(¬B^¬A)`) 
          ###  103 cases of ties in conjunction

strict1_dis_equal <- rank1_strict1 %>% 
  filter(`P(A∨B)` == `P(A∨¬B)` | `P(A∨B)` == `P(¬A∨B)` 
         | `P(A∨B)` == `P(¬B∨¬A)` |`P(¬A∨B)` == `P(A∨¬B)`  | `P(¬A∨B)` == `P(¬B∨¬A)` | `P(A∨¬B)` == `P(¬B∨¬A)`) 
         ###  94 cases of ties in disjunction
         ### there are 44 overlaps where we see tied ranings in conj as well as dis 

### there are 41 overlaps where we see tied ranings in conj as well as dis 
217 - 61 - (103+94)

### no. of cases where rankings in all prob. expressions show no ties
### 61 cases
rank1_strict1_strict <- setdiff(rank1_strict1, strict1_con_equal)
( rank1_strict1_strict <- setdiff(rank1_strict1_strict, strict1_dis_equal) )

########################################################################################################
############################## allow ties in conjunction and disjunction ##############################
########################################################################################################

rank1_strict1$`P(A)_P(¬A)_P(B)_P(¬B)` <- 
  paste(rank1_strict1$`P(A)`, rank1_strict1$`P(¬A)`, rank1_strict1$`P(B)`, rank1_strict1$`P(¬B)`)

rank1_strict1$`P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)` <-
  paste(rank1_strict1$`P(A^B)`, rank1_strict1$`P(A^¬B)`, rank1_strict1$`P(¬A^B)`, rank1_strict1$`P(¬B^¬A)`)

rank1_strict1$`P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)` <- 
  paste(rank1_strict1$`P(A∨B)`, rank1_strict1$`P(A∨¬B)`, rank1_strict1$`P(¬A∨B)`, rank1_strict1$`P(¬B∨¬A)`)

( rank1_strict1 <- rank1_strict1 %>% 
  select(`P(A)_P(¬A)_P(B)_P(¬B)`, `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`, `P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`) ) ### 217 observations

(rank1_strict1_marg <- rank1_strict1 %>% group_by(`P(A)_P(¬A)_P(B)_P(¬B)`) %>% count()) ###8 rows

### there are 8 permutations in P(A)_P(¬A)_P(B)_P(¬B)
range(rank1_strict1_marg$n)
### given a permutation in P(A)_P(¬A)_P(B)_P(¬B), there are [25, 29] possibilities in terms of the ranks in conj and dis

write.csv(rank1_strict1, file = 'rank1_strict1.csv')
write.csv(rank1_strict1_marg, file = 'rank1_strict1_marg.csv') ### constrainting the marginal 
############---------------------------------------------------------

(rank1_strict1_marg_conj <- rank1_strict1 %>%
  group_by(`P(A)_P(¬A)_P(B)_P(¬B)`, `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`) %>%
  nest() %>% 
  group_by(`P(A)_P(¬A)_P(B)_P(¬B)`) %>% count() )

range(rank1_strict1_marg_conj$n)
### given a permutation in P(A)_P(¬A)_P(B)_P(¬B), there are 9-11 possibilities in terms of the rankings in conj

( rank1_strict1_marg_disj <- rank1_strict1 %>%
  group_by(`P(A)_P(¬A)_P(B)_P(¬B)`, `P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`) %>%
  nest() %>% 
  group_by(`P(A)_P(¬A)_P(B)_P(¬B)`) %>% count() )
range(rank1_strict1_marg_disj$n)
### given a permutation in P(A)_P(¬A)_P(B)_P(¬B), there are 11-14 possibilities in terms of the rankings in dis

write.csv(rank1_strict1_marg_conj, file = 'rank1_strict1_marg_conj.csv' ) ### constrainting the marginal, count the conj
write.csv(rank1_strict1_marg_disj, file = 'rank1_strict1_marg_disj.csv') ### constrainting the marginal, count the disj
###############----------------------------------------------------------------

( rank1_strict1_marg_and_conj <-  rank1_strict1 %>%
  group_by(`P(A)_P(¬A)_P(B)_P(¬B)`, `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`) %>%
  count() )

range(rank1_strict1_marg_and_conj$n)
## if we constrain marginal as well as conj, there are 1-6 possibilities in terms of the rankings in dis

( rank1_strict1_marg_and_disj <- rank1_strict1 %>%
  group_by(`P(A)_P(¬A)_P(B)_P(¬B)`, `P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`) %>%
  count() )

range(rank1_strict1_marg_and_disj$n)
## if we constraint marginal as well as disj, there are 1-4 possibilities in terms of the rankings in conj

write.csv(rank1_strict1_marg_and_conj, file = 'rank1_strict1_marg_and_conj.csv' ) ### constrainting the marginal and conj
write.csv(rank1_strict1_marg_and_disj, file = 'rank1_strict1_marg_and_disj.csv' ) ### constrainting the marginal and disj
################-------------------------------------------

########################################################################################################
############################## do not allow ties in conjunction and disjunction ##############################
########################################################################################################
rank1_strict1_strict$`P(A)_P(¬A)_P(B)_P(¬B)` <- 
  paste(rank1_strict1_strict$`P(A)`, rank1_strict1_strict$`P(¬A)`, rank1_strict1_strict$`P(B)`, rank1_strict1_strict$`P(¬B)`)

rank1_strict1_strict$`P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)` <-
  paste(rank1_strict1_strict$`P(A^B)`, rank1_strict1_strict$`P(A^¬B)`, rank1_strict1_strict$`P(¬A^B)`, rank1_strict1_strict$`P(¬B^¬A)`)

rank1_strict1_strict$`P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)` <- 
  paste(rank1_strict1_strict$`P(A∨B)`, rank1_strict1_strict$`P(A∨¬B)`, rank1_strict1_strict$`P(¬A∨B)`, rank1_strict1_strict$`P(¬B∨¬A)`)

( rank1_strict1_strict <- rank1_strict1_strict %>% 
  select(`P(A)_P(¬A)_P(B)_P(¬B)`, `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`, `P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`) )
#### 61 rows in the dataset

(rank1_strict1_strict_marg <- rank1_strict1_strict %>% group_by(`P(A)_P(¬A)_P(B)_P(¬B)`) %>% count())

### there are 8 permutations in P(A)_P(¬A)_P(B)_P(¬B)
range(rank1_strict1_strict_marg$n)
### given a permutation in P(A)_P(¬A)_P(B)_P(¬B), there are [7, 9] permutations in terms of combinations of the ranks in conj and dis

write.csv(rank1_strict1_strict, file = 'rank1_strict1_strict.csv')
write.csv(rank1_strict1_strict_marg, file = 'rank1_strict1_strict_marg.csv')
######################--------------------------------------------------------------

(rank1_strict1_strict_marg_conj <- rank1_strict1_strict %>%
    group_by(`P(A)_P(¬A)_P(B)_P(¬B)`, `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`) %>%
    nest() %>% 
    group_by(`P(A)_P(¬A)_P(B)_P(¬B)`) %>% count() )
range(rank1_strict1_strict_marg_conj$n)
### given a permutation in P(A)_P(¬A)_P(B)_P(¬B), there are 5 possibilities in terms of the rankings in conj

( rank1_strict1_strict_marg_disj <- rank1_strict1_strict %>%
    group_by(`P(A)_P(¬A)_P(B)_P(¬B)`, `P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`) %>%
    nest() %>% 
    group_by(`P(A)_P(¬A)_P(B)_P(¬B)`) %>% count() )
range(rank1_strict1_strict_marg_disj$n)
### given a specific permutation in P(A)_P(¬A)_P(B)_P(¬B), there are [5, 7] possibilities in terms of the rankings in dis
write.csv(rank1_strict1_strict_marg_conj, file = 'rank1_strict1_strict_marg_conj.csv' ) 
write.csv(rank1_strict1_strict_marg_disj, file = 'rank1_strict1_strict_marg_disj.csv') 
###################-----------------------------------------------------------------------


( rank1_strict1_strict_marg_and_conj <-  rank1_strict1_strict %>%
    group_by(`P(A)_P(¬A)_P(B)_P(¬B)`, `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`) %>%
    count() )
range(rank1_strict1_strict_marg_and_conj$n)
## if we constrain marginal as well as conj, there are 1-3 possibilities in terms of the rankings in dis

( rank1_strict1_strict_marg_and_disj <- rank1_strict1_strict %>%
    group_by(`P(A)_P(¬A)_P(B)_P(¬B)`, `P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`) %>%
    count() )
range(rank1_strict1_strict_marg_and_disj$n)
## if we constraint marginal as well as disj, there are 1-2 possibilities in terms of the rankings in conj

write.csv(rank1_strict1_marg_and_conj, file = 'rank1_strict1_marg_and_conj.csv' ) 
write.csv(rank1_strict1_marg_and_disj, file = 'rank1_strict1_marg_and_disj.csv' ) 

