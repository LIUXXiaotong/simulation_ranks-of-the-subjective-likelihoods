unique_rank <- read.csv("unique_rank.csv", check.names=FALSE)
 unique_rank <- unique_rank[, -1] 

#################################################################################################################
### we ##3 constrain the disjunction events
### strict ranking (without ties)

### filter the condition where there are ties
unique_rank_ties3 <- unique_rank %>% 
  filter(`P(A∨B)` == `P(A∨¬B)` | `P(A∨B)` == `P(¬A∨B)` 
         | `P(A∨B)` == `P(¬B∨¬A)` |`P(¬A∨B)` == `P(A∨¬B)`  | `P(¬A∨B)` == `P(¬B∨¬A)` | `P(A∨¬B)` == `P(¬B∨¬A)` )  
### there are 185 cases of ties in disjunctions

### derive a new dataframe by substracting ties in disjunctions from unique_rank
 rank3_strict3 <- setdiff(unique_rank, unique_rank_ties3) ### left 177 observations 

### if there are no tied rankings among disjunctions in the initial place, 
### how about marginals and conjunctions?
strict3_marg_equal <- rank3_strict3 %>% 
  filter(`P(A)` == `P(¬A)` | `P(A)` == `P(B)` | `P(A)` == `P(¬B)` 
         |`P(B)` == `P(¬A)`  | `P(B)` == `P(¬B)` | `P(¬A)` == `P(¬B)`) 
### 54 cases of ties in marg

strict3_conj_equal <- rank3_strict3 %>% 
  filter(`P(A^B)` == `P(A^¬B)` | `P(A^B)` == `P(¬A^B)` 
         | `P(A^B)` == `P(¬B^¬A)` |`P(¬A^B)` == `P(A^¬B)`  | `P(¬A^B)` == `P(¬B^¬A)` | `P(A^¬B)` == `P(¬B^¬A)`) 
###  70 cases of ties in disj

### there are 8 overlaps where we see tied ranings in both marg and disj
177-61-(54+70)


### no. of cases where rankings in all prob. expressions show no ties
rank3_strict3_strict <- setdiff(rank3_strict3, strict3_marg_equal)
rank3_strict3_strict <- setdiff(rank3_strict3_strict, strict3_conj_equal)
### 61 cases

########################################################################################################
############################## allow ties in marginals and conjunctions  ##############################
##############################   work on the dataset rank3_strict3      ##############################
########################################################################################################

rank3_strict3$`P(A)_P(¬A)_P(B)_P(¬B)` <- 
  paste(rank3_strict3$`P(A)`, rank3_strict3$`P(¬A)`, rank3_strict3$`P(B)`, rank3_strict3$`P(¬B)`)

rank3_strict3$`P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)` <-
  paste(rank3_strict3$`P(A^B)`, rank3_strict3$`P(A^¬B)`, rank3_strict3$`P(¬A^B)`, rank3_strict3$`P(¬B^¬A)`)

rank3_strict3$`P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)` <- 
  paste(rank3_strict3$`P(A∨B)`, rank3_strict3$`P(A∨¬B)`, rank3_strict3$`P(¬A∨B)`, rank3_strict3$`P(¬B∨¬A)`)

( rank3_strict3 <- rank3_strict3 %>% 
  select(`P(A)_P(¬A)_P(B)_P(¬B)`, `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`, `P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`) ) ###177 observations

(rank3_strict3_disj <- rank3_strict3 %>% group_by(`P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`) %>% count())
### there are 24 permutations in P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)

range(rank3_strict3_disj$n)
### given a permutation in P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A), 
### there are [3, 1]1 possibilities in terms of the ranks in marg and conj

write.csv(rank3_strict3, file = 'rank3_strict3.csv')
write.csv(rank3_strict3_disj, file = 'rank3_strict3_disj.csv')
### ---------------------------------------------------------------------------------


(rank3_strict3_disj_marg <- rank3_strict3 %>%
    group_by(`P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`, `P(A)_P(¬A)_P(B)_P(¬B)`) %>%
    nest() %>% 
    group_by(`P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`) %>% count() )

range(rank3_strict3_disj_marg$n)
### given a permutation in P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A), 
### there are 1-8 possibilities in terms of the rankings in marg


( rank3_strict3_disj_conj <- rank3_strict3 %>%
    group_by(`P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`, `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`) %>%
    nest() %>% 
    group_by(`P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`) %>% count() )

range(rank3_strict3_disj_conj$n)
### given a permutation in P(A)_P(¬A)_P(B)_P(¬B), 
### there are 3-5 possibilities in terms of the rankings in conj

write.csv(rank3_strict3_disj_marg, file = 'rank3_strict3_disj_marg.csv' ) 
### constrainting the disj, count the marg
write.csv(rank3_strict3_disj_conj, file = 'rank3_strict3_disj_conj.csv') 
### constrainting the disj, count the conj
#############--------------------------------------------------------


( rank3_strict3_disj_and_marg <-  rank3_strict3 %>%
    group_by(`P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`, `P(A)_P(¬A)_P(B)_P(¬B)`) %>%
    count() )
range(rank3_strict3_disj_and_marg$n)
## if we constrain disj as well as marg, there are 1-4 possibilities in terms of the rankings in conj

( rank3_strict3_disj_and_conj <- rank3_strict3 %>%
    group_by(`P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`, `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`) %>%
    count() )
range(rank3_strict3_disj_and_conj $n)
## if we constraint disj as well as conj, there are 1-3 possibilities in terms of the rankings in marg

write.csv(rank3_strict3_disj_and_marg, file = 'rank3_strict3_disj_and_marg.csv' ) 
### constrainting the disj and marginal
write.csv(rank3_strict3_disj_and_conj, file = 'rank3_strict3_disj_and_conj.csv' ) 
### constrainting the disj and conj
############----------------------------------------------------------



########################################################################################################
##############################  do not allow ties in marginal and conjunction  #########################
##############################      work on the data rank3_strict3_strict      #########################
########################################################################################################
rank3_strict3_strict$`P(A)_P(¬A)_P(B)_P(¬B)` <- 
  paste(rank3_strict3_strict$`P(A)`, rank3_strict3_strict$`P(¬A)`, rank3_strict3_strict$`P(B)`, rank3_strict3_strict$`P(¬B)`)

rank3_strict3_strict$`P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)` <-
  paste(rank3_strict3_strict$`P(A^B)`, rank3_strict3_strict$`P(A^¬B)`, rank3_strict3_strict$`P(¬A^B)`, rank3_strict3_strict$`P(¬B^¬A)`)

rank3_strict3_strict$`P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)` <- 
  paste(rank3_strict3_strict$`P(A∨B)`, rank3_strict3_strict$`P(A∨¬B)`, rank3_strict3_strict$`P(¬A∨B)`, rank3_strict3_strict$`P(¬B∨¬A)`)

( rank3_strict3_strict <- rank3_strict3_strict %>% 
  select(`P(A)_P(¬A)_P(B)_P(¬B)`, `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`, `P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`) )
#### 61 rows in the dataset

(rank3_strict3_strict_disj <- rank3_strict3_strict %>% group_by(`P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`) %>% count())
### there are 24 permutations in P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)

range(rank3_strict3_strict_disj$n)
### given a permutation in P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A), 
### there are [1, 4] permutations in terms of combinations of the ranks in marg and conj

write.csv(rank3_strict3_strict, file = 'rank3_strict3_strict.csv')
write.csv(rank3_strict3_strict_disj, file = 'rank3_strict3_strict_disj.csv')
########------------------------------------------------------------------------

(rank3_strict3_strict_disj_marg <- rank3_strict3_strict %>%
    group_by(`P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`, `P(A)_P(¬A)_P(B)_P(¬B)`) %>%
    nest() %>% 
    group_by(`P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`) %>% count() )

range(rank3_strict3_strict_disj_marg$n)
### given a permutation in P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A),
### there are [1, 3] possibilities in terms of the rankings in marg


( rank3_strict3_strict_disj_conj <- rank3_strict3_strict %>%
    group_by(`P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`, `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`) %>%
    nest() %>% 
    group_by(`P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`) %>% count() )

range(rank3_strict3_strict_disj_conj$n)
### given a specific permutation in P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A), 
### there are [1, 3] possibilities in terms of the rankings in conj

write.csv(rank3_strict3_strict_disj_marg, file = 'rank3_strict3_strict_disj_marg.csv' ) 
write.csv(rank3_strict3_strict_disj_conj, file = 'rank3_strict3_strict_disj_conj.csv') 
###########--------------------------------------------------------------------------


( rank3_strict3_strict_disj_and_marg <-  rank3_strict3_strict %>%
    group_by(`P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`, `P(A)_P(¬A)_P(B)_P(¬B)`) %>%
    count() )

range(rank3_strict3_strict_disj_and_marg$n)
## if we constrain disj as well as marg, there are 1-2 possibilities in terms of the rankings in conj 

( rank3_strict3_strict_disj_and_conj <- rank3_strict3_strict %>%
    group_by(`P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`, `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`) %>%
    count() )

range(rank3_strict3_strict_disj_and_conj$n)
## if we constraint disj as well as conj, there are 1-2 possibilities in terms of the rankings in marg

write.csv(rank3_strict3_strict_disj_and_marg, file = 'rank3_strict3_strict_disj_and_marg.csv' ) 
write.csv(rank3_strict3_strict_disj_and_conj, file = 'rank3_strict3_strict_disj_and_conj.csv' ) 

