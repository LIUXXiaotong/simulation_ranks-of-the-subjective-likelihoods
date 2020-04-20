unique_rank <- read.csv("unique_rank.csv", check.names=FALSE)
 unique_rank <- unique_rank[, -1] 

#################################################################################################################
### we ##2 constrain the conjunction events
### strict ranking (without ties)

### filter the condition where there are ties
unique_rank_ties2 <- unique_rank %>% 
  filter(`P(A^B)` == `P(A^¬B)` | `P(A^B)` == `P(¬A^B)` | `P(A^B)` == `P(¬B^¬A)` 
         |`P(¬A^B)` == `P(A^¬B)`  | `P(¬A^B)` == `P(¬B^¬A)` | `P(A^¬B)` == `P(¬B^¬A)` )   ### there are 159 ties

### derive a new dataframe by substracting ties in conjunction from unique_rank
rank2_strict2 <- setdiff(unique_rank, unique_rank_ties2)  ### left 203 cases without ties



##### ------------------------------------------------
### if there are no tied rankings among conjunction events in the initial place, 
### how about marginal and disjunctions?
### marginal
strict2_marg_equal <- rank2_strict2 %>% 
  filter(`P(A)` == `P(¬A)` | `P(A)` == `P(B)` | `P(A)` == `P(¬B)` |`P(B)` == `P(¬A)`  | `P(B)` == `P(¬B)` | `P(¬A)` == `P(¬B)`) 
###  89 cases of ties in marginal

#### disjunction
strict2_disj_equal <- rank2_strict2 %>% 
  filter(`P(A∨B)` == `P(A∨¬B)` | `P(A∨B)` == `P(¬A∨B)` 
         | `P(A∨B)` == `P(¬B∨¬A)` |`P(¬A∨B)` == `P(A∨¬B)`  | `P(¬A∨B)` == `P(¬B∨¬A)` | `P(A∨¬B)` == `P(¬B∨¬A)`) 
###  96 cases of ties in disjunctions

### there are 63 overlaps where we see tied ranings in conj as well as dis 
### there are 43 overlaps where we see tied ranings in both marg and disj
203-61 - (89+96)

### no. of cases where rankings in all prob. expressions show no ties
### 61 cases
rank2_strict2_strict <- setdiff(rank2_strict2, strict2_marg_equal)
rank2_strict2_strict <- setdiff(rank2_strict2_strict, strict2_disj_equal)



########################################################################################################
############################## allow ties in marginal and disjunction ##############################
##############################   work on the dataset rank2_strict2      ##############################
########################################################################################################

rank2_strict2$`P(A)_P(¬A)_P(B)_P(¬B)` <- 
  paste(rank2_strict2$`P(A)`, rank2_strict2$`P(¬A)`, rank2_strict2$`P(B)`, rank2_strict2$`P(¬B)`)

rank2_strict2$`P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)` <-
  paste(rank2_strict2$`P(A^B)`, rank2_strict2$`P(A^¬B)`, rank2_strict2$`P(¬A^B)`, rank2_strict2$`P(¬B^¬A)`)

rank2_strict2$`P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)` <- 
  paste(rank2_strict2$`P(A∨B)`, rank2_strict2$`P(A∨¬B)`, rank2_strict2$`P(¬A∨B)`, rank2_strict2$`P(¬B∨¬A)`)

rank2_strict2 <- rank2_strict2 %>% 
  select(`P(A)_P(¬A)_P(B)_P(¬B)`, `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`, `P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`) 


(rank2_strict2_conj <- rank2_strict2 %>% group_by(`P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`) %>% count())
### there are 24 permutations in P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)
range(rank2_strict2_conj$n)
### given a permutation in P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A), there are [3, 16] possibilities 
### in terms of the combinations of the ranks in marg and dis

write.csv(rank2_strict2, file = 'rank2_strict2.csv')
write.csv(rank2_strict2_conj, file = 'rank2_strict2_conj.csv')
###########---------------------------------------------------------

(rank2_strict2_conj_marg <- rank2_strict2 %>%
    group_by(`P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`, `P(A)_P(¬A)_P(B)_P(¬B)`) %>%
    nest() %>% 
    group_by(`P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`) %>% count() )
range(rank2_strict2_conj_marg$n)
### given a permutation in P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A), there are 2-7 possibilities in terms of the rankings in marg

( rank2_strict2_conj_disj <- rank2_strict2 %>%
    group_by(`P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`, `P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`) %>%
    nest() %>% 
    group_by(`P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`) %>% count() )
range(rank2_strict2_conj_disj$n)
### given a permutation in `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`, there are 2-7 possibilities in terms of the rankings in dis

write.csv(rank2_strict2_conj_marg, file = 'rank2_strict2_conj_marg.csv' ) ### constrainting the conj, count the marg
write.csv(rank2_strict2_conj_disj, file = 'rank2_strict2_conj_disj.csv') ### constrainting the conj, count the disj
#############--------------------------------------------------------


( rank2_strict2_conj_and_marg <-  rank2_strict2 %>%
    group_by(`P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`, `P(A)_P(¬A)_P(B)_P(¬B)`) %>%
    count() )
range(rank2_strict2_conj_and_marg$n)
## if we constrain conjunction as well as marginal, there are 1-6 possibilities in terms of the rankings in dis

( rank2_strict2_conj_and_disj <- rank2_strict2 %>%
    group_by(`P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`, `P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`) %>%
    count() )
range(rank2_strict2_conj_and_disj$n)
## if we constraint conjunction as well as disjunction, there are 1-4 possibilities in terms of the rankings in marg

write.csv(rank2_strict2_conj_and_marg, file = 'rank2_strict2_conj_and_marg.csv' ) ### constrainting the conj and marginal
write.csv(rank2_strict2_conj_and_disj, file = 'rank2_strict2_conj_and_disj.csv' ) ### constrainting the conj and disj

############----------------------------------------------------------

########################################################################################################
############################## do not allow ties in conjunction and disjunction ########################
############################  work on the dataset rank2_strict2_strict      ###########################
########################################################################################################
rank2_strict2_strict$`P(A)_P(¬A)_P(B)_P(¬B)` <- 
  paste(rank2_strict2_strict$`P(A)`, rank2_strict2_strict$`P(¬A)`, rank2_strict2_strict$`P(B)`, rank2_strict2_strict$`P(¬B)`)

rank2_strict2_strict$`P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)` <-
  paste(rank2_strict2_strict$`P(A^B)`, rank2_strict2_strict$`P(A^¬B)`, rank2_strict2_strict$`P(¬A^B)`, rank2_strict2_strict$`P(¬B^¬A)`)

rank2_strict2_strict$`P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)` <- 
  paste(rank2_strict2_strict$`P(A∨B)`, rank2_strict2_strict$`P(A∨¬B)`, rank2_strict2_strict$`P(¬A∨B)`, rank2_strict2_strict$`P(¬B∨¬A)`)

( rank2_strict2_strict <- rank2_strict2_strict %>% 
  select(`P(A)_P(¬A)_P(B)_P(¬B)`, `P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`, `P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`) )
#### 61 rows in the dataset

(rank2_strict2_strict_conj <- rank2_strict2_strict %>% group_by(`P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`) %>% count())
###  ### there are 24 permutations in P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)

range(rank2_strict2_strict_conj$n)
### given a permutation in P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A), there are [1, 4] permutations 
### in terms of combinations of the ranks in marg and dis

write.csv(rank2_strict2_strict, file = 'rank2_strict2_strict.csv')
write.csv(rank2_strict2_strict_conj, file = 'rank2_strict2_strict_conj.csv')
########### ---------------------------------------------------------------------------------------------

(rank2_strict2_strict_conj_marg <- rank2_strict2_strict %>%
    group_by(`P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`, `P(A)_P(¬A)_P(B)_P(¬B)`) %>%
    nest() %>% 
    group_by(`P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`) %>% count() )
range(rank2_strict2_strict_conj_marg$n)
### given a permutation in P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A), 
### there are [1, 2] possibilities in terms of the rankings in marg

( rank2_strict2_strict_conj_disj <- rank2_strict2_strict %>%
    group_by(`P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`, `P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`) %>%
    nest() %>% 
    group_by(`P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`) %>% count() )
range(rank2_strict2_strict_conj_disj$n)
### given a specific permutation in P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A), 
### there are [1, 3] possibilities in terms of the rankings in dis

write.csv(rank2_strict2_strict_conj_marg, file = 'rank2_strict2_strict_conj_marg.csv' ) 
write.csv(rank2_strict2_strict_conj_disj, file = 'rank2_strict2_strict_conj_disj.csv') 
########### --------------------------------------------------------------------------------------


( rank2_strict2_strict_conj_and_marg <-  rank2_strict2_strict %>%
    group_by(`P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`, `P(A)_P(¬A)_P(B)_P(¬B)`) %>%
    count() )
range(rank2_strict2_strict_conj_and_marg$n)
## if we constrain conj as well as marg, there are 1-3 possibilities in terms of the rankings in dis

( rank2_strict2_strict_conj_and_disj <- rank2_strict2_strict %>%
    group_by(`P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`, `P(A∨B)_P(A∨¬B)_P(¬A∨B)_P(¬B∨¬A)`) %>%
    count() )
range(rank2_strict2_strict_conj_and_marg$n)
## if we constraint conj as well as disj, there are 1-3 possibilities in terms of the rankings in marg

write.csv(rank2_strict2_strict_conj_and_marg, file = 'rank2_strict2_strict_conj_and_marg.csv' ) 
write.csv( rank2_strict2_strict_conj_and_disj, file = 'rank2_strict2_strict_conj_and_disj.csv' ) 

