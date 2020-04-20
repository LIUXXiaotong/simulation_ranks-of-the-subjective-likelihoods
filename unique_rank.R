library(tidyverse)
options(digits = 4) ### reason: the after the mathmatical operations, the maximum digit we have is four

### rank function 
myRank <- function(x) {
  rank(-x, ties.method = "average")
}

ranking <- function (b, a_b, a_B) { ##input: P(B) & P(A|B) & P(A|¬B)
  a = a_b * b + a_B * (1-b)     ## P(A) = P(A|B)*P(B) + P(A|¬B)*P(¬B)
  #### --------------------- 
  ### calculate simple likelihoods
  marginal_value = numeric(4)
  names(marginal_value) <- c("P(A)", "P(¬A)", "P(B)", "P(¬B)")
  marginal_value[1] = a
  marginal_value[2] = 1-a
  marginal_value[3] = b
  marginal_value[4] = 1-b
  
  rank_marginal <- myRank(marginal_value)
  
  ### conditional likelihoods
  ###P(¬A|B)
  A_b = 1 - a_b
  ###P(¬A|¬B)
  A_B = 1 - a_B
  
  ### --------------------------------
  ### calculate consjunctions 
  conjunction_value = numeric(4)
  names(conjunction_value) <- c("P(A^B)", "P(A^¬B)", "P(¬A^B)", "P(¬B^¬A)")
  
  
  ###  conjuctions 
  conjunction_value[1] = a_b*b
  conjunction_value[2] = a_B*(1-b)
  conjunction_value[3] = A_b*b
  conjunction_value[4] = A_B*(1-b)
  
  
  ### disjunctios 
  disjunction_value = numeric(4)
  names(disjunction_value) <- c("P(A∨B)", "P(A∨¬B)", "P(¬A∨B)", "P(¬B∨¬A)")
  disjunction_value[1] = a + b - conjunction_value["P(A^B)"] ## math: P(A∨B) = P(A) + P(B) - P(A^B)
  disjunction_value[2] = a + (1-b) - conjunction_value["P(A^¬B)"]
  disjunction_value[3] = (1-a) + b - conjunction_value["P(¬A^B)"]
  disjunction_value[4] = (1-a) + (1-b) - conjunction_value["P(¬B^¬A)"]
  
  ### ---------------------- ranking the prob
  rank_conjunction = myRank(conjunction_value) 
  rank_disjunction = myRank(disjunction_value) 
  
  return( c(rank_marginal,  rank_conjunction, rank_disjunction) )
} 


b <- seq(0, 1, by = 0.01) 
length(b)


a_b <- seq(0, 1, by = 0.01)
length(a_b)

a_B <- seq(0, 1, by = 0.01)
length(a_B)



rank_value = matrix (0, length(b)*length(a_b)*length(a_B), 12)
colnames(rank_value) <- c("P(A)", "P(¬A)", "P(B)", "P(¬B)", 
                          "P(A^B)", "P(A^¬B)", "P(¬A^B)", "P(¬B^¬A)" ,
                          "P(A∨B)", "P(A∨¬B)", "P(¬A∨B)", "P(¬B∨¬A)")


m <- 1

for ( i in seq_along(b) ) {
  for ( k in seq_along(a_b) ) {
    for ( j in seq_along(a_B) ) {
      rank_value[m, ] <- ranking(b[i], a_b[k], a_B[j])
      m = m + 1 
    }
  }
}

( unique_rank <- unique(rank_value) %>% as.data.frame() )

write.csv(unique_rank, file = 'unique_rank.csv')


