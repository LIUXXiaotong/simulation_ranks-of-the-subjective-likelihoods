
library(tidyverse)
options(digits = 3)



likelihood <- function (b, a_b, a_B) { ##input: P(B) & P(A|B) & P(A|¬B)
  if ( (a_B *(1-b) + a_b * b) <= 1 ) { ## probability axiom 0 <= P(A) <= 1
    a = a_b * b + a_B * (1-b)     ## P(A) = P(A|B)*P(B) + P(A|¬B)*P(¬B)
    #### --------------------- 
    ### calculate marginal likelihoods
    marginal_value = numeric(4)
    names(marginal_value) <- c("P(A)", "P(¬A)", "P(B)", "P(¬B)")
    marginal_value[1] = a
    marginal_value[2] = 1-a
    marginal_value[3] = b
    marginal_value[4] = 1-b
    
    ### --------------------------------
    ### calculate conditional likelihood and consjunctions 
    ### notice: when p(y) = 0, p(x|y) = NaN (undefined)
    conjunction_value = numeric(4)
    names(conjunction_value) <- c("P(A^B)", "P(A^¬B)", "P(¬A^B)", "P(¬B^¬A)")
    
    
    if (a > 0 && a < 1) {
      
      ### P(¬B|A) 
      ### Math: P(¬B|A) * P(A) = P(A|¬B) * P(¬B)
      B_a = a_B * (1-b) / a
      ### P(B|A) 
      ### P(B|A) * P(A) = P(A|B) * P(B)
      b_a = a_b*b/a
      ### P(B|¬A)
      ## Math P(B) = P(B|A)*A + P(B|¬A)*P(¬A)
      b_A = (b - b_a*a) / (1-a)
      ### P(¬B|¬A) 
      ### Math: P(¬B) = P(¬B|A) * P(A) + P(¬B|¬A) * P(¬A)
      B_A = ( (1-b) - B_a * a ) / (1-a)
      
      ###  consjuctions 
      conjunction_value[1] = a_b*b
      conjunction_value[2] = a_B*(1-b)
      conjunction_value[3] = b_A*(1-a)
      conjunction_value[4] = B_A*(1-a)
    } else if (a == 0) {
      conjunction_value[1] = 0
      conjunction_value[2] = 0
      conjunction_value[3] = b ### P(¬A) = 1; Math: P(¬A^B) = P(B|¬A) * P(¬A) = P(B)
      conjunction_value[4] = (1-b) ### P(¬A) = 1; Math: P(¬A^¬B) = P(¬B|¬A) * P(¬A) = P(¬B)
    } else { ### a = 1
      conjunction_value[1] = a_b*b
      conjunction_value[2] = a_B*(1-b) ### notice: P(A) = P(A^B) + P(A^¬B) = 1 here 
      conjunction_value[3] = 0
      conjunction_value[4] = 0   
    }
    
    
    ### disjunctios 
    disjunction_value = numeric(4)
    names(disjunction_value) <- c("P(A∨B)", "P(A^∨B)", "P(¬A∨B)", "P(¬B∨¬A)")
    
    disjunction_value[1] = a + b - conjunction_value["P(A^B)"] ## math: P(A∨B) = P(A) + P(B) - P(A^B)
    disjunction_value[2] = a + (1-b) - conjunction_value["P(A^¬B)"]
    disjunction_value[3] = (1-a) + b - conjunction_value["P(¬A^B)"]
    disjunction_value[4] = (1-a) + (1-b) - conjunction_value["P(¬B^¬A)"]
    
    c(marginal_value, conjunction_value,  disjunction_value )
  }
}


b <- seq(0, 1, by = 0.1) 
length(b)


a_b <- seq(0, 1, by = 0.1)
length(a_b)

a_B <- seq(0, 1, by = 0.1)
length(a_B)



likelihood_value = matrix (0, length(b)*length(a_b)*length(a_B), 12)
colnames(likelihood_value) <- c("P(A)", "P(¬A)", "P(B)", "P(¬B)", "P(A^B)", "P(A^¬B)", "P(¬A^B)", "P(¬B^¬A)" ,"P(A∨B)", "P(A^∨B)", "P(¬A∨B)", "P(¬B∨¬A)")


m <- 1

for (i in seq_along(b)) {
  for (j in seq_along(a_b)) {
    for (k in seq_along(a_B)) {
      likelihood_value[m, ] <- likelihood(b[i], a_b[j], a_B[k]) 
      m = m + 1 
    }
  }
}

（ likelihood_value <- likelihood_value %>% data.frame() ）### issue: P(¬A^B), P(¬A^B) can be below 0 ?
