library(tidyverse)
library(ggplot2)

gen_rule_matrix <- function(draw_coeff=1.0, win_coeff=2.0, loose_coeff=0.0){
  m <- matrix(0, nrow = 3, ncol = 3, 
              dimnames = list(
                c("Rock","Paper","Scissor"), 
                c("Rock","Paper","Scissor")
                )
              )
  
  m[1,] <- c(draw_coeff, loose_coeff, win_coeff)
  m[2,] <- c(win_coeff, draw_coeff, loose_coeff)
  m[3,] <- c(loose_coeff, win_coeff, draw_coeff)
  
  m
}

gen_even_population <- function(num_rock, num_paper, num_scissor){
  vec <- c(rep(0, num_rock), rep(1, num_paper), rep(2, num_scissor))
  len <- length(vec)
  head(vec, len - (len%%2) )
}

duel_matrix <- function(population){
  population <- sample(population)
  m <- matrix(0, nrow=3, ncol=3)
  if (length(population) == 0){
    return(m)
  }
  for (i in seq(1, length(population), by=2)){
    x <- population[i] + 1
    y <- population[i+1] + 1
    m[x, y] <- (m[x, y] + 1)
  }
  m + t(m)
}

run_simulation <- function(w_draw, w_win, w_loose, pop_rock, pop_paper, pop_scissor, runs){
  rule_matrix <- gen_rule_matrix(w_draw, w_win, w_loose)
  population <- gen_even_population(pop_rock,pop_paper,pop_scissor)
  
  for (i in 1:runs){
    new_pop <- (rule_matrix*duel_matrix(population))%*%c(1,1,1)
    population <- gen_even_population(new_pop[1],new_pop[2],new_pop[3])
  }
  new_pop
}

