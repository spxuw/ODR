library(dplyr) 
library(dietaryindex)
setwd("/code_path")

rm(list = ls())

source("simulated_annealing_combined.R")

Demo_ASA24 <- read.csv(file = "Demo_ASA24.csv", header = T)

ASA_summary <- Demo_ASA24 %>%
  group_by(UserID, RecallNo) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = 'drop')

score <- "HEI2015" # diet score to be optimized

candidate <- 1 # candidate subject whose diet will be optimized (specific day). Subject "MCTs01" at day 1 (based on ASA_summary)


solution <- simulated_annealing_combined(diet = Demo_ASA24, candidate = candidate, niter = 20, bound = 0.4)


dat = data.frame(original_diet=as.numeric(solution$initial_score[candidate,4:17]),recommend_diet=as.numeric(solution$final_score[candidate,4:17]),
                   food=colnames(solution$final_score)[4:17])


plot(solution$alltemp,xlab="Iteration", ylab="Diet score")  
