simulated_annealing_combined <- function(index, niter = 5000, step = 0.1, bound = 0.4) {
  library(dietaryindex)
  
  normalized_euclidean_distance <- function(x, y) {
    x_mean <- mean(x)
    numerator <- sum((x - y)^2)
    denominator <- sum((x - x_mean)^2)
    distance_normalized <- sqrt(numerator / denominator)
    return(distance_normalized)
  }
  
  MCTs_23887_Items = read.csv(file = "../data/MCTs_23887_Items.csv",header = T,sep = ",")
  MCTs_23887_Items <- MCTs_23887_Items %>%
    mutate(Occ_Name = case_when(
      Occ_Name == 1 ~ "Breakfast",
      Occ_Name == 2 ~ "Brunch",
      Occ_Name == 3 ~ "Lunch",
      Occ_Name == 4 ~ "Dinner",
      Occ_Name == 5 ~ "Supper",
      Occ_Name == 6 ~ "Snack",
      Occ_Name == 7 ~ "Just a Drink",
      Occ_Name == 8 ~ "Just a Supplement",
      TRUE ~ "Other"
    ))
  
  MCTs_23887_Items$RecallNo = 1
  MCTs_23887_Items$RecallAttempt = 0
  MCTs_23887_Items$RecallStatus = 2
  MCTs_23887_Items_clean <- na.omit(MCTs_23887_Items)
  MCTs_23887_Items_clean['RecallNo'] <- MCTs_23887_Items_clean$RecordDayNo
  MCTs_23887_Items_clean$UserID = paste(MCTs_23887_Items_clean$UserID,MCTs_23887_Items_clean$RecallNo,sep = "_")
  MCTs_23887_Items_clean$UserName = paste(MCTs_23887_Items_clean$UserName,MCTs_23887_Items_clean$RecallNo,sep = "_")
  
  s_0 = MCTs_23887_Items_clean
    
  df_sum <- MCTs_23887_Items_clean %>%
    group_by(UserID, RecallNo) %>%
    summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = 'drop')
  df_sum = df_sum[,-c(13)]
  #colnames(df_sum)[c(1,2)] = c("UserID","RecallNo")
  df_sum['UserName'] = MCTs_23887_Items_clean$UserName[match(df_sum$UserID,MCTs_23887_Items_clean$UserID)]
  #df_sum$UserID = paste(df_sum$UserID,df_sum$RecallNo,sep = "_")
  #df_sum$UserName = paste(df_sum$UserName,df_sum$RecallNo,sep = "_")
  
  if (indices == "HEI2015"){
    EE = HEI2015_ASA24(df_sum,RECALL_SUMMARIZE=FALSE)
    HEI_min = which.min(EE$HEI2015_ALL)
    score = EE$HEI2015_ALL[HEI_min]
  }
  if (indices == "MED"){
    EE = MED_ASA24(df_sum,RECALL_SUMMARIZE=FALSE)
    HEI_min = which.min(EE$MED_ALL)
    score = EE$MED_ALL[HEI_min]
  }
  if (indices == "DII"){
    EE =  DII_ASA24(df_sum[19,],RECALL_SUMMARIZE = FALSE)
    HEI_min = 19
    score = 5-EE$DII_ALL[1]
  }
  
  EE_initial = EE
  
  s_b <- s_c <- s_n <- MCTs_23887_Items_clean
  f_b <- f_c <- f_n <- (100 - score)
  alltemp = 100 - f_c
  
  k = 1
  Temp0 = 500
  while ((k<niter)) {
    set.seed(k)
    s_n <- s_c
    Temp <- Temp0*0.9999^k
    strategy = sample(1:3,1)
    
    if (strategy == 1){
      rows_i <- which(s_n$UserID==df_sum$UserID[HEI_min] & s_n$RecallNo==df_sum$RecallNo[HEI_min])
      set.seed(k)
      selected_i <- sample(1:length(rows_i),1)
      same_occasions <- s_n[s_n$Occ_Name==s_n$Occ_Name[rows_i[selected_i]],]
      set.seed(k)
      selected_noni <- sample(1:nrow(same_occasions),1)
      s_n[rows_i[selected_i],16:129] = same_occasions[selected_noni,16:129]
    }
    if (strategy==2){
      rows_i <- which(s_n$UserID==df_sum$UserID[HEI_min] & s_n$RecallNo==df_sum$RecallNo[HEI_min])
      set.seed(k)
      selected_i <- sample(1:length(rows_i),1)
      set.seed(k)
      selected_noni <- sample(1:nrow(s_n),1)
      s_n = rbind(s_n,s_c[rows_i[selected_i],])
      s_n[nrow(s_n),16:129] = s_n[selected_noni,16:129]
    }
    # consider a random neighbor: delete
    if ((strategy==3)){
      rows_i <- which(s_n$UserID==df_sum$UserID[HEI_min] & s_n$RecallNo==df_sum$RecallNo[HEI_min])
      set.seed(k)
      selected_i <- sample(1:length(rows_i),1)
      s_n = s_n[-rows_i[selected_i],] 
    }
    
    df_sum <- s_n %>%
      group_by(UserID, RecallNo) %>%
      summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = 'drop')
    df_sum = df_sum[,-c(13)]
    #colnames(df_sum)[c(1,2)] = c("UserID","RecallNo")
    df_sum['UserName'] = MCTs_23887_Items_clean$UserName[match(df_sum$UserID,MCTs_23887_Items_clean$UserID)]
    
    if (indices == "HEI2015"){
      EE = HEI2015_ASA24(df_sum,RECALL_SUMMARIZE=FALSE)
      score_f = EE$HEI2015_ALL[HEI_min]
    }
    if (indices == "MED"){
      EE = MED_ASA24(df_sum,RECALL_SUMMARIZE=FALSE)
      score_f = EE$MED_ALL[HEI_min]
    }
    if (indices == "DII"){
      EE =  DII_ASA24(df_sum[19,],RECALL_SUMMARIZE=FALSE)
      score_f = 5-EE$DII_ALL[1]
    }
    
    real_counts <- table(s_n$Occ_Name[rows_i])
    real_counts_df <- data.frame(meal = names(real_counts), real_count = as.numeric(real_counts))
    
    rows_0 <- which(s_0$UserID==df_sum$UserID[HEI_min])
    orginal_counts <- table(s_0$Occ_Name[rows_0])
    orginal_counts_df <- data.frame(meal = names(orginal_counts), real_count = as.numeric(orginal_counts))
    
    meal_bounds <- data.frame(
      meal = orginal_counts_df$meal,
      lower_bound = orginal_counts_df$real_count-2,  # Replace with actual lower bounds
      upper_bound = orginal_counts_df$real_count + 3   # Replace with actual upper bounds
    )
    
    meal_bounds <- merge(meal_bounds, real_counts_df, by = "meal", all.x = TRUE)
    meal_bounds$real_count[is.na(meal_bounds$real_count)] <- 0
    meal_bounds$within_bounds <- with(meal_bounds, real_count >= lower_bound & real_count <= upper_bound)
    all_conditions_met <- all(meal_bounds$within_bounds)
    
    rows_i <- which(s_n$UserID==df_sum$UserID[HEI_min])
    rows_0 <- which(s_0$UserID==df_sum$UserID[HEI_min])
    
    euclidean_distance = (abs(sum(s_n$FoodAmt[rows_i])-sum(s_0$FoodAmt[rows_0])))/sum(s_0$FoodAmt[rows_0])
    intersections <- length(intersect(s_0$Food_Description[rows_0],s_n$Food_Description[rows_i]))/length(rows_0)
    lower_bound = 0 # 0.5 for main text
    # update current state
    set.seed(k)
    if ((euclidean_distance<bound) & (all_conditions_met) & (intersections>lower_bound)) {
      if (score_f > score || runif(1, 0, 1) < exp(-(score - score_f) / Temp)) {
        s_c <- s_n
        f_c <- 100 - score_f
        # update best state
        if (f_c < f_b) {
          s_b <- s_c
          f_b <- f_c
        }
      }
    }
    
    alltemp = c(alltemp,100 - f_c)
    k = k + 1
  }
  
  df_sum <- s_b %>%
    group_by(UserID, RecallNo) %>%
    summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = 'drop')
  df_sum = df_sum[,-c(13)]
  #colnames(df_sum)[c(1,2)] = c("UserID","RecallNo")
  df_sum['UserName'] = MCTs_23887_Items_clean$UserName[match(df_sum$UserID,MCTs_23887_Items_clean$UserID)]
  
  if (indices == "HEI2015"){
    EE = HEI2015_ASA24(df_sum,RECALL_SUMMARIZE=FALSE)
  }
  if (indices == "MED"){
    EE = MED_ASA24(df_sum,RECALL_SUMMARIZE=FALSE)
  }
  if (indices == "DII"){
    EE =  DII_ASA24(df_sum[19,],RECALL_SUMMARIZE=FALSE)
  }
  rows_i <- which(s_b$UserID==df_sum$UserID[HEI_min])
  return(list(iterations = niter, best_value = 100 - f_b*100, best_state = s_b[rows_i,],init_state = s_0[rows_0,], initial_score = EE_initial, final_score = EE, alltemp = alltemp))
}