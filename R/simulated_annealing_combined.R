#' Simulated Annealing for Dietary Optimization
#'
#' @param diet A dietary data frame.
#' @param candidate Index of the target individual.
#' @param niter Number of iterations.
#' @param bound Maximum dietary deviation (Euclidean).
#' @param diet_score Diet score to use ("HEI2015", "DASH", or "DII").
#' @return A list with optimized diet, scores, and history.
#' @export


simulated_annealing_combined <- function(diet, candidate, niter = 5000, bound = 0.4, diet_score = "HEI2015") {
  library(dietaryindex)
  library(dplyr)

  s_0 <- diet

  # summarize the diet of a subject per day
  df_sum <- diet %>%
    dplyr::group_by(UserID, RecallNo) %>%
    dplyr::summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)), .groups = 'drop')

  food_code_index <- which(colnames(df_sum) == "FoodCode")
  df_sum = df_sum[, -food_code_index]
  df_sum['UserName'] = diet$UserName[match(df_sum$UserID, diet$UserID)]

  # compute the original diet score
  EE_initial <- switch(diet_score,
                       HEI2015 = HEI2015_ASA24(df_sum, RECALL_SUMMARIZE = FALSE)$HEI2015_ALL[candidate],
                       MED = MED_ASA24(df_sum, RECALL_SUMMARIZE = FALSE)$MED_ALL[candidate],
                       DII = 5 - DII_ASA24(df_sum[candidate,], RECALL_SUMMARIZE = FALSE)$DII_ALL[1],
                       stop("Unsupported diet_score"))

  score <- EE_initial

  # initial diet and loss
  s_b <- s_c <- s_n <- diet
  f_b <- f_c <- f_n <- (100 - score)
  alltemp = 100 - f_c

  k = 1
  Temp0 = 500
  while ((k < niter)) {
    set.seed(k)
    s_n <- s_c
    Temp <- Temp0 * 0.9999^k
    strategy <- sample(1:3, 1)

    # replace
    if (strategy == 1) {
      rows_i <- which(s_n$UserID == df_sum$UserID[candidate] & s_n$RecallNo == df_sum$RecallNo[candidate])
      selected_i <- sample(rows_i, 1)
      same_occasions <- s_n[s_n$Occ_Name == s_n$Occ_Name[selected_i], ]
      selected_noni <- sample(nrow(same_occasions), 1)
      s_n[selected_i, 16:129] <- same_occasions[selected_noni, 16:129]
    }

    # add new one
    if (strategy == 2) {
      rows_i <- which(s_n$UserID == df_sum$UserID[candidate] & s_n$RecallNo == df_sum$RecallNo[candidate])
      selected_i <- sample(rows_i, 1)
      selected_noni <- sample(nrow(s_n), 1)
      s_n <- rbind(s_n, s_c[selected_i, ])
      s_n[nrow(s_n), 16:129] <- s_n[selected_noni, 16:129]
    }

    # remove existing one
    if (strategy == 3) {
      rows_i <- which(s_n$UserID == df_sum$UserID[candidate] & s_n$RecallNo == df_sum$RecallNo[candidate])
      selected_i <- sample(rows_i, 1)
      s_n <- s_n[-selected_i, ]
    }

    # compute new diet score
    df_sum <- s_n %>%
      dplyr::group_by(UserID, RecallNo) %>%
      dplyr::summarise(dplyr::across(where(is.numeric), sum, na.rm = TRUE), .groups = 'drop')
    df_sum = df_sum[, -food_code_index]
    df_sum['UserName'] = diet$UserName[match(df_sum$UserID, diet$UserID)]

    EE <- switch(diet_score,
                       HEI2015 = HEI2015_ASA24(df_sum, RECALL_SUMMARIZE = FALSE)$HEI2015_ALL[candidate],
                       MED = MED_ASA24(df_sum, RECALL_SUMMARIZE = FALSE)$MED_ALL[candidate],
                       DII = 5 - DII_ASA24(df_sum[candidate,], RECALL_SUMMARIZE = FALSE)$DII_ALL[1])

    score_f <- EE

    real_counts <- table(s_n$Occ_Name[rows_i])
    real_counts_df <- data.frame(meal = names(real_counts), real_count = as.numeric(real_counts))

    rows_0 <- which(s_0$UserID == df_sum$UserID[candidate])
    orginal_counts <- table(s_0$Occ_Name[rows_0])
    orginal_counts_df <- data.frame(meal = names(orginal_counts), real_count = as.numeric(orginal_counts))

    meal_bounds <- data.frame(
      meal = orginal_counts_df$meal,
      lower_bound = orginal_counts_df$real_count - 2,
      upper_bound = orginal_counts_df$real_count + 3
    )

    meal_bounds <- merge(meal_bounds, real_counts_df, by = "meal", all.x = TRUE)
    meal_bounds$real_count[is.na(meal_bounds$real_count)] <- 0
    meal_bounds$within_bounds <- with(meal_bounds, real_count >= lower_bound & real_count <= upper_bound)
    all_conditions_met <- all(meal_bounds$within_bounds)

    rows_i <- which(s_n$UserID == df_sum$UserID[candidate])
    rows_0 <- which(s_0$UserID == df_sum$UserID[candidate])

    euclidean_distance <- abs(sum(s_n$FoodAmt[rows_i]) - sum(s_0$FoodAmt[rows_0])) / sum(s_0$FoodAmt[rows_0])
    intersections <- length(intersect(s_0$Food_Description[rows_0], s_n$Food_Description[rows_i])) / length(rows_0)

    if ((euclidean_distance < bound) & all_conditions_met & (intersections > 0)) {
      if (score_f > score || runif(1) < exp(-(score - score_f) / Temp)) {
        s_c <- s_n
        f_c <- 100 - score_f
        if (f_c < f_b) {
          s_b <- s_c
          f_b <- f_c
        }
      }
    }

    alltemp <- c(alltemp, 100 - f_c)
    k <- k + 1
  }

  df_sum <- s_b %>%
    dplyr::group_by(UserID, RecallNo) %>%
    dplyr::summarise(dplyr::across(where(is.numeric), sum, na.rm = TRUE), .groups = 'drop')
  df_sum = df_sum[, -food_code_index]
  df_sum['UserName'] = diet$UserName[match(df_sum$UserID, diet$UserID)]

  EE <- switch(diet_score,
              HEI2015 = HEI2015_ASA24(df_sum, RECALL_SUMMARIZE = FALSE)$HEI2015_ALL[candidate],
              MED = MED_ASA24(df_sum, RECALL_SUMMARIZE = FALSE)$MED_ALL[candidate],
              DII = 5 - DII_ASA24(df_sum[candidate,], RECALL_SUMMARIZE = FALSE)$DII_ALL[1])

  rows_i <- which(s_b$UserID == df_sum$UserID[candidate])

  if (diet_score == "DII") {
    EE_initial = 5 - EE_initial
    EE = 5 - EE
  }

  return(list(
    iterations = niter,
    best_value = 100 - f_b * 100,
    best_state = s_b[rows_i, ],
    init_state = s_0[rows_0, ],
    initial_score = EE_initial,
    final_score = EE,
    alltemp = alltemp
  ))
}
