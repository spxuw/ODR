library(ggplot2)
library(ggpubr)
library(dplyr) 
library(readxl)
library(reshape2)
library(stringr)
library(dietaryindex)
library(RColorBrewer)
library(scales)
library(MASS)


setwd("/Users/xuwenwang/Dropbox/Projects/Diet_recommend/code_v2")

rm(list = ls())
source("simulated_annealing_combined.R")

pad_with_na <- function(vec, max_length) {length(vec) <- max_length
return(vec)
}

for (indices in c("HEI2015","MED","DII")){
  if (indices == "HEI2015"){
    sol11 <- simulated_annealing_combined(index="HEI2015",niter = 200, step = 0.1, bound=0.4)
    HEI_min = which.min(sol11$initial_score$HEI2015_ALL)
    dat11 = data.frame(ori=as.numeric(sol11$initial_score[HEI_min,4:17]),reco=as.numeric(sol11$final_score[HEI_min,4:17]),food=colnames(sol11$final_score)[4:17])
    dat11$food = factor(dat11$food,levels = dat11$food)
    
    g1 = ggplot(dat11) +
      geom_segment( aes(x=food, xend=food, y=(ori), yend=(reco)), color="grey") +
      geom_point( aes(x=food, y=(ori)), color="#1D3F46", size=2 ) +
      geom_point( aes(x=food, y=(reco)), color="#ED9951", size=2 ) +
      ylab("HEI2015")+ xlab("")+
      theme_bw()+
      theme(
        line = element_line(size = 0.5), # Thickness of all lines, mm
        rect = element_rect(size = 0.5), # Thickness of all rectangles, mm
        text = element_text(size = 8), # Size of all text, points
        axis.text.x = element_text(size = 8,color = 'black',angle = 30,hjust = 1), # Angle x-axis text
        axis.text.y = element_text(size = 8,color = 'black'), # Angle x-axis text
        axis.title.x = element_text(size = 8), # Size x-axis title
        axis.title.y = element_text(size = 8), # Size y-axis title
        panel.grid.minor = element_blank(), # No minor grid lines
        panel.grid.major.x = element_blank(), # No major x-axis grid lines
        panel.grid.major.y = element_blank(),
        legend.title = element_blank(),
        legend.position = 'bottom',
        strip.background = element_blank(),
        legend.box.background = element_rect(size = 0.2), # Box for legend
        legend.key.size = unit(4, unit = 'mm'),
        legend.text = element_text(size = 8)
      )
    
    # Combine unique Food_Description from ori and reco
    unique_food_descriptions <- unique(c(sol11$init_state$Food_Description, sol11$best_state$Food_Description))
    dat21 <- data.frame(
      Food_Description = unique_food_descriptions,
      ori = sapply(unique_food_descriptions, function(desc) {
        match_index <- which(sol11$init_state$Food_Description == desc)
        if (length(match_index) > 0) {
          return(sum(sol11$init_state$FoodAmt[match_index]))  # Use sum, mean, or any aggregation
        } else {
          return(0)
        }
      }),
      reco = sapply(unique_food_descriptions, function(desc) {
        match_index <- which(sol11$best_state$Food_Description == desc)
        if (length(match_index) > 0) {
          return(sum(sol11$best_state$FoodAmt[match_index]))  # Use sum, mean, or any aggregation
        } else {
          return(0)
        }
      }),
      combined_meal = sapply(unique_food_descriptions, function(desc) {
        ori_match_index <- which(sol11$init_state$Food_Description == desc)
        reco_match_index <- which(sol11$best_state$Food_Description == desc)
        
        ori_meals <- if (length(ori_match_index) > 0) {
          sol11$init_state$Occ_Name[ori_match_index]
        } else {
          character(0)
        }
        
        reco_meals <- if (length(reco_match_index) > 0) {
          sol11$best_state$Occ_Name[reco_match_index]
        } else {
          character(0)
        }
        
        # Combine and get unique meal names
        combined_meals <- unique(c(ori_meals, reco_meals))
        return(paste(combined_meals, collapse = ", "))
      })
    )
    
    categories <- c("Breakfast", "Brunch", "Lunch", "Dinner", "Supper", "Snack", "Just a Drink", "Just a Supplement", "Other")
    category_colors <- setNames(brewer.pal(n = length(categories), name = "Set3"), categories)
    category_colors["Brunch"] <- "black"
    category_colors <- as.data.frame(category_colors)
    label_colors <- category_colors$category_colors[match(dat21$combined_meal,rownames(category_colors))]
    label_colors[is.na(label_colors)] = "#D9D9D9"
    dat21$Food_Description <- factor(dat21$Food_Description,levels = dat21$Food_Description)
    
    write.table(dat21,"../results/DMAS_HEI_2015.csv",sep = ",")
    
    
    g2 = ggplot(dat21) +
      geom_segment( aes(x=Food_Description, xend=Food_Description, y=log10(ori+1), yend=log10(reco+1)), color="grey") +
      geom_point( aes(x=Food_Description, y=log10(ori+1)), color="#1D3F46", size=2 ) +
      geom_point( aes(x=Food_Description, y=log10(reco+1)), color="#ED9951", size=2 ) +
      ylab("Food grams")+
      theme_bw()+
      theme(
        line = element_line(size = 0.5), # Thickness of all lines, mm
        rect = element_rect(size = 0.5), # Thickness of all rectangles, mm
        text = element_text(size = 8), # Size of all text, points
        axis.text.x = element_text(size = 8,color = 'black',angle = 30,hjust = 1), # Angle x-axis text
        axis.text.y = element_text(size = 8,color = 'black'), # Angle x-axis text
        axis.title.x = element_text(size = 8), # Size x-axis title
        axis.title.y = element_text(size = 8), # Size y-axis title
        panel.grid.minor = element_blank(), # No minor grid lines
        panel.grid.major.x = element_blank(), # No major x-axis grid lines
        panel.grid.major.y = element_blank(),
        legend.title = element_blank(),
        legend.position = 'bottom',
        strip.background = element_blank(),
        legend.box.background = element_rect(size = 0.2), # Box for legend
        legend.key.size = unit(4, unit = 'mm'),
        legend.text = element_text(size = 8)
      )+
      theme(axis.text.x = element_text(color = label_colors))
    
    
  }
  if (indices == "MED"){
    sol12 <- simulated_annealing_combined(index="MED",niter = 200, step = 0.1, bound=0.4)
    HEI_min = which.min(sol12$initial_score$MED_ALL)
    dat12 = data.frame(ori=as.numeric(sol12$initial_score[HEI_min,3:13]),reco=as.numeric(sol12$final_score[HEI_min,3:13]),food=colnames(sol12$final_score)[3:13])
    dat12$food = factor(dat12$food,levels = dat12$food)
    
    g3 = ggplot(dat12) +
      geom_segment( aes(x=food, xend=food, y=(ori), yend=(reco)), color="grey") +
      geom_point( aes(x=food, y=(ori)), color="#1D3F46", size=2 ) +
      geom_point( aes(x=food, y=(reco)), color="#ED9951", size=2 ) +
      ylab("AMED")+ xlab("")+
    theme_bw()+
      theme(
        line = element_line(size = 0.5), # Thickness of all lines, mm
        rect = element_rect(size = 0.5), # Thickness of all rectangles, mm
        text = element_text(size = 8), # Size of all text, points
        axis.text.x = element_text(size = 8,color = 'black',angle = 30,hjust = 1), # Angle x-axis text
        axis.text.y = element_text(size = 8,color = 'black'), # Angle x-axis text
        axis.title.x = element_text(size = 8), # Size x-axis title
        axis.title.y = element_text(size = 8), # Size y-axis title
        panel.grid.minor = element_blank(), # No minor grid lines
        panel.grid.major.x = element_blank(), # No major x-axis grid lines
        panel.grid.major.y = element_blank(),
        legend.title = element_blank(),
        legend.position = 'bottom',
        strip.background = element_blank(),
        legend.box.background = element_rect(size = 0.2), # Box for legend
        legend.key.size = unit(4, unit = 'mm'),
        legend.text = element_text(size = 8)
      )
    
    unique_food_descriptions <- unique(c(sol12$init_state$Food_Description, sol12$best_state$Food_Description))
    dat22 <- data.frame(
      Food_Description = unique_food_descriptions,
      ori = sapply(unique_food_descriptions, function(desc) {
        match_index <- which(sol12$init_state$Food_Description == desc)
        if (length(match_index) > 0) {
          return(sum(sol12$init_state$FoodAmt[match_index]))  # Use sum, mean, or any aggregation
        } else {
          return(0)
        }
      }),
      reco = sapply(unique_food_descriptions, function(desc) {
        match_index <- which(sol12$best_state$Food_Description == desc)
        if (length(match_index) > 0) {
          return(sum(sol12$best_state$FoodAmt[match_index]))  # Use sum, mean, or any aggregation
        } else {
          return(0)
        }
      }),
      combined_meal = sapply(unique_food_descriptions, function(desc) {
        ori_match_index <- which(sol12$init_state$Food_Description == desc)
        reco_match_index <- which(sol12$best_state$Food_Description == desc)
        
        ori_meals <- if (length(ori_match_index) > 0) {
          sol12$init_state$Occ_Name[ori_match_index]
        } else {
          character(0)
        }
        
        reco_meals <- if (length(reco_match_index) > 0) {
          sol12$best_state$Occ_Name[reco_match_index]
        } else {
          character(0)
        }
        
        # Combine and get unique meal names
        combined_meals <- unique(c(ori_meals, reco_meals))
        return(paste(combined_meals, collapse = ", "))
      })
    )
    
    categories <- c("Breakfast", "Brunch", "Lunch", "Dinner", "Supper", "Snack", "Just a Drink", "Just a Supplement", "Other")
    category_colors <- setNames(brewer.pal(n = length(categories), name = "Set3"), categories)
    category_colors["Brunch"] <- "black"
    category_colors <- as.data.frame(category_colors)
    label_colors <- category_colors$category_colors[match(dat22$combined_meal,rownames(category_colors))]
    label_colors[is.na(label_colors)] = "#D9D9D9"
    dat22$Food_Description <- factor(dat22$Food_Description,levels = dat22$Food_Description)
    
    write.table(dat22,"../results/DMAS_AMED.csv",sep = ",")
    
    
    g4 = ggplot(dat22) +
      geom_segment( aes(x=Food_Description, xend=Food_Description, y=log10(ori+1), yend=log10(reco+1)), color="grey") +
      geom_point( aes(x=Food_Description, y=log10(ori+1)), color="#1D3F46", size=2 ) +
      geom_point( aes(x=Food_Description, y=log10(reco+1)), color="#ED9951", size=2 ) +
      ylab("Food grams")+
      theme_bw()+
      theme(
        line = element_line(size = 0.5), # Thickness of all lines, mm
        rect = element_rect(size = 0.5), # Thickness of all rectangles, mm
        text = element_text(size = 8), # Size of all text, points
        axis.text.x = element_text(size = 8,color = 'black',angle = 30,hjust = 1), # Angle x-axis text
        axis.text.y = element_text(size = 8,color = 'black'), # Angle x-axis text
        axis.title.x = element_text(size = 8), # Size x-axis title
        axis.title.y = element_text(size = 8), # Size y-axis title
        panel.grid.minor = element_blank(), # No minor grid lines
        panel.grid.major.x = element_blank(), # No major x-axis grid lines
        panel.grid.major.y = element_blank(),
        legend.title = element_blank(),
        legend.position = 'bottom',
        strip.background = element_blank(),
        legend.box.background = element_rect(size = 0.2), # Box for legend
        legend.key.size = unit(4, unit = 'mm'),
        legend.text = element_text(size = 8)
      )+
      theme(axis.text.x = element_text(color = label_colors))
    
  }
  if (indices == "DII"){
    sol13 <- simulated_annealing_combined(index="DII",niter = 200, step = 0.1, bound=0.4)
    HEI_min = 1
    dat13 = data.frame(ori=as.numeric(sol13$initial_score[HEI_min,3:16]),reco=as.numeric(sol13$final_score[HEI_min,3:16]),food=colnames(sol13$final_score)[3:16])
    dat13$food = factor(dat13$food,levels = dat13$food)
    
    g5 = ggplot(dat13) +
      geom_segment( aes(x=food, xend=food, y=(ori), yend=(reco)), color="grey") +
      geom_point( aes(x=food, y=(ori)), color="#1D3F46", size=2 ) +
      geom_point( aes(x=food, y=(reco)), color="#ED9951", size=2 ) +
      ylab("DII")+ xlab("")+
    theme_bw()+
      theme(
        line = element_line(size = 0.5), # Thickness of all lines, mm
        rect = element_rect(size = 0.5), # Thickness of all rectangles, mm
        text = element_text(size = 8), # Size of all text, points
        axis.text.x = element_text(size = 8,color = 'black',angle = 30,hjust = 1), # Angle x-axis text
        axis.text.y = element_text(size = 8,color = 'black'), # Angle x-axis text
        axis.title.x = element_text(size = 8), # Size x-axis title
        axis.title.y = element_text(size = 8), # Size y-axis title
        panel.grid.minor = element_blank(), # No minor grid lines
        panel.grid.major.x = element_blank(), # No major x-axis grid lines
        panel.grid.major.y = element_blank(),
        legend.title = element_blank(),
        legend.position = 'bottom',
        strip.background = element_blank(),
        legend.box.background = element_rect(size = 0.2), # Box for legend
        legend.key.size = unit(4, unit = 'mm'),
        legend.text = element_text(size = 8)
      )
    
    unique_food_descriptions <- unique(c(sol13$init_state$Food_Description, sol13$best_state$Food_Description))
    dat23 <- data.frame(
      Food_Description = unique_food_descriptions,
      ori = sapply(unique_food_descriptions, function(desc) {
        match_index <- which(sol13$init_state$Food_Description == desc)
        if (length(match_index) > 0) {
          return(sum(sol13$init_state$FoodAmt[match_index]))  # Use sum, mean, or any aggregation
        } else {
          return(0)
        }
      }),
      reco = sapply(unique_food_descriptions, function(desc) {
        match_index <- which(sol13$best_state$Food_Description == desc)
        if (length(match_index) > 0) {
          return(sum(sol13$best_state$FoodAmt[match_index]))  # Use sum, mean, or any aggregation
        } else {
          return(0)
        }
      }),
      combined_meal = sapply(unique_food_descriptions, function(desc) {
        ori_match_index <- which(sol13$init_state$Food_Description == desc)
        reco_match_index <- which(sol13$best_state$Food_Description == desc)
        
        ori_meals <- if (length(ori_match_index) > 0) {
          sol13$init_state$Occ_Name[ori_match_index]
        } else {
          character(0)
        }
        
        reco_meals <- if (length(reco_match_index) > 0) {
          sol13$best_state$Occ_Name[reco_match_index]
        } else {
          character(0)
        }
        
        # Combine and get unique meal names
        combined_meals <- unique(c(ori_meals, reco_meals))
        return(paste(combined_meals, collapse = ", "))
      })
    )
    
    categories <- c("Breakfast", "Brunch", "Lunch", "Dinner", "Supper", "Snack", "Just a Drink", "Just a Supplement", "Other")
    category_colors <- setNames(brewer.pal(n = length(categories), name = "Set3"), categories)
    category_colors["Brunch"] <- "black"
    category_colors <- as.data.frame(category_colors)
    label_colors <- category_colors$category_colors[match(dat23$combined_meal,rownames(category_colors))]
    label_colors[is.na(label_colors)] = "#D9D9D9"
    dat23$Food_Description <- factor(dat23$Food_Description,levels = dat23$Food_Description)
    
    write.table(dat23,"../results/DMAS_DII.csv",sep = ",")
    
    
    g6 = ggplot(dat23) +
      geom_segment( aes(x=Food_Description, xend=Food_Description, y=log10(ori+1), yend=log10(reco+1)), color="grey") +
      geom_point( aes(x=Food_Description, y=log10(ori+1)), color="#1D3F46", size=2 ) +
      geom_point( aes(x=Food_Description, y=log10(reco+1)), color="#ED9951", size=2 ) +
      ylab("Food grams")+
      theme_bw()+
      theme(
        line = element_line(size = 0.5), # Thickness of all lines, mm
        rect = element_rect(size = 0.5), # Thickness of all rectangles, mm
        text = element_text(size = 8), # Size of all text, points
        axis.text.x = element_text(size = 8,color = 'black',angle = 30,hjust = 1), # Angle x-axis text
        axis.text.y = element_text(size = 8,color = 'black'), # Angle x-axis text
        axis.title.x = element_text(size = 8), # Size x-axis title
        axis.title.y = element_text(size = 8), # Size y-axis title
        panel.grid.minor = element_blank(), # No minor grid lines
        panel.grid.major.x = element_blank(), # No major x-axis grid lines
        panel.grid.major.y = element_blank(),
        legend.title = element_blank(),
        legend.position = 'bottom',
        strip.background = element_blank(),
        legend.box.background = element_rect(size = 0.2), # Box for legend
        legend.key.size = unit(4, unit = 'mm'),
        legend.text = element_text(size = 8)
      )+
      theme(axis.text.x = element_text(color = label_colors))
    
  }
}
save.image("workspace.RData")

p0 = ggarrange(g1,g2,ncol = 2,align="hv",labels = c("a","b"),widths = c(0.6,1))
p1 = ggarrange(g3,g4,ncol = 2,align="hv",labels = c("c","d"),widths = c(0.6,1))
p2 = ggarrange(g5,g6,ncol = 2,align="hv",labels = c("e","f"),widths = c(0.6,1))

p4 = ggarrange(p0,p1,p2,nrow = 3,heights = c(1.5,1,0.85))

ggsave(p4,file="../figures/fig2_combined.pdf",width=13, height=16,scale = 0.7)
