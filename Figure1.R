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


g1 = list()
index = 1

for (indices in c("HEI2015","MED","DII")){
  MCTs_23887_Items = read.csv(file = "../data/MCTs_23887_Items.csv",header = T,sep = ",")
  MCTs_23887_Items$RecallNo = 1
  MCTs_23887_Items$RecallAttempt = 0
  MCTs_23887_Items$RecallStatus = 2
  MCTs_23887_Items_clean <- na.omit(MCTs_23887_Items)
  MCTs_23887_Items_clean['RecallNo'] <- MCTs_23887_Items_clean$RecordDayNo
  MCTs_23887_Items_clean$UserID = paste(MCTs_23887_Items_clean$UserID,MCTs_23887_Items_clean$RecallNo,sep = "_")
  MCTs_23887_Items_clean$UserName = paste(MCTs_23887_Items_clean$UserName,MCTs_23887_Items_clean$RecallNo,sep = "_")
  
  dat = NULL
  for (i in 1:200){
    df_sum <- MCTs_23887_Items_clean %>%
      group_by(UserID, RecallNo) %>%
      summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = 'drop')
    
    df_sum = df_sum[,-14]
    df_sum['UserName'] = MCTs_23887_Items_clean$UserName[match(df_sum$UserID,MCTs_23887_Items_clean$UserID)]

    HEI_min = sample(1:nrow(df_sum),1)
    
    if (indices == "HEI2015"){
      EE = HEI2015_ASA24(df_sum,RECALL_SUMMARIZE=FALSE)
      score = EE$HEI2015_ALL[HEI_min]
    }
    if (indices == "MED"){
      EE = MED_ASA24(df_sum,RECALL_SUMMARIZE=FALSE)
      score = EE$MED_ALL[HEI_min]
    }
    if (indices == "DII"){
      EE =  DII_ASA24(df_sum[HEI_min,],RECALL_SUMMARIZE = FALSE)
    }
    
    s_n = MCTs_23887_Items_clean
    rows_i <- which(s_n$UserID==df_sum$UserID[HEI_min] & s_n$RecallNo==df_sum$RecallNo[HEI_min])
    selected_i <- sample(1:length(rows_i),1)
    selected_noni <- sample(1:nrow(s_n),1)
    s_n = rbind(s_n,s_n[rows_i[selected_i],])
    s_n[nrow(s_n),16:129] = s_n[selected_noni,16:129]
  

    df_sum <- s_n %>%
      group_by(UserID, RecallNo) %>%
      summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = 'drop')
    df_sum = df_sum[,-c(14)]
    df_sum['UserName'] = MCTs_23887_Items_clean$UserName[match(df_sum$UserID,MCTs_23887_Items_clean$UserID)]
    
    if (indices == "HEI2015"){
      EE1 = HEI2015_ASA24(df_sum,RECALL_SUMMARIZE=FALSE)
      dat  = rbind(dat,as.numeric(EE1[HEI_min,4:9])-as.numeric(EE[HEI_min,4:9]))
    }
    if (indices == "MED"){
      EE1 = MED_ASA24(df_sum,RECALL_SUMMARIZE=FALSE)
      dat  = rbind(dat,as.numeric(EE1[HEI_min,c(3,5:9)]-EE[HEI_min,c(3,5:9)]))
    }
    if (indices == "DII"){
      EE1 =  DII_ASA24(df_sum[HEI_min,],RECALL_SUMMARIZE = FALSE)
      dat  = rbind(dat,as.numeric(EE1[1,c(3,5:9)]-EE[1,c(3,5:9)]))
    }
  }
  dat = as.data.frame(dat)
  
  for (j in 2:(ncol(dat))){
    g1[[index]] = ggplot(dat,aes_string(x = colnames(dat)[j], y = colnames(dat)[1])) +
      geom_point(color="#984ea3", size=1.5,alpha=0.8) +
      #stat_cor(p.accuracy = 0.001, r.accuracy = 0.01,method = "spearman")+
      xlab(colnames(dat)[j]) +
      ylab(colnames(dat)[1])+
      theme_bw()+
      theme(
        line = element_line(size = 0.5), # Thickness of all lines, mm
        rect = element_rect(size = 0.5), # Thickness of all rectangles, mm
        text = element_text(size = 8), # Size of all text, points
        axis.text.x = element_text(size = 8,color = 'black'), # Angle x-axis text
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
      index = index+1
  }
}

p3 = do.call(ggarrange, c(g1, list(nrow = 3, ncol = 5),align="hv"))

ggsave(p3,file="../figures/correlation.pdf",width=14, height=7.5,scale = 0.7)
