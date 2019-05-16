library(Matrix)
library(lme4)
library(nlme)
######READ ME##########
# This code creates Brief A raw scores and caseness indicators for the 9 subscales, 2 aggregate scores, and 1 global score
# as outlinfed in the BRIEF-A Manual: digital material found at CITATION

#Set directory where data is saved
setwd("C:/Users/cgoldbeck/Box Sync/soccer/Data/1-Raw")
#setwd("C:/Users/ealmirol/Box Sync/soccer/Data/1-Raw")

load("SixMo5_15_18.Rda") #choose your df to label, BRIEF A questions only asked at 6 months and beyond
df <- SixMo



############################Inhibit Raw Scoring###########################
df$InhibitRaw <- rep(NA_real_)
for (i in 1:nrow(df)){
  response <- c(df$`_5) Tap Fingers`[i], df$`_16) Trouble Sitting Still`[i], df$`_29) Problems Waiting`[i], 
                df$`_36) Inappropriate Sexual Comments`[i], df$`_43) Decisions That Get me Into Trouble`[i], df$`_55) Easily Distracted`[i], 
                df$`_58) Rush Things`[i], df$`_73) Impulsive`[i]) 
  if (sum(is.na(response)) < 3){
    response[which(is.na(response))] <- 0
    df$InhibitRaw[i] <- sum(response) + length(response)
  }
}
M18_29Inhibit <- cbind(c(8:24), c(36,40,43,46,50,53,57,60,63,67,70,74,77,80,84,87,91), 
                       c(9,19,30,45,54,63,74,84,92,95,97,98,99,99,99,99,99))

M30_39Inhibit <- cbind(c(8:24), c(37,41,44,48,51,54,58,61,65,68,71,75,78,82,85,88,92), 
                       c(10,25,41,53,67,79,86,92,96,97,97,98,98,99,99,99,99))

df$Inhibit <- rep(NA_character_)
for (i in 1:nrow(df)){
  if (df$Age[i] %in% c(30:39)){
    if (!is.na(df$InhibitRaw[i])){
      hold <- which(M30_39Inhibit[,1]==df$InhibitRaw[i])
      df$Inhibit[i] <- "Non-caseness"
      if (M30_39Inhibit[hold ,2] > 64){
        df$Inhibit[i] <- "Caseness"
      }
    }
  }
  else if (df$Age[i] %in% c(18:29)){
    if (!is.na(df$InhibitRaw[i])){
      hold <- which(M18_29Inhibit[,1]==df$InhibitRaw[i])
      df$Inhibit[i] <- "Non-caseness"
      if (M18_29Inhibit[hold ,2] > 64){
        df$Inhibit[i] <- "Caseness"
      }
    }
  }
}
rm(M18_29Inhibit, M30_39Inhibit)
############################Shift Raw Scoring###########################
df$ShiftRaw <- rep(NA_real_)
for (i in 1:nrow(df)){
  response <- c(df$`_8) Trouble Changing Tasks`[i], df$`_22) Trouble Accepting Different Ways`[i], df$`_32) Trouble Thinking Differently`[i],
                df$`_44) Bothered with Change`[i], df$`_61) Disturbed by Unexpected Changes`[i], df$`_67) Dont Get Over Problems Easily`[i]) 
  if (sum(is.na(response)) < 2){
    response[which(is.na(response))] <- 0
    df$ShiftRaw[i] <- sum(response) + length(response)
  }
}
M18_29Shift <- cbind(c(6:18), c(39,43,47,51,56,60,64,69,73,77,81,86,90))

M30_39Shift <- cbind(c(6:18), c(39,43,48,52,56,61,65,69,74,78,83,87,81))

df$Shift <- rep(NA_character_)
for (i in 1:nrow(df)){
  if (df$Age[i] %in% c(30:39)){
    if (!is.na(df$ShiftRaw[i])){
      hold <- which(M30_39Shift[,1]==df$ShiftRaw[i])
      df$Shift[i] <- "Non-caseness"
      if (M30_39Shift[hold ,2] > 64){
        df$Shift[i] <- "Caseness"
      }
    }
  }
  else if (df$Age[i] %in% c(18:29)){
    if (!is.na(df$ShiftRaw[i])){
      hold <- which(M18_29Shift[,1]==df$ShiftRaw[i])
      df$Shift[i] <- "Non-caseness"
      if (M18_29Shift[hold ,2] > 64){
        df$Shift[i] <- "Caseness"
      }
    }
  }
}

rm(M18_29Shift, M30_39Shift)
############################EmotionalControl Raw Scoring###########################
df$EmotionalControlRaw <- rep(NA_real_)
for (i in 1:nrow(df)){
  response <- c(df$`_1) Angry Outbursts`[i], df$`_12) Emotionally Overreact`[i], df$`_19) Emotional Outbursts`[i], 
                df$`_28) React Emotionally`[i], df$`_33) Overreact to Small Problems`[i], df$`_42) Easily Emotionally Upset`[i],
                df$`_51) Intense but Quick Anger`[i], df$`_57) Too Emotional`[i], df$`_69) Mood Changes Frequently`[i], 
                df$`_72) Upset Easily`[i]) 
  if (sum(is.na(response)) < 3){
    response[which(is.na(response))] <- 0
    df$EmotionalControlRaw[i] <- sum(response) + length(response)
  }
}
M18_29EmotionalControl <- cbind(c(10:30), c(38,40,43,45,47,49,51,54,56,58,60,63,65,67,69,72,74,76,78,80,83))

M30_39EmotionalControl <- cbind(c(10:30), c(38,41,43,46,48,50,53,55,57,60,62,64,67,69,71,74,76,79,81,83,86))

df$EmotionalControl <- rep(NA_character_)
for (i in 1:nrow(df)){
  if (df$Age[i] %in% c(30:39)){
    if (!is.na(df$EmotionalControlRaw[i])){
      hold <- which(M30_39EmotionalControl[,1]==df$EmotionalControlRaw[i])
      df$EmotionalControl[i] <- "Non-caseness"
      if (M30_39EmotionalControl[hold ,2] > 64){
        df$EmotionalControl[i] <- "Caseness"
      }
    }
  }
  else if (df$Age[i] %in% c(18:29)){
    if (!is.na(df$EmotionalControlRaw[i])){
      hold <- which(M18_29EmotionalControl[,1]==df$EmotionalControlRaw[i])
      df$EmotionalControl[i] <- "Non-caseness"
      if (M18_29EmotionalControl[hold ,2] > 64){
        df$EmotionalControl[i] <- "Caseness"
      }
    }
  }
}
rm(M18_29EmotionalControl, M30_39EmotionalControl)
############################SelfMonitor Raw Scoring###########################
df$SelfMonitorRaw <- rep(NA_real_)
for (i in 1:nrow(df)){
  response <- c(df$`_13) Dont Notice When Make Others Feel Bad`[i], df$`_23) Talk Wrong TIme`[i], 
                df$`_37) Dont Understand When People are Upset`[i], df$`_50) Say Without Thinking`[i], 
                df$`_64) Dont Think Before Acting`[i], df$`_70) Dont Think About Consequences`[i]) 
  if (sum(is.na(response)) < 2){
    response[which(is.na(response))] <- 0
    df$SelfMonitorRaw[i] <- sum(response) + length(response)
  }
}
M18_29SelfMonitor <- cbind(c(6:18), c(37,42,46,50,54,59,63,67,72,76,80,84,89))

M30_39SelfMonitor <- cbind(c(6:18), c(37,42,46,51,55,59,64,68,72,77,81,86,90))

df$SelfMonitor <- rep(NA_character_)
for (i in 1:nrow(df)){
  if (df$Age[i] %in% c(30:39)){
    if (!is.na(df$SelfMonitorRaw[i])){
      hold <- which(M30_39SelfMonitor[,1]==df$SelfMonitorRaw[i])
      df$SelfMonitor[i] <- "Non-caseness"
      if (M30_39SelfMonitor[hold ,2] > 64){
        df$SelfMonitor[i] <- "Caseness"
      }
    }
  }
  else if (df$Age[i] %in% c(18:29)){
    if (!is.na(df$SelfMonitorRaw[i])){
      hold <- which(M18_29SelfMonitor[,1]==df$SelfMonitorRaw[i])
      df$SelfMonitor[i] <- "Non-caseness"
      if (M18_29SelfMonitor[hold ,2] > 64){
        df$SelfMonitor[i] <- "Caseness"
      }
    }
  }
}
rm(M18_29SelfMonitor, M30_39SelfMonitor)
############################Initiate Raw Scoring###########################

df$InitiateRaw <- rep(NA_real_)
for (i in 1:nrow(df)){
  response <- c(df$`_6) Need Reminders`[i], df$`_14) Trouble Getting Ready`[i], df$`_20) Lie Around`[i], 
                df$`_25) Problems Starting Alone`[i], df$`_45) Difficulty Getting Excited`[i], df$`_49) Trouble Getting Started`[i], 
                df$`_53) Start Things Last Minute`[i], df$`_62) Trouble Coming Up with Ideas`[i]) 
  if (sum(is.na(response)) < 3){
    response[which(is.na(response))] <- 0
    df$InitiateRaw[i] <- sum(response) + length(response)
  }
}

#Initiate T-Score & Percentile

M18_29initiate <- cbind(c(8:24), c(37,40,43,47,50,53,56,60,63,66,69,73,76,79,82,85,89))

M30_39initiate <- cbind(c(8:24), c(37,41,44,47,51,54,57,61,64,68,71,74,78,81,84,88,91))

df$Initiate <- rep(NA_character_)
for (i in 1:nrow(df)){
  if (df$Age[i] %in% c(30:39)){
    if (!is.na(df$InitiateRaw[i])){
      hold <- which(M30_39initiate[,1]==df$InitiateRaw[i])
      df$Initiate[i] <- "Non-caseness"
      if (M30_39initiate[hold ,2] > 64){
        df$Initiate[i] <- "Caseness"
      }
    }
  }
  else if (df$Age[i] %in% c(18:29)){
    if (!is.na(df$InitiateRaw[i])){
      hold <- which(M18_29initiate[,1]==df$InitiateRaw[i])
      df$Initiate[i] <- "Non-caseness"
      if (M18_29initiate[hold ,2] > 64){
        df$Initiate[i] <- "Caseness"
      }
    }
  }
}
rm(M18_29initiate, M30_39initiate)
############################Memory Raw Scoring###########################


df$MemoryRaw <- rep(NA_real_)
for (i in 1:nrow(df)){
  response <- c(df$`_4) Trouble Concentrating`[i], df$`_11) Trouble with Jobs of More than One Step`[i], df$`_17) Forget`[i], 
                df$`_26) Trouble Staying on Same Topic`[i], df$`_35) Short Attention Span`[i], df$`_46) Forget Instructions`[i], 
                df$`_56) Trouble Remembering Things`[i], df$`_68) Trouble Doing More than One Thing at a Time`[i]) 
  if (sum(is.na(response)) < 3){
    response[which(is.na(response))] <- 0
    df$MemoryRaw[i] <- sum(response) + length(response)
  }
}

#Memory T-Score & Percentile

M18_29Memory <- cbind(c(8:24), c(39,43,46,49,53,56,59,63,66,69,73,76,79,83,86,89,93))

M30_39Memory <- cbind(c(8:24), c(39,43,46,50,53,57,60,64,67,71,74,78,81,85,88,91,95))

df$Memory <- rep(NA_character_)
for (i in 1:nrow(df)){
  if (df$Age[i] %in% c(30:39)){
    if (!is.na(df$MemoryRaw[i])){
      hold <- which(M30_39Memory[,1]==df$MemoryRaw[i])
      df$Memory[i] <- "Non-caseness"
      if (M30_39Memory[hold ,2] > 64){
        df$Memory[i] <- "Caseness"
      }
    }
  }
  else if (df$Age[i] %in% c(18:29)){
    if (!is.na(df$MemoryRaw[i])){
      hold <- which(M18_29Memory[,1]==df$MemoryRaw[i])
      df$Memory[i] <- "Non-caseness"
      if (M18_29Memory[hold ,2] > 64){
        df$Memory[i] <- "Caseness"
      }
    }
  }
}
rm(M18_29Memory, M30_39Memory)
############################ Organize Raw Scoring###########################


df$OrganizeRaw <- rep(NA_real_)
for (i in 1:nrow(df)){
  response <- c(df$`_9) Overwhelmed by large tasks`[i], df$`_15) Trouble Prioritizing`[i], df$`_21) Start Tasks without Materials`[i], 
                df$`_34) Dont Plan Ahead`[i], df$`_39) Unrealistic Goals`[i], df$`_47) Good Ideas`[i], 
                df$`_54) Difficulty Finishing Tasks`[i], df$`_63) Dont Plan Ahead`[i], df$`_66) Problems Organizing Activities`[i], df$`_71) Trouble Organizing Work`[i]) 
  if (sum(is.na(response)) < 3){
    response[which(is.na(response))] <- 0
    df$OrganizeRaw[i] <- sum(response) + length(response)
  }
}

#Organize T-Score & Percentile

M18_29Organize <- cbind(c(10:30), c(38,41,44,46,49,52,54,57,60,62,65,68,70,73,76,78,81,84,86,89,92)) 

M30_39Organize <- cbind(c(10:30), c(39,42,44,47,50,53,55,58,61,64,66,69,72,75,77,80,83,86,88,91,94))

df$Organize <- rep(NA_character_)
for (i in 1:nrow(df)){
  if (df$Age[i] %in% c(30:39)){
    if (!is.na(df$OrganizeRaw[i])){
      hold <- which(M30_39Organize[,1]==df$OrganizeRaw[i])
      df$Organize[i] <- "Non-caseness"
      if (M30_39Organize[hold ,2] > 64){
        df$Organize[i] <- "Caseness"
      }
    }
  }
  else if (df$Age[i] %in% c(18:29)){
    if (!is.na(df$OrganizeRaw[i])){
      hold <- which(M18_29Organize[,1]==df$OrganizeRaw[i])
      df$Organize[i] <- "Non-caseness"
      if (M18_29Organize[hold ,2] > 64){
        df$Organize[i] <- "Caseness"
      }
    }
  }
}
rm(M18_29Organize, M30_39Organize)
############################ Task Raw Scoring###########################


df$TaskRaw <- rep(NA_real_)
for (i in 1:nrow(df)){
  response <- c(df$`_2) Careless Errors`[i], df$`_18) Dont Check Work`[i], df$`_24) Misjudge Tasks`[i], 
                df$`_41) Careless Mistakes`[i], df$`_52) Trouble Finishing Tasks`[i], df$`_75) Problems Completing Work`[i]) 
  if (sum(is.na(response)) < 2){
    response[which(is.na(response))] <- 0
    df$TaskRaw[i] <- sum(response) + length(response)
  }
}

#Task T-Score & Percentile

M18_29Task <- cbind(c(6:18), c(36,40,45,50,54,59,63,68,72,77,81,86,90)) 

M30_39Task <- cbind(c(6:18), c(37,42,46,51,56,60,65,69,74,79,83,88,93))

df$Task <- rep(NA_character_)
for (i in 1:nrow(df)){
  if (df$Age[i] %in% c(30:39)){
    if (!is.na(df$TaskRaw[i])){
      hold <- which(M30_39Task[,1]==df$TaskRaw[i])
      df$Task[i] <- "Non-caseness"
      if (M30_39Task[hold ,2] > 64){
        df$Task[i] <- "Caseness"
      }
    }
  }
  else if (df$Age[i] %in% c(18:29)){
    if (!is.na(df$TaskRaw[i])){
      hold <- which(M18_29Task[,1]==df$TaskRaw[i])
      df$Task[i] <- "Non-caseness"
      if (M18_29Task[hold ,2] > 64){
        df$Task[i] <- "Caseness"
      }
    }
  }
}
rm(M18_29Task, M30_39Task)
############################ Materials Raw Scoring###########################


df$MaterialsRaw <- rep(NA_real_)
for (i in 1:nrow(df)){
  response <- c(df$`_3) Disorganized`[i], df$`_7) Messy Closet`[i], df$`_30) Disorganized`[i], 
                df$`_31) Lose Things`[i], df$`_40) Messy Bathroom`[i], df$`_60) Messy Room or House`[i], 
                df$`_65) Trouble Finding Things`[i], df$`_74) Dont Pick Up After Myself`[i]) 
  if (sum(is.na(response)) < 3){
    response[which(is.na(response))] <- 0
    df$MaterialsRaw[i] <- sum(response) + length(response)
  }
}

#Materials T-Score & Percentile

M18_29Materials <- cbind(c(8:24), c(36,39,42,45,47,50,53,56,58,61,64,67,69,72,75,78,81))

M30_39Materials <- cbind(c(8:24), c(37,40,43,45,48,51,54,57,60,63,66,69,71,74,77,80,83))

df$Materials <- rep(NA_character_)
for (i in 1:nrow(df)){
  if (df$Age[i] %in% c(30:39)){
    if (!is.na(df$MaterialsRaw[i])){
      hold <- which(M30_39Materials[,1]==df$MaterialsRaw[i])
      df$Materials[i] <- "Non-caseness"
      if (M30_39Materials[hold ,2] > 64){
        df$Materials[i] <- "Caseness"
      }
    }
  }
  else if (df$Age[i] %in% c(18:29)){
    if (!is.na(df$MaterialsRaw[i])){
      hold <- which(M18_29Materials[,1]==df$MaterialsRaw[i])
      df$Materials[i] <- "Non-caseness"
      if (M18_29Materials[hold ,2] > 64){
        df$Materials[i] <- "Caseness"
      }
    }
  }
}
rm(M18_29Materials, M30_39Materials)
###########################################Sum Raw BRI scores######################################################################################

df$BRIRaw <- df$InhibitRaw + df$ShiftRaw + df$EmotionalControlRaw + df$SelfMonitorRaw

M18_29BRI <- cbind(c(30:90), c(35:95))

M30_39BRI <- cbind(c(30:90), c(35,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,
                               58,59,60,61,62,63,64,65,66,68,69,70,71,72,73,74,75,76,77,78,79,80,
                               81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97))

df$BRI <- rep(NA_character_)
for (i in 1:nrow(df)){
  if (df$Age[i] %in% c(30:39)){
    if (!is.na(df$BRIRaw[i])){
      hold <- which(M30_39BRI[,1]==df$BRIRaw[i])
      df$BRI[i] <- "Non-caseness"
      if (M30_39BRI[hold ,2] > 64){
        df$BRI[i] <- "Caseness"
      }
    }
  }
  else if (df$Age[i] %in% c(18:29)){
    if (!is.na(df$BRIRaw[i])){
      hold <- which(M18_29BRI[,1]==df$BRIRaw[i])
      df$BRI[i] <- "Non-caseness"
      if (M18_29BRI[hold ,2] > 64){
        df$BRI[i] <- "Caseness"
      }
    }
  }
}
rm(M18_29BRI, M30_39BRI)
###########################################Sum Raw MI scores######################################################################################

df$MIRaw <- df$InitiateRaw + df$MemoryRaw + df$OrganizeRaw + df$TaskRaw + df$MaterialsRaw

#Metacondition Index Score

M18_29MI <- cbind(c(40:120), c(36,36,37,38,39,39,40,41,41,2,43,44,44,45,46,47,47,48,49,
                               50,50,51,52,53,53,54,55,56,56,57,58,59,59,60,61,62,62,63,64,65,65,66,67,68,68,69,70,70,71,72,
                               73,73,74,75,76,76,77,78,79,79,80,81,82,82,83,84,85,85,86,87,88,88,89,90,91,91,92,93,94,94,95))

M30_39MI <- cbind(c(40:120), c(36,37,37,38,39,40,40,41,42,43,44,44,45,46,47,47,48,49,50,
                               51,51,52,53,54,54,55,56,57,58,58,59,60,61,61,62,63,64,64,65,66,67,68,68,69,70,71,71,72,73,74,
                               75,75,76,77,78,78,79,80,81,82,82,83,84,85,85,86,87,88,88,89,90,91,92,92,93,94,95,95,96,97,98))


df$MI <- rep(NA_character_)
for (i in 1:nrow(df)){
  if (df$Age[i] %in% c(30:39)){
    if (!is.na(df$MIRaw[i])){
      hold <- which(M30_39MI[,1]==df$MIRaw[i])
      df$MI[i] <- "Non-caseness"
      if (M30_39MI[hold ,2] > 64){
        df$MI[i] <- "Caseness"
      }
    }
  }
  else if (df$Age[i] %in% c(18:29)){
    if (!is.na(df$MIRaw[i])){
      hold <- which(M18_29MI[,1]==df$MIRaw[i])
      df$MI[i] <- "Non-caseness"
      if (M18_29MI[hold ,2] > 64){
        df$MI[i] <- "Caseness"
      }
    }
  }
}
rm(M18_29MI, M30_39MI)
###########################################Sum Raw GEC scores######################################################################################

df$GECRaw <- df$BRIRaw + df$MIRaw

M18_29GEC <- cbind(c(70:210), c(34,35,35,35,36,36,37,37,38,38,39,39,40,40,41,41,41,42,42,43,43,
                                44,44,45,45,46,46,47,47,47,48,48,49,49,50,50,51,51,52,52,53,53,53,
                                54,54,55,55,56,56,57,57,58,58,59,59,59,60,60,61,61,62,62,63,63,64,
                                64,65,65,65,66,66,67,67,68,68,69,69,70,70,71,71,71,72,72,73,73,74,74,
                                75,75,76,76,77,77,77,78,78,79,79,80,80,81,81,82,82,83,83,83,84,84,
                                85,85,86,86,87,87,88,88,89,89,89,90,90,91,91,92,92,93,93,94,94,95,
                                95,95,96,96,97,97,98,98,99))

M30_39GEC <- cbind(c(70:210), c(35,35,35,36,36,37,37,38,38,39,39,40,40,41,41,42,42,43,43,44,44,45,
                                45,46,46,46,47,47,48,48,49,49,50,50,51,51,52,52,53,53,54,54,55,55,
                                56,56,57,57,57,58,58,59,59,60,60,61,61,62,62,63,63,64,64,65,65,66,
                                66,67,67,68,68,68,69,69,70,70,71,71,72,72,73,73,74,74,75,75,76,76,
                                77,77,78,78,79,79,79,80,80,81,81,82,82,83,83,84,84,85,85,86,86,87,
                                87,88,88,89,89,90,90,90,91,91,92,92,93,93,94,94,95,95,96,96,97,97,
                                98,98,99,99,100,100,100,101,101))

df$GEC <- rep(NA_character_)
for (i in 1:nrow(df)){
  if (df$Age[i] %in% c(30:39)){
    if (!is.na(df$GECRaw[i])){
      hold <- which(M30_39GEC[,1]==df$GECRaw[i])
      df$GEC[i] <- "Non-caseness"
      if (M30_39GEC[hold ,2] > 64){
        df$GEC[i] <- "Caseness"
      }
    }
  }
  else if (df$Age[i] %in% c(18:29)){
    if (!is.na(df$GECRaw[i])){
      hold <- which(M18_29GEC[,1]==df$GECRaw[i])
      df$GEC[i] <- "Non-caseness"
      if (M18_29GEC[hold ,2] > 64){
        df$GEC[i] <- "Caseness"
      }
    }
  }
}
rm(M18_29GEC, M30_39GEC)

###########################################Negativity scores#####################################
df$Negativity <- rep(NA_character_)
for (i in 1:nrow(df)){
  response <- c(df$`_1) Angry Outbursts`[i], df$`_8) Trouble Changing Tasks`[i], df$`_19) Emotional Outbursts`[i],
                df$`_21) Start Tasks without Materials`[i], df$`_22) Trouble Accepting Different Ways`[i], df$`_23) Talk Wrong TIme`[i],
                df$`_29) Problems Waiting`[i], df$`_36) Inappropriate Sexual Comments`[i], df$`_39) Unrealistic Goals`[i], df$`_40) Messy Bathroom`[i]) 
  if (sum(is.na(response)) < 1){
    if (sum(response == 2) > 5){
      df$Negativity[i] <- "Elevated"
    }
    else{
      df$Negativity[i] <- "Acceptable"
    }
  }
}

###########################################Infrequency scores#####################################
df$Infrequency <- rep(NA_character_)
for (i in 1:nrow(df)){
  response <- c(-1*(df$`_10) Forget Name`[i])+2, df$`_27) Tired`[i], -1*(df$`_38) Trouble Counting`[i])+2, 
                df$`_48) Mistakes`[i], df$`_59) Get Annoyed`[i]) 
  if (sum(is.na(response)) < 1){
    if (sum(response == 0) > 2){
      df$Infrequency[i] <- "Infrequent"
    }
    else{
      df$Infrequency[i] <- "Acceptable"
    }
  }
}

###########################################Inconsistency scores#####################################
df$Inconsistency <- rep(NA_character_)
for (i in 1:nrow(df)){
  response <- c(abs(df$`_2) Careless Errors`[i] - df$`_41) Careless Mistakes`[i]), 
                abs(df$`_25) Problems Starting Alone`[i] - df$`_49) Trouble Getting Started`[i]),
                abs(df$`_28) React Emotionally`[i] - df$`_42) Easily Emotionally Upset`[i]),
                abs(df$`_33) Overreact to Small Problems`[i] - df$`_72) Upset Easily`[i]),
                abs(df$`_34) Dont Plan Ahead`[i] - df$`_63) Dont Plan Ahead`[i]),
                abs(df$`_44) Bothered with Change`[i] - df$`_61) Disturbed by Unexpected Changes`[i]),
                abs(df$`_46) Forget Instructions`[i] - df$`_56) Trouble Remembering Things`[i]),
                abs(df$`_52) Trouble Finishing Tasks`[i] - df$`_75) Problems Completing Work`[i]),
                abs(df$`_60) Messy Room or House`[i] - df$`_74) Dont Pick Up After Myself`[i]),
                abs(df$`_64) Dont Think Before Acting`[i] - df$`_70) Dont Think About Consequences`[i])) 
  if (sum(is.na(response)) < 1){
    if (sum(response != 0) > 7){
      df$Inconsistency[i] <- "Inconsistent"
    }
    else{
      df$Inconsistency[i] <- "Acceptable"
    }
  }
}

#total caseness by BRI, MI and GEC
table(df$BRI)
table(df$MI)
table(df$GEC)

#Descriptive Statistics
summary(df$BRIRaw)
mean(df$BRIRaw, na.rm = T) 
sd(df$BRIRaw, na.rm = T)

summary(df$MIRaw)
mean(df$MIRaw, na.rm = T) 
sd(df$MIRaw, na.rm = T)

summary(df$GECRaw)
mean(df$GECRaw, na.rm = T) 
sd(df$GECRaw, na.rm = T)



####### Part 2 ######################
#Test for group differences: Control vs Treatment
#combine intervention: Training + Soccer vs. Control
df$condition[df$Soccer==1 | df$Training==1] <- 1
df$condition[df$Control==1] <- 0

#Descriptive Statistics by group
tapply(df$BRIRaw, df$condition, summary)
tapply(df$BRIRaw, df$condition, mean, na.rm = T)
tapply(df$BRIRaw, df$condition, sd, na.rm = T)

tapply(df$MIRaw, df$condition, summary)
tapply(df$MIRaw, df$condition, mean, na.rm = T)
tapply(df$MIRaw, df$condition, sd, na.rm = T)

tapply(df$GECRaw, df$condition, summary)
tapply(df$GECRaw, df$condition, mean, na.rm = T)
tapply(df$GECRaw, df$condition, sd, na.rm = T)

#t-tests between control and intervention, raw mean scores for each subscale
t.test(df$InhibitRaw~df$condition)
t.test(df$ShiftRaw~df$condition)
t.test(df$EmotionalControlRaw~df$condition)
t.test(df$SelfMonitorRaw~df$condition)
t.test(df$InitiateRaw~df$condition)
t.test(df$MemoryRaw~df$condition)
t.test(df$OrganizeRaw~df$condition)
t.test(df$TaskRaw~df$condition)
t.test(df$BRIRaw~df$condition)
t.test(df$MIRaw~df$condition)
t.test(df$GECRaw~df$condition)


#Examine if scores change between 6mo and 12mo for traids that have those
df <- BaselineTo12Mo
df <- df[which(df$Time %in% c(2,3)), ]
df <- df[which(df$`Neighbourhood code` %in% names(table(df$`Neighbourhood code`))),]

n <- c("InhibitRaw", "ShiftRaw", "EmotionalControlRaw", "SelfMonitorRaw", "InitiateRaw", "MemoryRaw", "OrganizeRaw", 
       "TaskRaw", "MaterialsRaw", "BRIRaw", "MIRaw", "GECRaw")

result <- matrix(nrow = 12, ncol = 5)

#GROUP ERROR POPS UP

for (i in 1:length(n)){
  hold <- n[i]
  model <- lmer(get(hold) ~  Time+Group + (1|`Participant ID ICF`), data=df)
  summary(model)
  Vcov <- vcov(model, useScale = FALSE)
  beta <- fixef(model)
  se <- sqrt(diag(Vcov))
  zval <- beta / se
  pval <- 2 * pnorm(abs(zval), lower.tail = FALSE)
  result[i, ] <- c(n[i], pval)
}

result <- as.data.frame(result)
names(result) <- c("var", "Intercept", "Time", "Soccer", "Training")


