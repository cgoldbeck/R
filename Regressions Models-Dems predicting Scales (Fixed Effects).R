library(Matrix)
library(lme4)

######READ ME##########
# This script will loop through logistic regressions for inputed outcomes and fixed effects for neighborhoods
#
#
# Because as previously found, there was no estimable variance from a random neighborhood effect model, 
# we include it as a fixed effect instead. Random effects code can be found at 
# \Box Sync\soccer\Data\7-Projects\Impulsitivity under Regressions Models-Scales predicting outcome (Random Effects)
#
#
# "Brief-A MI Coding V2" needs to be run to make the scores prior to this script
# Location: \Box Sync\soccer\Data\7-Projects\Impulsitivity

#Set directory where data is saved
#setwd("C:/Users/cgoldbeck/Box Sync/soccer/Data/1-Raw")
setwd("C:/Users/ealmirol/Box Sync/soccer/Data/1-Raw")

load("SixMo5_15_18.Rda") #choose your df to label, BRIEF A questions only asked at 6 months and beyond
df <- SixMo
load("Baseline5_15_18.Rda") #Baseline Needs to be loaded to get education variables only asked at baseline

# We can easily set our predictors here
p <- c("Age", "Education", "Marital status", "Income", "Housing description (ref=Infomral)", "Electricity", "Participant living with parents",
       "Participant living with their children", "Recently Fired")

# We can easily set our outcomes here
o <- c("InhibitRaw", "ShiftRaw", "EmotionalControlRaw", "SelfMonitorRaw", "InitiateRaw", "MemoryRaw", "OrganizeRaw", 
       "TaskRaw", "MaterialsRaw", "BRIRaw", "MIRaw", "GECRaw")

#We need to know how many neighborhoods we have fixed effects terms, there will be NC-1 terms
df$`Neighbourhood code` <- factor(df$`Neighbourhood code`)
NC <- length(table(factor(df$`Neighbourhood code`)))

################### Coding of Demographic predictors#########################
#Labeling Variables of interest
Baseline$Education <- NA_integer_
Baseline$Education[Baseline$`Participant education`==0] <- 0
Baseline$Education[Baseline$`Participant education`==1] <- 1
Baseline$Education[Baseline$`Participant education`==2] <- 2
Baseline$Education[Baseline$`Participant education`==3] <- 3
Baseline$Education[Baseline$`Participant education`==4] <- 4
Baseline$Education[Baseline$`Participant education`==5] <- 5
Baseline$Education[Baseline$`Participant education`==6] <- 6
Baseline$Education[Baseline$`Participant education`==7] <- 7
Baseline$Education[Baseline$`Participant education`==8] <- 8
Baseline$Education[Baseline$`Participant education`==9] <- 9
Baseline$Education[Baseline$`Participant education`==10] <- 10
Baseline$Education[Baseline$`Participant education`==11] <- 11
Baseline$Education[Baseline$`Participant education`==12 | Baseline$`Participant education`==13 | 
                     Baseline$`Participant education` ==6] <- 12
df <- merge(df, Baseline[which(Baseline$'Participant ID ICF' %in% df$'Participant ID ICF'), 
                         c('Participant ID ICF', 'Education')], by = 'Participant ID ICF', all = TRUE)


df$`Marital status`[df$`Relationship status`==1 | df$`Relationship status`==2 | df$`Partner living with you` ==2 | 
                      df$`Partner living with you`==3] <- 0
df$`Marital status`[df$`Relationship status`==3 | df$`Relationship status`==4 | df$`Partner living with you` ==1] <- 1
df$`Housing description (ref=Infomral)` <- -1*df$`Housing description` + 2
df$`Electricity` <- -1*df$`Electricity` + 2 
df$`Participant living with parents` <- -1*df$`Participant living with parents` + 2
df$`Participant living with their children` <- -1*df$`Participant living with their children` + 2
df$`Recently Fired` <- NA_integer_
df$`Recently Fired`[which(df$`Fired ever` > 0)] <- 1
df$`Recently Fired`[which(df$`Fired ever` == 0)] <- 0
df$Income <- rep("Less Than 2000 RAND")
for (i in 1:nrow(df)){
  if (df$`Monthly income`[i] > 3){
    df$Income[i] <- "Above 2000 RAND"
  }
}
df$Income <- factor(df$Income)

################### Dems Models #########################
result_Dem <- matrix(nrow = length(o) * length(p), ncol = 6) # 6 things we need: outcome, predictor, estimate, LL, UL, and pval
result_Dem[, 2] <- rep(p)
Neighborhood_Dem <- matrix(nrow = length(o) * length(p) * (NC - 1), ncol = 5) #NC -1  Neighborhood variables. 5 things we need: outcome, predictor, neighborhood code, estimate, pval


for (j in 1:length(o)){
  for (i in 1:length(p)){
    predictor <- p[i]
    outcome <- o[j]
    result_Dem[length(p) * (j - 1) + 1, 1] <- o[j]
    model <- glm(get(outcome) ~  get(predictor) + `Neighbourhood code`, data = df, family = "gaussian")
    Vcov <- vcov(model, useScale = FALSE) #Begin steps to make pval
    beta <- coef(model)
    se <- sqrt(diag(Vcov))
    zval <- beta / se
    pval <- 2 * pnorm(abs(zval), lower.tail = FALSE) #pval
    result_Dem[length(p) * (j - 1) + i, c(3:6)] <- round(cbind(Est = coef(model)[2], LL = coef(model)[2] - 1.96 * se[2], UL = coef(model)[2] + 1.96 * se[2], 
                                                      PValue = pval[2]), 2)
    #collect important results above, these will be what we report
    Neighborhood_Dem[1:(NC - 1) + (i - 1) * (NC - 1) + (j - 1) * length(p) * (NC - 1), ][1:(length(coef(model)) - 2), ] <- 
      cbind(rep(outcome), rep(predictor), names(coef(model))[c(-1,-2)], 
            round(cbind(Est = exp(coef(model)[3:length(coef(model))]), PValue = pval[3:length(coef(model))]), 2))
    #collect neighborhood effevts here, less important but for potential reference
  }
}
result_Dem <- as.data.frame(result_Dem)
names(result_Dem) <- c("Outcome", "Predictor", "B Estimate", "LL", "UL", "P-Value")
Neighborhood_Dem <- as.data.frame(Neighborhood_Dem)
names(Neighborhood_Dem) <- c("Outcome", "Predictor", "Neighborhood Code", "Estimate", "P-Value")
#View(result_Dem)

