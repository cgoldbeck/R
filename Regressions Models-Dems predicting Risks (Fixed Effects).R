library(Matrix)
library(lme4)
library(data.table)

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

#We can easily set our outcomes here, which will affect all models below
p <- c("Age", "Education", "Marital status", "Income", "Housing description (ref=Infomral)", "Participant living with parents",
       "Participant living with their children", "Recently Fired")

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

################### Start Coding  outcomes#########################
################### Coding of substance outcomes#########################
df$`Results for test THC`[which(df$`Results for test THC` == 3)] <- NA_integer_
df$`Results for test THC` <- -1*(df$`Results for test THC`) + 2
df$`Results for test Mandraxx`[which(df$`Results for test Mandraxx` == 3)] <- NA_integer_
df$`Results for test Mandraxx` <- -1*(df$`Results for test Mandraxx`) + 2
df$`Results for test MET`[which(df$`Results for test MET` == 3)] <- NA_integer_
df$`Results for test MET` <- -1*(df$`Results for test MET`) + 2
df$`Results for test ETG`[which(df$`Results for test ETG` == 3)] <- NA_integer_
df$`Results for test ETG` <- -1*(df$`Results for test ETG`) + 2
df$MultiUser <- NA_integer_
for (i in 1:nrow(df)){
  use <- sum(c(df$`Results for test THC`[i], df$`Results for test Mandraxx`[i], df$`Results for test MET`[i]), na.rm = T)
  if (use > 1){
    df$MultiUser[i] <- 1
  }
  if (use == 1){
    df$MultiUser[i] <- 0
  }
}

#Drinking Variable
#If they binge (6 or more) at least once a week AND have a negative consequence bc of drinking
df$`Problematic Drinker` <- rep(NA_character_)
for (i in 1:nrow(df)){
  if (!is.na(df$`Often Drink 6 or More`[i])){
    if (df$`Often Drink 6 or More`[i] > 3 & (df$`Often Find Not Able to Stop Drinking Once Started`[i] > 1 | 
                                             df$`Often Need Morning Drink After Heavy Drinking`[i] > 1 | 
                                             df$`Alcohol Upon Waking`[i] > 1 | 
                                             df$`Often Failed Doing What Expected Because of Drinking`[i] > 1 |
                                             df$`Often Feel Guilt After Drinking`[i] > 1 | 
                                             df$`Often Unable to Remember What Happened Due to Drinking`[i] > 1 |
                                             df$`Anyone Injured Due to Drinking`[i] > 1 | 
                                             df$`Anyone Concerned About Your Drinking`[i] > 1 | df$`Need to Cut Down Drinking`[i] > 1 |
                                             df$`Memory Loss With Alcohol`[i] > 1 | 
                                             df$`Ever Quarreled With Girlfriend About Your Drinking`[i] > 1)){
      df$`Problematic Drinker`[i] <- "Problematic Drinker"
    }
    if (!(df$`Often Drink 6 or More`[i] > 3 & (df$`Often Find Not Able to Stop Drinking Once Started`[i] > 1 | 
                                               df$`Often Need Morning Drink After Heavy Drinking`[i] > 1 | 
                                               df$`Alcohol Upon Waking`[i] > 1 | 
                                               df$`Often Failed Doing What Expected Because of Drinking`[i] > 1 |
                                               df$`Often Feel Guilt After Drinking`[i] > 1 | 
                                               df$`Often Unable to Remember What Happened Due to Drinking`[i] > 1 |
                                               df$`Anyone Injured Due to Drinking`[i] > 1 | 
                                               df$`Anyone Concerned About Your Drinking`[i] > 1 | df$`Need to Cut Down Drinking`[i] > 1 |
                                               df$`Memory Loss With Alcohol`[i] > 1 | 
                                               df$`Ever Quarreled With Girlfriend About Your Drinking`[i] > 1))){
      df$`Problematic Drinker`[i] <- "Not Problematic Drinker"
    }
  }
}

df$`Problematic Drinker` <- as.numeric(factor(df$`Problematic Drinker`, levels = c("Not Problematic Drinker", "Problematic Drinker"), 
                                              labels = c(0,1))) - 1

################### Coding of Sex outcomes#########################
df$`Results for HIV`[which(df$`Results for HIV` == 3)] <- NA_integer_
df$`Results for HIV` <- -1*(df$`Results for HIV`) + 2
df$NegativeHIVPerseption <- NA_real_
for (i in 1:nrow(df)){
  response <- c(df$`Feel People Blame The Person for Being Positive`[i], df$`May Lose Job if Someone Finds Out`[i], 
                df$`Think Illness is Punishment`[i], df$`Avoided Treatment Because People Might Find Out`[i], 
                df$`Think People Avoid Persons with HIV`[i], df$`Fear Family Rejection`[i], df$`Fear of Not Getting Good Health`[i]) 
  if (sum(is.na(response)) < 1){
    if (sum(response > 1) > 3){
      df$NegativeHIVPerseption[i] <- 1
    }
    else{
      df$NegativeHIVPerseption[i] <- 0
    }
  }
}
df$`Always used condom in last 2 weeks` <- 1
df$`Always used condom in last 2 weeks`[df$`Condom Usage`/df$`Number of Sexual Encounters with Partner` < 1] <- 0
df$`Always used condom in last 2 weeks`[is.na(df$`Condom Usage`/df$`Number of Sexual Encounters with Partner`)] <- NA_integer_
#one obs had 33 for number of condom uses for 3 sexual encounters, so 110% use, must be typo


df$`Multiple Partners 6 months` <- NA_integer_
df$`Multiple Partners 6 months`[df$`Casual partners in last six months` + 
                                  df$`Numbers of partners have you had in the last six months` <= 1] <- 0
df$`Multiple Partners 6 months`[df$`Casual partners in last six months` + 
                                  df$`Numbers of partners have you had in the last six months` > 1] <- 1

df$`STD Last 6 months` <- NA_integer_
df$`STD Last 6 months`[df$`Sexually transmitted diseases` == 0] <- 0
df$`STD Last 6 months`[df$`Sexually transmitted diseases` > 0] <- 1

################### Coding of IPV outcomes#########################
#should I put missing == 0?

df$ForceContact[df$`Forced Physical Contact` > 0 | df$`Recent Forced Physical Contact` > 0] <- 1
df$ForceContact[is.na(df$ForceContact)] <- 0

df$IPViolence[df$`Often Quarreled With Partner` > 0  | df$`Recent Quarrels with Partner`> 0 | df$`Hit girlfriends`==1] <- 1
df$IPViolence[is.na(df$IPViolence)] <- 0

table(df$`Threatened to Hurt Partner`)
table(df$`Recently Threatened to Hurt Partner`)
table(df$`Threatened to Hurt Partner`, df$`Recently Threatened to Hurt Partner`)

df$Threatened <- NA_real_
df$Threatened[df$`Recently Threatened to Hurt Partner`>=2 | df$`Threatened to Hurt Partner`>=2]  <- 1

#variables not used
#df$`Friends hitting girlfriends`
#df$`Violence with casual partner`
#df$`Partner Likely Having Sex With Other People`
#df$`Ever Had Fight Over Partner Drinking`


################### Coding of Violence outcomes#########################

#loop not working for GroupViolence
df$GroupViolence <- NA_real_
for (i in 1:nrow(df)){
  response <- c(df$`Group violence`[i], df$`Group violence involvement`[i], df$`Gang Member`[i]) 
  if (sum(is.na(response)) < 1){
    if (sum(response == 1) > 0){
      df$GroupViolence[i] <- 1
    }
    else{
      df$GroupViolence[i] <- 0
    }
  }
}

#df$GroupViolence2[df$`Group violence`==1 | df$`Group violence involvement`==1 | df$`Gang Member`==1] <- 1
#df$GroupViolence2[is.na(df$GroupViolence)] <- 0

df$PrisonArrest[df$`Prison Sentences`==1 | df$`Times Arrested` > 0] <- 1
df$PrisonArrest[is.na(df$PrisonArrest)] <- 0

df$PhysicalFights[df$`Physical Fights with Men` > 0 | df$`Physical Fights with Family` > 0] <- 1
df$PhysicalFights[is.na(df$PhysicalFights)] <- 0

#variables not used
#df$`Sex with Man for Presents or Money`

################### Coding of depression outcomes#########################
df$Depression <- df$`Being Bothered` + df$`Poor Appetite` + df$`Feeling the Blues` + df$`Lack of Focus` + df$`Feeling Depressed` + 
  df$Effort + df$`Life a Failure` + df$Fear + df$`Restless Sleep` + df$`Quieter Than Usual` + df$Lonely + 
  df$`Unfriendly People` + df$Crying + df$Sadness + df$`People Dislike Me` + df$Motivation + 
  (-1*df$`Feeling Equal` + 3) + (-1*df$`Hope for Future` + 3) + (-1*df$Happy + 3) + (-1*df$`Enjoyment of Life` + 3)

df$Depressed <- 0
for (i in 1:nrow(df)){
  if (!is.na(df$Depression[i])){
    if (df$Depression[i] > 15){
      df$Depressed[i] <- 1
    }
    if (is.na(df$Depression[i])){
      df$Depressed[i] <- NA_integer_
    }
  }
}
################### End Coding  outcomes#########################


####### Part 1 ######################
####### Logistic models, scores predicting risk outcomes ##########
################### Substance Models #########################
o <- c("Results for test THC", "Results for test Mandraxx", "Results for test MET", "MultiUser", 
       "Results for test ETG","Problematic Drinker")

result_Substance <- matrix(nrow = length(o) * length(p), ncol = 6) # 6 things we need: outcome, predictor, estimate, LL, UL, and pval
result_Substance[, 2] <- rep(p)
Neighborhood_Substance <- matrix(nrow = length(o) * length(p) * (NC - 1), ncol = 5) #NC -1  Neighborhood variables for length(o) * length(p) number of models. 5 things we need: outcome, predictor, neighborhood code, estimate, pval

for (j in 1:length(o)){
  for (i in 1:length(p)){
    predictor <- p[i]
    outcome <- o[j]
    result_Substance[length(p) * (j - 1) + 1, 1] <- o[j]
    model <- glm(get(outcome) ~  get(predictor) + `Neighbourhood code`, data = df, family = "binomial")
    Vcov <- vcov(model, useScale = FALSE) #Begin steps to make pval
    beta <- coef(model)
    se <- sqrt(diag(Vcov))
    zval <- beta / se
    pval <- 2 * pnorm(abs(zval), lower.tail = FALSE) #pval
    result_Substance[length(p) * (j - 1) + i, c(3:6)] <- round(cbind(Est = exp(coef(model)[2]), LL = exp(coef(model)[2] - 1.96 * se[2]), 
                                                                     UL = exp(coef(model)[2] + 1.96 * se[2]), PValue = pval[2]), 5)
    #collect important results above, these will be what we report
    #Neighborhood_Substance[1:(NC - 1) + (i - 1) * (NC - 1) + (j - 1) * length(p) * (NC - 1), ][1:(length(coef(model)) - 2), ] <- 
    #  cbind(rep(outcome), rep(predictor), names(coef(model))[c(-1,-2)], 
    #        round(cbind(Est = exp(coef(model)[3:length(coef(model))]), PValue = pval[3:length(coef(model))]), 5))
    #collect neighborhood effevts here, less important but for potential reference
    
  }
}

result_Substance <- as.data.frame(result_Substance)
names(result_Substance) <- c("Outcome", "Predictor", "OR", "LL", "UL", "P-Value")
Neighborhood_Substance <- as.data.frame(Neighborhood_Substance)
names(Neighborhood_Substance) <- c("Outcome", "Predictor", "Neighborhood Code", "Estimate", "P-Value")
#View(result_Substance)

################### Sex Models #########################
o <- c("Always used condom in last 2 weeks", 
       "Multiple Partners 6 months", "STD Last 6 months")

result_Sex <- matrix(nrow = length(o) * length(p), ncol = 6) # 6 things we need: outcome, predictor, estimate, LL, UL, and pval
result_Sex[, 2] <- rep(p)
Neighborhood_Sex <- matrix(nrow = length(o) * length(p) * (NC - 1), ncol = 5) #NC -1  Neighborhood variables for length(o) * length(p) number of models. 5 things we need: outcome, predictor, neighborhood code, estimate, pval

for (j in 1:length(o)){
  for (i in 1:length(p)){
    predictor <- p[i]
    outcome <- o[j]
    result_Sex[length(p) * (j - 1) + 1, 1] <- o[j]
    model <- glm(get(outcome) ~  get(predictor) + `Neighbourhood code`, data = df, family = "binomial")
    Vcov <- vcov(model, useScale = FALSE) #Begin steps to make pval
    beta <- coef(model)
    se <- sqrt(diag(Vcov))
    zval <- beta / se
    pval <- 2 * pnorm(abs(zval), lower.tail = FALSE) #pval
    result_Sex[length(p) * (j - 1) + i, c(3:6)] <- round(cbind(Est = exp(coef(model)[2]), LL = exp(coef(model)[2] - 1.96 * se[2]), 
                                                               UL = exp(coef(model)[2] + 1.96 * se[2]), PValue = pval[2]), 5)
    #collect important results above, these will be what we report
    #Neighborhood_Sex[1:(NC - 1) + (i - 1) * (NC - 1) + (j - 1) * length(p) * (NC - 1), ] <- 
    #  cbind(rep(outcome), rep(predictor), names(coef(model))[c(-1,-2)], 
    #        round(cbind(Est = exp(coef(model)[3:length(coef(model))]), PValue = pval[3:length(coef(model))]), 5))
    #collect neighborhood effevts here, less important but for potential reference
    
  }
}

result_Sex <- as.data.frame(result_Sex)
names(result_Sex) <- c("Outcome", "Predictor", "OR", "LL", "UL", "P-Value")
Neighborhood_Sex <- as.data.frame(Neighborhood_Sex)
names(Neighborhood_Sex) <- c("Outcome", "Predictor", "Neighborhood Code", "Estimate", "P-Value")
#View(result_Sex)

################### IPV Models #########################
o <- c("ForceContact", "IPViolence")

#"Threatened" variable is not working because can't categorize


result_IPV <- matrix(nrow = length(o) * length(p), ncol = 6) # 6 things we need: outcome, predictor, estimate, LL, UL, and pval
result_IPV[, 2] <- rep(p)
Neighborhood_IPV <- matrix(nrow = length(o) * length(p) * (NC - 1), ncol = 5) #NC -1  Neighborhood variables for length(o) * length(p) number of models. 5 things we need: outcome, predictor, neighborhood code, estimate, pval

for (j in 1:length(o)){
  for (i in 1:length(p)){
    predictor <- p[i]
    outcome <- o[j]
    result_IPV[length(p) * (j - 1) + 1, 1] <- o[j]
    model <- glm(get(outcome) ~  get(predictor) + `Neighbourhood code`, data = df, family = "binomial")
    Vcov <- vcov(model, useScale = FALSE) #Begin steps to make pval
    beta <- coef(model)
    se <- sqrt(diag(Vcov))
    zval <- beta / se
    pval <- 2 * pnorm(abs(zval), lower.tail = FALSE) #pval
    result_IPV[length(p) * (j - 1) + i, c(3:6)] <- round(cbind(Est = exp(coef(model)[2]), LL = exp(coef(model)[2] - 1.96 * se[2]), 
                                                               UL = exp(coef(model)[2] + 1.96 * se[2]), PValue = pval[2]), 5)
    #collect important results above, these will be what we report
    #Neighborhood_IPV[1:(NC - 1) + (i - 1) * (NC - 1) + (j - 1) * length(p) * (NC - 1), ] <- 
    #  cbind(rep(outcome), rep(predictor), names(coef(model))[c(-1,-2)], 
    #        round(cbind(Est = exp(coef(model)[3:length(coef(model))]), PValue = pval[3:length(coef(model))]), 5))
    #collect neighborhood effevts here, less important but for potential reference
    
  }
}

result_IPV <- as.data.frame(result_IPV)
names(result_IPV) <- c("Outcome", "Predictor", "OR", "LL", "UL", "P-Value")
Neighborhood_IPV <- as.data.frame(Neighborhood_IPV)
names(Neighborhood_IPV) <- c("Outcome", "Predictor", "Neighborhood Code", "Estimate", "P-Value")
#View(result_IPV)

################### Violence Models #########################
o <- c("GroupViolence", "PrisonArrest", "PhysicalFights")

result_Violence <- matrix(nrow = length(o) * length(p), ncol = 6) # 6 things we need: outcome, predictor, estimate, LL, UL, and pval
result_Violence[, 2] <- rep(p)
Neighborhood_Violence <- matrix(nrow = length(o) * length(p) * (NC - 1), ncol = 5) #NC -1  Neighborhood variables for length(o) * length(p) number of models. 5 things we need: outcome, predictor, neighborhood code, estimate, pval

for (j in 1:length(o)){
  for (i in 1:length(p)){
    predictor <- p[i]
    outcome <- o[j]
    result_Violence[length(p) * (j - 1) + 1, 1] <- o[j]
    model <- glm(get(outcome) ~  get(predictor) + `Neighbourhood code`, data = df, family = "binomial")
    Vcov <- vcov(model, useScale = FALSE) #Begin steps to make pval
    beta <- coef(model)
    se <- sqrt(diag(Vcov))
    zval <- beta / se
    pval <- 2 * pnorm(abs(zval), lower.tail = FALSE) #pval
    result_Violence[length(p) * (j - 1) + i, c(3:6)] <- round(cbind(Est = exp(coef(model)[2]), LL = exp(coef(model)[2] - 1.96 * se[2]), 
                                                                    UL = exp(coef(model)[2] + 1.96 * se[2]), PValue = pval[2]), 5)
    #collect important results above, these will be what we report
    #Neighborhood_Violence[1:(NC - 1) + (i - 1) * (NC - 1) + (j - 1) * length(p) * (NC - 1), ] <- 
    #  cbind(rep(outcome), rep(predictor), names(coef(model))[c(-1,-2)], 
    #        round(cbind(Est = exp(coef(model)[3:length(coef(model))]), PValue = pval[3:length(coef(model))]), 5))
    #collect neighborhood effevts here, less important but for potential reference
    
  }
}

result_Violence <- as.data.frame(result_Violence)
names(result_Violence) <- c("Outcome", "Predictor", "OR", "LL", "UL", "P-Value")
Neighborhood_Violence <- as.data.frame(Neighborhood_Violence)
names(Neighborhood_Violence) <- c("Outcome", "Predictor", "Neighborhood Code", "Estimate", "P-Value")
#View(result_Violence)

################### Depression Models #########################b
o <- c("Depressed")

result_Depression <- matrix(nrow = length(o) * length(p), ncol = 6) # 6 things we need: outcome, predictor, estimate, LL, UL, and pval
result_Depression[, 2] <- rep(p)
Neighborhood_Depression <- matrix(nrow = length(o) * length(p) * (NC - 1), ncol = 5) #NC -1  Neighborhood variables for length(o) * length(p) number of models. 5 things we need: outcome, predictor, neighborhood code, estimate, pval

for (j in 1:length(o)){
  for (i in 1:length(p)){
    predictor <- p[i]
    outcome <- o[j]
    result_Depression[length(p) * (j - 1) + 1, 1] <- o[j]
    model <- glm(get(outcome) ~  get(predictor) + `Neighbourhood code`, data = df, family = "binomial")
    Vcov <- vcov(model, useScale = FALSE) #Begin steps to make pval
    beta <- coef(model)
    se <- sqrt(diag(Vcov))
    zval <- beta / se
    pval <- 2 * pnorm(abs(zval), lower.tail = FALSE) #pval
    result_Depression[length(p) * (j - 1) + i, c(3:6)] <- round(cbind(Est = exp(coef(model)[2]), LL = exp(coef(model)[2] - 1.96 * se[2]), 
                                                                      UL = exp(coef(model)[2] + 1.96 * se[2]), PValue = pval[2]), 5)
    #collect important results above, these will be what we report
    #Neighborhood_Depression[1:(NC - 1) + (i - 1) * (NC - 1) + (j - 1) * length(p) * (NC - 1), ] <- 
    #  cbind(rep(outcome), rep(predictor), names(coef(model))[c(-1,-2)], 
    #        round(cbind(Est = exp(coef(model)[3:length(coef(model))]), PValue = pval[3:length(coef(model))]), 5))
    #collect neighborhood effevts here, less important but for potential reference
    
  }
}

result_Depression <- as.data.frame(result_Depression)
names(result_Depression) <- c("Outcome", "Predictor", "OR", "LL", "UL", "P-Value")
Neighborhood_Depression <- as.data.frame(Neighborhood_Depression)
names(Neighborhood_Depression) <- c("Outcome", "Predictor", "Neighborhood Code", "Estimate", "P-Value")
#View(result_Depression)
results <- rbind(result_Substance, result_Violence, result_IPV, result_Sex, result_Depression) #combine the results genderated from above code

####### Part 2 ######################
####### OR and CI for a SD change in Scores ###########
#Choose the scores we want to focus on
scores <- c("BRIRaw", "MIRaw", "GECRaw")

results <- rbind(result_Substance, result_Violence, result_IPV, result_Sex, result_Depression) #combine the results genderated from above code
results_SD <- matrix(NA, nrow = length(o) * length(s), ncol = ncol(results)) #new matrix for results, nrow all combinations scores and outcomes
results_SD[, c(1, 2, 6)] <- as.matrix(results[results$Predictor %in% scores, c(1, 2, 6)]) #copy over 1-outcome, 2-predictor, 6-p-values, these are unchanged 

#Loop through results raising OR, LL, UL to the SD of their corresponding score
#We do this bc the original OR estiamtes are quite small as the Raw scores are on a large scale
#so rather than a 1 unit change in the scores, we show a SD change
for (i in 1:length(scores)){
  #OR - 3 is the OR row
  results_SD[results$Predictor==scores[i], c(3)] <- round(as.numeric(as.character(results[results$Predictor==scores[i], c("OR")]))^
                                                            sd(df[, scores[i]], na.rm = T), 2)
  
  #LL - 4 is the LL row
  results_SD[results$Predictor==scores[i], c(4)] <- round(as.numeric(as.character(results[results$Predictor==scores[i], c("LL")]))^
                                                            sd(df[, scores[i]], na.rm = T), 2)
  
  #UL - 5 is the UL row
  results_SD[results$Predictor==scores[i], c(5)] <- round(as.numeric(as.character(results[results$Predictor==scores[i], c("UL")]))^
                                                            sd(df[, scores[i]], na.rm = T), 2)
  
}
results_SD[, 6] <- round(as.numeric(results_SD[, 6]), 2) #round p-value
results_SD <- as.data.frame(results_SD)
names(results_SD) <- names(results)