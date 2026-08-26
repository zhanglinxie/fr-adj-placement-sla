install.packages("xlsx")
install.packages("xtable")

#get()
                
library(lme4)
library(car) 
library(lmerTest) 
library(ordinal) 
require(sciplot)
library(ggplot2)
library(tidyverse)
library(officer)
library(formattable)
library(xtable)


library(readxl)
library(xlsx)

setwd("/Users/lucie/Desktop/Project1_Overgeneralization/Data")

# Load all data ----------------------------------------------------------------  

Data1 <- read_excel("expCh_5_sujet_2.xlsx")
#View(Data1)
Data1 <- Data1[-c(32,37),]
#View(Data1)

Data2 <- read_excel("expCh_5.xlsx")
#View(Data2)
Data3<- merge(Data1,Data2, by = "ID")
Data4 <- Data3[c("ID", "Group", "Item", "Condition", "Accuracy")] 
#View(Data4)

Data4 <- Data4 %>%
  mutate(LangEnvironment = "Instructed")

expCh_immersion <- read_excel("expCh_immersionAdv.xlsx")
#View(expCh_immersion)
expCh_immersion_subject <- read_excel("expCh_immersion_subject.xlsx")
#View(expCh_immersion_subject)
Data_Ch_immersion <- merge(expCh_immersion,expCh_immersion_subject)
#View(Data_Ch_immersion)
expCh_immersion_selected <- Data_Ch_immersion[c("ID", "Group", "Item", "Condition", "Accuracy")]
#View(expCh_immersion_selected)

expCh_immersion_selected <- expCh_immersion_selected %>% 
  mutate(LangEnvironment = "Immersed")

Data5 <- rbind(Data4,expCh_immersion_selected)
#View(Data5)
table(Data5$Group)


# Loading L1 French data --------------------------------
Data6 <- read_excel("expFr_5.xlsx")
Data11 <- Data6[c("ID", "Group", "Item", "Condition", "Accuracy")] 
Data11 <- Data11 %>%
  mutate(LangEnvironment = "Native")
#View(Data11)

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ----- L1 vs. Beginner vs. Intermediate vs. Advanced -----------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Loading all data (L1 + L2) ----------------------------------------------------------------------------------------------------------
Data <-rbind(Data5, Data11)
#View(Data)
summary(Data)
table(Data$Group)

Data$Condition = as.factor(Data$Condition)
Data$Group = as.factor(Data$Group)
summary(Data$ID)
#View(Data)


Data %>%
  distinct(ID, Group) %>%   # keep one row per participant per group
  count(Group) 

#Centralize data 
Data$Frequency_c<-scale(ifelse(Data$Condition=="PreN_Unknown" |Data$Condition=="PostN_Unknown",0,1))
Data$Adj_Type_c<-scale(ifelse(Data$Condition=="PostN_Unknown"|Data$Condition=="PostN_Known",1,0))
#View(Data)

# Graphic 
Data$Frequency <- ifelse(Data$Condition=="PreN_Unknown" |Data$Condition=="PostN_Unknown","Low_Frqt","High_Frqt")
Data$Adj_Type <- (ifelse(Data$Condition=="PostN_Unknown"|Data$Condition=="PostN_Known","PostN_Adj","PreN_Adj"))
#View(Data)

class(Data$Frequency)


# Descriptive stat ------------------------------------------------------------------------Barplot 

# Accuracy by condition ------------------------------------------------------------------------

# Build a table 
graph <- do.call(data.frame, aggregate(Data$Accuracy, by = list(Group = Data$Group, Adj_Type = Data$Adj_Type, Frequency = Data$Frequency), FUN = function(x) c(mean = mean(x), sd = sd(x), n = length(x))))
graph$se <- graph$x.sd/sqrt(graph$x.n)
colnames(graph) <- c("Group", "Adj_Type","Frequency","percent_correct","sd","n","se")

View(graph)


graph$Adj_Type<- factor(graph$Adj_Type, levels = c("PreN_Adj","PostN_Adj"))
graph$Group <- factor(graph$Group, levels = c("Beginner","Intermediate","Advanced","L1French"))
graph$Frequency <- factor(graph$Frequency, levels = c("Low_Frqt","High_Frqt"))

#Plot a figure with text 
library(ggplot2)
library(scales)

# set dodge width once to avoid mismatches
ggplot(data=graph, aes(x=Adj_Type,y=percent_correct,fill=Frequency)) + 
  geom_bar(stat = "identity", position = "dodge", width =0.6) + 
  geom_text(aes(label=percent(round(percent_correct,3.5))),size=3,vjust=-0.5,position = position_dodge(0.5))+
  geom_errorbar(aes(y = percent_correct, x = Adj_Type, ymax = percent_correct + se, ymin = percent_correct - se, group = Frequency), position = position_dodge(width = 0.8), width = 0.05, size = 0.75) +
  scale_y_continuous(labels = scales::percent,limits=c(0,1))+ 
  facet_grid(~Group)+
  theme_bw()


# one dodge for everything
pd <- position_dodge(width = 0.6)

ggplot(graph, aes(x = Frequency, y = percent_correct, fill = Adj_Type)) +
  geom_bar(stat = "identity", position = pd, width = 0.6) +
  geom_errorbar(
    aes(ymin = percent_correct - se,
        ymax = percent_correct + se,
        group = Adj_Type),     # group by the dodged variable (fill)
    position = pd,
    width = 0.05,
    size = 0.75
  ) +
  geom_text(
    aes(label = percent(percent_correct, accuracy = 0.1)),  # e.g., 0.1% steps
    position = pd,
    vjust = -0.5,
    size = 3
  ) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  facet_grid(~ Group) +
  coord_cartesian(ylim = c(0, 1.05)) +  # avoids clipping text above the bars
  theme_bw()

Data$ID <- as.factor(Data$ID)
Data$Item <- as.factor(Data$Item)

table(Data$ID, Data$Condition) #within
table(Data$Item, Data$Condition) #between

table(Data$ID, Data$Frequency) #within
table(Data$Item, Data$Frequency) #between

table(Data$ID, Data$Group) #between
table(Data$Item, Data$Group) #within

table(Data$Group)
Data$Group <- relevel(Data$Group, ref = "Beginner")


## Bayes model ----

library(brms)
library(bayesplot)
library(insight)
library(ggridges)

# Bays_model = brm(Accuracy ~ Group*Adj_Type_c*Frequency_c+(Frequency_c*Adj_Type_c|ID)+(Group|Item), data=Data,
#                  family = bernoulli(link="logit"),
#                  iter = 4000,
#                  chains = 4,
#                  cores = 4)


# Save the model object to a file
#save(Bays_model, file = "Bayes_model.RData")

# Load the model back into R
load("Bayes_model.RData")

# View the model 
Bays_model

conditional_effects(Bays_model)

class(Data$Adj_Type_c)
table(Data$Adj_Type_c)

class(Data$Frequency_c)
table(Data$Frequency_c)

# get parameters of interest 
posteriors_parameters <- get_parameters(Bays_model)

head(posteriors_parameters)
# Look at the length of the posteriors - how many iterations 
nrow(posteriors_parameters)
# 8000 iterations in total 

#See default prior distribution 
prior_summary(Bays_model)

#See posterior distribution
summary(Bays_model)

stancode(Bays_model)


#Extract posterior samples
post_samples_Extracted = posterior_samples(Bays_model)
head(post_samples_Extracted %>% round(1))

# proportion of positive samples for parameter b_Intercept
mean(post_samples_Extracted$b_Intercept > 0)
# More redundant result than non-redundant result for the reference group in color condition 

# Testing the main effects of the two factors 
mean(post_samples_Extracted$b_Adj_Type_c > 0)
mean(post_samples_Extracted$b_Frequency_c > 0)

mean(post_samples_Extracted$`b_Adj_Type_c:Frequency_c` <0)

mean(post_samples_Extracted$b_GroupIntermediate > 0)
mean(post_samples_Extracted$b_GroupAdvanced > 0)
mean(post_samples_Extracted$b_GroupL1French > 0)


#Strong interaction effect 
mean(post_samples_Extracted$`b_GroupAdvanced:Adj_Type_c`>0)
mean(post_samples_Extracted$`b_GroupIntermediate:Adj_Type_c`>0)
mean(post_samples_Extracted$`b_GroupL1French:Adj_Type_c`<0)
mean(post_samples_Extracted$`b_GroupIntermediate:Frequency_c`<0)
mean(post_samples_Extracted$`b_GroupAdvanced:Frequency_c`>0)
mean(post_samples_Extracted$`b_GroupL1French:Frequency_c`<0)

mean(post_samples_Extracted$`b_GroupIntermediate:Adj_Type_c:Frequency_c`>0)
mean(post_samples_Extracted$`b_GroupAdvanced:Adj_Type_c:Frequency_c`<0)
mean(post_samples_Extracted$`b_GroupL1French:Adj_Type_c:Frequency_c`<0)

# Plot the posterior distributions of the parameters for fixed effects (modifier type and language type)
Bays_model %>% 
  data.frame() %>% 
  select(starts_with("b")) %>%
  pivot_longer(everything(), names_to = "parameters", values_to = "estimates") %>%
  group_by(parameters) %>% 
  mutate(mean_estimates = mean(estimates)) %>%  # Compute mean of estimates for each parameter
  ggplot(aes(x = estimates, 
             y = reorder(parameters, mean_estimates),  # Reorder based on mean estimates
             fill = after_stat(quantile))) +
  geom_vline(xintercept = 0, color = "red") +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    scale = 0.9, 
    quantile_lines = TRUE, 
    quantiles = c(0.025, 0.975), 
    show.legend = F,
    rel_min_height = 0.01
  ) +
  scale_fill_manual(values = c("grey", "skyblue", "grey")) +
  labs(y = "Parameters (Ordered by Mean)", x = "Estimates") +
  theme_minimal()


Data$Frequency <- as.factor(Data$Frequency)
Data$Adj_Type <- as.factor(Data$Adj_Type)



library(brms)
library(bayesplot)
library(insight)
library(ggridges)

Bays_model_factor = brm(Accuracy ~ Group*Adj_Type*Frequency+(Frequency*Adj_Type|ID)+(Group|Item), data=Data,
                        family = bernoulli(link="logit"),
                        iter = 4000,
                        chains = 4,
                        cores = 4)

# Save the model object to a file
save(Bays_model_factor, file = "Bayes_model_factor.RData")

# Load the model back into R
load("Bayes_model_factor.RData")

# View the model 
Bays_model_factor



Bays_model_factor %>% 
  data.frame() %>% 
  select(starts_with("b")) %>%
  pivot_longer(everything(), names_to = "parameters", values_to = "estimates") %>%
  group_by(parameters) %>% 
  mutate(mean_estimates = mean(estimates)) %>%  # Compute mean of estimates for each parameter
  ggplot(aes(x = estimates, 
             y = reorder(parameters, mean_estimates),  # Reorder based on mean estimates
             fill = after_stat(quantile))) +
  geom_vline(xintercept = 0, color = "red") +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    scale = 0.9, 
    quantile_lines = TRUE, 
    quantiles = c(0.025, 0.975), 
    show.legend = F,
    rel_min_height = 0.01
  ) +
  scale_fill_manual(values = c("grey", "skyblue", "grey")) +
  labs(y = "Parameters (Ordered by Mean)", x = "Estimates") +
  theme_minimal()

conditional_effects(Bays_model_factor)

conds <- data.frame(Group = c("Beginner","Advanced"))  # example subset
three_way <- conditional_effects(Bays_model_factor,
                                 effects = "Adj_Type:Frequency",
                                 conditions = conds,
                                 re_formula = NA)
plot(three_way)





# get parameters of interest 
posteriors_parameters <- get_parameters(Bays_model_factor)
head(posteriors_parameters)
# Look at the length of the posteriors - how many iterations 
nrow(posteriors_parameters)
# 8000 iterations in total 

#See default prior distribution 
prior_summary(Bays_model_factor)

#See posterior distribution
summary(Bays_model_factor)

stancode(Bays_model_factor)


#Extract posterior samples
post_samples_Extracted = posterior_samples(Bays_model_factor)
head(post_samples_Extracted %>% round(1))

# proportion of positive samples for parameter b_Intercept
mean(post_samples_Extracted$b_Intercept > 0)
# More redundant result than non-redundant result for the reference group in color condition 

# Testing the main effects of the two factors 
mean(post_samples_Extracted$b_GroupL1French > 0)
mean(post_samples_Extracted$b_GroupAdvanced > 0)
mean(post_samples_Extracted$b_GroupIntermediate > 0)

mean(post_samples_Extracted$`b_GroupAdvanced:Adj_TypePreN_Adj`<0)


# No strong interaction effect 
mean(post_samples_Extracted$`b_GroupAdvanced:FrequencyLow_Frqt`>0)
mean(post_samples_Extracted$`b_GroupL1French:Adj_TypePreN_Adj`>0)


mean(post_samples_Extracted$b_Adj_TypePreN_Adj < 0)

mean(post_samples_Extracted$`b_Adj_TypePreN_Adj:FrequencyLow_Frqt`< 0)
mean(post_samples_Extracted$`b_Adj_TypePreN_Adj:FrequencyLow_Frqt`< 0)



priors <- get_prior(Accuracy ~ Group*Adj_Type*Frequency+(Frequency*Adj_Type|ID)+(Group|Item), data=Data,
                    family = bernoulli(link="logit"))

priors$prior[priors$class == "b"& priors$coef == ""] <- "normal(0,10)"

Bays_model_factor_prior = brm(Accuracy ~ Group*Adj_Type*Frequency+(Frequency*Adj_Type|ID)+(Group|Item), data=Data,
                              family = bernoulli(link="logit"),
                              prior = priors,
                              iter = 4000,
                              chains = 4,
                              cores = 4)





# Language environment: Instructed vs. Immersed 

View(Data)
unique(Data$LangEnvironment)

# Build a table 
graph2 <- do.call(data.frame, aggregate(Data$Accuracy, by = list(Group = Data$Group, Adj_Type = Data$Adj_Type, Frequency = Data$Frequency,Language_Environment = Data$LangEnvironment), FUN = function(x) c(mean = mean(x), sd = sd(x), n = length(x))))
graph2$se <- graph2$x.sd/sqrt(graph2$x.n)
colnames(graph2) <- c("Group", "Adj_Type","Frequency","Language_Environment","percent_correct","sd","n","se")
View(graph2)


# Only look at the advacend group : immersion vs. instruction vs. L1French
graph3 <- graph2 %>%
  filter(Group %in% c ("Advanced","L1French"))


graph3$Adj_Type<- factor(graph3$Adj_Type, levels = c("PreN_Adj","PostN_Adj"))
graph3$Frequency <- factor(graph3$Frequency, levels = c("Low_Frqt","High_Frqt"))
graph3$Language_Environment <- factor(graph3$Language_Environment, levels = c("Instructed","Immersed","Native"))


#Plot a figure with text 
library(ggplot2)
library(scales)

# set dodge width once to avoid mismatches
ggplot(data=graph3, aes(x=Frequency,y=percent_correct,fill=Adj_Type)) + 
  geom_bar(stat = "identity", position = "dodge", width =0.6) + 
  geom_text(aes(label=percent(round(percent_correct,3.5))),size=3,vjust=-0.5,position = position_dodge(0.5))+
  geom_errorbar(aes(y = percent_correct, x = Frequency, ymax = percent_correct + se, ymin = percent_correct - se, group = Adj_Type), position = position_dodge(width = 0.8), width = 0.05, size = 0.75) +
  scale_y_continuous(labels = scales::percent,limits=c(0,1))+ 
  facet_grid(~Language_Environment)+
  theme_bw()

View(Data)

Data_Only_Adv <- Data %>%
  filter(Group %in% c("Advanced","L1French"))

View(Data_Only_Adv)

library(dplyr)

Data_Only_Adv %>%
  distinct(ID, LangEnvironment) %>%   # keep one row per participant per group
  count(LangEnvironment) 

Data_Only_Adv$ID <- as.factor(Data_Only_Adv$ID)
Data_Only_Adv$Item <- as.factor(Data_Only_Adv$Item)

Data_Only_Adv$LangEnvironment <- as.factor(Data_Only_Adv$LangEnvironment)

Data_Only_Adv$LangEnvironment<- relevel(Data_Only_Adv$LangEnvironment, ref = "Immersed")


table(Data_Only_Adv$ID, Data_Only_Adv$Condition) #within
table(Data_Only_Adv$Item, Data_Only_Adv$Condition) #between

table(Data_Only_Adv$ID, Data_Only_Adv$Frequency) #within
table(Data_Only_Adv$Item, Data_Only_Adv$Frequency) #between


table(Data_Only_Adv$ID, Data_Only_Adv$LangEnvironment) #between 
table(Data_Only_Adv$Item, Data_Only_Adv$LangEnvironment) #within


Bays_model_Advanced_LgEnv = brm(Accuracy ~ LangEnvironment*Adj_Type_c*Frequency_c+(Frequency_c*Adj_Type_c|ID)+(LangEnvironment|Item), data=Data_Only_Adv,
                 family = bernoulli(link="logit"),
                 iter = 4000,
                 chains = 4,
                 cores = 4)


# Save the model object to a file
save(Bays_model_Advanced_LgEnv, file = "Bays_model_Advanced_LgEnv.RData")

# Load the model back into R
load("Bays_model_Advanced_LgEnv.RData")

# View the model 
Bays_model_Advanced_LgEnv


# get parameters of interest 
posteriors_parameters <- get_parameters(Bays_model_Advanced_LgEnv)
head(posteriors_parameters)
# Look at the length of the posteriors - how many iterations 
nrow(posteriors_parameters)
# 8000 iterations in total 

#See default prior distribution 
prior_summary(Bays_model_Advanced_LgEnv)

#See posterior distribution
summary(Bays_model_Advanced_LgEnv)

stancode(Bays_Bays_model_Advanced_LgEnv)

# Plot the posterior distributions of the parameters for fixed effects (modifier type and language type)
Bays_model_Advanced_LgEnv %>% 
  data.frame() %>% 
  select(starts_with("b")) %>%
  pivot_longer(everything(), names_to = "parameters", values_to = "estimates") %>%
  group_by(parameters) %>% 
  mutate(mean_estimates = mean(estimates)) %>%  # Compute mean of estimates for each parameter
  ggplot(aes(x = estimates, 
             y = reorder(parameters, mean_estimates),  # Reorder based on mean estimates
             fill = after_stat(quantile))) +
  geom_vline(xintercept = 0, color = "red") +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    scale = 0.9, 
    quantile_lines = TRUE, 
    quantiles = c(0.025, 0.975), 
    show.legend = F,
    rel_min_height = 0.01
  ) +
  scale_fill_manual(values = c("grey", "skyblue", "grey")) +
  labs(y = "Parameters (Ordered by Mean)", x = "Estimates") +
  theme_minimal()+
  coord_cartesian(xlim = c(-20, 50))



#Extract posterior samples
post_samples_Extracted = posterior_samples(Bays_model_Advanced_LgEnv)
head(post_samples_Extracted %>% round(1))

# proportion of positive samples for parameter b_Intercept
mean(post_samples_Extracted$b_Intercept > 0)
# More redundant result than non-redundant result for the reference group in color condition 

# Effects for reference group (Instructed L2 speakers)
mean(post_samples_Extracted$b_Adj_Type_c > 0)
mean(post_samples_Extracted$b_Frequency_c > 0)
mean(post_samples_Extracted$`b_Adj_Type_c:Frequency_c` <0)

# Evidence for difference between instructed L2 speakers vs. immersed L2 speakers 
mean(post_samples_Extracted$b_LangEnvironmentImmersed > 0)

#strong two way-interaction:  adjective type vs. language environment 
mean(post_samples_Extracted$`b_LangEnvironmentImmersed:Adj_Type_c`>0)

#no strong two way-interaction: frequency vs. language environment 
mean(post_samples_Extracted$`b_LangEnvironmentImmersed:Frequency_c` > 0)

#no strong three-way interaction
mean(post_samples_Extracted$`b_LangEnvironmentImmersed:Adj_Type_c:Frequency_c` < 0)


Bays_model_Advanced_L1Fr_LgEnv = brm(Accuracy ~ LangEnvironment*Adj_Type_c*Frequency_c+(Frequency_c*Adj_Type_c|ID)+(LangEnvironment|Item), data=Data_Only_Adv,
                                family = bernoulli(link="logit"),
                                iter = 4000,
                                chains = 4,
                                cores = 4)

