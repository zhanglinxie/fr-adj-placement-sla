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
library(dplyr)


library(readxl)
library(xlsx)


library(readr)
# read_csv 通常能自动处理大部分编码问题

setwd("/Users/lucie/Desktop/NewExp/Data")


response <- read_xlsx("response.xlsx")
Frlevel <- read_xlsx("Frlevel.xlsx")
profil_long <- read_xlsx("profil3.xlsx")


# 无论 Q_Name 这一列里有多少种题目，一行代码搞定
profil_wide <- profil_long %>%
  pivot_wider(
    names_from = Questions,   # 题目名这一列，里面的内容会变成新列名
    values_from = Answers  # 答案内容这一列，填入对应的格子里
  )
profil_wide

profil <- read_xlsx("profil_wide.xlsx")
mean(as.numeric(profil$age), na.rm = TRUE) #33.28 
profil %>%
  count(gender) #f:17,male9
profil %>%
  count(niveauEN)
profil %>% 
  count(niveauFr)
profil %>% 
  count(edu)

# 如果没安装，先运行 install.packages("writexl")
#library(writexl)

# 语法：write_xlsx(你要保存的变量名, "路径/文件名.xlsx")
# write_xlsx(profil_wide,"/Users/lucie/Desktop/NewExp/Data/profil_wide.xlsx")



df <- merge(response,Frlevel,by="ID")

library(dplyr)
library(stringr)

df <- df %>%
  mutate(Accuracy= if_else(str_trim(Response) == str_trim(CorrectAnswer), 1, 0))
View(df)
df %>% count(Condition)

#Centralize data 
df$Frequency<-ifelse(df$Condition=="PréN_NonConnu" |df$Condition=="PostN_NonConnu","Rare","Frequent")
View(df)
df$AdjPosition<-ifelse(df$Condition=="PréN_NonConnu"|df$Condition=="PréN_Connu","PreN","PostN")
View(df)


#Centralize data 
df$Frequency_c<-scale(ifelse(df$Condition=="PréN_NonConnu" |df$Condition=="PostN_NonConnu",0,1))
View(df)
df$AdjPosition_c<-scale(ifelse(df$Condition=="PréN_NonConnu"|df$Condition=="PréN_Connu",0,1))
View(df)





# Descriptive stat ------------------------------------------------------------------------Barplot 

# Accuracy by condition ------------------------------------------------------------------------

# Build a table 
graph <- do.call(data.frame, aggregate(df$Accuracy, by = list(AdjPosition = df$AdjPosition, Frequency = df$Frequency), FUN = function(x) c(mean = mean(x), sd = sd(x), n = length(x))))
graph$se <- graph$x.sd/sqrt(graph$x.n)
colnames(graph) <- c("Adj_Position","Frequency","Accuracy","sd","n","se")

View(graph)


#Recode the 5 levels to category - Regroup 5 level to beginner vs. intermediate vs. advanced group 
df <- df %>%
  mutate(FrProficiency_Group = case_when(
    FrProficiency == "A2" ~ "Beginner",
    FrProficiency %in% c("B1", "B2") ~ "Intermediate",
    FrProficiency %in% c("C1", "C2") ~ "Advanced",
    TRUE ~ "Other/Unknown" # Always good to have a fallback
  ))


graph

# Descriptive stat ------------------------------------------------------------------------Barplot 

# Accuracy by condition ------------------------------------------------------------------------

# 1. Update the grouping in your summary data
graph <- graph %>%
  group_by( Learner_Group, Adj_Position, Frequency) %>% # Add it here!
  summarise(
    Accuracy = mean(Accuracy, na.rm = TRUE),
    se = sd(Accuracy, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# 2. Now run your plot code
ggplot(data = graph, aes(x = Adj_Position, y = Accuracy, fill = Frequency)) + 
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.6) + 
  geom_text(aes(label = scales::percent(round(Accuracy, 3))), 
            size = 3, vjust = -0.5, 
            position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymax = Accuracy + se, ymin = Accuracy - se), 
                position = position_dodge(width = 0.8), width = 0.1) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1.1)) + # Increased limit for labels
  theme_bw() +
  facet_wrap(~  Learner_Group )



graph$Adj_Position<- factor(graph$Adj_Position, levels = c("PreN","PostN"))
graph$Frequency <- factor(graph$Frequency, levels = c("Rare","Frequent"))

#Plot a figure with text 
library(ggplot2)
library(scales)

# set dodge width once to avoid mismatches
ggplot(data=graph, aes(x=Adj_Position,y=Accuracy,fill=Frequency)) + 
  geom_bar(stat = "identity", position = "dodge", width =0.6) + 
  geom_text(aes(label=percent(round(Accuracy,3.5))),size=3,vjust=-0.5,position = position_dodge(0.5))+
  geom_errorbar(aes(y = Accuracy, x = Adj_Position, ymax = Accuracy + se, ymin = Accuracy - se, group = Frequency), position = position_dodge(width = 0.8), width = 0.05, size = 0.75) +
  scale_y_continuous(labels = scales::percent,limits=c(0,1))+
  theme_bw()


library(ggplot2)
library(scales)


df$ID <- as.factor(df$ID)
df$Item <- as.factor(df$Item)

library(lme4)



## Bayes model ----

library(brms)
library(bayesplot)
library(insight)
library(ggridges)

table(df$ID, df$Condition) #within
table(df$Item, df$Condition) #between

table(df$ID, df$Frequency) #within
table(df$Item, df$Frequency) #between

# Convert to an ordinal variable
FrProficiency <- factor(df$FrProficiency, 
                levels = c("A2", "B1","B2","C1","C2"), 
                ordered = TRUE)


Bays_model = brm(Accuracy ~ FrProficiency*AdjPosition_c*Frequency_c+(Frequency_c*AdjPosition_c|ID)+(FrProficiency|Item), data=df,
                 family = bernoulli(link="logit"),
                 iter = 4000,
                 chains = 4,
                 cores = 4)


save(Bays_model, file = "Bayes_model.RData")

# Load the model back into R
load("Bayes_model.RData")

# View the model 
Bays_model


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
mean(post_samples_Extracted$b_AdjPosition_c> 0)
mean(post_samples_Extracted$b_Frequency_c < 0)
mean(post_samples_Extracted$`b_AdjPosition_c:Frequency_c`<0)


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


#2. Frequency factors as a continuous variable 
# 注意：语法是 "新名字" = "旧名字"

is.numeric(df$Item)

library(dplyr)


library(tidyverse)

mapping_df <- data.frame(
  Freq = c(671.86, 226.45, 1106.8, 209.45, 252.69, 76.43, 27.51, 2.65, 9.46, 29.36, 
           44.64, 52.59, 12.14, 12.31, 69.17, 23.09, 17.07, 1.23, 1.11, 0.70, 0.78, 1.73, 4.76),
  Item = c(1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24)
)

# 1. 主表清理
df <- df %>%
  mutate(Item_clean = as.character(round(as.numeric(as.character(Item)), 4)))

# 2. 映射表清理（注意括号位置）
mapping_df <- mapping_df %>%
  mutate(Item_clean = as.character(round(as.numeric(as.character(Item)), 4))) # 4 是 round 的参数

# 3. 关联
df <- df %>%
  left_join(mapping_df %>% select(Item_clean, Freq), by = "Item_clean")

Bays_model_FreqAsConVar = brm(Accuracy ~ FrProficiency*AdjPosition_c*Freq+(Freq*AdjPosition_c|ID)+(FrProficiency|Item), data=df,
                 family = bernoulli(link="logit"),
                 iter = 4000,
                 chains = 4,
                 cores = 4)



save(Bays_model_FreqAsConVar, file = "Bays_model_FreqAsConVar.RData")

# Load the model back into R
load("Bays_model_FreqAsConVar.RData")

# View the model 
Bays_model_FreqAsConVar


# get parameters of interest 
posteriors_parameters <- get_parameters(Bays_model_FreqAsConVar)

head(posteriors_parameters)
# Look at the length of the posteriors - how many iterations 
nrow(posteriors_parameters)
# 8000 iterations in total 

#See default prior distribution 
prior_summary(Bays_model_FreqAsConVar)

#See posterior distribution
summary(Bays_model_FreqAsConVar)

stancode(Bays_model_FreqAsConVar)


#Extract posterior samples
post_samples_Extracted = posterior_samples(Bays_model_FreqAsConVar)
head(post_samples_Extracted %>% round(1))

# proportion of positive samples for parameter b_Intercept
mean(post_samples_Extracted$b_Intercept > 0)
# More redundant result than non-redundant result for the reference group in color condition 

# Testing the main effects of the two factors 
mean(post_samples_Extracted$b_AdjPosition_c> 0)
mean(post_samples_Extracted$b_Freq < 0)
mean(post_samples_Extracted$`b_AdjPosition_c:Freq`<0)


# Plot the posterior distributions of the parameters for fixed effects (modifier type and language type)
Bays_model_FreqAsConVar %>% 
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

