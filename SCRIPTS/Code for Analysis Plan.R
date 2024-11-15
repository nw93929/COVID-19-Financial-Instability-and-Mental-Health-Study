## Load libraries and dataset
library(tidyverse)
library(dplyr)

load("SurveyDF.rda")
data <- da38964.0001

# extract numbers inside parentheses
extract_number <- function(x) {
  gsub("\\((\\d+)\\) .*", "\\1", x)
}
# apply the function to column 4-372
data[, 4:372] <- lapply(data[, 4:372], extract_number)

# convert character into integer
data[, 4:372] <- lapply(data[, 4:372], function(x) as.integer(x))
data[, 4:372] <- as.data.frame(data[, 4:372])

# Select variables of interest
data <- data %>%
  select(RACEREC, HOUSING_2, DRUG_DIS, RUCC, ALC_DIS, SEX, PHYS_HEALTH, EDUC, MILITARY,
         SLEEPHRS, MARSTAT, AGE, HHINC, MENT_HEALTH, EMP_1, EMP_2, STUDENT, 
         COV_EXTUI, COV_STIMULUS, COV_RENTMORT, COV_OTHERPAYMENT, COV_MEDBILLS,
         COV_MONEYFOOD, COV_CREDITDEBT, COV_MORTDEF_2, SAT1, SAT2, SAT3, SAT4, SAT5,
         DOWN, WORRY, INTEREST, NERVOUS, FINAL_WGT)

# data type and level validation
data = data %>%
  mutate_if(~is.numeric(.), as.factor) %>%
  mutate_at(vars(DOWN, WORRY, INTEREST, NERVOUS, SAT1, SAT2, SAT3, SAT4, SAT5, AGE), as.numeric)

data$SEX = factor(data$SEX, levels = c(1, 2, 3), labels = c("Male", "Female", "Other"))
data$MARSTAT = factor(data$MARSTAT, levels = c(1, 2, 3,4,5,6,99), 
                      labels = c("Married", "Divorced", "Separated", 
                                 "Widowed", "Single", "Unmarried couple", 
                                  "Refusal"))


cols_to_recode <- c("COV_RENTMORT", "COV_STIMULUS", "COV_OTHERPAYMENT", 
                    "COV_MEDBILLS", "COV_MONEYFOOD", "COV_CREDITDEBT",
                    "COV_MORTDEF_2", "COV_EXTUI")

# Recode values for all specified columns
data <- data %>%
  mutate(across(all_of(cols_to_recode), ~ recode(.,
                                                 `1` = "Yes",
                                                 `2` = "No",
                                                 `99` = "Refusal")))


data$RACEREC = factor(data$RACEREC, levels = c(1,2,3,4,5,6,98))
data$DRUG_DIS = factor(data$DRUG_DIS, levels = c(1,2,98,99), 
                       labels = c("Yes", "No", "Don't Know", "Refusal"))
data$RUCC = factor(data$RUCC, levels = c(1,2,3,4,5,6,7,8,9))
data$ALC_DIS = factor(data$ALC_DIS, levels = c(1,2,98,99), 
                      labels = c("Yes", "No", "Don't Know", "Refusal"))
data$PHYS_HEALTH = factor(data$PHYS_HEALTH, levels = c(1,2,3,4,5), 
                          labels = c("Excellent", "Very Good", "Good", "Fair", "Poor"))
data$EDUC = factor(data$EDUC, levels = c(1,2,3,4,5,6))
data$MILITARY = factor(data$MILITARY, levels = c(1,2,3,4,99), 
                       labels = c("Never", "Current", "Past", "Training", "Refusal"))
data$HHINC = factor(data$HHINC, levels = c(1,2,3,4,5,6,7,8,9,10,11,12,98))

data <- data %>% 
  mutate(across(SAT1:SAT5, ~ case_when(
    . %in% 1:5 ~ as.numeric(.),
    . %in% c(6, 7, 98, 99) ~ NA_real_,
    TRUE ~ NA_real_
  )))
data <- data %>% 
  mutate(across(c(WORRY, DOWN, INTEREST, NERVOUS), ~ case_when(
    . %in% 1:4 ~ as.numeric(.),
    . %in% c(5,6, 98, 99) ~ NA_real_,
    TRUE ~ NA_real_
  )))
# turn the worry variables scale into PHQ4 corresponding scale
data <- data %>%
  mutate(across(c(DOWN, WORRY, INTEREST, NERVOUS), ~ case_when(
    . == 1 ~ 0,
    . == 2 ~ 1,
    . == 3 ~ 2,
    . == 4 ~ 3,
    TRUE ~ NA_real_ # Keep any unexpected values as NA
  )))

# SAT variable creation
data = data %>% mutate(SAT_total = ifelse(rowSums(select(., SAT1:SAT5) == 99, na.rm = TRUE) > 0, NA, 
                                          rowSums(select(., SAT1:SAT5), na.rm = TRUE)))

# PHQ variable creation
data = data %>% mutate(across(c(DOWN, WORRY, INTEREST, NERVOUS), 
                              ~ ifelse(. %in% c(98,99), NA, .)))
data = data %>% mutate(PHQ = rowSums(select(., DOWN, WORRY, INTEREST, NERVOUS), 
                                     na.rm = TRUE))

# EMPLOYMENT variable creation
data = data %>% mutate(EMP = factor(paste(EMP_1, EMP_2, STUDENT, sep = "_"), 
                                    levels = c("0_0_0", "0_0_1", "0_1_0", 
                                               "0_1_1", "1_0_0", "1_0_1"), 
                                    labels = c("None", "Student", 
                                               "EmployedMultiple", 
                                               "EmployedMultiple and Student", 
                                               "EmployedSingle", 
                                               "EmployedSingle and Student")))

# COVID rentmort + other variable creation
data = data %>% 
  mutate(COV_MISSPAY = if_else(COV_RENTMORT == "Yes" | COV_OTHERPAYMENT == "Yes", "Yes", "No"))
data$COV_MISSPAY <- factor(data$COV_MISSPAY)

data$SLEEPHRS<-as.numeric((data$SLEEPHRS))
data$FINAL_WGT<-as.numeric((data$FINAL_WGT))

## Analyze paterns of missing data
### Replace refusal, 98, 99, 100 with NA
data[data == "Refusal" | data == 98 | data == 99 | data == 100 | data == 96] <- NA
colSums(is.na(data))
data <- na.omit(data)

### MCA dimension reduction method for selecting categorical variable
library(FactoMineR)
library(factoextra)
library(MASS)

mca_data_clean <- data[, c("COV_STIMULUS", "COV_MORTDEF_2", 
                                            "COV_MEDBILLS", "COV_OTHERPAYMENT", 
                                            "COV_CREDITDEBT", "COV_RENTMORT", 
                                            "COV_MONEYFOOD", "COV_EXTUI")]
mca_data_clean <- na.omit(mca_data_clean)

library(MASS)
mca_data_clean <- mca_data_clean %>%
  mutate(across(everything(), as.factor))  # Convert all columns to factors

# Check the structure of the updated data frame
str(mca_data_clean)
all(sapply(mca_data_clean, is.factor))
sapply(mca_data_clean, function(x) length(unique(x)))
mca2 = mca(mca_data_clean, nf = 5)

# eigenvalues
mca2$d^2
total_variance <- sum(mca2$d^2)

# Calculate the variance explained by each dimension
variance_explained <- mca2$d^2 / total_variance

# Create a data frame for better visualization
variance_df <- data.frame(
  Dimension = paste("Dimension", 1:length(mca2$d^2)),
  Eigenvalue = mca2$d^2,
  Variance_Explained = variance_explained
)
## visualize the variance explained by each dimension in MCA
ggplot(variance_df, aes(x = Dimension, y = Variance_Explained)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = scales::percent(Variance_Explained)), vjust = -0.5) +
  labs(title = "Variance Explained by Each Dimension in MCA",
       x = "Dimensions",
       y = "Variance Explained") +
  theme_minimal()
head(mca2$cs) #column coordinates
head(mca2$rs) #row coordinates

# data frame for ggplot
cats = apply(mca_data_clean, 2, function(x) nlevels(as.factor(x)))
mca2_vars_df = data.frame(mca2$cs, Variable = rep(names(cats), cats))

library(ggplot2)
# plot
ggplot(data = mca2_vars_df, 
       aes(x = X1, y = X2, label = rownames(mca2_vars_df))) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_text(aes(colour = Variable)) +
  ggtitle("MCA plot of variables using R package MASS")


set.seed(123)  # For reproducibility
kmeans_result <- kmeans(mca2_vars_df[, c("X1", "X2")], centers = 6)  # Adjust centers as needed
mca2_vars_df$cluster <- as.factor(kmeans_result$cluster)
library(ggrepel)
ggplot(data = mca2_vars_df, aes(x = X1, y = X2, label = rownames(mca2_vars_df), colour = cluster)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_text_repel() +
  ggtitle("MCA plot of variables using R package MASS with Clustering")


## Remove unnecessary columns
data$SAT1<-NULL
data$SAT2<-NULL
data$SAT3<-NULL
data$SAT4<-NULL
data$SAT5<-NULL
data$DOWN<-NULL
data$NERVOUS<-NULL
data$INTEREST<-NULL
data$WORRY<-NULL
data$COV_RENTMORT<-NULL
data$COV_OTHERPAYMENT<-NULL
data$STUDENT<-NULL
data$EMP_1<-NULL
data$EMP_2<-NULL

## MLR w/ Life Satisfaction 
library(dplyr)
dataSat<-data
dataSat$PHQ<-NULL
data.no.wt <- dataSat %>% select(-FINAL_WGT)

hist(data.no.wt$SAT_total)

## Checking MLR assumptions 
result<-lm(SAT_total~., data=data.no.wt, weights = data$FINAL_WGT)
par(mfrow=c(2,2))
plot(result)

## ACF
acf(result$res, main="ACF Plot")

## VIFs for multicollinearity
library(car)
vif(result)

## ANOVA F test and individual t tests
summary(result)

## Model building
### Start with demographic var
demo_vars <- c("SEX", "EDUC", "RACEREC", "EMP", "MARSTAT", "HOUSING_2", 
               "RUCC", "PHYS_HEALTH", "HHINC", "MENT_HEALTH","AGE", 
               "MILITARY","SLEEPHRS")
cat_demo <- c("SEX", "EDUC", "RACEREC", "EMP", "MARSTAT", "HOUSING_2", 
              "SLEEPHRS", "RUCC", "PHYS_HEALTH", "MILITARY", "HHINC")
# Convert categorical variables to factors
data.no.wt[cat_demo] <- lapply(data.no.wt[cat_demo], as.factor)

# Create a model with only demographic variables
demo_model <- lm(SAT_total ~ ., data = data.no.wt[, c("SAT_total", demo_vars)], weights = data$FINAL_WGT)
summary(demo_model)

### Model comparison
### Forward Selection on demographic variables
forward_model<-step(regnull, scope=list(lower=regnull, upper=regfull), direction="forward")
summary(forward_model)

### Add Financial Variables
fin_vars <- c("COV_EXTUI", "COV_STIMULUS", "COV_MEDBILLS", "MONEYFOOD", "CREDITDEBIT") 
demoFin_result <- lm(SAT_total~MENT_HEALTH + PHYS_HEALTH + MARSTAT + HOUSING_2 +
                       SEX + AGE + SLEEPHRS + HHINC + EDUC + EMP + MILITARY + 
                       COV_EXTUI+COV_STIMULUS+COV_MEDBILLS+ 
                       COV_MONEYFOOD+COV_CREDITDEBT, 
                     data = data.no.wt, weights = data$FINAL_WGT)

summary(demoFin_result)

### Partial F test for financial reduced model
###### drop COV_STIMULUS, and  COV_MEDBILLS
reducedFindemo <- lm(SAT_total~MENT_HEALTH + PHYS_HEALTH + MARSTAT + HOUSING_2 +
                       SEX + AGE + SLEEPHRS + HHINC + EDUC + EMP + MILITARY + 
                       COV_MONEYFOOD+COV_CREDITDEBT+COV_EXTUI, 
                     data = data.no.wt, weights = data$FINAL_WGT)

anova(reducedFindemo, demoFin_result)

### Partial F test to compare demo model and Findemo model
anova(forward_model, reducedFindemo)

### Check assumptions of our final model
par(mfrow=c(2,2))
plot(reducedFindemo)



