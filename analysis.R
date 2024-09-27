setwd("C:\\Users\\Barakat Ogunjoun\\OneDrive\\Desktop\\Publications")

#import libraries
library(brms)
library(rstanarm)
library(readr)
library(cowplot)
library(fastDummies)
library(corrplot)
library(ggplot2)
library(dplyr)
library(lmtest)
library(car)
library(mgcv)
library(pROC)
library(UPG)

#import dataset
data  <- read.csv("survey lung cancer.csv", header = T)
head(data)
summary(data)
str(data)

# Cross-tabulation for SMOKING and LUNG_CANCER
smoking_gender <- xtabs(~LUNG_CANCER + GENDER, data = data)
smoking_gender <- as.data.frame.matrix(smoking_gender)
smoking_gender$SMOKING <- rownames(smoking_gender)
smoking_vs_cancer <- table(data$SMOKING, data$LUNG_CANCER)

# Convert the cross-tabulation to a data frame
smoking_vs_cancer_df <- as.data.frame.matrix(smoking_vs_cancer)
smoking_vs_cancer_df$SMOKING <- rownames(smoking_vs_cancer_df)

# Plot
ggplot(smoking_gender, aes(x = SMOKING, y = M, fill = F)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Cross-tabulation of Smoking and Lung Cancer", x = "Smoking", y = "Lung Cancer") +
  theme_minimal()

lung_cancer_data <- data[data$LUNG_CANCER == "YES", ]
# Plot age distribution
ggplot(lung_cancer_data, aes(x = AGE)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(x = "Age", y = "Count", title = "Age Distribution of Patients with Lung Cancer") +
  theme_minimal()

#correlation heatmap
# Convert all variables except 'z' to character
data[, -which(names(data) == "AGE")] <- lapply(data[, -which(names(data) == "AGE")], 
                                               as.character)
str(data)data <- dummy_cols(data, select_columns = c("GENDER", "SMOKING",
                                             "YELLOW_FINGERS", "ANXIETY", "ALLERGY",
                                             "PEER_PRESSURE", "CHRONIC.DISEASE",
                                             "FATIGUE", "WHEEZING", "COUGHING",
                                             "ALCOHOL.CONSUMING", "CHEST.PAIN",
                                             "SWALLOWING.DIFFICULTY", 
                                             "SHORTNESS.OF.BREATH", "LUNG_CANCER"), 
                   remove_selected_columns = TRUE)
data = subset(data, select = -c(GENDER_F, SMOKING_1,YELLOW_FINGERS_1, 
                                ANXIETY_1, ALLERGY_1, PEER_PRESSURE_1, COUGHING_1,
                                CHRONIC.DISEASE_1, FATIGUE_1, WHEEZING_1, 
                                ALCOHOL.CONSUMING_1, CHEST.PAIN_1, 
                                SWALLOWING.DIFFICULTY_1, SHORTNESS.OF.BREATH_1, LUNG_CANCER_NO))
data <- data %>%
  rename(gender = GENDER_M, smoking = SMOKING_2, `yellow fingers` = YELLOW_FINGERS_2, 
         anxiety = ANXIETY_2, allergy = ALLERGY_2, `peer pressure` = PEER_PRESSURE_2, coughing = COUGHING_2, 
         `chronic disease` = CHRONIC.DISEASE_2, fatigue = FATIGUE_2, wheezing = WHEEZING_2,
         `alcohol consumption` = ALCOHOL.CONSUMING_2, `chest pain` = CHEST.PAIN_2, 
         `swallowing difficulty` = SWALLOWING.DIFFICULTY_2, 
         `shortnesss of breath` = SHORTNESS.OF.BREATH_2, `lung cancer` = LUNG_CANCER_YES)
head(data)
attach(data)
summary(data)
correlation_matrix <- cor(data)
corrplot(correlation_matrix, method = "color")

ggplot(data, aes(x = factor(gender), fill = factor(`lung cancer`))) +
  geom_bar(position = "dodge") +
  labs(x = "Gender", y = "Count", fill = "Lung cancer") +
  scale_fill_discrete(labels = c("No", "Yes")) +
  scale_x_discrete(labels = c("Male", "Female"))

data_proportions <- data %>%
  group_by(`lung cancer`, smoking) %>%
  tally() %>%
  group_by(smoking) %>%
  mutate(Proportion = n / sum(n))
ggplot(data_proportions, aes(x = factor(`lung cancer`), y= Proportion, fill = factor(smoking))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(limits = c(0,1), labels = scales::percent_format()) +
  labs(x = "Lung cancer", y = "Proportion", fill = "Smoking") +
  scale_fill_discrete(labels = c("No", "Yes")) +
  scale_x_discrete(labels = c("No", "Yes"))+
  facet_wrap(~ smoking, labeller = as_labeller(c('0' = "No", '1' = "Yes")))


colnames(data)
#Logit model
logit_model <- glm(factor(`lung cancer`) ~ factor(smoking) + factor(anxiety) +
                     factor(`yellow fingers`) + factor(`chronic disease`) + 
                     factor(fatigue) + factor(allergy) + factor(coughing) +
                     factor(`peer pressure`) + factor(wheezing) + AGE +
                     factor(`alcohol consumption`) + factor(coughing) + 
                     factor(`swallowing difficulty`) + factor(`chest pain`) +
                     factor(`shortnesss of breath`) + factor(gender), 
                   family = binomial(link = "logit"), data = data)
summary(logit_model)
sd(logit_model$fitted.values)
BIC(logit_model)
confint(logit_model)

predicted_logit <- data.frame(probability.of.LC = logit_model$fitted.values, LC = as.factor(data$`lung cancer`))
predicted_logit <- predicted_logit[order(predicted_logit$probability.of.LC, decreasing = FALSE),]  
predicted_logit$rank <- 1:nrow(predicted_logit)

ggplot(data = predicted_logit, aes(x = rank, y = probability.of.LC)) +
  geom_point(aes(color = LC, alpha = 1, stroke = 2)) +
  xlab("Index") +
  ylab("Predicted probability of getting lung cancer (logit)")

# Predict outcomes
predicted_probabilities <- predict(logit_model, type = "response")
predicted_classes <- ifelse(predicted_probabilities > 0.5, "Yes", "No")
# Actual classes
actual_classes <- data$`lung cancer`

# Create confusion matrix
confusion_matrix <- table(Predicted = predicted_classes, Actual = actual_classes)
print(confusion_matrix)

# Calculate precision
predicted_classes_logit <- ifelse(predicted_classes == "Yes", 1, 0)
#actual_classes <- ifelse(actual_classes == "YES", 1, 0)
precision_logit <- sum(predicted_classes_logit & actual_classes) / sum(predicted_classes_logit)
print(precision_logit)

# Calculate recall
recall_logit <- sum(predicted_classes_logit & actual_classes) / sum(actual_classes)
print(recall_logit)

# Calculate F1-score
f1_score_logit <- 2 * (precision_logit * recall_logit) / (precision_logit + recall_logit)
print(f1_score_logit)


#ROC and AUC
roc_obj <- roc(actual_classes, predicted_classes_logit)
plot(roc_obj)
auc(roc_obj)
ggroc(roc_obj) + ggtitle("Logit ROC Curve") + xlab("1 - Specificity") + ylab("Sensitivity")
auc_value <- round(auc(roc_obj), 4)
ggroc(roc_obj) + ggtitle(paste0("Logit ROC Curve (AUC = ", auc_value, ")"))


plot(logit_model)
residualcl <- resid(logit_model)

plot(density(residualcl))
shapiro.test(residualcl)
bptest(logit_model)
bgtest(logit_model)
vif(logit_model)

#Probit model
probit_model <- glm(factor(`lung cancer`) ~ factor(smoking) + factor(anxiety) +
                      factor(`yellow fingers`) + factor(`chronic disease`) + 
                      factor(fatigue) + factor(allergy) + factor(coughing) +
                      factor(`peer pressure`) + factor(wheezing) + AGE +
                      factor(`alcohol consumption`) + factor(coughing) + 
                      factor(`swallowing difficulty`) + factor(`chest pain`) +
                      factor(`shortnesss of breath`) + factor(gender), 
                    family = binomial(link = "probit"), data = data)
summary(probit_model)
sd(probit_model$fitted.values)
BIC(probit_model)
confint(probit_model)

predicted_probit <- data.frame(probability.of.LC = probit_model$fitted.values, LC = as.factor(data$`lung cancer`))
predicted_probit <- predicted_probit[order(predicted_probit$probability.of.LC, decreasing = FALSE),]  
predicted_probit$rank <- 1:nrow(predicted_probit)

ggplot(data = predicted_probit, aes(x = rank, y = probability.of.LC)) +
  geom_point(aes(color = LC, alpha = 1, stroke = 2)) +
  xlab("Index") +
  ylab("Predicted probability of getting lung cancer (probit)")

# Predict outcomes
predicted_probabilities_probit <- predict(probit_model, type = "response")
predicted_classes_probit <- ifelse(predicted_probabilities_probit > 0.5, "YES", "NO")

# Create confusion matrix
confusion_matrix_probit <- table(Predicted = predicted_classes_probit, Actual = actual_classes)
print(confusion_matrix_probit)

# Calculate precision
predicted_classes_probit <- ifelse(predicted_classes_probit == "YES", 1, 0)
actual_classes <- ifelse(actual_classes == "YES", 1, 0)
precision_probit <- sum(predicted_classes_probit & actual_classes) / sum(predicted_classes_probit)
print(precision_probit)

# Calculate recall
recall_probit <- sum(predicted_classes_probit & actual_classes) / sum(actual_classes)
print(recall_probit)

# Calculate F1-score
f1_score_probit <- 2 * (precision_probit * recall_probit) / (precision_probit + recall_probit)
print(f1_score_probit)

#ROC and AUC
roc_obj_probit <- roc(actual_classes, predicted_classes_probit)
plot(roc_obj_probit)
auc(roc_obj_probit)
ggroc(roc_obj_probit) + ggtitle("Probit ROC Curve") + xlab("1 - Specificity") + ylab("Sensitivity")
auc_value <- round(auc(roc_obj_probit), 4)
ggroc(roc_obj_probit) + ggtitle(paste0("Probit ROC Curve (AUC = ", auc_value, ")"))

plot(probit_model)
residualcp <- resid(probit_model)
plot(density(residualcl))
shapiro.test(residualcl)
bptest(probit_model)
bgtest(probit_model)
vif(probit_model)


# Bayesian model with UPG

#Logit
model_logit <- UPG(y = LUNG_CANCER_YES, X = model.matrix(~ factor(SMOKING) + factor(YELLOW_FINGERS) + 
                                                           factor(CHRONIC.DISEASE) + factor(FATIGUE) + 
                                                           factor(ALLERGY) +factor(ALCOHOL.CONSUMING) + 
                                                           factor(COUGHING) + factor(SWALLOWING.DIFFICULTY)), 
                   model = "logit")

# Print model summary
summary(model_logit)
posterior <- as.numeric(model_logit$posterior[[1]])
sd(posterior)
AIC(model_logit)
BIC(model_logit)
UPG.Diag(model_logit)

#probit
model_probit <- UPG(y = LUNG_CANCER_YES, X = model.matrix(~ factor(SMOKING) + factor(YELLOW_FINGERS) + 
                                                            factor(CHRONIC.DISEASE) + factor(FATIGUE) + 
                                                            factor(ALLERGY) + factor(ALCOHOL.CONSUMING) + 
                                                            factor(COUGHING) + factor(SWALLOWING.DIFFICULTY)), 
                    model = "probit")

# Print model summary
summary(model_probit)
sd(model_probit$posterior$beta)
AIC((model_probit))
BIC(model_probit)
UPG.Diag(model_probit)

###Bayesian model with brms###
# Fit Bayesian probit regression model
model_probit2 <- brm(LUNG_CANCER_YES ~ factor(SMOKING) + factor(YELLOW_FINGERS) + factor(CHRONIC.DISEASE) + 
                      factor(FATIGUE) + factor(ALLERGY) + factor(ALCOHOL.CONSUMING) + factor(COUGHING) + 
                      factor(SWALLOWING.DIFFICULTY), family = bernoulli(link = "probit"), data = data)

# Fit Bayesian logistic regression model
model_logit2 <- brm(LUNG_CANCER_YES ~ factor(SMOKING) + factor(YELLOW_FINGERS) + factor(CHRONIC.DISEASE) + 
                     factor(FATIGUE) + factor(ALLERGY) + factor(ALCOHOL.CONSUMING) + factor(COUGHING) + 
                     factor(SWALLOWING.DIFFICULTY), family = bernoulli(link = "logit"))

# Summary of the models
summary(model_probit2)
summary(model_logit2)

# Plot posterior predictive checks
pp_check(model_probit)
pp_check(model_logit)
