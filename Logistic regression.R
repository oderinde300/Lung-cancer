setwd("C:\\Users\\Barakat Ogunjoun\\OneDrive\\Desktop\\Publications")

#import libraries
library(readr)
library(fastDummies)
library(corrplot)
library(ggplot2)
library(lmtest)
library(car)
library(mgcv)
library(UPG)

#import dataset
data  <- read.csv("survey lung cancer.csv", header = T)
head(data)
summary(data)
str(data)

# Cross-tabulation for SMOKING and LUNG_CANCER
smoking_vs_cancer <- table(data$SMOKING, data$LUNG_CANCER)

# Convert the cross-tabulation to a data frame
smoking_vs_cancer_df <- as.data.frame.matrix(smoking_vs_cancer)
smoking_vs_cancer_df$SMOKING <- rownames(smoking_vs_cancer_df)

# Plot
ggplot(smoking_vs_cancer_df, aes(x = SMOKING, y = YES, fill = NO)) +
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

data <-dummy_cols(data, select_columns = c("GENDER","LUNG_CANCER"))
data <- dummy_cols(data, select_columns = c("GENDER","LUNG_CANCER"), remove_selected_columns = TRUE)
data = subset(data, select = -c(GENDER_M,LUNG_CANCER_NO,AGE) )
head(data)
attach(data)
summary(data)
correlation_matrix <- cor(data)
corrplot(correlation_matrix, method = "color")



#Logit model
logit_model <- glm(LUNG_CANCER_YES ~ factor(SMOKING) + factor(YELLOW_FINGERS) + factor(CHRONIC.DISEASE) + 
                     factor(FATIGUE) + factor(ALLERGY) + factor(ALCOHOL.CONSUMING) + factor(COUGHING) + 
                     factor(SWALLOWING.DIFFICULTY), family = binomial(link = "logit"))
summary(logit_model)
BIC(logit_model)
plot(logit_model)
residualcl <- resid(logit_model)

plot(density(residualcl))
shapiro.test(residualcl)
bptest(logit_model)
bgtest(logit_model)
vif(logit_model)

#Probit model
probit_model <- glm(LUNG_CANCER_YES ~ factor(SMOKING) + factor(YELLOW_FINGERS) + factor(CHRONIC.DISEASE) + 
                      factor(FATIGUE) + factor(ALLERGY) + factor(ALCOHOL.CONSUMING) + factor(COUGHING) + 
                      factor(SWALLOWING.DIFFICULTY), family = binomial(link = "probit"))
summary(probit_model)
BIC(probit_model)
plot(probit_model)
residualcp <- resid(probit_model)
plot(density(residualcl))
shapiro.test(residualcl)
bptest(logit_model)
bgtest(logit_model)
vif(logit_model)


# Bayesian model

#Logit
model_logit <- UPG(y = LUNG_CANCER_YES, X = model.matrix(~ factor(SMOKING) + factor(YELLOW_FINGERS) + 
                                                           factor(CHRONIC.DISEASE) + factor(FATIGUE) + 
                                                           factor(ALLERGY) +factor(ALCOHOL.CONSUMING) + 
                                                           factor(COUGHING) + factor(SWALLOWING.DIFFICULTY)), 
                   model = "logit")

# Print model summary
summary(model_logit)
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
AIC((model_probit))
BIC(model_probit)
UPG.Diag(model_probit)
