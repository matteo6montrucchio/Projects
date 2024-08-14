# Load the required libraries
library(ggplot2)
library(cluster)
library(reshape2)

# Display the first few rows of the dataset
heart <- read.csv("/Users/matteomontrucchio/Desktop/heart.csv")
View(heart)

# Display the summary after conversions
summary(heart)

# Check for missing values
sum(is.na(heart))

# Check to see the distribution
table(heart$output)
ggplot(heart, aes(x = factor(output), fill = factor(output))) +
  geom_bar() +
  labs(title = "Likelihood of Heart Attack",
       x = "Heart Attack",
       y = "Count") +
  scale_fill_manual(values = c("skyblue", "salmon"), name = "Heart attack",
                    labels = c("Less Likely", "More Likely")) +
  theme_minimal()


# UNIVARIATE ANALYSIS  

# Create histograms for continuos variables and check for abnormalities
hist(heart$age)
hist(heart$trtbps, breaks = 100)
hist(heart$chol, breaks = 80)
hist(heart$thalachh, breaks = 60)
hist(heart$oldpeak, breaks = 60)

# Barplots for non-continuous variables
ggplot(heart, aes(x = factor(sex, labels = c("Female", "Male")))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Gender",
       x = "Gender",
       y = "Count") +
  theme_minimal()

# Bar plot for Chest Pain Type
ggplot(heart, aes(x = factor(cp, labels = c("Typical Angina", "Atypical Angina", "Non-Anginal Pain", "Asymptomatic")))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Chest Pain Type",
       x = "Chest Pain Type",
       y = "Count") +
  theme_minimal()

# Bar plot for Fasting Blood Sugar
ggplot(heart, aes(x = factor(fbs, labels = c("False", "True")))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Fasting Blood Sugar",
       x = "Fasting Blood Sugar",
       y = "Count") +
  theme_minimal()

# Bar plot for Resting Electrocardiographic Results
ggplot(heart, aes(x = factor(restecg, labels = c("Normal", "ST-T Wave Abnormality", "Probable/Definite LVH")))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Resting Electrocardiographic Results",
       x = "Resting Electrocardiographic Results",
       y = "Count") +
  theme_minimal()

# Bar plot for Exercise-Induced Angina
ggplot(heart, aes(x = factor(exng, labels = c("No", "Yes")))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Exercise-Induced Angina",
       x = "Exercise-Induced Angina",
       y = "Count") +
  theme_minimal()

# Bar plot for Slope of the Peak Exercise ST Segment
ggplot(heart, aes(x = factor(slp, labels = c("Upsloping", "Flat", "Downsloping")))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Slope of the Peak Exercise ST Segment",
       x = "Slope of the Peak Exercise ST Segment",
       y = "Count") +
  theme_minimal()

# Bar plot for Number of Major Vessels
ggplot(heart, aes(x = factor(caa))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Number of Major Vessels",
       x = "Number of Major Vessels",
       y = "Count") +
  theme_minimal()

# Bar plot for Thallium Stress Test Result (excluding level 0)
ggplot(heart[heart$thall != 0, ], aes(x = factor(thall, labels = c("Normal", "Fixed Defect", "Reversible Defect")))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Thallium Stress Test Result",
       x = "Thallium Stress Test Result",
       y = "Count") +
  theme_minimal()


#CORRELATION

#Identify possible correlation btw the variables and the output
numeric_vars <- c("age", "sex", "cp", "trtbps", "chol", "fbs", "restecg", "thalachh", "exng", "oldpeak", "slp", "caa", "thall", "output")
numeric_data <- heart[, numeric_vars]
correlation_matrix <- cor(numeric_data)
correlation_with_output <- correlation_matrix[,"output"]
print(correlation_matrix)
print(correlation_with_output)
correlation_data <- data.frame(variable = names(correlation_with_output)[-14], correlation = correlation_with_output[-14])
corr_df <- melt(correlation_matrix)

ggplot(data = corr_df, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  geom_text(aes(Var2, Var1, label = round(value, 2)), size = 3) +
  scale_fill_gradient2(low = "blue", high = "red",
                       limit = c(-1,1), name="Correlation") +
  theme_minimal() +
  labs(x = "", y = "", title = "Correlation Matrix") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  


#BIVARIATE ANALYSIS
# Filter the data for cases where heart disease is equal to 1, I removed thall =0 bc there were just 2 instances and is not relevant
heart_disease_1 <- subset(heart, output == 1& thall != 0)

# Bar plot for non continuos variables
ggplot(heart_disease_1, aes(x = factor(sex, labels = c("Female", "Male")))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Gender in Heart Attack (output = 1)",
       x = "Gender",
       y = "Count") +
  theme_minimal()
ggplot(heart_disease_1, aes(x = factor(cp, labels = c("Typical Angina", "Atypical Angina", "Non-Anginal Pain", "Asymptomatic")))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Chest Pain Type in Heart Attack (output = 1)",
       x = "Chest Pain Type",
       y = "Count") +
  theme_minimal()
ggplot(heart_disease_1, aes(x = factor(fbs, labels = c("False", "True")))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Fasting Blood Sugar in Heart Attack (output = 1)",
       x = "Fasting Blood Sugar",
       y = "Count") +
  theme_minimal()
ggplot(heart_disease_1, aes(x = factor(restecg, labels = c("Normal", "ST-T Wave Abnormality", "Probable/Definite LVH")))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Resting Electrocardiographic Results in Heart Attack(output = 1)",
       x = "Resting Electrocardiographic Results",
       y = "Count") +
  theme_minimal()
ggplot(heart_disease_1, aes(x = factor(exng, labels = c("No", "Yes")))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Exercise-Induced Angina in Heart Attack (output = 1)",
       x = "Exercise-Induced Angina",
       y = "Count") +
  theme_minimal()
ggplot(heart_disease_1, aes(x = factor(slp, labels = c("Upsloping", "Flat", "Downsloping")))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Slope of the Peak Exercise ST Segment in Heart Attack (output = 1)",
       x = "Slope of the Peak Exercise ST Segment",
       y = "Count") +
  theme_minimal()
ggplot(heart_disease_1, aes(x = factor(caa))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Number of Major Vessels in Heart Attack (output = 1)",
       x = "Number of Major Vessels",
       y = "Count") +
  theme_minimal()
# Filter the data to exclude level 0 (Unknown) in 'thall'
heart_disease_1 <- subset(heart_disease_1, thall != 0)
ggplot(heart_disease_1, aes(x = factor(thall, labels = c("Normal", "Fixed Defect", "Reversible Defect")))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Thallium Stress Test Result in Heart Attack (output = 1)",
       x = "Thallium Stress Test Result",
       y = "Count") +
  theme_minimal()

#HISTOGRAMS  FOR CONTINUOUS VARIABLES
# Convert "output" to factor with levels "Less Likely" and "More Likely"
heart$output <- factor(heart$output, levels = c(0, 1), labels = c("Less Likely", "More Likely"))

# Check for NA values in the 'output' variable after the conversion
sum(is.na(heart$output))

# Plot histogram for age colored by output using geom_bar
ggplot(heart, aes(x = age, fill = output)) +
  geom_histogram(binwidth = 1, position = "identity", color = "black") +
  labs(x = "Age", y = "Frequency", title = "Heart Attack Status (H.A.) w.r.t. Age") +
  scale_fill_manual(values = c("Less Likely" = "lightblue", "More Likely" = "lightcoral")) +
  theme_minimal()

# Plot histogram for oldpeak colored by output using geom_bar
ggplot(heart, aes(x = oldpeak, fill = output)) +
  geom_histogram(binwidth = 0.5, position = "identity", color = "black") +
  labs(x = "Oldpeak", y = "Frequency", title = "Heart Attack Status (H.A.) w.r.t. Oldpeak") +
  scale_fill_manual(values = c("Less Likely" = "lightblue", "More Likely" = "lightcoral")) +
  theme_minimal()

# Plot histogram for trtbps colored by output using geom_bar
ggplot(heart, aes(x = trtbps, fill = output)) +
  geom_histogram(binwidth = 5, position = "identity", color = "black") +
  labs(x = "Resting Blood Pressure", y = "Frequency", title = "Heart Attack Status (H.A.) w.r.t. Resting Blood Pressure") +
  scale_fill_manual(values = c("Less Likely" = "lightblue", "More Likely" = "lightcoral")) +
  theme_minimal()

ggplot(heart, aes(x = thalachh, fill = output)) +
  geom_histogram(binwidth = 5, position = "identity", color = "black") +
  labs(x = "Maximum Heart Rate Achieved", y = "Frequency", 
       title = "Heart Attack Status (H.A.) w.r.t. Maximum Heart Rate Achieved") +
  scale_fill_manual(values = c("Less Likely" = "lightblue", "More Likely" = "lightcoral")) +
  theme_minimal()

ggplot(heart, aes(x = chol, fill = output)) +
  geom_histogram(binwidth = 10, position = "identity", color = "black") +
  labs(x = "Cholesterol Levels", y = "Frequency", 
       title = "Heart Attack Status (H.A.) w.r.t. Cholesterol Levels") +
  scale_fill_manual(values = c("Less Likely" = "lightblue", "More Likely" = "lightcoral")) +
  theme_minimal()



#PCA
# Exclude 'output' variable from the dataset
heart_pca <- heart[, -which(names(heart) == "output")]
# Scale numerical columns for PCA
heart_pca_scaled <- scale(heart_pca)
# Perform PCA
heart_pca_result <- prcomp(heart_pca_scaled)
# Display summary of PCA
summary(heart_pca_result)
# Display the first few rows of PCA results
head(heart_pca_result$x)
head(heart_pca_result$x[, 1:8])
# Plot PCA 
plot(heart_pca_result$x[, 1:8], col = "black", main = "PCA Results")



#HIERARCHICAL CLUSTERING
# Create hierarchical clustering
heart_pc_dist <- dist(heart_pca_result$x[, 1:8])
heart_pc_hc <- hclust(heart_pc_dist)

# Plot the hierarchical clustering
plot(heart_pc_hc)

# Cut the tree to form clusters
heart_pc_hc_clusters <- cutree(heart_pc_hc, 2)
heart_pc_hc_clusters
# Display a table of clusters
table(heart_pc_hc_clusters)

# K-MEANS
heart.sc = scale(heart)
heart.km2 = kmeans(heart.sc, 2)
heart.km2
heart.km2$cluster
aggregate(heart, by=list(heart.km2$cluster), FUN=mean)
aggregate(heart, by=list(heart.km2$cluster), FUN=summary)
library(fpc)
heart.dist = dist(heart.sc)
cluster.stats(heart.dist, heart.km2$cluster)
library(fpc)
cluster.stats(heart.dist, heart.km2$cluster)
library(cluster)
silhouette(heart.km2$cluster, heart.dist)
heart.km2.s = silhouette(heart.km2$cluster, heart.dist)
heart.km2.s[,1]
heart.km2.s[,2]
heart.km2.s[,3]
myclustercolors = c("red", "green")
plot(heart$age, heart$output, col = myclustercolors[heart.km2$cluster])
plot(heart$age, heart$trtbps, col = myclustercolors[heart.km2$cluster])
plot(heart$cp, heart$output, col = myclustercolors[heart.km2$cluster])
plot(heart$thalachh, heart$age, col = myclustercolors[heart.km2$cluster])
plot(heart$age, heart$thalachh, col = myclustercolors[heart.km2$cluster])
plot(heart$exng, heart$output, col = myclustercolors[heart.km2$cluster])
plot(heart$trtbps, heart$chol, col = myclustercolors[heart.km2$cluster])
plot(heart$trtbps, heart$thalachh, col = myclustercolors[heart.km2$cluster])

library(dbscan)
hullplot(heart[, 1:2], heart.km2$cluster)
hullplot(heart[, c(1,8)], heart.km2$cluster)
hullplot(heart[, c(3,14)], heart.km2$cluster)
hullplot(heart[, c(5,1)], heart.km2$cluster)

# FUZZY C-MEANS
library(e1071)
cmeans(heart.sc, 2)
heart.cm2 = cmeans(heart.sc, 2)
heart.cm2$membership

#REGRESSION
#Regression Model
install.packages("car") 
library(car)

#GLM -> logistic regression
heart$output <- factor(heart$output)
logreg <-glm(output ~ age + sex +cp + trtbps + chol + fbs + restecg +
               thalachh + exng + oldpeak + slp + caa + thall,
             family = binomial,
             data = heart)
summary(logreg)

logreg2 <-glm( output ~ sex + cp + thalachh + exng + oldpeak + caa + thall,
               family = binomial,
               data = heart)
summary(logreg2)

residuals <- residuals(logreg2, type = "deviance")

# Standardized residuals
std_resid <- residuals / sqrt(1 - hatvalues(logreg2))

# Create QQ plot for standardized residuals
qqnorm(std_resid)
qqline(std_resid)

#I want to know whether there are too many variables that have high correlation with each other
vif(logreg2)

#Residual Plots
residualPlots(logreg2)

#BAYES CLASSIFIER
heart$sex = as.factor(heart$sex)
heart$cp = as.factor(heart$cp)
heart$exng = as.factor(heart$exng)
heart$caa = as.factor(heart$caa)
heart$thall = as.factor(heart$thall)
nrecords = nrow(heart)
ntrain = as.integer(nrecords*0.8) 
heart.idx = sample(nrecords, ntrain)
heart.train = heart[heart.idx,] 
heart.test = heart[-heart.idx,]

#install.packages("e1071")
library(e1071)
heart.bayes = naiveBayes(output ~ sex + cp + thalachh + exng + oldpeak + caa + thall,
                         data = heart.train)

heart.pred4 = predict(object = heart.bayes, newdata = heart.test)
heart.pred4
cm4 = table(heart.test$output, heart.pred4)
#accuracy 
(cm4[1,1]+cm4[2,2])/sum(cm4)

# DECISION TREE

heart$sex = as.factor(heart$sex)
heart$cp = as.factor(heart$cp)
heart$exng = as.factor(heart$exng)
heart$caa = as.factor(heart$caa)
heart$thall = as.factor(heart$thall)
heart$output = as.factor(heart$output)
nrecords = nrow(heart)
ntrain = as.integer(nrecords * 0.8)
heart.idx = sample(nrecords, ntrain)
heart.train = heart[heart.idx,]
heart.test = heart[-heart.idx,]
library(rpart)
library(rpart.plot)
heart.dt = rpart(output ~ sex + cp + thalachh + exng + oldpeak + caa + thall, data = heart.train)
summary(heart.dt)
rpart.plot(heart.dt)
heart.pred = predict(heart.dt, heart.test, type = "class")
cm = table(heart.test$output, heart.pred)
cm
(cm[1,1]+cm[2,2])/sum(cm)