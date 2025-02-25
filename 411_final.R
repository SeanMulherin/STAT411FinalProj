

library(tidyverse)
library(corrplot)
library(caret)
library(leaps)
library(psych)


diab_df <- read.csv('diabetes.csv')
diab_df$Diabetes_012 <- recode(diab_df$Diabetes_012, '2'=1) |> as.numeric()
head(diab_df)

# Diabetes_012 is the response var, indicating risk level for developing Type 2 diab

############################################################################
# 1. EDA
############################################################################

cor_mat <- cor(diab_df)

barplot(table(diab_df$Diabetes_012),
        col = '#2774AE',
        main = 'Diabetes Risk Counts',
        xlab = 'Risk of Developing Type 2 Diabetes',
        names.arg = c('Low', 'Moderate-High'))

corrplot(cor_mat, method = "color", type = "upper",
         col = colorRampPalette(c("#2774AE", "white", "salmon"))(200),
         tl.col = "black", tl.srt = 45, diag = FALSE, tl.cex = 0.8)

corr_ordered <- cor_mat[, 1] |> sort(decreasing = TRUE); print(corr_ordered)
# Diabetes risk has highest POS corr with: GenHlth, HighBP, BMI, DiffWalk, HighChol, Age, HeartDiseaseorAttack, PhysHlth
# Diabetes risk has highest NEG corr with: Income, Education, PhysActivity


############################################################################
# 1. Linear Regression
############################################################################
set.seed(123)
n <- nrow(diab_df)
train_i <- sample(1:n, 0.8*n)
train_df <- diab_df[train_i, ]
test_df <- diab_df[-train_i, ]

model <- glm(Diabetes_012 ~ ., data = train_df, family = 'binomial')
summary(model)
plot(model)

pred <- predict(model, newdata = test_df)
pred <- ifelse(pred > 0.5, 1, 0)
pred <- factor(pred, levels = c(0, 1))
observed <- factor(test_df$Diabetes_012, levels = c(0, 1))

conf_matrix <- confusionMatrix(pred, observed, positive = '1')
print(conf_matrix)


model_subs <- regsubsets(diab_df[, -1], diab_df[, 1])
rs <- summary(model_subs)
plot(1:8, rs$rsq, main='n Predictors vs R Square',
     xlab='# of Predictors', ylab = 'R Square')

############################################################################
# 2. PCA
############################################################################
cor_mat <- cor(diab_df[, -1]) # remove response var

eigen_vals <- eigen(cor_mat)$values 
eigen_vecs <- eigen(cor_mat)$vectors 

pc_prop_of_total_var <- eigen_vals / sum(eigen_vals)

prop_table <- data.frame(
  PC_i = 1:length(pc_prop_of_total_var),
  proportion_percent = 100*pc_prop_of_total_var
  ) |> arrange(desc(proportion_percent))

prop_table
eigen_vecs

############################################################################
# 3. Factor Analysis
############################################################################
# We use cor mat because vars are on diff scales
# We use MLE instead of PCA to see if there are any latent factors
# PCA method is used to reduce dimensionality
# fm_pca <- principal(cor_mat, nfactors = 2, rotate = "varimax", covar = FALSE)

fa.parallel(cor_mat, fa = "fa", fm = "mle")
fm_mle <- fa(r = cor_mat, nfactors = 2, fm = "mle", rotate = "varimax", covar = FALSE)

factors_anal_results <- unclass(fm_mle$loadings)
colnames(factors_anal_results) <- c('Factor 1', 'Factor 2')
factors_anal_results






