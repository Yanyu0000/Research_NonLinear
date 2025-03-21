# Non linear Simulation 
setwd("/Users/yanyuma/Documents/GitHub/Research_NonLinear")
library(ggplot2)
library(dplyr)
library(patchwork)
library(mlogit)
library(nnet)
library(randomForest)
library(pdp)
library(iml)
#library(rpart)
#library(partykit)

############################## Data Generate Process ##############################
set.seed(123)      #random seed
number <- 5000     # number of test sets
n <- number*2      # number we need to generate all data 
x1 <- x2 <- x3 <- x4 <- x5 <- x6 <- x7 <- rep(NA, n)
i <- 1
while(i<=10000){
  x2_r <- rnorm(1, mean = 60, sd = 10)           # Attribute - calorie
  x3_r <- rnorm(1, mean = 100, sd = 10)        # Attribute - juice size
  x4_r <- rbinom(1, size = 1, prob = 0.5)        # Attribute - organic
  x1_r <- (6000 - 0.8*(x2_r-60)^2 - 0.3*(x3_r-100)^2 + x4_r*30)/1000 + rnorm(1,3,1)  # Attribute - price
  if(x1_r > 0){
    x2[i] <- x2_r
    x3[i] <- x3_r
    x4[i] <- x4_r
    x1[i] <- x1_r
    x5[i] <- 0.1*x2_r + rnorm(1, mean = 25, sd = 5)     # Attribute - sugar is correlated with calorie
    x6[i] <- rbinom(1, size = 1, prob = 0.5)            # Attribute - recall
    x7[i] <- sample(1:4, 1)                             # Attribute - package color
    i=i+1
  }
}

## Random draw and divide equally into two options
x1_option1 <- sample(x1)[1:number]                 # apple juice price
x1_option2 <- sample(x1)[(number+1):n]             # berry juice price  
x2_option1 <- sample(x2)[1:number]                 # apple juice calorie
x2_option2 <- sample(x2)[(number+1):n]             # berry juice calorie
x3_option1 <- sample(x3)[1:number]                 # apple juice size
x3_option2 <- sample(x3)[(number+1):n]             # berry juice size  
x4_option1 <- sample(x4)[1:number]                 # apple juice: organic = 1, not organic = 0
x4_option2 <- sample(x4)[(number+1):n]             # berry juice: organic = 1, not organic = 0
x5_option1 <- sample(x5)[1:number]                 # apple juice sugar per serving 
x5_option2 <- sample(x5)[(number+1):n]             # berry juice sugar per serving 
x6_option1 <- sample(x6)[1:number]                 # apple juice recall 
x6_option2 <- sample(x6)[(number+1):n]             # berry juice recall
x7_option1 <- sample(x7)[1:number]                 # apple juice package color
x7_option2 <- sample(x7)[(number+1):n]             # berry juice package color

alpha0_A <- -0.6   # coefficients for Apple Juice
beta0_A <- 1
alpha1_A <- 0.6
alpha2_A <- 0.2
alpha3_A <- 0.4
beta1_A <- 0.5
beta2_A <- 0.1
beta3_A <- 0.2
  
alpha0_B <- -0.6  # coefficients for Berry Juice
beta0_B <- 1
alpha1_B <- 0.7
alpha2_B <- 0.3
alpha3_B <- 0.3
beta1_B <- 0.5
beta2_B <- 0.2
beta3_B <- 0.1

data_juice <- data.frame(
  ApplePrice = x1_option1,
  AppleCalorie = x2_option1,
  AppleSize = x3_option1,
  AppleOrganic = x4_option1,
  AppleSugar = x5_option1,
  AppleRecall = x6_option1,
  ApplePackage = x7_option1,
  BerryPrice = x1_option2,
  BerryCalorie = x2_option2,
  BerrySize = x3_option2,
  BerryOrganic = x4_option2,
  BerrySugar = x5_option2,
  BerryRecall = x6_option2,
  BerryPackage = x7_option2
)

data_juice$V_Apple <- alpha0_A * x1_option1^beta0_A + alpha1_A * x2_option1^alpha2_A * x3_option1^alpha3_A * x4_option1 +  beta1_A * x2_option1^beta2_A * x3_option1^beta3_A * (1-x4_option1)
data_juice$V_Berry <- alpha0_B * x1_option2^beta0_B + alpha1_B * x2_option2^alpha2_B * x3_option2^alpha3_B * x4_option2 +  beta1_B * x2_option2^beta2_B * x3_option2^beta3_B * (1-x4_option2) 
data_juice$e_Apple <- rlogis(nrow(data_juice), location = 0, scale = 3)
data_juice$e_Berry <- rlogis(nrow(data_juice), location = 0, scale = 3)
data_juice$U_Apple <- data_juice$V_Apple + data_juice$e_Apple         # U_Apple = V_Apple + e_Apple
data_juice$U_Berry <- data_juice$V_Berry + data_juice$e_Berry         # U_Apple = V_Apple + e_Apple
data_juice <- data_juice %>%
  mutate(
    Choice = ifelse(U_Apple > 0 & U_Apple > U_Berry, "Apple Juice",
                    ifelse(U_Berry > 0 & U_Berry >= U_Apple, "Berry Juice", "Opt out"))
  )

############################## Plot ##############################
data_juice_plot <- data.frame(
  U_Apple = data_juice$U_Apple,     
  U_Berry = data_juice$U_Berry)
ggplot(data_juice_plot, aes(x = U_Apple, y = U_Berry)) +     # first glance of data 
  geom_point() +  
  labs(
    x = expression(V[ia] + epsilon[ia]),
    y = expression(V[ib] + epsilon[ib])
  ) +
  theme_minimal() 

## plot 
data_juice_plot <- data_juice_plot %>%
  mutate(
    Choice = ifelse(U_Apple > 0 & U_Apple > U_Berry, "Apple Juice",
                    ifelse(U_Berry > 0 & U_Berry >= U_Apple, "Berry Juice", "Opt out"))
  )
ggplot(data_juice_plot, aes(x = U_Apple, y = U_Berry, colour = Choice)) +
  geom_point() +  
  labs(
    x = expression(V[ia] + epsilon[ia]),
    y = expression(V[ib] + epsilon[ib])
  ) +
  geom_segment(aes(x = -25, y=0, xend=0, yend=0), colour="black")+
  geom_segment(aes(x = 0, y=-20, xend=0, yend=0), colour="black")+
  geom_segment(aes(x = 0, y=0, xend=30, yend=30), colour="black")+
  theme_minimal()

ggsave("output/Juice Choice.pdf", width = 8, height = 6)

############################## Nonlinear Marginal utility ##############################
data_juice <- subset(data_juice, select = -c(V_Apple, e_Apple,U_Apple,V_Berry,e_Berry,U_Berry))

# first-order derivative
functionApple <- formula (y ~ alpha0_A * x1_option1^beta0_A + alpha1_A * x2_option1^alpha2_A * x3_option1^alpha3_A * x4_option1 +  beta1_A * x2_option1^beta2_A * x3_option1^beta3_A * (1-x4_option1) )
dfApplePrice <- deriv(functionApple,c("alpha0_A",
                                      "x1_option1",
                                      "beta0_A",
                                      "alpha1_A",
                                      "x2_option1",
                                      "alpha2_A",
                                      "x3_option1",
                                      "alpha3_A",
                                      "x4_option1",
                                      "beta1_A",
                                      "beta2_A",
                                      "beta3_A"),function.arg = TRUE)
betaApplePrice <- attr(dfApplePrice(alpha0_A,
                                    x1_option1,
                                    beta0_A,
                                    alpha1_A,
                                    x2_option1,
                                    alpha2_A,
                                    x3_option1,
                                    alpha3_A,
                                    x4_option1,
                                    beta1_A,
                                    beta2_A,
                                    beta3_A), "gradient") [,2]    # Marginal utility of Apple price 

dfAppleCalorie <- deriv(functionApple,c("alpha0_A",
                                        "x1_option1",
                                        "beta0_A",
                                        "alpha1_A",
                                        "x2_option1",
                                        "alpha2_A",
                                        "x3_option1",
                                        "alpha3_A",
                                        "x4_option1",
                                        "beta1_A",
                                        "beta2_A",
                                        "beta3_A"),function.arg = TRUE)
betaAppleCalorie <- attr(dfAppleCalorie(alpha0_A,
                                        x1_option1,
                                        beta0_A,
                                        alpha1_A,
                                        x2_option1,
                                        alpha2_A,
                                        x3_option1,
                                        alpha3_A,
                                        x4_option1,
                                        beta1_A,
                                        beta2_A,
                                        beta3_A), "gradient")[,5]   # Marginal utility of Apple Calorie
#0.6*0.2*x2_option1[1]^(-0.8)*x3_option1[1]^0.4*x4_option1[1]+0.5*0.1*x2_option1[1]^(-0.9)*x3_option1[1]^0.2*(1-x4_option1[1]) # manual test 

dfAppleSize <- deriv(functionApple,c("alpha0_A",
                                     "x1_option1",
                                     "beta0_A",
                                     "alpha1_A",
                                     "x2_option1",
                                     "alpha2_A",
                                     "x3_option1",
                                     "alpha3_A",
                                     "x4_option1",
                                     "beta1_A",
                                     "beta2_A",
                                     "beta3_A"),function.arg = TRUE)
betaAppleSize <- attr(dfAppleSize(alpha0_A,
                                  x1_option1,
                                  beta0_A,
                                  alpha1_A,
                                  x2_option1,
                                  alpha2_A,
                                  x3_option1,
                                  alpha3_A,
                                  x4_option1,
                                  beta1_A,
                                  beta2_A,
                                  beta3_A),"gradient") [,7]         # Marginal utility of Apple Size
#0.6*x2_option1[1]^0.2*0.4*x3_option1[1]^(-0.6)*x4_option1[1]+0.5*x2_option1[1]^0.1*0.2*x3_option1[1]^(-0.8)*(1-x4_option1[1]) # manual test

dfAppleOrganic <- deriv(functionApple,c("alpha0_A",
                                        "x1_option1",
                                        "beta0_A",
                                        "alpha1_A",
                                        "x2_option1",
                                        "alpha2_A",
                                        "x3_option1",
                                        "alpha3_A",
                                        "x4_option1",
                                        "beta1_A",
                                        "beta2_A",
                                        "beta3_A"),function.arg = TRUE)
betaAppleOrganic <- attr(dfAppleOrganic(alpha0_A,
                                        x1_option1,
                                        beta0_A,
                                        alpha1_A,
                                        x2_option1,
                                        alpha2_A,
                                        x3_option1,
                                        alpha3_A,
                                        x4_option1,
                                        beta1_A,
                                        beta2_A,
                                        beta3_A),"gradient") [,9]      # Marginal utility of Apple Organic 
#0.6*x2_option1[1]^0.2*x3_option1[1]^0.4 - 0.5*x2_option1[1]^0.1*x3_option1[1]^0.2 # manual test 

functionBerry <- formula (y ~ alpha0_B * x1_option2^beta0_B + alpha1_B * x2_option2^alpha2_B * x3_option2^alpha3_B * x4_option2 +  beta1_B * x2_option2^beta2_B * x3_option2^beta3_B * (1-x4_option2))
dfBerryPrice <- deriv(functionBerry,c("alpha0_B",
                                      "x1_option2",
                                      "beta0_B",
                                      "alpha1_B",
                                      "x2_option2",
                                      "alpha2_B",
                                      "x3_option2",
                                      "alpha3_B",
                                      "x4_option2",
                                      "beta1_B",
                                      "beta2_B",
                                      "beta3_B"),function.arg = TRUE)
betaBerryPrice <- attr(dfBerryPrice(alpha0_B,
                                    x1_option2,
                                    beta0_B,
                                    alpha1_B,
                                    x2_option2,
                                    alpha2_B,
                                    x3_option2,
                                    alpha3_B,
                                    x4_option2,
                                    beta1_B,
                                    beta2_B,
                                    beta3_B),"gradient") [,2]          # Marginal utility of Berry price 

dfBerryCalorie <- deriv(functionBerry,c("alpha0_B",
                                        "x1_option2",
                                        "beta0_B",
                                        "alpha1_B",
                                        "x2_option2",
                                        "alpha2_B",
                                        "x3_option2",
                                        "alpha3_B",
                                        "x4_option2",
                                        "beta1_B",
                                        "beta2_B",
                                        "beta3_B"),function.arg = TRUE)
betaBerryCalorie <- attr(dfBerryCalorie(alpha0_B,
                                        x1_option2,
                                        beta0_B,
                                        alpha1_B,
                                        x2_option2,
                                        alpha2_B,
                                        x3_option2,
                                        alpha3_B,
                                        x4_option2,
                                        beta1_B,
                                        beta2_B,
                                        beta3_B),"gradient") [,5]            # Marginal utility of Berry Calorie

dfBerrySize <- deriv(functionBerry,c("alpha0_B",
                                     "x1_option2",
                                     "beta0_B",
                                     "alpha1_B",
                                     "x2_option2",
                                     "alpha2_B",
                                     "x3_option2",
                                     "alpha3_B",
                                     "x4_option2",
                                     "beta1_B",
                                     "beta2_B",
                                     "beta3_B"),function.arg = TRUE)
betaBerrySize <- attr(dfBerrySize(alpha0_B,
                                  x1_option2,
                                  beta0_B,
                                  alpha1_B,
                                  x2_option2,
                                  alpha2_B,
                                  x3_option2,
                                  alpha3_B,
                                  x4_option2,
                                  beta1_B,
                                  beta2_B,
                                  beta3_B),"gradient") [,7]             # Marginal utility of Berry Size

dfBerryOrganic <- deriv(functionBerry,c("alpha0_B",
                                        "x1_option2",
                                        "beta0_B",
                                        "alpha1_B",
                                        "x2_option2",
                                        "alpha2_B",
                                        "x3_option2",
                                        "alpha3_B",
                                        "x4_option2",
                                        "beta1_B",
                                        "beta2_B",
                                        "beta3_B"),function.arg = TRUE)
betaBerryOrganic <- attr(dfBerryOrganic(alpha0_B,
                                        x1_option2,
                                        beta0_B,
                                        alpha1_B,
                                        x2_option2,
                                        alpha2_B,
                                        x3_option2,
                                        alpha3_B,
                                        x4_option2,
                                        beta1_B,
                                        beta2_B,
                                        beta3_B),"gradient") [,9]            # Marginal utility of Berry Organic


# Nonlinear Willingness to pay 
NOL_WTP_AppleCalorie <- -betaAppleCalorie/betaApplePrice  # (WTP_i)
NOL_WTP_AppleSize <- -betaAppleSize/betaApplePrice
NOL_WTP_AppleOrganic <--betaAppleOrganic/betaApplePrice

NOL_WTP_BerryCalorie <--betaBerryCalorie/betaApplePrice
NOL_WTP_BerrySize <- -betaBerrySize/betaApplePrice
NOL_WTP_BerryOrganic <- -betaBerryOrganic/betaApplePrice

## result for Average(WTP)  
mean_NOL_WTP_AppleCalorie <- mean(NOL_WTP_AppleCalorie) # Average(WTP_i)
mean_NOL_WTP_AppleSize <- mean(NOL_WTP_AppleSize)
mean_NOL_WTP_AppleOrganic <- mean(NOL_WTP_AppleOrganic)
sd_NOL_WTP_AppleCalorie <- sd(NOL_WTP_AppleCalorie)
sd_NOL_WTP_AppleSize <- sd(NOL_WTP_AppleSize)
sd_NOL_WTP_AppleOrganic <- sd(NOL_WTP_AppleOrganic)

mean_NOL_WTP_BerryCalorie <- mean(NOL_WTP_BerryCalorie)
mean_NOL_WTP_BerrySize <- mean(NOL_WTP_BerrySize)
mean_NOL_WTP_BerryOrganic <- mean(NOL_WTP_BerryOrganic)
sd_NOL_WTP_BerryCalorie <- sd(NOL_WTP_BerryCalorie)
sd_NOL_WTP_BerrySize <- sd(NOL_WTP_BerrySize)
sd_NOL_WTP_BerryOrganic <- sd(NOL_WTP_BerryOrganic)



############################## Multinominal Logit Model ##############################
Apple_data <- data_juice %>%
  mutate(AppleChoice = ifelse(Choice == "Apple Juice",1,0)) %>%
  select(-BerryPrice,-BerryCalorie,-BerrySize,-BerryOrganic,-BerrySugar,-BerryRecall,-BerryPackage,-Choice) 
MNL_Apple <- multinom(AppleChoice ~ ApplePrice + AppleCalorie + AppleSize + AppleOrganic + AppleSugar + AppleRecall + ApplePackage, data = Apple_data)
coef(MNL_Apple)
MNL_WTP_AppleCalorie <- -coef(MNL_Apple)["AppleCalorie"]/coef(MNL_Apple)["ApplePrice"]
MNL_WTP_AppleSize <- -coef(MNL_Apple)["AppleSize"]/coef(MNL_Apple)["ApplePrice"]
MNL_WTP_AppleOrganic <- -coef(MNL_Apple)["AppleOrganic"]/coef(MNL_Apple)["ApplePrice"]

Berry_data <- data_juice %>%
  mutate(BerryChoice = ifelse(Choice == "Berry Juice",1,0)) %>%
  select(-ApplePrice,-AppleCalorie,-AppleSize,-AppleOrganic,-AppleSugar,-AppleRecall,-ApplePackage,-Choice) 
MNL_Berry <- multinom(BerryChoice ~ BerryPrice + BerryCalorie + BerrySize + BerryOrganic + BerrySugar + BerryRecall + BerryPackage, data = Berry_data)
coef(MNL_Berry)
MNL_WTP_BerryCalorie <- -coef(MNL_Berry)["BerryCalorie"]/coef(MNL_Berry)["BerryPrice"]
MNL_WTP_BerrySize <- -coef(MNL_Berry)["BerrySize"]/coef(MNL_Berry)["BerryPrice"]
MNL_WTP_BerryOrganic <- -coef(MNL_Berry)["BerryOrganic"]/coef(MNL_Berry)["BerryPrice"]

compare_mean_MNL <- data.frame(
  Nonlinear = c(mean_NOL_WTP_AppleCalorie, mean_NOL_WTP_AppleSize, mean_NOL_WTP_AppleOrganic, mean_NOL_WTP_BerryCalorie, mean_NOL_WTP_BerrySize, mean_NOL_WTP_BerryOrganic),
  MNL = c(MNL_WTP_AppleCalorie, MNL_WTP_AppleSize, MNL_WTP_AppleOrganic, MNL_WTP_BerryCalorie, MNL_WTP_BerrySize, MNL_WTP_BerryOrganic)
)
print(compare_mean_MNL)



############################## Random Forest ##############################
train_indices <- sample(seq_len(nrow(data_juice)), size = 0.5 * nrow(data_juice))
train_data <- data_juice[train_indices, ]
test_data <- data_juice[-train_indices, ]
rf_model <- randomForest(as.factor(train_data$Choice) ~ ., importance = TRUE, data = train_data, ntree=5000)
ntree <- rf_model$ntree 

# importance plot
importance <- importance(rf_model)
importance_df <- as.data.frame(importance)
importance_df$Feature <- rownames(importance_df)
ggplot(importance_df, aes(x = reorder(Feature, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Feature Importance", x = "Feature", y = "Importance")

ggsave("output/importance.pdf", width = 8, height = 6)

# prediction rate
rf_predictions <- predict(rf_model, test_data)
conf_matrix <- confusionMatrix(rf_predictions, test_data$Choice)
print(conf_matrix)


#ntree <- rf_model$ntree 
#for (i in 1:ntree) {
#  cat("Tree", i, ":\n")
#  print(getTree(rf_model, k = i, labelVar = TRUE))
#  cat("\n")
#}

# single decision tree
# install.packages("devtools")
# devtools::install_github("araastat/reprtree")
# library(reprtree)
# reprtree::plot.getTree(rf_model, k = 1)
# tree_1 <- getTree(rf_model, k = 1, labelVar = TRUE)

# library(rpart)
# rpart_tree <- as.party(rpart(Choice ~ ., data = test_data))  
# plot(rpart_tree)  


# original RF
tree_predictions <- predict(rf_model, test_data, predict.all = TRUE)
print(tree_predictions$individual)     # Query the selection of each tree for each observation 

n_observations <- nrow(tree_predictions$individual)
n_trees <- ncol(tree_predictions$individual)
categories <- c("Apple Juice", "Berry Juice", "Opt out")
result_matrix <- matrix(0, nrow = n_observations, ncol = length(categories))
colnames(result_matrix) <- categories

for (i in 1:n_observations) {
  observation_predictions <- tree_predictions$individual[i, ]
  category_counts <- table(factor(observation_predictions, levels = categories))
  category_percentages <- prop.table(category_counts) 
  result_matrix[i, ] <- category_counts
}

result_df <- as.data.frame(result_matrix)
for (category in categories) {
  result_df[[paste0(category, "%")]] <- result_df[[category]] / n_trees 
}

print(result_df)

# change of ApplePrice 
ApplePrice_modified <- test_data

delta_ApplePrice <- 0.1
ApplePrice_modified$ApplePrice <- ApplePrice_modified$ApplePrice + delta_ApplePrice
ApplePrice_predictions_modified <- predict(rf_model, ApplePrice_modified, predict.all = TRUE)
ApplePrice_matrix_modified <- matrix(0, nrow = n_observations, ncol = length(categories))
colnames(ApplePrice_matrix_modified) <- categories

for (i in 1:n_observations) {
  observation_predictions_modified <- ApplePrice_predictions_modified$individual[i, ]
  category_counts_modified <- table(factor(observation_predictions_modified, levels = categories))
  ApplePrice_matrix_modified[i, ] <- category_counts_modified
}

result_ApplePrice_modified <- as.data.frame(ApplePrice_matrix_modified)
print(head(result_ApplePrice_modified))

derivative_ApplePrice <- (result_ApplePrice_modified$`Apple Juice` - result_df$`Apple Juice` )/(delta_ApplePrice*ntree) 

# change of AppleCalorie 
AppleCalorie_modified <- test_data

delta_AppleCalorie <- 0.1
AppleCalorie_modified$AppleCalorie <- AppleCalorie_modified$AppleCalorie + delta_AppleCalorie
AppleCalorie_predictions_modified <- predict(rf_model, AppleCalorie_modified, predict.all = TRUE)
AppleCalorie_matrix_modified <- matrix(0, nrow = n_observations, ncol = length(categories))
colnames(AppleCalorie_matrix_modified) <- categories

for (i in 1:n_observations) {
  observation_predictions_modified <- AppleCalorie_predictions_modified$individual[i, ]
  category_counts_modified <- table(factor(observation_predictions_modified, levels = categories))
  AppleCalorie_matrix_modified[i, ] <- category_counts_modified
}

result_AppleCalorie_modified <- as.data.frame(AppleCalorie_matrix_modified)
print(head(result_AppleCalorie_modified))

derivative_AppleCalorie <- (result_AppleCalorie_modified$`Apple Juice` - result_df$`Apple Juice` )/(delta_AppleCalorie*ntree) 

# change of AppleSize 
AppleSize_modified <- test_data

delta_AppleSize <- 0.1
AppleSize_modified$AppleSize <- AppleSize_modified$AppleSize + delta_AppleSize
AppleSize_predictions_modified <- predict(rf_model, AppleSize_modified, predict.all = TRUE)
AppleSize_matrix_modified <- matrix(0, nrow = n_observations, ncol = length(categories))
colnames(AppleSize_matrix_modified) <- categories

for (i in 1:n_observations) {
  observation_predictions_modified <- AppleSize_predictions_modified$individual[i, ]
  category_counts_modified <- table(factor(observation_predictions_modified, levels = categories))
  AppleSize_matrix_modified[i, ] <- category_counts_modified
}

result_AppleSize_modified <- as.data.frame(AppleSize_matrix_modified)
print(head(result_AppleSize_modified))

derivative_AppleSize <- (result_AppleSize_modified$`Apple Juice` - result_df$`Apple Juice` )/(delta_AppleSize*ntree) 

# change of AppleOrganic
AppleOrganic_modified <- test_data
ntrees_Aorg <-sum(AppleOrganic_modified$AppleOrganic == 0)
AppleOrganic_modified$AppleOrganic[AppleOrganic_modified$AppleOrganic == 1] <- 0 ## ANOTHER METHOD: focus on the good which is no-organic at first,when it turns to organic, the change of purchase%  


delta_AppleOrganic <- 1
AppleOrganic_modified$AppleOrganic <- AppleOrganic_modified$AppleOrganic + delta_AppleOrganic
AppleOrganic_predictions_modified <- predict(rf_model, AppleOrganic_modified, predict.all = TRUE)
AppleOrganic_matrix_modified <- matrix(0, nrow = n_observations, ncol = length(categories))
colnames(AppleOrganic_matrix_modified) <- categories

for (i in 1:n_observations) {
  observation_predictions_modified <- AppleOrganic_predictions_modified$individual[i, ]
  category_counts_modified <- table(factor(observation_predictions_modified, levels = categories))
  AppleOrganic_matrix_modified[i, ] <- category_counts_modified
}

result_AppleOrganic_modified <- as.data.frame(AppleOrganic_matrix_modified)
print(head(result_AppleOrganic_modified))

derivative_AppleOrganic <- (result_AppleOrganic_modified$`Apple Juice` - result_df$`Apple Juice` )/(delta_AppleOrganic*ntrees_Aorg) 


# change of BerryPrice 
BerryPrice_modified <- test_data

delta_BerryPrice <- 0.1
BerryPrice_modified$BerryPrice <- BerryPrice_modified$BerryPrice + delta_BerryPrice
BerryPrice_predictions_modified <- predict(rf_model, BerryPrice_modified, predict.all = TRUE)
BerryPrice_matrix_modified <- matrix(0, nrow = n_observations, ncol = length(categories))
colnames(BerryPrice_matrix_modified) <- categories

for (i in 1:n_observations) {
  observation_predictions_modified <- BerryPrice_predictions_modified$individual[i, ]
  category_counts_modified <- table(factor(observation_predictions_modified, levels = categories))
  BerryPrice_matrix_modified[i, ] <- category_counts_modified
}

result_BerryPrice_modified <- as.data.frame(BerryPrice_matrix_modified)
print(head(result_BerryPrice_modified))

derivative_BerryPrice <- (result_BerryPrice_modified$`Berry Juice` - result_df$`Berry Juice` )/(delta_BerryPrice*ntree) 

# change of BerryCalorie 
BerryCalorie_modified <- test_data

delta_BerryCalorie <- 0.1
BerryCalorie_modified$BerryCalorie <- BerryCalorie_modified$BerryCalorie + delta_BerryCalorie
BerryCalorie_predictions_modified <- predict(rf_model, BerryCalorie_modified, predict.all = TRUE)
BerryCalorie_matrix_modified <- matrix(0, nrow = n_observations, ncol = length(categories))
colnames(BerryCalorie_matrix_modified) <- categories

for (i in 1:n_observations) {
  observation_predictions_modified <- BerryCalorie_predictions_modified$individual[i, ]
  category_counts_modified <- table(factor(observation_predictions_modified, levels = categories))
  BerryCalorie_matrix_modified[i, ] <- category_counts_modified
}

result_BerryCalorie_modified <- as.data.frame(BerryCalorie_matrix_modified)
print(head(result_BerryCalorie_modified))

derivative_BerryCalorie <- (result_BerryCalorie_modified$`Berry Juice` - result_df$`Berry Juice` )/(delta_BerryCalorie*ntree) 

# change of BerrySize 
BerrySize_modified <- test_data

delta_BerrySize <- 0.1
BerrySize_modified$BerrySize <- BerrySize_modified$BerrySize + delta_BerrySize
BerrySize_predictions_modified <- predict(rf_model, BerrySize_modified, predict.all = TRUE)
BerrySize_matrix_modified <- matrix(0, nrow = n_observations, ncol = length(categories))
colnames(BerrySize_matrix_modified) <- categories

for (i in 1:n_observations) {
  observation_predictions_modified <- BerrySize_predictions_modified$individual[i, ]
  category_counts_modified <- table(factor(observation_predictions_modified, levels = categories))
  BerrySize_matrix_modified[i, ] <- category_counts_modified
}

result_BerrySize_modified <- as.data.frame(BerrySize_matrix_modified)
print(head(result_BerrySize_modified))

derivative_BerrySize <- (result_BerrySize_modified$`Berry Juice` - result_df$`Berry Juice` )/(delta_BerrySize*ntree) 

# change of BerryOrganic
BerryOrganic_modified <- test_data
ntrees_Borg <-sum(AppleOrganic_modified$BerryOrganic == 0)
BerryOrganic_modified$BerryOrganic[BerryOrganic_modified$BerryOrganic == 1] <- 0

delta_BerryOrganic <- 1
BerryOrganic_modified$BerryOrganic <- BerryOrganic_modified$BerryOrganic + delta_BerryOrganic
BerryOrganic_predictions_modified <- predict(rf_model, BerryOrganic_modified, predict.all = TRUE)
BerryOrganic_matrix_modified <- matrix(0, nrow = n_observations, ncol = length(categories))
colnames(BerryOrganic_matrix_modified) <- categories

for (i in 1:n_observations) {
  observation_predictions_modified <- BerryOrganic_predictions_modified$individual[i, ]
  category_counts_modified <- table(factor(observation_predictions_modified, levels = categories))
  BerryOrganic_matrix_modified[i, ] <- category_counts_modified
}

result_BerryOrganic_modified <- as.data.frame(BerryOrganic_matrix_modified)
print(head(result_BerryOrganic_modified))

derivative_BerryOrganic <- (result_BerryOrganic_modified$`Berry Juice` - result_df$`Berry Juice` )/(delta_BerryOrganic*ntrees_Borg) 

# wtp 
wtp_rf_AppleCalorie <- - derivative_AppleCalorie/derivative_ApplePrice
wtp_rf_AppleSize <- - derivative_AppleSize/derivative_ApplePrice
wtp_rf_AppleOrganic <- - derivative_AppleOrganic/derivative_ApplePrice
wtp_rf_BerryCalorie <- - derivative_BerryCalorie/derivative_BerryPrice
wtp_rf_BerrySize <- - derivative_BerrySize/derivative_BerryPrice
wtp_rf_BerryOrganic <- - derivative_BerryOrganic/derivative_BerryPrice

mean_rf_WTP_AppleCalorie <- mean(wtp_rf_AppleCalorie[is.finite(wtp_rf_AppleCalorie)])
mean_rf_WTP_AppleSize <- mean(wtp_rf_AppleSize[is.finite(wtp_rf_AppleSize)])
mean_rf_WTP_AppleOrganic <- mean(wtp_rf_AppleOrganic[is.finite(wtp_rf_AppleOrganic)])
mean_rf_WTP_BerryCalorie <- mean(wtp_rf_BerryCalorie[is.finite(wtp_rf_BerryCalorie)])
mean_rf_WTP_BerrySize <- mean(wtp_rf_BerrySize[is.finite(wtp_rf_BerrySize)])
mean_rf_WTP_BerryOrganic <- mean(wtp_rf_BerryOrganic[is.finite(wtp_rf_BerryOrganic)])

## compare result
compare_mean <- data.frame(
  Nonlinear = c(mean_NOL_WTP_AppleCalorie, mean_NOL_WTP_AppleSize, mean_NOL_WTP_AppleOrganic, mean_NOL_WTP_BerryCalorie, mean_NOL_WTP_BerrySize, mean_NOL_WTP_BerryOrganic),
  MNL = c(MNL_WTP_AppleCalorie, MNL_WTP_AppleSize, MNL_WTP_AppleOrganic, MNL_WTP_BerryCalorie, MNL_WTP_BerrySize, MNL_WTP_BerryOrganic),
  RandomForest = c(mean_rf_WTP_AppleCalorie,mean_rf_WTP_AppleSize,mean_rf_WTP_AppleOrganic,mean_rf_WTP_BerryCalorie,mean_rf_WTP_BerrySize,mean_rf_WTP_BerryOrganic)
)
print(compare_mean)

# histogram of the true WTPs + draw a vertical line showing the MNL estimates of the mean in red and the true means in blue
pdf("output/wtp_histograms.pdf", width = 8, height = 6)
par(mfrow=c(2,3))
hist(NOL_WTP_AppleCalorie, main="WTP for Apple Calorie", xlab="WTP")
abline(v = MNL_WTP_AppleCalorie, col = "red")
abline(v = mean_rf_WTP_AppleCalorie, col = "blue")
hist(NOL_WTP_AppleSize, main="WTP for Apple Size", xlab="WTP")
abline(v = MNL_WTP_AppleSize, col = "red")
abline(v = mean_rf_WTP_AppleSize, col = "blue")
hist(NOL_WTP_AppleOrganic, main="WTP for Apple Organic", xlab="WTP", xlim = c(4,15))
abline(v = MNL_WTP_AppleOrganic, col = "red")
abline(v = mean_rf_WTP_AppleOrganic, col = "blue")
hist(NOL_WTP_BerryCalorie, main="WTP for Berry Calorie", xlab="WTP")
abline(v = MNL_WTP_BerryCalorie, col = "red")
abline(v = mean_rf_WTP_BerryCalorie, col = "blue")
hist(NOL_WTP_BerrySize, main="WTP for Berry Size", xlab="WTP")
abline(v = MNL_WTP_BerrySize, col = "red")
abline(v = mean_rf_WTP_BerrySize, col = "blue")
hist(NOL_WTP_BerryOrganic, main="WTP for Berry Organic", xlab="WTP",xlim = c(1,20))
abline(v = MNL_WTP_BerryOrganic, col = "red")
abline(v = mean_rf_WTP_BerryOrganic, col = "blue")
dev.off()

# boxplot
pdf("output/wtp_boxplot.pdf", width = 8, height = 6)
par(mfrow=c(2,3))
boxplot(NOL_WTP_AppleCalorie, wtp_rf_AppleCalorie, names = c("True", "RF"), main = "AppleCalorie",ylim = c(-1,1), col = c("lightblue", "lightcoral"))
boxplot(NOL_WTP_AppleSize, wtp_rf_AppleSize, names = c("True", "RF"), main = "AppleSize", ylim = c(-1,1), col = c("lightblue", "lightcoral"))
boxplot(NOL_WTP_AppleOrganic, wtp_rf_AppleOrganic, names = c("True", "RF"), main = "AppleOrganic", ylim = c(-500,500),col = c("lightblue", "lightcoral"))
boxplot(NOL_WTP_BerryCalorie, wtp_rf_BerryCalorie, names = c("True", "RF"), main = "BerryCalorie",ylim = c(-1,1), col = c("lightblue", "lightcoral"))
boxplot(NOL_WTP_BerrySize, wtp_rf_BerrySize, names = c("True", "RF"), main = "BerrySize", ylim = c(-1,1), col = c("lightblue", "lightcoral"))
boxplot(NOL_WTP_BerryOrganic, wtp_rf_BerryOrganic, names = c("True", "RF"), main = "BerryOrganic", ylim = c(-500,500),col = c("lightblue", "lightcoral"))
dev.off()
