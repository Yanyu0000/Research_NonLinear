# Non linear Simulation 
library(ggplot2)
library(dplyr)
library(patchwork)
#library(evd)
#library(dfidx)
library(mlogit)
library(nnet)
library(randomForest)
library(pdp)
library(iml)

############################## Data Generate Process ##############################
set.seed(12345)      #random seed
number <- 5000     # number of test sets
n <- number*2      # number we need to generate all data 
x1 <- x2 <- x3 <- x4 <- rep(NA, n)
i <- 1
while(i<=10000){
  x2_r <- rnorm(1, mean = 60, sd = 10)           # Attribute - calorie
  x3_r <- rnorm(1, mean = 500, sd = 50)          # Attribute - juice size
  x4_r <- rbinom(1, size = 1, prob = 0.5)        # Attribute - organic
  x1_r <- (6000 - 0.8*(x2_r-60)^2 - 0.3*(x3_r-500)^2 + x4_r*30)/1000 + rnorm(1,3,1)  # Attribute - price
  if(x1_r > 0){
    x2[i] <- x2_r
    x3[i] <- x3_r
    x4[i] <- x4_r
    x1[i] <- x1_r
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

alpha0_A <- 0.6   # coefficients for Apple Juice
alpha1_A <- -0.6
alpha2_A <- 0.2
alpha3_A <- 0.4
alpha4_A <- 0.5
beta0_A <- 0.5
beta1_A <- 0.3
beta2_A <- 0.1
beta3_A <- 0.2
beta4_A <- 0.5

alpha0_B <- 0.6   # coefficients for Berry Juice
alpha1_B <- -0.5
alpha2_B <- 0.3
alpha3_B <- 0.3
alpha4_B <- 0.5
beta0_B <- 0.2
beta1_B <- 1.2
beta2_B <- 0.2
beta3_B <- 0.1
beta4_B <- 0.5

data_juice <- data.frame(
  ApplePrice = x1_option1,
  AppleCalorie = x2_option1,
  AppleSize = x3_option1,
  AppleOrganic = x4_option1,
  BerryPrice = x1_option1,
  BerryCalorie = x2_option1,
  BerrySize = x3_option1,
  BerryOrganic = x4_option1
)

data_juice$V_Apple <- alpha1_A * x1_option1^beta1_A + alpha0_A * x2_option1^alpha2_A * x3_option1^alpha3_A * x4_option1 +  beta0_A * x2_option1^beta2_A * x3_option1^beta3_A * (1-x4_option1) 
data_juice$V_Berry <- alpha1_B * x1_option2^beta1_B + alpha0_B * x2_option2^alpha2_B * x3_option2^alpha3_B * x4_option2 +  beta0_B * x2_option2^beta2_B * x3_option2^beta3_B * (1-x4_option2)
data_juice$e_Apple <- rlogis(nrow(data_juice), location = 0, scale = 3)
data_juice$e_Berry <- rlogis(nrow(data_juice), location = 0, scale = 3)

data_juice_plot <- data.frame(
  U_Apple = data_juice$V_Apple + data_juice$e_Apple,         # U_Apple = V_Apple + e_Apple
  U_Berry = data_juice$V_Berry + data_juice$e_Berry          # U_Berry = V_Berry + e_Berry
)
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


############################## Nonlinear Marginal utility ##############################
# first-order derivative
functionApple <- formula (y ~ alpha1_A * x1_option1^beta1_A + alpha0_A * x2_option1^alpha2_A * x3_option1^alpha3_A * x4_option1 +  beta0_A * x2_option1^beta2_A * x3_option1^beta3_A * (1-x4_option1) )
dfApplePrice <- deriv(functionApple,"x1_option1",function.arg = TRUE)
betaApplePrice <- attr(dfApplePrice(x1_option1), "gradient")     # Marginal utility of Apple price 
-0.6*0.3*x1_option1[1]^(-0.7) # manual test

dfAppleCalorie <- deriv(functionApple,"x2_option1",function.arg = TRUE)
betaAppleCalorie <- attr(dfAppleCalorie(x2_option1), "gradient")   # Marginal utility of Apple Calorie
0.6*0.2*x2_option1[1]^(-0.8)*x3_option1[1]^0.4*x4_option1[1]+0.5*0.1*x2_option1[1]^(-0.9)*x3_option1[1]^0.2*(1-x4_option1[1]) # manual test 

dfAppleSize <- deriv(functionApple,"x3_option1",function.arg = TRUE)
betaAppleSize <- attr(dfAppleSize(x3_option1),"gradient")          # Marginal utility of Apple Size
0.6*x2_option1[1]^0.2*0.4*x3_option1[1]^(-0.6)*x4_option1[1]+0.5*x2_option1[1]^0.1*0.2*x3_option1[1]^(-0.8)*(1-x4_option1[1]) # manual test

dfAppleOrganic <- deriv(functionApple,"x4_option1",function.arg = TRUE)
betaAppleOrganic <- attr(dfAppleOrganic(x4_option1),"gradient")     # Marginal utility of Apple Organic 
0.6*x2_option1[1]^0.2*x3_option1[1]^0.4 - 0.5*x2_option1[1]^0.1*x3_option1[1]^0. # manual test 

functionBerry <- formula (y ~ alpha1_B * x1_option2^beta1_B + alpha0_B * x2_option2^alpha2_B * x3_option2^alpha3_B * x4_option2 +  beta0_B * x2_option2^beta2_B * x3_option2^beta3_B * (1-x4_option2) )
dfBerryPrice <- deriv(functionBerry,"x1_option2",function.arg = TRUE)
betaBerryPrice <- attr(dfBerryPrice(x1_option2),"gradient")            # Marginal utility of Berry price 

dfBerryCalorie <- deriv(functionBerry,"x2_option2",function.arg = TRUE)
betaBerryCalorie <- attr(dfBerryCalorie(x2_option2),"gradient")          # Marginal utility of Berry Calorie

dfBerrySize <- deriv(functionBerry,"x3_option2",function.arg = TRUE)
betaBerrySize <- attr(dfBerrySize(x3_option2),"gradient")            # Marginal utility of Berry Size

dfBerryOrganic <- deriv(functionBerry,"x4_option2",function.arg = TRUE)
betaBerryOrganic <- attr(dfBerryOrganic(x4_option2),"gradient")          # Marginal utility of Berry Organic

## plot 
pic_ApplePrice <- ggplot(data = data.frame(betaApplePrice), aes(x = betaApplePrice)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.001, fill = "steelblue", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  theme_minimal() +
  labs(x = expression(paste( beta[italic(ApplePrice)])), y = "frequency") +
  theme(text = element_text(size = 14))

pic_AppleCalorie <- ggplot(data = data.frame(betaAppleCalorie), aes(x = betaAppleCalorie)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.0005, fill = "steelblue", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  theme_minimal() +
  labs(x = expression(paste( beta[italic(AppleCalorie)])), y = "frequency") +
  theme(text = element_text(size = 14))

pic_AppleSize <- ggplot(data = data.frame(betaAppleSize), aes(x = betaAppleSize)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.00008, fill = "steelblue", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  theme_minimal() +
  labs(x = expression(paste( beta[italic(AppleSize)])), y = "frequency") +
  theme(text = element_text(size = 14))

pic_AppleOrganic <- ggplot(data = data.frame(betaAppleOrganic), aes(x = betaAppleOrganic)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.025, fill = "steelblue", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  theme_minimal() +
  labs(x = expression(paste( beta[italic(AppleOrganic)])), y = "frequency") +
  theme(text = element_text(size = 14))
pic_ApplePrice+pic_AppleCalorie+pic_AppleSize+pic_AppleOrganic  # combine plot 

pic_BerryPrice <- ggplot(data = data.frame(betaBerryPrice), aes(x = betaBerryPrice)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.001, fill = "steelblue", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  theme_minimal() +
  labs(x = expression(paste( beta[italic(BerryPrice)])), y = "frequency") +
  theme(text = element_text(size = 14))

pic_BerryCalorie <- ggplot(data = data.frame(betaBerryCalorie), aes(x = betaBerryCalorie)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.0005, fill = "steelblue", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  theme_minimal() +
  labs(x = expression(paste( beta[italic(BerryCalorie)])), y = "frequency") +
  theme(text = element_text(size = 14))

pic_BerrySize <- ggplot(data = data.frame(betaBerrySize), aes(x = betaBerrySize)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.00005, fill = "steelblue", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  theme_minimal() +
  labs(x = expression(paste( beta[italic(BerrySize)])), y = "frequency") +
  theme(text = element_text(size = 14))

pic_BerryOrganic <- ggplot(data = data.frame(betaBerryOrganic), aes(x = betaBerryOrganic)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.025, fill = "steelblue", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  theme_minimal() +
  labs(x = expression(paste( beta[italic(BerryOrganic)])), y = "frequency") +
  theme(text = element_text(size = 14))
pic_BerryPrice+pic_BerryCalorie+pic_BerrySize+pic_BerryOrganic  # combine plot 


# Nonlinear Willingness to pay 
NOL_WTP_AppleCalorie <- -betaAppleCalorie/betaApplePrice  # WTP(Averagr)
NOL_WTP_AppleSize <- -betaAppleSize/betaApplePrice
NOL_WTP_AppleOrganic <--betaAppleOrganic/betaApplePrice

NOL_WTP_BerryCalorie <--betaBerryCalorie/betaApplePrice
NOL_WTP_BerrySize <- -betaBerrySize/betaApplePrice
NOL_WTP_BerryOrganic <- -betaBerryOrganic/betaApplePrice


NOL_WTP_AppleCalorie <- -betaAppleCalorie/betaApplePrice  # Average(WTP)
NOL_WTP_AppleSize <- -betaAppleSize/betaApplePrice
NOL_WTP_AppleOrganic <--betaAppleOrganic/betaApplePrice
  
NOL_WTP_BerryCalorie <--betaBerryCalorie/betaApplePrice
NOL_WTP_BerrySize <- -betaBerrySize/betaApplePrice
NOL_WTP_BerryOrganic <- -betaBerryOrganic/betaApplePrice

## plot 
pic_NOL_WTP_AppleCalorie <- ggplot(data = data.frame(NOL_WTP_AppleCalorie), aes(x = NOL_WTP_AppleCalorie)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.005, fill = "steelblue", color = "gray", alpha = 0.7) +
  geom_density(color = "pink", size = 1) +
  theme_minimal() +
  labs(x = "WTP AppleCalorie") +
  theme(text = element_text(size = 14))
 
pic_NOL_WTP_AppleSize <- ggplot(data = data.frame(NOL_WTP_AppleSize), aes(x = NOL_WTP_AppleSize)) +
   geom_histogram(aes(y = ..density..), binwidth = 0.0005, fill = "steelblue", color = "gray", alpha = 0.7) +
   geom_density(color = "pink", size = 1) +
   theme_minimal() +
   labs(x = "WTP AppleSize") +
   theme(text = element_text(size = 14))

pic_NOL_WTP_AppleOrganic <- ggplot(data = data.frame(NOL_WTP_AppleOrganic), aes(x = NOL_WTP_AppleOrganic)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.2, fill = "steelblue", color = "gray", alpha = 0.7) +
  geom_density(color = "pink", size = 1) +
  theme_minimal() +
  labs(x = "WTP AppleOrganic") +
  theme(text = element_text(size = 14))
pic_NOL_WTP_AppleCalorie + pic_NOL_WTP_AppleSize + pic_NOL_WTP_AppleOrganic

pic_NOL_WTP_BerryCalorie <- ggplot(data = data.frame(NOL_WTP_BerryCalorie), aes(x = NOL_WTP_BerryCalorie)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "steelblue", color = "gray", alpha = 0.7) +
  geom_density(color = "lightgreen", size = 1) +
  theme_minimal() +
  labs(x = "WTP BerryCalorie") +
  theme(text = element_text(size = 14))

pic_NOL_WTP_BerrySize <- ggplot(data = data.frame(NOL_WTP_BerrySize), aes(x = NOL_WTP_BerrySize)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.0001, fill = "steelblue", color = "gray", alpha = 0.7) +
  geom_density(color = "lightgreen", size = 1) +
  theme_minimal() +
  labs(x = "WTP BerrySize") +
  theme(text = element_text(size = 14))

pic_NOL_WTP_BerryOrganic <- ggplot(data = data.frame(NOL_WTP_BerryOrganic), aes(x = NOL_WTP_BerryOrganic)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.5, fill = "steelblue", color = "gray", alpha = 0.7) +
  geom_density(color = "lightgreen", size = 1) +
  theme_minimal() +
  labs(x = "WTP BerryOrganic") +
  theme(text = element_text(size = 14))
pic_NOL_WTP_BerryCalorie + pic_NOL_WTP_BerrySize + pic_NOL_WTP_BerryOrganic

## result for Average(WTP)
mean_NOL_WTP_AppleCalorie <- mean(NOL_WTP_AppleCalorie)
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

# WTP(Average)
NOL_WTPAverage_AppleCalorie <- -mean(betaAppleCalorie)/mean(betaApplePrice)  
NOL_WTPAverage_AppleSize <- -mean(betaAppleSize)/mean(betaApplePrice)
NOL_WTPAverage_AppleOrganic <- -mean(betaAppleOrganic)/mean(betaApplePrice)

NOL_WTPAverage_BerryCalorie <- -mean(betaBerryCalorie)/mean(betaApplePrice)
NOL_WTPAverage_BerrySize <- -mean(betaBerrySize)/mean(betaApplePrice)
NOL_WTPAverage_BerryOrganic <- -mean(betaBerryOrganic)/mean(betaApplePrice)

############################## Multinomial Logit Model ##############################
data_juice <- data_juice %>%
  mutate(Choice = data_juice_plot$Choice) %>%
  select(-V_Apple,-V_Berry,-e_Apple,-e_Berry)           
Apple_data <- data_juice %>%
  mutate(AppleChoice = ifelse(Choice == "Apple Juice",1,0)) %>%
  select(-BerryPrice,-BerryCalorie,-BerrySize,-BerryOrganic,-Choice) 
MNL_Apple <- multinom(AppleChoice ~ ApplePrice + AppleCalorie + AppleSize + AppleOrganic, data = Apple_data)
coef(MNL_Apple)
MNL_WTP_AppleCalorie <- -coef(MNL_Apple)["AppleCalorie"]/coef(MNL_Apple)["ApplePrice"]
MNL_WTP_AppleSize <- -coef(MNL_Apple)["AppleSize"]/coef(MNL_Apple)["ApplePrice"]
MNL_WTP_AppleOrganic <- -coef(MNL_Apple)["AppleOrganic"]/coef(MNL_Apple)["ApplePrice"]

Berry_data <- data_juice %>%
  mutate(BerryChoice = ifelse(Choice == "Berry Juice",1,0)) %>%
  select(-ApplePrice,-AppleCalorie,-AppleSize,-AppleOrganic,-Choice) 
MNL_Berry <- multinom(BerryChoice ~ BerryPrice + BerryCalorie + BerrySize + BerryOrganic, data = Berry_data)
coef(MNL_Berry)
MNL_WTP_BerryCalorie <- -coef(MNL_Berry)["BerryCalorie"]/coef(MNL_Berry)["BerryPrice"]
MNL_WTP_BerrySize <- -coef(MNL_Berry)["BerrySize"]/coef(MNL_Berry)["BerryPrice"]
MNL_WTP_BerryOrganic <- -coef(MNL_Berry)["BerryOrganic"]/coef(MNL_Berry)["BerryPrice"]

compare_mean <- data.frame(
  Nonlinear = c(NOL_WTPAverage_AppleCalorie, NOL_WTPAverage_AppleSize, NOL_WTPAverage_AppleOrganic, NOL_WTPAverage_BerryCalorie, NOL_WTPAverage_BerrySize, NOL_WTPAverage_BerryOrganic),
  MNL = c(MNL_WTP_AppleCalorie, MNL_WTP_AppleSize, MNL_WTP_AppleOrganic, MNL_WTP_BerryCalorie, MNL_WTP_BerrySize, MNL_WTP_BerryOrganic)
)
print(compare_mean)

#library(reshape2)
#compare_mean$Project <- c("Apple Calorie", "Apple Size", "Apple Organic", "Berry Calorie", "Berry Size", "Berry Organic")
#compare_mean_long <- melt(compare_mean, id.vars = "Project", variable.name = "Model", value.name = "WTP")
#ggplot(compare_mean_long, aes(x = WTP, fill = Model)) +
#  geom_histogram(position = "dodge", binwidth = 20, alpha = 0.7) +
#  facet_wrap(~ Project, scales = "free_y") +
#  labs(title = "Comparison of WTP by Model and Project",
#       x = "Willingness to Pay (WTP)",
#       y = "Frequency") +
#  theme_minimal() +
#  scale_fill_manual(values = c("blue", "red"))  # Optional: Cus


############################## Random Forest ##############################
train_indices <- sample(seq_len(nrow(data_juice)), size = 0.5 * nrow(data_juice))
train_data <- data_juice[train_indices, ]
test_data <- data_juice[-train_indices, ]
rf_model <- randomForest(as.factor(train_data$Choice) ~ ., importance = TRUE, data = train_data, ntree = 100)

importance <- importance(rf_model)
importance_df <- as.data.frame(importance)
importance_df$Feature <- rownames(importance_df)
ggplot(importance_df, aes(x = reorder(Feature, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Feature Importance", x = "Feature", y = "Importance")

# Partial Dependence plot 
par(mfrow = c(2, 3))
pd_AppleCalorie <- partial(rf_model, pred.var = "AppleCalorie", prob = TRUE)
pd1 <- plot(pd_AppleCalorie, main = "Partial Dependence Plot for 'AppleCalorie'")

pd_AppleSize <- partial(rf_model, pred.var = "AppleSize", prob = TRUE)
pd2 <- plot(pd_AppleSize, main = "Partial Dependence Plot for 'BerrySize'")

pd_AppleOrganic <- partial(rf_model, pred.var = "AppleOrganic", prob = TRUE)
pd3 <- plot(pd_AppleOrganic, main = "Partial Dependence Plot for 'AppleOrganic'")

pd_BerryCalorie <- partial(rf_model, pred.var = "BerryCalorie", prob = TRUE)
pd4 <- plot(pd_BerryCalorie, main = "Partial Dependence Plot for 'BerryCalorie'")

pd_BerrySize <- partial(rf_model, pred.var = "BerrySize", prob = TRUE)
pd5 <- plot(pd_BerrySize, main = "Partial Dependence Plot for 'BerrySize'")

pd_BerryOrganic <- partial(rf_model, pred.var = "BerryOrganic", prob = TRUE)
pd6 <- plot(pd_BerryOrganic, main = "Partial Dependence Plot for 'BerryOrganic'")
par(mfrow = c(2, 3))

