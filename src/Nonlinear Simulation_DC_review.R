# Non linear Simulation 
library(ggplot2)
library(dplyr)
library(patchwork)
library(mlogit)
library(nnet)
library(randomForest)
library(pdp)
library(iml)
library(reshape2)
library(caret) # To plot confusion matrices
#library(rpart)
#library(partykit)


############################## Data Generation Process ##############################
set.seed(123)
number <- 10000
n <- number*2

# Define parameters for the four utility functions
# Price sensitivity
alpha <- -0.5      

# Intercept (to shift distribution of choices)
beta_const <- 7.0

# Apple parameters
beta_cal_A <- -1.2       
beta_size_A <- -0.7       
beta_interact_A <- 0.5    

# Berry parameters
beta_cal_B <- -1.3        
beta_size_B <- -0.6       
beta_interact_B <- 0.6    

# Organic effect (multiplicative)
organic_effect_A <- 0.8  # For Apple (0.8 means 80% less negative impact)
organic_effect_B <- 0.6  # For Berry (0.6 means 60% less negative impact)

# Initialize vectors
x1 <- x2 <- x3 <- x4 <- x5 <- x6 <- x7 <- rep(NA, n)

i <- 1
while(i <= n) {
  x1_r <- runif(1, 1, 5)            # Random price
  x2_r <- runif(1, -4, 3)           # Normalized calorie
  x3_r <- runif(1, -4, 3)           # Normalized size
  x4_r <- rbinom(1, size = 1, prob = 0.5)  # Organic (binary)
  
  x1[i] <- x1_r
  x2[i] <- x2_r
  x3[i] <- x3_r
  x4[i] <- x4_r
  
  # Other attributes
  x5[i] <- 0.2*x2_r + runif(1, -0.3, 0.3)  # Sugar correlated with calorie
  x6[i] <- rbinom(1, size = 1, prob = 0.5) # Recall
  x7[i] <- sample(1:4, 1)                  # Package color
  
  i <- i + 1
}

# Split attributes between options
x1_option1 <- sample(x1)[1:number]
x1_option2 <- sample(x1)[(number+1):n]
x2_option1 <- sample(x2)[1:number]
x2_option2 <- sample(x2)[(number+1):n]
x3_option1 <- sample(x3)[1:number]
x3_option2 <- sample(x3)[(number+1):n]
x4_option1 <- sample(x4)[1:number]
x4_option2 <- sample(x4)[(number+1):n]
x5_option1 <- sample(x5)[1:number]
x5_option2 <- sample(x5)[(number+1):n]
x6_option1 <- sample(x6)[1:number]
x6_option2 <- sample(x6)[(number+1):n]
x7_option1 <- sample(x7)[1:number]
x7_option2 <- sample(x7)[(number+1):n]

# Create dataframe
data_juice <- data.frame(
  ID = 1:number,
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

# Calculate utilities based on product type and organic status
data_juice$V_Apple <- rep(NA, nrow(data_juice))
data_juice$V_Berry <- rep(NA, nrow(data_juice))

# Loop through each row to apply the right utility function
for (i in 1:nrow(data_juice)) {
  # Calculate base utility components for Apple
  apple_quadratic_component <- (beta_cal_A * (data_juice$AppleCalorie[i]^2) + 
                                beta_size_A * (data_juice$AppleSize[i]^2) + 
                                beta_interact_A * data_juice$AppleCalorie[i] * data_juice$AppleSize[i])
  
  # Apply organic multiplicative effect for Apple
  if (data_juice$AppleOrganic[i] == 1) {
    # For organic products, reduce the negative impact
    apple_quadratic_component <- apple_quadratic_component * organic_effect_A
  }
  
  # Complete Apple utility calculation
  data_juice$V_Apple[i] <- beta_const + alpha * data_juice$ApplePrice[i] + apple_quadratic_component
  
  # Calculate base utility components for Berry
  berry_quadratic_component <- (beta_cal_B * (data_juice$BerryCalorie[i]^2) + 
                               beta_size_B * (data_juice$BerrySize[i]^2) + 
                               beta_interact_B * data_juice$BerryCalorie[i] * data_juice$BerrySize[i])
  
  # Apply organic multiplicative effect for Berry
  if (data_juice$BerryOrganic[i] == 1) {
    # For organic products, reduce the negative impact
    berry_quadratic_component <- berry_quadratic_component * organic_effect_B
  }
  
  # Complete Berry utility calculation
  data_juice$V_Berry[i] <- beta_const + alpha * data_juice$BerryPrice[i] + berry_quadratic_component
}

# Add error terms and calculate total utility
data_juice$e_Apple <- rlogis(nrow(data_juice), location = 0, scale = 0.25)
data_juice$e_Berry <- rlogis(nrow(data_juice), location = 0, scale = 0.25)
data_juice$U_Apple <- data_juice$V_Apple + data_juice$e_Apple
data_juice$U_Berry <- data_juice$V_Berry + data_juice$e_Berry

# Determine choice
data_juice <- data_juice %>%
  mutate(
    Choice = ifelse(U_Apple > 0 & U_Apple > U_Berry, "Apple Juice",
                    ifelse(U_Berry > 0 & U_Berry >= U_Apple, "Berry Juice", "Opt out"))
  )

############################## Plot decisions ##############################
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
  geom_point(alpha = 0.5) +  
  labs(
    x = expression(V[ia] + epsilon[ia]),
    y = expression(V[ib] + epsilon[ib])
  ) +
  geom_segment(aes(x = -4, y=0, xend=0, yend=0), colour="black")+
  geom_segment(aes(x = 0, y=-4, xend=0, yend=0), colour="black")+
  geom_segment(aes(x = 0, y=0, xend=3, yend=3), colour="black")+
  theme_minimal()

# Save as square pdf
ggsave("output/decision_plot.pdf", width = 6, height = 6)


############################## Plot Level Curves ##############################
grid_size <- 100
calorie_values <- seq(-4, 3, length.out = grid_size)
size_values <- seq(-4, 3, length.out = grid_size)
grid_data <- expand.grid(Calorie = calorie_values, Size = size_values)

# Function to calculate utility (excluding price and constant)
calculate_utility <- function(calorie, size, beta_cal, beta_size, beta_interact, organic_effect = 1) {
  quadratic_component <- beta_const + beta_cal * (calorie^2) + beta_size * (size^2) + beta_interact * calorie * size
  return(quadratic_component * organic_effect)
}

# Calculate utilities for each combination
grid_data$AppleOrganic <- calculate_utility(grid_data$Calorie, grid_data$Size, 
                                          beta_cal_A, beta_size_A, beta_interact_A, 
                                          organic_effect_A)

grid_data$AppleNonOrganic <- calculate_utility(grid_data$Calorie, grid_data$Size, 
                                             beta_cal_A, beta_size_A, beta_interact_A)

grid_data$BerryOrganic <- calculate_utility(grid_data$Calorie, grid_data$Size, 
                                          beta_cal_B, beta_size_B, beta_interact_B, 
                                          organic_effect_B)

grid_data$BerryNonOrganic <- calculate_utility(grid_data$Calorie, grid_data$Size, 
                                             beta_cal_B, beta_size_B, beta_interact_B)

# Convert to long format for plotting
grid_data_long <- melt(grid_data, id.vars = c("Calorie", "Size"), 
                      variable.name = "Product", value.name = "Utility")

# Create a common set of contour levels for comparison
contour_levels <- seq(-20, 10, by = 2)

# Plot all four in one grid
ggplot(grid_data_long, aes(x = Size, y = Calorie, z = Utility)) +
  geom_contour(aes(color = ..level..), breaks = contour_levels) +
  facet_wrap(~ Product, nrow = 2) +
  scale_color_viridis_c() +
  geom_abline(slope = 1, linetype = "dashed", color = "gray") +  # 45-degree line
  labs(title = "Utility Level Curves by Product Type and Organic Status",
       x = "Size (normalized)", 
       y = "Calorie (normalized)",
       color = "Utility") +
  coord_fixed() +  # Equal aspect ratio
  theme_minimal()

  ggsave("output/level_curves.pdf", width = 8, height = 6)




############################## Calculate True WTP ##############################

# WTP Formulas for the Quadratic Utility with Organic Multiplicative Effect
# 
# General Utility Function:
# U = β_const + α × Price + [β_cal × (Calorie^2) + β_size × (Size^2) + β_interact × Calorie × Size] × [organic_effect if organic]
#
# Partial Derivatives:
# ∂U/∂Price = α
#
# For Apple Juice:
# ∂U/∂Calorie = [2 × β_cal_A × Calorie + β_interact_A × Size] × [organic_effect_A if organic, 1 otherwise]
# ∂U/∂Size = [2 × β_size_A × Size + β_interact_A × Calorie] × [organic_effect_A if organic, 1 otherwise]
#
# For Berry Juice:
# ∂U/∂Calorie = [2 × β_cal_B × Calorie + β_interact_B × Size] × [organic_effect_B if organic, 1 otherwise]
# ∂U/∂Size = [2 × β_size_B × Size + β_interact_B × Calorie] × [organic_effect_B if organic, 1 otherwise]
#
# WTP Formulas (Marginal Rate of Substitution):
# WTP_Calorie = -(∂U/∂Calorie)/(∂U/∂Price)
# WTP_Size = -(∂U/∂Size)/(∂U/∂Price)
#
# For Apple Juice:
# WTP_Calorie_Apple = -[2 × β_cal_A × Calorie + β_interact_A × Size] × [organic_effect_A if organic, 1 otherwise] / α
# WTP_Size_Apple = -[2 × β_size_A × Size + β_interact_A × Calorie] × [organic_effect_A if organic, 1 otherwise] / α
#
# For Berry Juice:
# WTP_Calorie_Berry = -[2 × β_cal_B × Calorie + β_interact_B × Size] × [organic_effect_B if organic, 1 otherwise] / α
# WTP_Size_Berry = -[2 × β_size_B × Size + β_interact_B × Calorie] × [organic_effect_B if organic, 1 otherwise] / α
#
# For the Organic attribute (which is discrete), we calculate the WTP as the difference in utility:
# WTP_Organic_Apple = -[(Utility with Organic=1) - (Utility with Organic=0)] / α
# = -[(quadratic component × organic_effect_A) - (quadratic component)] / α
# = -(quadratic component) × (organic_effect_A - 1) / α
#
# WTP_Organic_Berry follows the same pattern with organic_effect_B

# Function to calculate WTP based on our derived formulas
calculate_wtp <- function(data, alpha) {
  # Initialize result data frame
  wtp_data <- data.frame(
    # Apple Juice WTP
    WTP_AppleCalorie = numeric(nrow(data)),
    WTP_AppleSize = numeric(nrow(data)),
    WTP_AppleOrganic = numeric(nrow(data)),
    
    # Berry Juice WTP
    WTP_BerryCalorie = numeric(nrow(data)),
    WTP_BerrySize = numeric(nrow(data)),
    WTP_BerryOrganic = numeric(nrow(data))
  )
  
  # Calculate WTP for each observation
  for (i in 1:nrow(data)) {
    # Get attribute values for this observation
    apple_calorie <- data$AppleCalorie[i]
    apple_size <- data$AppleSize[i]
    apple_organic <- data$AppleOrganic[i]
    
    berry_calorie <- data$BerryCalorie[i]
    berry_size <- data$BerrySize[i]
    berry_organic <- data$BerryOrganic[i]
    
    # Apple quadratic component (for organic WTP calculation)
    apple_quadratic <- beta_cal_A * (apple_calorie^2) + 
                       beta_size_A * (apple_size^2) + 
                       beta_interact_A * apple_calorie * apple_size
    
    # Berry quadratic component (for organic WTP calculation)
    berry_quadratic <- beta_cal_B * (berry_calorie^2) + 
                       beta_size_B * (berry_size^2) + 
                       beta_interact_B * berry_calorie * berry_size
    
    # Apply organic effect if product is organic
    apple_organic_effect <- ifelse(apple_organic == 1, organic_effect_A, 1)
    berry_organic_effect <- ifelse(berry_organic == 1, organic_effect_B, 1)
    
    # Calculate WTP for Apple attributes
    # WTP_Calorie = -[2 × β_cal × Calorie + β_interact × Size] × [organic_effect if organic, 1 otherwise] / α
    wtp_data$WTP_AppleCalorie[i] <- -(2 * beta_cal_A * apple_calorie + 
                                      beta_interact_A * apple_size) * 
                                     apple_organic_effect / alpha
    
    # WTP_Size = -[2 × β_size × Size + β_interact × Calorie] × [organic_effect if organic, 1 otherwise] / α
    wtp_data$WTP_AppleSize[i] <- -(2 * beta_size_A * apple_size + 
                                   beta_interact_A * apple_calorie) * 
                                  apple_organic_effect / alpha
    
    # WTP_Organic = -(quadratic component) × (organic_effect - 1) / α
    wtp_data$WTP_AppleOrganic[i] <- -apple_quadratic * (organic_effect_A - 1) / alpha
    
    # Calculate WTP for Berry attributes
    wtp_data$WTP_BerryCalorie[i] <- -(2 * beta_cal_B * berry_calorie + 
                                      beta_interact_B * berry_size) * 
                                     berry_organic_effect / alpha
    
    wtp_data$WTP_BerrySize[i] <- -(2 * beta_size_B * berry_size + 
                                   beta_interact_B * berry_calorie) * 
                                  berry_organic_effect / alpha
    
    wtp_data$WTP_BerryOrganic[i] <- -berry_quadratic * (organic_effect_B - 1) / alpha
  }
  
  return(wtp_data)
}

# Apply the function to our dataset
true_wtp <- calculate_wtp(data_juice, alpha)

# Add the WTP values to our main dataset
data_juice <- cbind(data_juice, true_wtp)

# Calculate summary statistics for the WTP values
wtp_nonlinear_summary <- true_wtp %>%
  summarize_at(
    vars(starts_with("WTP_")), 
    list(Mean = ~mean(., na.rm = TRUE),
         SD = ~sd(., na.rm = TRUE),
         Min = ~min(., na.rm = TRUE),
         Max = ~max(., na.rm = TRUE))
  ) %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = c("Variable", "Statistic"),
    names_pattern = "WTP_(.+)_(.+)"
  ) %>%
  tidyr::pivot_wider(
    names_from = Statistic,
    values_from = value
  )

# Print the summary
print(wtp_nonlinear_summary)

# Create histograms of WTP distributions
pdf("output/wtp_histograms.pdf", width = 8, height = 6)
par(mfrow=c(2,3))
hist(data_juice$WTP_AppleCalorie, main="WTP for Apple Calorie", xlab="WTP")
hist(data_juice$WTP_AppleSize, main="WTP for Apple Size", xlab="WTP")
hist(data_juice$WTP_AppleOrganic, main="WTP for Apple Organic", xlab="WTP")
hist(data_juice$WTP_BerryCalorie, main="WTP for Berry Calorie", xlab="WTP")
hist(data_juice$WTP_BerrySize, main="WTP for Berry Size", xlab="WTP")
hist(data_juice$WTP_BerryOrganic, main="WTP for Berry Organic", xlab="WTP")
dev.off()


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
  Nonlinear = wtp_nonlinear_summary$Mean,
  MNL = c(MNL_WTP_AppleCalorie, MNL_WTP_AppleSize, MNL_WTP_AppleOrganic, MNL_WTP_BerryCalorie, MNL_WTP_BerrySize, MNL_WTP_BerryOrganic)
)
print(compare_mean_MNL)

# Plot again the histogram of the true WTPs from above but draw a vertical line showing the MNL estimates of the mean in red and the true means in blue
pdf("output/wtp_histograms_MNL.pdf", width = 8, height = 6)
par(mfrow=c(2,3))
hist(data_juice$WTP_AppleCalorie, main="WTP for Apple Calorie", xlab="WTP")
abline(v = MNL_WTP_AppleCalorie, col = "red")
#abline(v = wtp_nonlinear_summary$Mean[1], col = "blue")
hist(data_juice$WTP_AppleSize, main="WTP for Apple Size", xlab="WTP")
abline(v = MNL_WTP_AppleSize, col = "red")
#abline(v = wtp_nonlinear_summary$Mean[2], col = "blue")
hist(data_juice$WTP_AppleOrganic, main="WTP for Apple Organic", xlab="WTP")
abline(v = MNL_WTP_AppleOrganic, col = "red")
#abline(v = wtp_nonlinear_summary$Mean[3], col = "blue")
hist(data_juice$WTP_BerryCalorie, main="WTP for Berry Calorie", xlab="WTP")
abline(v = MNL_WTP_BerryCalorie, col = "red")
#abline(v = wtp_nonlinear_summary$Mean[4], col = "blue")
hist(data_juice$WTP_BerrySize, main="WTP for Berry Size", xlab="WTP")
abline(v = MNL_WTP_BerrySize, col = "red")
#abline(v = wtp_nonlinear_summary$Mean[5], col = "blue")
hist(data_juice$WTP_BerryOrganic, main="WTP for Berry Organic", xlab="WTP")
abline(v = MNL_WTP_BerryOrganic, col = "red")
#abline(v = wtp_nonlinear_summary$Mean[6], col = "blue")
dev.off()



############################## Random Forest ##############################

# Select only Choice and the attributes for training this model
data_rf <- data_juice %>%
  select(Choice, ApplePrice, AppleCalorie, AppleSize, AppleOrganic, AppleSugar, AppleRecall, ApplePackage,
         BerryPrice, BerryCalorie, BerrySize, BerryOrganic, BerrySugar, BerryRecall, BerryPackage)

# Use sample splitting to address bias. First train on partition 1 (P1) and predict on P2. Then, flip it
P1_indices <- sample(seq_len(nrow(data_rf)), size = 0.5 * nrow(data_rf))
P1_data <- data_rf[P1_indices, ]
P2_data <- data_rf[-P1_indices, ]

P1_rf_model <- randomForest(as.factor(P1_data$Choice) ~ ., data = P1_data, ntree=1000) # Needs to tune the number of trees later
P2_rf_model <- randomForest(as.factor(P2_data$Choice) ~ ., data = P2_data, ntree=1000) # Needs to tune the number of trees later

# For every observation in each partition, predict the probability of each choice
P1_bl_probs <- predict(P2_rf_model, P1_data, type = "prob")
P2_bl_probs <- predict(P1_rf_model, P2_data, type = "prob")

# Store in the data
P1_data$prob_bl_apple <- P1_bl_probs[, "Apple Juice"]
P1_data$prob_bl_berry <- P1_bl_probs[, "Berry Juice"]
P2_data$prob_bl_apple <- P2_bl_probs[, "Apple Juice"]
P2_data$prob_bl_berry <- P2_bl_probs[, "Berry Juice"]

# Confusion matrix for P1 model predictions on P2 data
P2_actual <- P2_data$Choice
P2_pred <- predict(P1_rf_model, P2_data)
conf_matrix_P2 <- confusionMatrix(as.factor(P2_pred), as.factor(P2_actual))
print(conf_matrix_P2)

# Repeat for P2 model predictions on P1 data if desired
P1_actual <- P1_data$Choice
P1_pred <- predict(P2_rf_model, P1_data)
conf_matrix_P1 <- confusionMatrix(as.factor(P1_pred), as.factor(P1_actual))
print(conf_matrix_P1)


############################## Random Forest WTP Calculation ##############################

# First, let's calculate the standard deviations we'll need for step size
h_ApplePrice <- 0.1 * sd(data_juice$ApplePrice)
h_BerryPrice <- 0.1 * sd(data_juice$BerryPrice)

h_factor <- 0.1
h_AppleCalorie <- h_factor * sd(data_juice$AppleCalorie)
h_AppleSize <- h_factor * sd(data_juice$AppleSize)
h_BerryCalorie <- h_factor * sd(data_juice$BerryCalorie)
h_BerrySize <- h_factor * sd(data_juice$BerrySize)

# Function to calculate WTP using counterfactuals
calculate_rf_wtp <- function(data, model, partition_name) {
  # Store original data
  original_data <- data
  
  # Get baseline probabilities
  baseline_probs <- data[, paste0("prob_bl_", c("apple", "berry"))]
  
  # Initialize dataframes to store partial derivatives
  pd_results <- list()
  
  # Create partial derivative datasets - APPLE ATTRIBUTES
  # ApplePrice
  pd_data <- original_data
  pd_data$ApplePrice <- pd_data$ApplePrice + h_ApplePrice
  pd_probs <- predict(model, pd_data, type = "prob")
  pd_results$ApplePrice <- (pd_probs[, "Apple Juice"] - baseline_probs$prob_bl_apple)/h_ApplePrice
  # To avoid issues with close to zero denominators, we will take the mean of the partial derivatives
  # In doing so, we will not allow the price effect to depend on characteristics
  pd_results$ApplePrice <- mean(pd_results$ApplePrice)
  
  # AppleCalorie
  pd_data <- original_data
  pd_data$AppleCalorie <- pd_data$AppleCalorie + h_AppleCalorie
  pd_probs <- predict(model, pd_data, type = "prob")
  pd_results$AppleCalorie <- (pd_probs[, "Apple Juice"] - baseline_probs$prob_bl_apple)/h_AppleCalorie
  
  # AppleSize
  pd_data <- original_data
  pd_data$AppleSize <- pd_data$AppleSize + h_AppleSize
  pd_probs <- predict(model, pd_data, type = "prob")
  pd_results$AppleSize <- (pd_probs[, "Apple Juice"] - baseline_probs$prob_bl_apple)/h_AppleSize
  
  # AppleOrganic (binary - compare all 0 vs all 1)
  # First prediction: all AppleOrganic set to 0
  pd_data_org0 <- original_data
  pd_data_org0$AppleOrganic <- 0
  probs_org0 <- predict(model, pd_data_org0, type = "prob")

  # Second prediction: all AppleOrganic set to 1
  pd_data_org1 <- original_data
  pd_data_org1$AppleOrganic <- 1
  probs_org1 <- predict(model, pd_data_org1, type = "prob")

  # Calculate effect: prob(organic=1) - prob(organic=0)
  pd_results$AppleOrganic <- probs_org1[, "Apple Juice"] - probs_org0[, "Apple Juice"]
  
  # Create counterfactual datasets - BERRY ATTRIBUTES
  # BerryPrice
  pd_data <- original_data
  pd_data$BerryPrice <- pd_data$BerryPrice + h_BerryPrice
  pd_probs <- predict(model, pd_data, type = "prob")
  pd_results$BerryPrice <- (pd_probs[, "Berry Juice"] - baseline_probs$prob_bl_berry)/h_BerryPrice
  # To avoid issues with close to zero denominators, we will take the mean of the partial derivatives
  # In doing so, we will not allow the price effect to depend on characteristics
  pd_results$BerryPrice <- mean(pd_results$BerryPrice)
  
  # BerryCalorie
  pd_data <- original_data
  pd_data$BerryCalorie <- pd_data$BerryCalorie + h_BerryCalorie
  pd_probs <- predict(model, pd_data, type = "prob")
  pd_results$BerryCalorie <- (pd_probs[, "Berry Juice"] - baseline_probs$prob_bl_berry)/h_BerryCalorie
  
  # BerrySize
  pd_data <- original_data
  pd_data$BerrySize <- pd_data$BerrySize + h_BerrySize
  pd_probs <- predict(model, pd_data, type = "prob")
  pd_results$BerrySize <- (pd_probs[, "Berry Juice"] - baseline_probs$prob_bl_berry)/h_BerrySize
  
  # BerryOrganic (binary - compare all 0 vs all 1)
  # First prediction: all BerryOrganic set to 0
  pd_data_org0 <- original_data
  pd_data_org0$BerryOrganic <- 0
  probs_org0 <- predict(model, pd_data_org0, type = "prob")

  # Second prediction: all BerryOrganic set to 1
  pd_data_org1 <- original_data
  pd_data_org1$BerryOrganic <- 1
  probs_org1 <- predict(model, pd_data_org1, type = "prob")

  # Calculate effect: prob(organic=1) - prob(organic=0)
  pd_results$BerryOrganic <- probs_org1[, "Berry Juice"] - probs_org0[, "Berry Juice"]
  
  # Calculate WTP
  # WTP = -(marginal effect from attribute change) / (marginal effect from price change)
  # Note: We need to handle cases where the price effect is 0 or very small
  
  # Convert results to a data frame
  wtp_results <- data.frame(
    RF_WTP_AppleCalorie = -pd_results$AppleCalorie / pd_results$ApplePrice,
    RF_WTP_AppleSize = -pd_results$AppleSize / pd_results$ApplePrice,
    RF_WTP_AppleOrganic = -pd_results$AppleOrganic / pd_results$ApplePrice,
    RF_WTP_BerryCalorie = -pd_results$BerryCalorie / pd_results$BerryPrice,
    RF_WTP_BerrySize = -pd_results$BerrySize / pd_results$BerryPrice,
    RF_WTP_BerryOrganic = -pd_results$BerryOrganic / pd_results$BerryPrice
  )

  # Replace Inf and NaN with NA
  for (col in names(wtp_results)) {
    wtp_results[is.infinite(wtp_results[[col]]) | is.nan(wtp_results[[col]]), col] <- NA
  }
  
  # Add partition identifier
  wtp_results$partition <- partition_name
  
  return(wtp_results)
}

# Apply the function to both partitions
P1_wtp <- calculate_rf_wtp(P1_data, P2_rf_model, "P1")
P2_wtp <- calculate_rf_wtp(P2_data, P1_rf_model, "P2")

# Combine the results
rf_wtp <- rbind(P1_wtp, P2_wtp)

# Calculate summary statistics for the RF WTP values
rf_wtp_summary <- rf_wtp %>%
  summarize_at(
    vars(starts_with("RF_WTP_")), 
    list(Mean = ~mean(., na.rm = TRUE),
         SD = ~sd(., na.rm = TRUE),
         Min = ~min(., na.rm = TRUE),
         Max = ~max(., na.rm = TRUE))
  ) %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = c("Variable", "Statistic"),
    names_pattern = "RF_WTP_(.+)_(.+)"
  ) %>%
  tidyr::pivot_wider(
    names_from = Statistic,
    values_from = value
  )

# Print the summary
print(rf_wtp_summary)

# Compare with true WTP (combine true WTP with random forest partition identifiers)
P1_true_wtp <- cbind(true_wtp[P1_indices,], partition = "P1")
P2_true_wtp <- cbind(true_wtp[-P1_indices,], partition = "P2")
combined_true_wtp <- rbind(P1_true_wtp, P2_true_wtp)

# Create comparison dataframe
wtp_comparison <- data.frame(
  Attribute = c("Apple Calorie", "Apple Size", "Apple Organic", 
                "Berry Calorie", "Berry Size", "Berry Organic"),
  
  True_Mean = c(
    mean(combined_true_wtp$WTP_AppleCalorie),
    mean(combined_true_wtp$WTP_AppleSize),
    mean(combined_true_wtp$WTP_AppleOrganic),
    mean(combined_true_wtp$WTP_BerryCalorie),
    mean(combined_true_wtp$WTP_BerrySize),
    mean(combined_true_wtp$WTP_BerryOrganic)
  ),

  MNL_Mean = c(
    mean(MNL_WTP_AppleCalorie, na.rm = TRUE),
    mean(MNL_WTP_AppleSize, na.rm = TRUE),
    mean(MNL_WTP_AppleOrganic, na.rm = TRUE),
    mean(MNL_WTP_BerryCalorie, na.rm = TRUE),
    mean(MNL_WTP_BerrySize, na.rm = TRUE),
    mean(MNL_WTP_BerryOrganic, na.rm = TRUE)
  ),
  
  RF_Mean = c(
    mean(rf_wtp$RF_WTP_AppleCalorie, na.rm = TRUE),
    mean(rf_wtp$RF_WTP_AppleSize, na.rm = TRUE),
    mean(rf_wtp$RF_WTP_AppleOrganic, na.rm = TRUE),
    mean(rf_wtp$RF_WTP_BerryCalorie, na.rm = TRUE),
    mean(rf_wtp$RF_WTP_BerrySize, na.rm = TRUE),
    mean(rf_wtp$RF_WTP_BerryOrganic, na.rm = TRUE)
  )
)

print(wtp_comparison)

# Combine true WTP with random forest WTP for comparison
# First ensure we have matching rows with partition info
rf_wtp_with_ids <- cbind(rf_wtp, row_id = c(P1_indices, setdiff(1:nrow(data_juice), P1_indices)))
true_wtp_with_ids <- cbind(true_wtp, row_id = 1:nrow(true_wtp))

# Merge datasets
comparison_data <- merge(
  true_wtp_with_ids,
  rf_wtp_with_ids,
  by = "row_id"
)

# Create a long format for easier plotting
# List of attributes to compare
attributes <- c(
  "AppleCalorie" = "Apple Calorie",
  "AppleSize" = "Apple Size",
  "AppleOrganic" = "Apple Organic",
  "BerryCalorie" = "Berry Calorie",
  "BerrySize" = "Berry Size",
  "BerryOrganic" = "Berry Organic"
)

# Create comparison plots for each attribute
plot_list <- list()

for (attr_code in names(attributes)) {
  attr_name <- attributes[attr_code]
  true_col <- paste0("WTP_", attr_code)
  rf_col <- paste0("RF_WTP_", attr_code)
  
  # Get data for this attribute
  plot_data <- comparison_data[, c(true_col, rf_col)]
  names(plot_data) <- c("True_WTP", "RF_WTP")
  
  # Remove NA/Inf values
  plot_data <- plot_data[is.finite(plot_data$True_WTP) & 
                          is.finite(plot_data$RF_WTP) &
                          !is.na(plot_data$True_WTP) & 
                          !is.na(plot_data$RF_WTP), ]
  
  # Create scatterplot
  p <- ggplot(plot_data, aes(x = True_WTP, y = RF_WTP)) +
    geom_point(alpha = 0.3) +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    geom_smooth(method = "loess", color = "blue") +
    labs(
      title = paste("True vs. RF WTP for", attr_name),
      x = "True WTP",
      y = "Random Forest WTP"
    ) +
    theme_minimal() +
    # Add correlation coefficient
    annotate(
      "text", 
      x = max(plot_data$True_WTP, na.rm = TRUE) * 0.8,
      y = min(plot_data$RF_WTP, na.rm = TRUE) * 1.2,
      label = paste("Correlation:", 
                   round(cor(plot_data$True_WTP, plot_data$RF_WTP, 
                             use = "complete.obs"), 2))
    )
  # Save in pdf
  ggsave(paste0("output/", attr_code, "_comparison.pdf"), p)
 
  plot_list[[attr_code]] <- p
}

# Also create boxplots comparing distributions
comparison_long <- data.frame(
  Attribute = character(),
  Method = character(),
  WTP = numeric()
)

for (attr_code in names(attributes)) {
  attr_name <- attributes[attr_code]
  true_col <- paste0("WTP_", attr_code)
  rf_col <- paste0("RF_WTP_", attr_code)
  
  # Get data for this attribute
  true_data <- data.frame(
    Attribute = attr_name,
    Method = "True",
    WTP = comparison_data[[true_col]]
  )
  
  rf_data <- data.frame(
    Attribute = attr_name,
    Method = "Random Forest",
    WTP = comparison_data[[rf_col]]
  )
  
  comparison_long <- rbind(comparison_long, true_data, rf_data)
}

# Remove NA/Inf values
comparison_long <- comparison_long[is.finite(comparison_long$WTP) & !is.na(comparison_long$WTP), ]

# Create boxplot
ggplot(comparison_long, aes(x = Attribute, y = WTP, fill = Method)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Distribution of True vs. Random Forest WTP Estimates",
    x = "Attribute",
    y = "Willingness to Pay (WTP)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  ) +
  scale_fill_manual(values = c("True" = "darkblue", "Random Forest" = "darkgreen"))

# Save in PDF
ggsave("output/wtp_boxplot.pdf", width = 8, height = 6)



##### TRYING WITH LOG APPROACH

# With this approach, can use try bigger step sizes
h_ApplePrice <- 0.3 * sd(data_juice$ApplePrice)
h_BerryPrice <- 0.3 * sd(data_juice$BerryPrice)

h_factor <- 0.3
h_AppleCalorie <- h_factor * sd(data_juice$AppleCalorie)
h_AppleSize <- h_factor * sd(data_juice$AppleSize)
h_BerryCalorie <- h_factor * sd(data_juice$BerryCalorie)
h_BerrySize <- h_factor * sd(data_juice$BerrySize)

calculate_rf_wtp_logratio <- function(data, model, partition_name) {
  # Store original data
  original_data <- data
  
  # Get baseline probabilities
  baseline_probs <- data[, paste0("prob_bl_", c("apple", "berry"))]
  
  # Initialize dataframes to store partial derivatives
  pd_results <- list()
  
  # Create partial derivative datasets - APPLE ATTRIBUTES
  # ApplePrice
  pd_data <- original_data
  pd_data$ApplePrice <- pd_data$ApplePrice + h_ApplePrice
  pd_probs <- predict(model, pd_data, type = "prob")
  pd_results$ApplePrice <- log(pd_probs[, "Apple Juice"]/baseline_probs$prob_bl_apple)/h_ApplePrice
  # To avoid issues with close to zero denominators, we will take the mean of the partial derivatives
  # In doing so, we will not allow the price effect to depend on characteristics
  pd_results$ApplePrice <- mean(pd_results$ApplePrice)
  
  # AppleCalorie
  pd_data <- original_data
  pd_data$AppleCalorie <- pd_data$AppleCalorie + h_AppleCalorie
  pd_probs <- predict(model, pd_data, type = "prob")
  pd_results$AppleCalorie <- log(pd_probs[, "Apple Juice"] / baseline_probs$prob_bl_apple)/h_AppleCalorie
  
  # AppleSize
  pd_data <- original_data
  pd_data$AppleSize <- pd_data$AppleSize + h_AppleSize
  pd_probs <- predict(model, pd_data, type = "prob")
  pd_results$AppleSize <- log(pd_probs[, "Apple Juice"] / baseline_probs$prob_bl_apple)/h_AppleSize
  
  # AppleOrganic (binary - compare all 0 vs all 1)
  # First prediction: all AppleOrganic set to 0
  pd_data_org0 <- original_data
  pd_data_org0$AppleOrganic <- 0
  probs_org0 <- predict(model, pd_data_org0, type = "prob")

  # Second prediction: all AppleOrganic set to 1
  pd_data_org1 <- original_data
  pd_data_org1$AppleOrganic <- 1
  probs_org1 <- predict(model, pd_data_org1, type = "prob")

  # Calculate effect: prob(organic=1) - prob(organic=0)
  pd_results$AppleOrganic <- log(probs_org1[, "Apple Juice"] / probs_org0[, "Apple Juice"])
  
  # Create counterfactual datasets - BERRY ATTRIBUTES
  # BerryPrice
  pd_data <- original_data
  pd_data$BerryPrice <- pd_data$BerryPrice + h_BerryPrice
  pd_probs <- predict(model, pd_data, type = "prob")
  pd_results$BerryPrice <- log(pd_probs[, "Berry Juice"] / baseline_probs$prob_bl_berry)/h_BerryPrice
  # To avoid issues with close to zero denominators, we will take the mean of the partial derivatives
  # In doing so, we will not allow the price effect to depend on characteristics
  pd_results$BerryPrice <- mean(pd_results$BerryPrice)
  
  # BerryCalorie
  pd_data <- original_data
  pd_data$BerryCalorie <- pd_data$BerryCalorie + h_BerryCalorie
  pd_probs <- predict(model, pd_data, type = "prob")
  pd_results$BerryCalorie <- log(pd_probs[, "Berry Juice"] / baseline_probs$prob_bl_berry)/h_BerryCalorie
  
  # BerrySize
  pd_data <- original_data
  pd_data$BerrySize <- pd_data$BerrySize + h_BerrySize
  pd_probs <- predict(model, pd_data, type = "prob")
  pd_results$BerrySize <- log(pd_probs[, "Berry Juice"] / baseline_probs$prob_bl_berry)/h_BerrySize
  
  # BerryOrganic (binary - compare all 0 vs all 1)
  # First prediction: all BerryOrganic set to 0
  pd_data_org0 <- original_data
  pd_data_org0$BerryOrganic <- 0
  probs_org0 <- predict(model, pd_data_org0, type = "prob")

  # Second prediction: all BerryOrganic set to 1
  pd_data_org1 <- original_data
  pd_data_org1$BerryOrganic <- 1
  probs_org1 <- predict(model, pd_data_org1, type = "prob")

  # Calculate effect: prob(organic=1) - prob(organic=0)
  pd_results$BerryOrganic <- log(probs_org1[, "Berry Juice"] / probs_org0[, "Berry Juice"])
  
  # Calculate WTP
  # WTP = -(marginal effect from attribute change) / (marginal effect from price change)
  # Note: We need to handle cases where the price effect is 0 or very small
  
  # Convert results to a data frame
  wtp_results <- data.frame(
    RF_WTP_AppleCalorie = -pd_results$AppleCalorie / pd_results$ApplePrice,
    RF_WTP_AppleSize = -pd_results$AppleSize / pd_results$ApplePrice,
    RF_WTP_AppleOrganic = -pd_results$AppleOrganic / pd_results$ApplePrice,
    RF_WTP_BerryCalorie = -pd_results$BerryCalorie / pd_results$BerryPrice,
    RF_WTP_BerrySize = -pd_results$BerrySize / pd_results$BerryPrice,
    RF_WTP_BerryOrganic = -pd_results$BerryOrganic / pd_results$BerryPrice
  )

  # Replace Inf and NaN with NA
  for (col in names(wtp_results)) {
    wtp_results[is.infinite(wtp_results[[col]]) | is.nan(wtp_results[[col]]), col] <- NA
  }
  
  # Add partition identifier
  wtp_results$partition <- partition_name
  
  return(wtp_results)
}

# Apply the function to both partitions
P1_wtp_logr <- calculate_rf_wtp_logratio(P1_data, P2_rf_model, "P1")
P2_wtp_logr <- calculate_rf_wtp_logratio(P2_data, P1_rf_model, "P2")

# Combine the results
rf_wtp_logr <- rbind(P1_wtp_logr, P2_wtp_logr)

# Calculate summary statistics for the RF WTP values
rf_wtp__logr_summary <- rf_wtp_logr %>%
  summarize_at(
    vars(starts_with("RF_WTP_")), 
    list(Mean = ~mean(., na.rm = TRUE),
         SD = ~sd(., na.rm = TRUE),
         Min = ~min(., na.rm = TRUE),
         Max = ~max(., na.rm = TRUE))
  ) %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = c("Variable", "Statistic"),
    names_pattern = "RF_WTP_(.+)_(.+)"
  ) %>%
  tidyr::pivot_wider(
    names_from = Statistic,
    values_from = value
  )

# Print the summary
print(rf_wtp__logr_summary)

# Create comparison dataframe
wtp_comparison <- data.frame(
  Attribute = c("Apple Calorie", "Apple Size", "Apple Organic", 
                "Berry Calorie", "Berry Size", "Berry Organic"),
  
  True_Mean = c(
    mean(combined_true_wtp$WTP_AppleCalorie),
    mean(combined_true_wtp$WTP_AppleSize),
    mean(combined_true_wtp$WTP_AppleOrganic),
    mean(combined_true_wtp$WTP_BerryCalorie),
    mean(combined_true_wtp$WTP_BerrySize),
    mean(combined_true_wtp$WTP_BerryOrganic)
  ),

  MNL_Mean = c(
    mean(MNL_WTP_AppleCalorie, na.rm = TRUE),
    mean(MNL_WTP_AppleSize, na.rm = TRUE),
    mean(MNL_WTP_AppleOrganic, na.rm = TRUE),
    mean(MNL_WTP_BerryCalorie, na.rm = TRUE),
    mean(MNL_WTP_BerrySize, na.rm = TRUE),
    mean(MNL_WTP_BerryOrganic, na.rm = TRUE)
  ),
  
  RF_Mean = c(
    mean(rf_wtp$RF_WTP_AppleCalorie, na.rm = TRUE),
    mean(rf_wtp$RF_WTP_AppleSize, na.rm = TRUE),
    mean(rf_wtp$RF_WTP_AppleOrganic, na.rm = TRUE),
    mean(rf_wtp$RF_WTP_BerryCalorie, na.rm = TRUE),
    mean(rf_wtp$RF_WTP_BerrySize, na.rm = TRUE),
    mean(rf_wtp$RF_WTP_BerryOrganic, na.rm = TRUE)
  ),
  
  RF_Logratio_Mean = c(
    mean(rf_wtp_logr$RF_WTP_AppleCalorie, na.rm = TRUE),
    mean(rf_wtp_logr$RF_WTP_AppleSize, na.rm = TRUE),
    mean(rf_wtp_logr$RF_WTP_AppleOrganic, na.rm = TRUE),
    mean(rf_wtp_logr$RF_WTP_BerryCalorie, na.rm = TRUE),
    mean(rf_wtp_logr$RF_WTP_BerrySize, na.rm = TRUE),
    mean(rf_wtp_logr$RF_WTP_BerryOrganic, na.rm = TRUE)
  )
)

print(wtp_comparison)

# Combine true WTP with random forest WTP for comparison
# First ensure we have matching rows with partition info
rf_wtp_with_ids <- cbind(rf_wtp_logr, row_id = c(P1_indices, setdiff(1:nrow(data_juice), P1_indices)))
true_wtp_with_ids <- cbind(true_wtp, row_id = 1:nrow(true_wtp))

# Merge datasets
comparison_data <- merge(
  true_wtp_with_ids,
  rf_wtp_with_ids,
  by = "row_id"
)

# Create a long format for easier plotting
# List of attributes to compare
attributes <- c(
  "AppleCalorie" = "Apple Calorie",
  "AppleSize" = "Apple Size",
  "AppleOrganic" = "Apple Organic",
  "BerryCalorie" = "Berry Calorie",
  "BerrySize" = "Berry Size",
  "BerryOrganic" = "Berry Organic"
)

# Create comparison plots for each attribute
plot_list <- list()

for (attr_code in names(attributes)) {
  attr_name <- attributes[attr_code]
  true_col <- paste0("WTP_", attr_code)
  rf_col <- paste0("RF_WTP_", attr_code)
  
  # Get data for this attribute
  plot_data <- comparison_data[, c(true_col, rf_col)]
  names(plot_data) <- c("True_WTP", "RF_WTP")
  
  # Remove NA/Inf values
  plot_data <- plot_data[is.finite(plot_data$True_WTP) & 
                          is.finite(plot_data$RF_WTP) &
                          !is.na(plot_data$True_WTP) & 
                          !is.na(plot_data$RF_WTP), ]
  
  # Create scatterplot
  p <- ggplot(plot_data, aes(x = True_WTP, y = RF_WTP)) +
    geom_point(alpha = 0.3) +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    geom_smooth(method = "loess", color = "blue") +
    labs(
      title = paste("True vs. RF WTP for", attr_name),
      x = "True WTP",
      y = "Random Forest WTP"
    ) +
    theme_minimal() +
    # Add correlation coefficient
    annotate(
      "text", 
      x = max(plot_data$True_WTP, na.rm = TRUE) * 0.8,
      y = min(plot_data$RF_WTP, na.rm = TRUE) * 1.2,
      label = paste("Correlation:", 
                   round(cor(plot_data$True_WTP, plot_data$RF_WTP, 
                             use = "complete.obs"), 2))
    )
  # Save in pdf
  ggsave(paste0("output/", attr_code, "_comparison.pdf"), p)
 
  plot_list[[attr_code]] <- p
}

# Also create boxplots comparing distributions
comparison_long <- data.frame(
  Attribute = character(),
  Method = character(),
  WTP = numeric()
)

for (attr_code in names(attributes)) {
  attr_name <- attributes[attr_code]
  true_col <- paste0("WTP_", attr_code)
  rf_col <- paste0("RF_WTP_", attr_code)
  
  # Get data for this attribute
  true_data <- data.frame(
    Attribute = attr_name,
    Method = "True",
    WTP = comparison_data[[true_col]]
  )
  
  rf_data <- data.frame(
    Attribute = attr_name,
    Method = "Random Forest",
    WTP = comparison_data[[rf_col]]
  )
  
  comparison_long <- rbind(comparison_long, true_data, rf_data)
}

# Remove NA/Inf values
comparison_long <- comparison_long[is.finite(comparison_long$WTP) & !is.na(comparison_long$WTP), ]

# Create boxplot
ggplot(comparison_long, aes(x = Attribute, y = WTP, fill = Method)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Distribution of True vs. Random Forest WTP Estimates",
    x = "Attribute",
    y = "Willingness to Pay (WTP)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  ) +
  scale_fill_manual(values = c("True" = "darkblue", "Random Forest" = "darkgreen"))


