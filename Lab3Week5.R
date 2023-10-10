#Activity 1: Basic Imputation Methods

library(titanic)

titanic_train$Age

library(ggplot2)
library(dplyr)

#View the Age distribution using histogram
ggplot(titanic_train, aes(Age)) +
  geom_histogram(color = "#000000", fill = "#FC6C85") +
  ggtitle("Variable distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

#Perform simple value imputation and view the data
value_imputed <- data.frame(
  original = titanic_train$Age,
  imputed_zero = replace(titanic_train$Age,
                         is.na(titanic_train$Age), 0),
  imputed_mean = replace(titanic_train$Age,
                         is.na(titanic_train$Age), mean(titanic_train$Age, na.rm = TRUE)),
  imputed_median = replace(titanic_train$Age,
                           is.na(titanic_train$Age), median(titanic_train$Age, na.rm =
                                                              TRUE))
)
value_imputed

# Create histograms after imputation
h1 <- ggplot(value_imputed, aes(x = original)) +
  geom_histogram(fill = "#FC8EAC", color = "#000000", position =
                   "identity") +
  ggtitle("Original distribution") +
  theme_classic()
h2 <- ggplot(value_imputed, aes(x = imputed_zero)) +
  geom_histogram(fill = "#A7C7E7", color = "#000000", position =
                   "identity") +
  ggtitle("Zero-imputed distribution") +
  theme_classic()
h3 <- ggplot(value_imputed, aes(x = imputed_mean)) +
  geom_histogram(fill = "#C3B1E1", color = "#000000", position =
                   "identity") +
  ggtitle("Mean-imputed distribution") +
  theme_classic()
h4 <- ggplot(value_imputed, aes(x = imputed_median)) +
  geom_histogram(fill = "#9CAF88", color = "#000000", position =
                   "identity") +
  ggtitle("Median-imputed distribution") +
  theme_classic()

plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)

#Activity 2: Impute Missing Values in R with MICE

library(mice)
titanic_numeric <- titanic_train %>%
  select(Survived, Pclass, SibSp, Parch, Age)
md.pattern(titanic_numeric)

mice_imputed <- data.frame(
  original = titanic_train$Age,
  imputed_pmm = complete(mice(titanic_numeric, method =
                                "pmm"))$Age,
  imputed_cart = complete(mice(titanic_numeric, method =
                                 "cart"))$Age,
  imputed_lasso = complete(mice(titanic_numeric, method =
                                  "lasso.norm"))$Age)
mice_imputed

# histogram distribution
h1 <- ggplot(mice_imputed, aes(x = original)) +
  geom_histogram(fill = "#FC8EAC", color = "#000000", position =
                   "identity") +
  ggtitle("Original distribution") +
  theme_classic()
h2 <- ggplot(mice_imputed, aes(x = imputed_pmm)) +
  geom_histogram(fill = "#A7C7E7", color = "#000000", position =
                   "identity") +
  ggtitle("PMM-imputed distribution") +
  theme_classic()
h3 <- ggplot(mice_imputed, aes(x = imputed_cart)) +
  geom_histogram(fill = "#C3B1E1", color = "#000000", position =
                   "identity") +
  ggtitle("Cart-imputed distribution") +
  theme_classic()
h4 <- ggplot(mice_imputed, aes(x = imputed_lasso)) +
  geom_histogram(fill = "#EFECDD", color = "#000000", position =
                   "identity") +
  ggtitle("Lasso-imputed distribution") +
  theme_classic()

plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)

# Activity 3: Imputation with R missForest Package

library(missForest)

missForest_imputed <- data.frame(
  original = titanic_numeric$Age, 
  imputed_missForest = missForest(titanic_numeric)$ximp$Age
)
missForest_imputed

#Visualize distribution
h1 <- ggplot(missForest_imputed, aes(x = original)) +
  geom_histogram(fill = "#F3EFDC", color = "#000000", position =
                   "identity") +
  ggtitle("Original distribution") +
  theme_classic()
h2 <- ggplot(missForest_imputed, aes(x = imputed_missForest)) +
  geom_histogram(fill = "#017CB5", color = "#000000", position =
                   "identity") +
  ggtitle("missForest-imputed distribution") +
  theme_classic()

plot_grid(h1, h2,nrow = 2, ncol = 1)

#Activity 4: Normalize data with scaling methods

log_scale = log(as.data.frame(titanic$Fare))


process <- preProcess(as.data.frame(titanic$Fare),
                      method=c("range"))
norm_scale <- predict(process, as.data.frame(titanic$Fare))

scale_data <- as.data.frame(scale(titanic$Fare))

#Activity 5: Feature Encoding

gender_encode <- ifelse(titanic_train$Sex == "male",1,0)
table(gender_encode)

#declare new_dat
new_dat =
  data.frame(titanic_train$Fare,titanic_train$Sex,titanic_train$Embarked)
summary(new_dat)

library(caret)
dmy <- dummyVars(" ~ .", data = new_dat, fullRank = T)
dat_transformed <- data.frame(predict(dmy, newdata = new_dat))
glimpse(dat_transformed)


summary(new_dat$titanic_train.Fare)
bins <- c(-Inf, 7.91, 31.00, Inf)

bin_names <- c("Low", "Mid50", "High")
new_dat$new_Fare <- cut(new_dat$titanic_train.Fare, breaks =
                          bins, labels = bin_names)
summary(new_dat$titanic_train.Fare)
summary(new_dat$new_Fare)
