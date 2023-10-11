str(Churn_Train)
?Churn_Train

is.na(Churn_Train)

Churn_Train$Tenure

library(ggplot2)
library(dplyr)
library(mice)
library(missForest)
library(knitr)
library(rmarkdown)
library(cowplot)


#View the Age distribution using histogram
ggplot(Churn_Train, aes(x = Tenure)) +
  geom_histogram(color = "#000000", fill = "#FC6C85") +
  ggtitle("Variable distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

#Perform simple value imputation and view the data
value_imputed <- data.frame(
  original = Churn_Train$Tenure,
  imputed_zero = replace(Churn_Train$Tenure,
                         is.na(Churn_Train$Tenure), 0),
  imputed_mean = replace(Churn_Train$Tenure,
                         is.na(Churn_Train$Tenure), mean(Churn_Train$Tenure, na.rm = TRUE)),
  imputed_median = replace(Churn_Train$Tenure,
                           is.na(Churn_Train$Tenure), median(Churn_Train$Tenure, na.rm =
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


churn_numeric <- Churn_Train %>%
  select(Tenure, Total.Charges, Monthly.Charges, Senior.Citizen)
md.pattern(churn_numeric)

mice_imputed <- data.frame(
  original = Churn_Train$Tenure,
  imputed_pmm = complete(mice(churn_numeric, method =
                                "pmm"))$Tenure,
  imputed_cart = complete(mice(churn_numeric, method =
                                 "cart"))$Tenure,
  imputed_lasso = complete(mice(churn_numeric, method =
                                  "lasso.norm"))$Tenure)
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

missForest_imputed <- data.frame(
  original = churn_numeric$Tenure, 
  imputed_missForest = missForest(churn_numeric)$ximp$Tenure
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

# Render the R Markdown report to PDF
rmarkdown::render("preprocessing_report.Rmd", output_file = "preprocessing_report.pdf")



