#Exercise 1
str(Churn_Train)
?Churn_Train

#check missing value
is.na(Churn_Train)

#replace missing value
Churn_Train <- Churn_Train %>%
  mutate(Total.Charges=replace(Total.Charges, is.na(Total.Charges), mean(Total.Charges, na.rm = T)))
is.na(Churn_Train$Total.Charges)

#describe
describe(Churn_Train)
describe(Churn_Train, Senior.Citizen, Tenure, Monthly.Charges, Total.Charges)
describe(Churn_Train,Senior.Citizen:Monthly.Charges)
describe(Churn_Train,-(Tenure))

#normality
normality(Churn_Train)
normality(Churn_Train, Senior.Citizen, Tenure, Monthly.Charges, Total.Charges)
normality(Churn_Train,Senior.Citizen:Monthly.Charges) #select all columns between sales and income
normality(Churn_Train,-(Tenure)) #select all column except sales until income

plot_normality(Churn_Train, Senior.Citizen, Tenure, Monthly.Charges, Total.Charges)

#correlate
correlate(Churn_Train)
correlate(Churn_Train, Senior.Citizen, Tenure, Monthly.Charges, Total.Charges)
correlate(Churn_Train,Senior.Citizen:Monthly.Charges)
correlate(Churn_Train,-(Tenure))

Churn_Train %>%
  correlate()%>%
  plot()

correlate(Churn_Train, Senior.Citizen, Tenure, Monthly.Charges, Total.Charges)%>%
  plot()

categ <- target_by(Churn_Train, Churn)

cat_num <- relate(categ, Total.Charges)
cat_num
summary(cat_num)
plot(cat_num)

library(dlookr)


# Create an EDA report
report <- eda_paged_report(Churn_Train)

# Output the report as an HTML file (you can change the format to 'pdf' if needed)
output_file <- "Churn_EDA_Report.html"
paged_report_output(report, output_file)

# Print the location of the generated report file
cat("Static EDA report saved as:", output_file, "\n")

