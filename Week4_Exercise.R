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

Churn_Train %>%
  eda_web_report(target = "Total.Charges", subtitle = " Churn_Train",
                 output_dir = "./", output_file = "EDA.html", theme = "orange")

