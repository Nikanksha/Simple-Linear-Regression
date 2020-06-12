#simple linera regression:1) Calories_consumed-> predict weight gained using calories consumed
#2) Delivery_time -> Predict delivery time using sorting time 
#3) Emp_data -> Build a prediction model for Churn_out_rate
#4) Salary_hike -> Build a prediction model for Salary_hike
#Do the necessary transformations for input variables for
#getting better R^2 value for the model prepared.

#1
cal <- calories_consumed
summary(cal)
#Weight.gained..grams. Calories.Consumed
#Min.   :  62.0        Min.   :1400     
#1st Qu.: 114.5        1st Qu.:1728     
#Median : 200.0        Median :2250     
#Mean   : 357.7        Mean   :2341     
#3rd Qu.: 537.5        3rd Qu.:2775     
#Max.   :1100.0        Max.   :3900 
var(cal$Calories.Consumed)
sd(cal$Calories.Consumed)
var(cal$Weight.gained..grams.)
sd(cal$Weight.gained..grams.)
#creating weight gain model
weightgainmodel<-lm(Weight.gained..grams.~ Calories.Consumed,data = calories_consumed)
summary(weightgainmodel)
plot(weightgainmodel)
#Hence the P-value is less than 0.05. So X varibale is significance and also Multiple R-Square value is 0.8968. That's mean this model will predict the output 89.68% time correct
#Output
#var(cal$Calories.Consumed)
#[1] 565668.7
#> sd(cal$Calories.Consumed)
#[1] 752.1095
#> var(cal$Weight.gained..grams.)
#[1] 111350.7
#> sd(cal$Weight.gained..grams.)
#[1] 333.6925
 #creating weight gain model
 # > weightgainmodel<-lm(Weight.gained..grams.~ Calories.Consumed,data = calories_consumed)
#> summary(weightgainmodel)

#Call:
 # lm(formula = Weight.gained..grams. ~ Calories.Consumed, data = calories_consumed)

#Residuals:
 # Min      1Q  Median      3Q     Max 
#-158.67 -107.56   36.70   81.68  165.53 

#Coefficients:
 # Estimate Std. Error t value Pr(>|t|)    
#(Intercept)       -625.75236  100.82293  -6.206 4.54e-05 ***
#  Calories.Consumed    0.42016    0.04115  10.211 2.86e-07 ***
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 111.6 on 12 degrees of freedom
#Multiple R-squared:  0.8968,	Adjusted R-squared:  0.8882 
#F-statistic: 104.3 on 1 and 12 DF,  p-value: 2.856e-07

#2
deliveryTime <- delivery_time
summary(deliveryTime)
var(deliveryTime$Delivery.Time)
sd(deliveryTime$Delivery.Time)
var(deliveryTime$Sorting.Time)
sd(deliveryTime$Sorting.Time)
deliverymodel<-lm(Delivery.Time ~ Sorting.Time ,data = delivery_time)
summary(deliverymodel)

plot(deliverymodel)
#Hence the P-value is less than 0.05. So X varibale is significance and also Multiple R-Square value is 0.6823. That's mean this model will predict the output 68.23% time correct
#Output:
#summary(deliveryTime)
#Delivery.Time    Sorting.Time  
#Min.   : 8.00   Min.   : 2.00  
#1st Qu.:13.50   1st Qu.: 4.00  
#Median :17.83   Median : 6.00  
#Mean   :16.79   Mean   : 6.19  
#3rd Qu.:19.75   3rd Qu.: 8.00  
#Max.   :29.00   Max.   :10.00  
#> var(deliveryTime$Delivery.Time)
#[1] 25.75462
#> sd(deliveryTime$Delivery.Time)
#[1] 5.074901
#> var(deliveryTime$Sorting.Time)
#[1] 6.461905
#> sd(deliveryTime$Sorting.Time)
#[1] 2.542028
#> deliverymodel<-lm(Delivery.Time ~ Sorting.Time ,data = delivery_time)
#> summary(deliverymodel)

#Call:
 # lm(formula = Delivery.Time ~ Sorting.Time, data = delivery_time)

#Residuals:
 # Min      1Q  Median      3Q     Max 
#-5.1729 -2.0298 -0.0298  0.8741  6.6722 

#Coefficients:
 # Estimate Std. Error t value Pr(>|t|)    
#(Intercept)    6.5827     1.7217   3.823  0.00115 ** 
 # Sorting.Time   1.6490     0.2582   6.387 3.98e-06 ***

#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 2.935 on 19 degrees of freedom
#Multiple R-squared:  0.6823,	Adjusted R-squared:  0.6655 
#F-statistic:  40.8 on 1 and 19 DF,  p-value: 3.983e-06

#3)
empdata <- emp_data
summary(empdata)
var(empdata$Salary_hike)
sd(empdata$Salary_hike)
var(empdata$Churn_out_rate)
sd(empdata$Churn_out_rate)
empmodel<-lm(Salary_hike ~ Churn_out_rate ,data = emp_data)
summary(empmodel)
plot(empmodel)

 #3)
 # > empdata <- emp_data
#> summary(empdata)
#Salary_hike   Churn_out_rate 
#Min.   :1580   Min.   :60.00  
#1st Qu.:1618   1st Qu.:65.75  
#Median :1675   Median :71.00  
#Mean   :1689   Mean   :72.90  
#3rd Qu.:1724   3rd Qu.:78.75  
#Max.   :1870   Max.   :92.00  
#> var(empdata$Salary_hike)
#[1] 8481.822
#> sd(empdata$Salary_hike)
#[1] 92.09681
#> var(empdata$Churn_out_rate)
#[1] 105.2111
#> sd(empdata$Churn_out_rate)
#[1] 10.25725
#> empmodel<-lm(Salary_hike ~ Churn_out_rate ,data = emp_data)
#> summary(empmodel)

#Call:
 # lm(formula = Salary_hike ~ Churn_out_rate, data = emp_data)

#Residuals:
 # Min     1Q Median     3Q    Max 
#-35.97 -23.13 -21.41  19.24  75.80 

#Coefficients:
 # Estimate Std. Error t value Pr(>|t|)    
#(Intercept)    2285.365     95.912  23.828 1.02e-08 ***
  #Churn_out_rate   -8.186      1.304  -6.277 0.000239 ***
  #---
 # Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 40.13 on 8 degrees of freedom
#Multiple R-squared:  0.8312,	Adjusted R-squared:  0.8101 
#F-statistic:  39.4 on 1 and 8 DF,  p-value: 0.0002386


#4)
salarydata <- Salary_Data
summary(salarydata)
var(salarydata$YearsExperience)
sd(salarydata$YearsExperience)
var(salarydata$Salary)
sd(salarydata$Salary)
salarymodel<-lm(YearsExperience ~ Salary ,data = Salary_Data)
summary(salarymodel)
plot(salarymodel)
#Hence the P-value is less than 0.05. So X varibale is significance and also Multiple R-Square value is 0.957 That's mean this model will predict the output 95.7% time correct

#output:
#summary(salarydata)
#YearsExperience      Salary      
#Min.   : 1.100   Min.   : 37731  
#1st Qu.: 3.200   1st Qu.: 56721  
#Median : 4.700   Median : 65237  
#Mean   : 5.313   Mean   : 76003  
#3rd Qu.: 7.700   3rd Qu.:100545  
#Max.   :10.500   Max.   :122391  
#> var(salarydata$YearsExperience)
#[1] 8.053609
#> sd(salarydata$YearsExperience)
#[1] 2.837888
#> var(salarydata$Salary)
#[1] 751550960
#> sd(salarydata$Salary)
#[1] 27414.43
#> salarymodel<-lm(YearsExperience ~ Salary ,data = Salary_Data)
#> summary(salarymodel)

#Call:
 # lm(formula = YearsExperience ~ Salary, data = Salary_Data)

#Residuals:
 # Min       1Q   Median       3Q      Max 
#-1.12974 -0.46457  0.04105  0.54311  0.79669 

#Coefficients:
 # Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -2.383e+00  3.273e-01  -7.281  6.3e-08 ***
  #Salary       1.013e-04  4.059e-06  24.950  < 2e-16 ***
  #---
 # Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 0.5992 on 28 degrees of freedom
#Multiple R-squared:  0.957,	Adjusted R-squared:  0.9554 
#F-statistic: 622.5 on 1 and 28 DF,  p-value: < 2.2e-16
