# 1
install.packages('readxl')
library('readxl')



#a)
Return_data <- read_excel("jaggia_ba_2e_ch07_data.xlsx", sheet="Return") 
View(Return_data)

Return_fit <- lm(Return ~ PE+ PS, data = Return_data)  
return_summary <- summary(Return_fit)  

# The signs are as expected. As PE increases, the predicted returns increase, 
# and as PS decreases, the predicted returns increase. 

#b)

# As the PS ratio increases by 1 unit, the predicted return of the firm decreases 
# by 3.3681 percentage points, holding PE constant.

#c)

predict(Return_fit, data.frame(PE=10, PS=2))
predict(Return_fit, data.frame(PE=10, PS=2), interval = "confidence", level = 0.95)

# instead of giving single point estimate, give confidence intervals for
# prediction instead to give range of most plausible values.


#d)
return_summary$sigma
# This gives the standard error

#e)
return_summary$r.squared
# R^2 is 0.4028
# This means that 40.28% of the sample variation in returns is explained by the 
# sample regression equation. 

## Additional - resdiual plots

plot(Return_fit)

# plot 1 allows you to detect the shape of residuals e.g. constant variance, patterns
# should be randomly displaced around 0 and evenly spread throughout

# plot 2 checks whether residuals follow a standard normal distribution
# if they fall along diagonal then fitness is good

# plot 3 isnt too important for this course

# plot 4 shows standardised residuals and leverage
# any residuals which are above 3 or below -3 are most likely outliers.
# for every 100 data points there should be only 1 that is below/above -3/3
# leverage shows how influential each data point is on the model i.e. the drag of each point






# 2

#a)
Houses_data <- read_excel("jaggia_ba_2e_ch07_data.xlsx", sheet="Houses") 
Houses_fit <- lm(Price ~Sqft+Beds+Baths+Colonial, data = Houses_data)  
Houses_summary <- summary(Houses_fit) 

#b)

# Jointly significant -> f-stat

# The reported p value is approximately 0. At the 5% significance level, we have evidence to 
# reject the null hypothesis. The predictor variables are jointly significant in explaining the 
# price of all homes in Arlington.  

#c)





# 3
myData <- read_excel("jaggia_ba_2e_ch07_data.xlsx", sheet="Rental") 
Model <- lm(Rent~ Bed + Bath + Sqft, data=myData) 
Rent_summary <- summary(Model) 

# Rent̂=300.4116 + 225.8100Bed+89.2661Bath+0.2096Sqft



# b)
# Residual plots and more
plot(Model)

#c)
#Since home prices tend to vary more as they get larger, the amount of rent charged is likely to 
#vary more as home size increases. Therefore, the variable Sqft may cause heteroskedasticity. 
