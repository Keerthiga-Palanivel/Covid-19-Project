############################################################
#Step 0: Initialize the PDF Generation
############################################################

pdf(file = "./Visualization.pdf", width = 10, height = 6)

############################################################
#Step 1: Preprocess the Population Data
############################################################
#This Dataset comprises a huge collection of county wise information about the USA. 
#Load the population data from CSV file
#Remove the unnecessary columns
#Format the column names to the standard format of other dataframes

population.table = read.csv("us_counties.csv",header=TRUE)
population = subset(population.table, select=c("FIPS", "Total_age65plus"))
colnames(population) = c("FIPS", "Total_65Plus")


############################################################
#Step 2: Preprocess the Covid Data
############################################################
#This dataset comprises of all COVID-19 infection related essential information.
#Load the covid data from CSV file
#Remove the unnecessary columns
#Format the column names to the standard format of other dataframes
#Format the dataframe and sort it based on FIPS

covid.table = read.csv("us_counties_covid_19_dataset.csv", header=TRUE)
covid.table = subset(covid.table, select=c("fips", "deaths"))
colnames(covid.table) = c("FIPS", "Deaths")
covid.table = covid.table[complete.cases(covid.table$FIPS), ]
covid = covid.table[order(covid.table$FIPS), ]


############################################################
#Step 3: Combine population and covid dataframes
############################################################
#Merge the covid dataframe and population dataframe
#Remove the FIPS. Aggregated Deaths and Total_65Plus columns
#Normalize the variables

combined = merge(
  x = population, 
  y = covid[, c("FIPS", "Deaths")], 
  by = "FIPS",all.y = TRUE)

final_df = data.frame(
  "Deaths"= tapply(combined$Deaths, combined$FIPS, max),
  "Population_65Plus"= tapply(combined$Total_65Plus, combined$FIPS, mean))
final_df$Deaths = scale(final_df$Deaths)
final_df$Population_65Plus = scale(final_df$Population_65Plus)

############################################################
#Step 4: Generate the dependent and independent variables
############################################################
#Define the Independent variable (Population of people older than 65 years) as x_variable from US_Counties.csv
#Define the Dependent variable (Total number of deaths) as y_variable from us_counties_covid_19_dataset.csv
#Find the Mean and Standard Deviation of y_variable
#Compute the correlation between x_variable and y_variable

x_variable = final_df$Population_65Plus
y_variable = final_df$Death
y_variable = y_variable[na.rm = TRUE]
y_mean = mean(y_variable,use="complete.obs")
y_sd = sd(y_variable, na.rm = TRUE)
y_min = min(y_variable)
y_max = max(y_variable)
correlation = cor(x_variable,y_variable,use="complete.obs")

############################################################
#Step 5: Visualization
############################################################
#Plot the Histogram to visualize the distribution
#Draw the normalized line to evaluate the distribution
#Plot the correlation graph between x_variable and y_variable
#Compute and add the linear model to the graph to visualize the correlation
#Generate the visualization PDF

min_text = sprintf("%0.2f", y_min)
max_text = sprintf("%0.2f", y_max)
hist_message = paste("The data is of range ", min_text, " to ", max_text)
hist(y_variable, 
     main = "Histogram of Normalized Aggregation of COVID-19 Deaths",
     xlab = "Normalized value of COVID-19 Deaths per county (in numbers)",
     xlim = c(-10,30),
     breaks = 15, freq = F)
lines(seq(-2,30, by=.5), dnorm(seq(-2,30, by=.5),y_mean,y_sd),col = "red")
text(15, 0.2, hist_message, col = 2)




cor_text = sprintf("%0.2f", correlation)
cor_message = paste("The Correlation value is ", cor_text)
plot(x_variable, y_variable,
     main = "Correlation between Population over 65 years of age and COVID-19 Deaths",
     xlab = "Normalized sum of Population over 65 years of age (in numbers)",
     ylab = "Normalized sum of COVID-19 Deaths (in numbers)",
     pch = 19,
     frame = T)
model <- lm(y_variable ~ x_variable, data = final_df)
abline(model, col = "violet")
text(20, 10, cor_message, col = 2)


dev.off()

############################################################
#Step 6: Statistical Analysis
############################################################
#Validate the results using cor.test(), summary() and t.test()

cor.test(x_variable,y_variable,use="pairwise.complete.obs",method ="pearson")
cor.test(x_variable,y_variable,method ="kendall")
cor.test(x_variable,y_variable,method ="spearman",exact=FALSE)
summary(final_df)
t.test(x_variable,y_variable)


################### End of Code#############################