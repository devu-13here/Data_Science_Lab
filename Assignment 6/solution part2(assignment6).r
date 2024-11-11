getwd()
R: mean(mtcars$mpg) ?
  

# Read the CSV file
data <- read.csv("DataSet.csv")

# Show the first 4 records and read them by head function
head(data, 4)

# Show the last 4 records
tail(data, 4)

# Convert Exp_date to Date format
data$Exp_date <- as.Date(data$Exp_date, format="%Y-%m-%d")

# Find correlation between Quantity_in_stock and Exp_date (using year of Exp_date)
cor(data$Quantity_in_stock, as.numeric(format(data$Exp_date, "%Y")))


# Plot a bar graph
barplot(data$Sales, names.arg=data$Manf_year, col="lightblue", xlab="Year of Manufacturing", ylab="Sales", main="Sales by Manufacturing Year")


# Find the companies with more than one type of medicine
company_med_count <- table(data$Company)
company_med_count
company_med_count[company_med_count > 1]



# List all unique medicine names (Med_Name)
unique(data$Med_Name)




#Identify expiring medicines (those with exp date within a year)
expiring_medicines <- data[data$Exp_date <= Sys.date() + 365,]
expiring_medicines

# Identify expiring medicines (those with Exp_date within the next year)
expiring_medicines <- data[data$Exp_date <= Sys.Date() + 365,]

# Boxplot for expiring medicines
boxplot(expiring_medicines$Quantity_in_stock ~ expiring_medicines$Med_Name, col="lightgreen", xlab="Medicine", ylab="Quantity in Stock", main="Expiring Medicines")

# Calculate the average stock
mean(data$Quantity_in_stock)

# Perform linear regression between Manufacturing Year and Sales
model <- lm(Sales ~ Manf_year, data=data)

# Plot regression line
plot(data$Manf_year, data$Sales, pch=16, col="blue", xlab="Manufacturing Year", ylab="Sales", main="Regression between Manufacturing Year and Sales")
abline(model, col="red")

#



















#PRACTICE PROBLEMS
#*
#List down all unique company names
unique(data$Company)
#List down all unique med ID
unique(data$MedID)
