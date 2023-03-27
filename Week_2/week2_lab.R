# Please complete all exercises below. You may use stringr, lubridate, or the forcats library. 
# 
# Place this at the top of your script:
library(stringr)
library(lubridate)
library(forcats)

# Exercise 1
#Read the sales_pipe.txt file into an R data frame as sales. 

sales_pipe_df <- read.delim(file= "Week_2/Data/sales_pipe.txt", sep="|", header=TRUE , stringsAsFactors = FALSE)



# Exercise 2
# You can extract a vector of columns names from a data frame 
#using the colnames() function. Notice the first column has 
#some odd characters. Change the column name for the FIRST 
#column in the sales date frame to Row.ID. 
# 
# **Note: You will need to assign the first element of colnames 
#to a single character.**
#   

#rename column 1 header to Row.ID
colnames(sales_pipe_df)[1] <- "Row.ID"

# 
# # Exercise 3
# Convert both Order.ID and Order.Date to date vectors within the 
#sales data frame. What is the number of days between the most 
#recent order and the oldest order?
#   How many years is that?
#   How many weeks?
#   
#   **Note: Use lubridate**
#   

#
#convert Order.Date to vector of Dates
sales_pipe_df$Order.Date.Format <- mdy(sales_pipe_df$Order.Date)

#create variables for minimum and maximum Order dates, 
#then difference between the two dates, and convert to number of days. 
#Then convert number of days to weeks and years, 
#save each in a variable, and print.
order_max_date <- max(sales_pipe_df$Order.Date.Format)
order_min_date <- min(sales_pipe_df$Order.Date.Format)
difference_dates <- order_max_date - order_min_date
days_between_all_orders<- time_length(difference_dates,"day")
days_between_all_orders
weeks_between_all_orders <- time_length(difference_dates, "week")
weeks_between_all_orders
years_between_all_orders <- time_length(difference_dates, "year")
years_between_all_orders

# 
# 
# # Exercise 4
# #What is the average number of days it takes to ship an order?
#   

#convert Ship.Date to vector of Dates and find difference in days 
#between order and shipment dates, then convert to integers and 
#store in df
sales_pipe_df$Ship.Date.Format <- mdy(sales_pipe_df$Ship.Date)
sales_pipe_df$Shipping.Days <- time_length((sales_pipe_df$Ship.Date.Format - sales_pipe_df$Order.Date.Format), "day")
mean(sales_pipe_df$Shipping.Days)

# 
# 
# # Exercise 5
# How many customers have the first name Bill?
#   You will need to split the customer name into first and 
#last name segments and then use a regular expression to 
#match the first name bill. Use the length() function to 
#determine the number of customers with the first name Bill 
#in the sales data. 
# 

#split first and last name column into 2 columns, 
#then paste them to exisitng df and rename them.
customer_name <- str_split_fixed(string=sales_pipe_df$Customer.Name, pattern=" ", n=2)
sales_pipe_df$Customer.First.Name <- paste(customer_name[,1])
sales_pipe_df$Customer.Last.Name <- paste(customer_name[,2])

#Then use regex to determine if first name is Bill.
#Then use logical indexing to find all the "Bill"s then use
#length to determine the number of elements in the vector.
matched_names <- str_match(sales_pipe_df$Customer.First.Name, pattern= "Bill")
length(which(matched_names == "Bill"))



# 
# # Exercise 6
# How many mentions of the word 'table' are there in the 
#Product.Name column? 
#   **Note you can do this in one line of code**
#   

# find "table" in column using reg expression matching, 
#and take length of which matches
length(which(str_match(sales_pipe_df$Product.Name, pattern = "table")== "table"))
# 
#
# 
# # Exercise 7
# Create a table of counts for each state in  the sales data. 
# The counts table should be ordered alphabetically from A to Z. 
# 
#create a table of states and their counts, and sort alphabetically by name 
table(sort(sales_pipe_df$State, decreasing = FALSE))

  
# # Exercise 8
# Create an alphabetically ordered barplot for each sales 
#Category in the State of Texas. 
# 
# create a df for sales in Texas, then a table for the 
#categories listed alphabetically, and a barplot of the table
sales_catergoies_texas_df <- sales_pipe_df[(sales_pipe_df$State== "Texas") ,]
barplot(table(sort(sales_catergoies_texas_df$Category, decreasing = FALSE)))


# 
# # Exercise 9
# Find the average profit by region.
# **Note: You will need to use the aggregate() function to do this. 
#To understand how the function works type ?aggregate in the console.**
#   

#aggregate the Profit column, listing by Region, with the mean function 
aggregate(sales_pipe_df$Profit, list(sales_pipe_df$Region), FUN=mean)


# # Exercise 10
# Find the average profit by order year. 
# **Note: You will need to use the aggregate() function to do this. 
# To understand how the function works type ?aggregate in the console.**
#   
#extract year from order date and add year to df
Order.Year <- str_split_fixed(string=sales_pipe_df$Order.Date.Format, pattern="-", n=3)
sales_pipe_df$Order.Year <- paste(Order.Year[,1])

#then aggregate profits by year and find mean
aggregate(sales_pipe_df$Profit, list(sales_pipe_df$Order.Year), FUN=mean)
