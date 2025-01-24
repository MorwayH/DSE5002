#set working directory
setwd("Week_1")

#exercise 1
#Assign 10 to the variable x. Assign 5 to the variable y. 
#Assign 20 to the variable z.
x <- 10
y <- 5
z <- 20

# Exercise 2
# Show that x is less than z but greater than y.
# Note: your output must be a SINGLE boolean, 
# do not output a boolean for each expression.
(x < z) & (y < x)

# Exercise 3
# Show that x and y do not equal z.
# Note: your output must be a SINGLE boolean, 
#do not output a boolean for each expression.
(x != z) & (y != z)

# Exercise 4
# Show that the formula x + 2y = z.
# Note: your output must be a SINGLE boolean.
(x + 2*y) == z

# Exercise 5
# I have created a vector (test_vector) of integers for you. 
#Determine if any of x, y, or z are in the vector.
# Note: your output must be a SINGLE boolean, 
#do not output a boolean for each expression.
test_vector <- c(1,5,11:22)
(x %in% test_vector) | (y %in% test_vector) | (z %in% test_vector)
 
# Exercise 6
# Show which value is contained in the test vector. 
#To do this you will need to create an element-wise logical
# vector using operators. x == vector. 
# Once you have done that you will need to use slicing to return all
# indices that have matches. Note: your output should be two integers
indices <- (x == test_vector) | (y == test_vector) | (z == test_vector)
test_vector[indices]
