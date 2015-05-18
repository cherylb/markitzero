#--------------
#Assigning a variable
#-------------
cat("Assign values to a variable using different methods\n\n")
print("using <- to assign 10")
testing <- 10
print(testing)
print("using = to assign 20")
testing = 20
print(testing)
print("using assign to assign 30")
assign("testing", 30)
print(testing)
cat("------------------------\n\n")

#--------------
#sum and is.na functions
#-------------
cat("calculate total of missing values\n\n")
x <- c(5, 8 , NA, 21, NA, 55)
print("a vector with some missing values")
print(x)
y <- sum(is.na(x))
print("calculated sum of the missing values is")
print(y)

cat("------------------------\n\n")

#--------------
#library vs require
#-------------

#load real package 'lattice' using library loads package
#and returns a list of loaded functions
cat("library vs require to load packages\n\n")

cat("use libary to load lattice package\n")
y <- library(lattice)
print("new library is:")
print(y)
#can verify y includes lattice, is an extra step
print("library loaded lattice package?")
print("lattice" %in% y)

detach("package:lattice")
search() #shows it is removed
cat("-----\n\n")

#loading the package lattice using require loads the package
#and returns TRUE with confirmation

cat("Use requre to load lattice\n")
y = require(lattice) #y returns a True value if package is loaded
print("require loaded lattice package?")
print(y)
detach("package:lattice")
cat("-----\n\n")

#load a missing package using require returns the value False and a warning
cat("use requre to load a missing package\n\n")
y <- require("nonsense")
print("require loaded nonsense package?")
print(y)
cat("-----\n\n")

#load a misisng package using library returns an error
cat("use library to load a missing package")
print("library loads nonsense package?")
library("nonsense")

#using require can help you handle packages that are missing or misspelled 
#by providing information about the success or failure of the load
#library will error the code out if the package is not available which
#could cause problems if it happens within another function
#but could be exactly what you want to happen

