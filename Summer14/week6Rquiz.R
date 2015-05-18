
#Copy Pizza Data in R
pizzapath <- "C:/Users/Cheryl/Downloads/pizza-store-data.csv"
pizza <- read.csv(file = pizzapath, header = T, stringsAsFactors = F)

#View head of data
print("Preview of pizza store data")
print(head(pizza))


#description of data
#date:  Character, date of the observation
#store: Character, name of the pizza store
#pizzas: integer, number of pizzas sold
#customers:  integer, number of customers
  
# summary of data
print("Summary of pizza store data")
print(summary(pizza))

