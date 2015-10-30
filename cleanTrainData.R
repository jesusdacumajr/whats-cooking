# Question: How do I use the ingredients of a recipe to classify its cuisine?

# Load the Yummly training data from Kaggle

library(jsonlite)

temp <- tempfile()
trainfile <- "http://www.kaggle.com/c/whats-cooking/download/train.json.zip"
download.file(trainfile, temp, method = "curl")
data <- fromJSON(unz(temp, "train.json"))

# Clean the data

library(dplyr)

totalIngredients <- 0
for(ii in 1:nrow(recipes)){
  totalIngredients <- totalIngredients + length(recipes[ii,3][[1]]) # Find the total number of ingredients from all recipes
  } 

ingredients <- data.frame(id = integer(totalIngredients), ingredient = character(totalIngredients), stringsAsFactors = FALSE)

count <- 1

for(ii in 1:nrow(recipes)){
  for(jj in 1:length(recipes[ii,3][[1]])){
    ingredients[count,1] <- recipes[ii,1]
    ingredients[count,2] <- recipes[ii,3][[1]][jj]
    count <- count + 1
  }
}