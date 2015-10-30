library(dplyr)

# Make tbl
recipes <- tbl_df(recipes)
recipes$cuisine <- factor(recipes$cuisine)
ingredients <- tbl_df(ingredients)
ingredients <- inner_join(recipes[,c(1,2)], ingredients, by = "id")

# Explore
str(recipes)
unique(recipes$cuisine)
count(recipes, cuisine) %>% arrange(desc(n))

# Find top ingredients per cuisine

topN <- 25
specialIng <- character()

for(i in 1:length(levels(ingredients$cuisine))){
    cuisineIng <- filter(ingredients, cuisine==levels(ingredients$cuisine)[i]) %>%
        count(ingredient) %>% arrange(desc(n)) %>% select(ingredient)
    specialIng <- c(specialIng, head(cuisineIng$ingredient, topN))
}

specialIng <- unique(specialIng)

# Find top cuisine per ingredient

topIng <- data.frame(stringsAsFactors = FALSE)
for(i in 1:length(specialIng)){
    highpct <- filter(ingredients, ingredient == specialIng[i]) %>% count(cuisine) %>% 
        mutate(pct = n / sum(n)) %>% arrange(desc(pct))
    highpct$ingredient <- specialIng[i]
    topIng <- rbind(topIng, highpct[1,])
}

topIng <- arrange(topIng, desc(pct))

# Clean ingredient names

filter(ingredients, ingredient == "salt") %>% count(cuisine) %>% 
    mutate(pct = n / sum(n)) %>% arrange(desc(pct))

unique(ingredients$ingredient[grep("salt", ingredients$ingredient, ignore.case = TRUE)])

ingredients$ingredient[grep("naan", ingredients$ingredient, ignore.case = TRUE)] <- "naan"
ingredients$ingredient[grep("soy sauce", ingredients$ingredient, ignore.case = TRUE)] <- "soy sauce"
ingredients$ingredient[grep("jalapeno", ingredients$ingredient, ignore.case = TRUE)] <- "jalapenos"
ingredients$ingredient[grep("tortilla", ingredients$ingredient, ignore.case = TRUE)] <- "tortillas"
ingredients$ingredient[grep("chili powder", ingredients$ingredient, ignore.case = TRUE)] <- "chili powder"

# Exploratory plots
#boxplot comparisons
#pairwise plot
#clusters