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

ingredients$ingredient[grep("tortilla", ingredients$ingredient, ignore.case = TRUE)] <- "tortillas"
ingredients$ingredient[grep("dashi", ingredients$ingredient, ignore.case = TRUE)] <- "dashi"
ingredients$ingredient[grep("salsa", ingredients$ingredient, ignore.case = TRUE)] <- "salsa"
ingredients$ingredient[grep("gochu", ingredients$ingredient, ignore.case = TRUE)] <- "gochujang"
ingredients$ingredient[grep("kimchi", ingredients$ingredient, ignore.case = TRUE)] <- "kimchi"
ingredients$ingredient[grep("shao", ingredients$ingredient, ignore.case = TRUE)] <- "shaoxing wine"
ingredients$ingredient[grep("masala", ingredients$ingredient, ignore.case = TRUE)] <- "masala"
ingredients$ingredient[grep("fermented black beans", ingredients$ingredient, ignore.case = TRUE)] <- "chinese douchi"
ingredients$ingredient[grep("black beans", ingredients$ingredient, ignore.case = TRUE)] <- "black beans"
ingredients$ingredient[grep("nori", ingredients$ingredient, ignore.case = TRUE)] <- "nori"
ingredients$ingredient[grep("avocado", ingredients$ingredient, ignore.case = TRUE)] <- "avocado"
ingredients$ingredient[grep("andouille", ingredients$ingredient, ignore.case = TRUE)] <- "andouille"
ingredients$ingredient[grep("parmesan", ingredients$ingredient, ignore.case = TRUE)] <- "parmesan"
ingredients$ingredient[grep("parm.gian", ingredients$ingredient, ignore.case = TRUE)] <- "parmesan"
ingredients$ingredient[grep("cajun", ingredients$ingredient, ignore.case = TRUE)] <- "cajun-creole seasoning"
ingredients$ingredient[grep("creole", ingredients$ingredient, ignore.case = TRUE)] <- "cajun-creole seasoning"
ingredients$ingredient[grep("tu.*meric", ingredients$ingredient, ignore.case = TRUE)] <- "turmeric"
ingredients$ingredient[grep("hoisin", ingredients$ingredient, ignore.case = TRUE)] <- "hoisin sauce"
ingredients$ingredient[grep("couscous", ingredients$ingredient, ignore.case = TRUE)] <- "couscous"
ingredients$ingredient[grep("cumin ?seed", ingredients$ingredient, ignore.case = TRUE)] <- "cumSeed"
ingredients$ingredient[grep("cumin", ingredients$ingredient, ignore.case = TRUE)] <- "cumin"
ingredients$ingredient[grep("buttermilk", ingredients$ingredient, ignore.case = TRUE)] <- "buttermilk"
ingredients$ingredient[grep("light soy sauce", ingredients$ingredient, ignore.case = TRUE)] <- "light/dark soy"
ingredients$ingredient[grep("dark soy sauce", ingredients$ingredient, ignore.case = TRUE)] <- "light/dark soy"
ingredients$ingredient[grep("soy sauce", ingredients$ingredient, ignore.case = TRUE)] <- "soy sauce"
ingredients$ingredient[grep("oyster.*sauc", ingredients$ingredient, ignore.case = TRUE)] <- "oyster sauce"
ingredients$ingredient[grep("feta", ingredients$ingredient, ignore.case = TRUE)] <- "feta cheese"
ingredients$ingredient[grep("sour cream", ingredients$ingredient, ignore.case = TRUE)] <- "sour cream"
ingredients$ingredient[grep("thai basil", ingredients$ingredient, ignore.case = TRUE)] <- "asian leaves"
ingredients$ingredient[grep("asian basil", ingredients$ingredient, ignore.case = TRUE)] <- "asian leaves"
ingredients$ingredient[grep("holy basil", ingredients$ingredient, ignore.case = TRUE)] <- "asian leaves"
ingredients$ingredient[grep("anise basil", ingredients$ingredient, ignore.case = TRUE)] <- "asian leaves"
ingredients$ingredient[grep("basil", ingredients$ingredient, ignore.case = TRUE)] <- "basil"
ingredients$ingredient[grep("jalapeno", ingredients$ingredient, ignore.case = TRUE)] <- "jalapenos"
ingredients$ingredient[grep("poblano", ingredients$ingredient, ignore.case = TRUE)] <- "poblanos"
ingredients$ingredient[grep("lemon.?grass", ingredients$ingredient, ignore.case = TRUE)] <- "lemongrass"
ingredients$ingredient[grep("diced tomato.*chili", ingredients$ingredient, ignore.case = TRUE)] <- "diced tomatoes and chilies"
ingredients$ingredient[grep("green chil", ingredients$ingredient, ignore.case = TRUE)] <- "green chilies"
ingredients$ingredient[grep("chili powder", ingredients$ingredient, ignore.case = TRUE)] <- "chili powder"
ingredients$ingredient[grep("curry powder", ingredients$ingredient, ignore.case = TRUE)] <- "curry powder"
ingredients$ingredient[grep("pesto", ingredients$ingredient, ignore.case = TRUE)] <- "pesto"
ingredients$ingredient[grep("chopped cilantro", ingredients$ingredient, ignore.case = TRUE)] <- "cilantro"
ingredients$ingredient[grep("fresh cilantro", ingredients$ingredient, ignore.case = TRUE)] <- "cilantro"
ingredients$ingredient[grep("cilantro leaves", ingredients$ingredient, ignore.case = TRUE)] <- "cilantro"
ingredients$ingredient[grep("cilantro sprigs", ingredients$ingredient, ignore.case = TRUE)] <- "cilantro"
ingredients$ingredient[grep("dry white wine", ingredients$ingredient, ignore.case = TRUE)] <- "white wine"
ingredients$ingredient[grep("Italian parsley", ingredients$ingredient, ignore.case = TRUE)] <- "flat leat parsley"
ingredients$ingredient[grep("sesame.*oil", ingredients$ingredient, ignore.case = TRUE)] <- "sesame oil"
ingredients$ingredient[grep("starch", ingredients$ingredient, ignore.case = TRUE)] <- "starch"
ingredients$ingredient[grep("fish sauce", ingredients$ingredient, ignore.case = TRUE)] <- "fish sauce"
ingredients$ingredient[grep("beets", ingredients$ingredient, ignore.case = TRUE)] <- "beets"
ingredients$ingredient[grep("tuna.*oil", ingredients$ingredient, ignore.case = TRUE)] <- "tuna in oil"
ingredients$ingredient[grep("olive oil mayo", ingredients$ingredient, ignore.case = TRUE)] <- "mayonnaise"
ingredients$ingredient[grep("olive oil", ingredients$ingredient, ignore.case = TRUE)] <- "olive oil"
ingredients$ingredient[grep("vietnamese coriander", ingredients$ingredient, ignore.case = TRUE)] <- "vietnamese rau ram"
ingredients$ingredient[grep("coriander", ingredients$ingredient, ignore.case = TRUE)] <- "coriander"

# cachaca
# ghee
# sake
# mirin
# clove
# baking soda
# lime and lime juice?

# Exploratory plots
#boxplot comparisons
#pairwise plot
#clusters