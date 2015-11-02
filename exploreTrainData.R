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

topN <- 40
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

# Add features for ethnic ingredients (before cleaning)

features <- recipes[,c(1,2)]

addFeature <- function(pattern, exact = FALSE){
    if(!exact){
        ifelse(features$id 
               %in% ingredients$id[grep(pattern, ingredients$ingredient, ignore.case = TRUE)],
               1, 0)        
    } else{
        ifelse(features$id 
               %in% ingredients$id[ingredients$ingredient==pattern],
               1, 0)
    }

}

features$chinese <- addFeature("chinese")
features$irish <- addFeature("irish")
features$italian <- addFeature("italian")
features$jamaican <- addFeature("jamaican")
features$japanese <- addFeature("japanese")
features$korean <- addFeature("korean")
features$mexican <- addFeature("mexican")
features$spanish <- addFeature("spanish")
features$thai <- addFeature("thai")
features$vietnamese <- addFeature("vietnamese")

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
ingredients$ingredient[grep("curry paste", ingredients$ingredient, ignore.case = TRUE)] <- "curry paste"
ingredients$ingredient[grep("fermented black beans", ingredients$ingredient, ignore.case = TRUE)] <- "douchi"
ingredients$ingredient[grep("black beans", ingredients$ingredient, ignore.case = TRUE)] <- "black beans"
ingredients$ingredient[grep("five.*spice", ingredients$ingredient, ignore.case = TRUE)] <- "chinese five-spice powder"
ingredients$ingredient[grep("nori", ingredients$ingredient, ignore.case = TRUE)] <- "nori"
ingredients$ingredient[grep("avocado", ingredients$ingredient, ignore.case = TRUE)] <- "avocado"
ingredients$ingredient[grep("andouille", ingredients$ingredient, ignore.case = TRUE)] <- "andouille"
ingredients$ingredient[grep("mustard seed", ingredients$ingredient, ignore.case = TRUE)] <- "mustard seeds"
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
ingredients$ingredient[grep("thai basil", ingredients$ingredient, ignore.case = TRUE)] <- "asian leaves"
ingredients$ingredient[grep("asian basil", ingredients$ingredient, ignore.case = TRUE)] <- "asian leaves"
ingredients$ingredient[grep("holy basil", ingredients$ingredient, ignore.case = TRUE)] <- "asian leaves"
ingredients$ingredient[grep("anise basil", ingredients$ingredient, ignore.case = TRUE)] <- "asian leaves"
ingredients$ingredient[grep("basil", ingredients$ingredient, ignore.case = TRUE)] <- "basil"
ingredients$ingredient[grep("pecan", ingredients$ingredient, ignore.case = TRUE)] <- "pecans"
ingredients$ingredient[grep("feta", ingredients$ingredient, ignore.case = TRUE)] <- "feta cheese"
ingredients$ingredient[grep("sour cream", ingredients$ingredient, ignore.case = TRUE)] <- "sour cream"
ingredients$ingredient[grep("jalapeno", ingredients$ingredient, ignore.case = TRUE)] <- "jalapenos"
ingredients$ingredient[grep("poblano", ingredients$ingredient, ignore.case = TRUE)] <- "poblanos"
ingredients$ingredient[grep("corn.*meal", ingredients$ingredient, ignore.case = TRUE)] <- "cornmeal"
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
ingredients$ingredient[grep("kaffir lime", ingredients$ingredient, ignore.case = TRUE)] <- "makrut"
ingredients$ingredient[grep("lime leaves", ingredients$ingredient, ignore.case = TRUE)] <- "makrut"
ingredients$ingredient[grep("lime zest", ingredients$ingredient, ignore.case = TRUE)] <- "zest"
ingredients$ingredient[grep("lime peel", ingredients$ingredient, ignore.case = TRUE)] <- "zest"
ingredients$ingredient[grep("lime rind", ingredients$ingredient, ignore.case = TRUE)] <- "zest"
ingredients$ingredient[grep("lemon.*lime", ingredients$ingredient, ignore.case = TRUE)] <- "lemon soda"
ingredients$ingredient[grep("lime", ingredients$ingredient, ignore.case = TRUE)] <- "lime juice"
ingredients$ingredient[grep("lime", ingredients$ingredient, ignore.case = TRUE)] <- "lime juice"
ingredients$ingredient[grep("allspice", ingredients$ingredient, ignore.case = TRUE)] <- "allspice"
ingredients$ingredient[grep("saffron", ingredients$ingredient, ignore.case = TRUE)] <- "saffron"
ingredients$ingredient[grep("sherry.*vinegar", ingredients$ingredient, ignore.case = TRUE)] <- "sherry vinegar"
ingredients$ingredient[grep("yolk", ingredients$ingredient, ignore.case = TRUE)] <- "egg yolks"
ingredients$ingredient[grep("shallot", ingredients$ingredient, ignore.case = TRUE)] <- "shallots"
ingredients$ingredient[grep("thyme", ingredients$ingredient, ignore.case = TRUE)] <- "thyme"
ingredients$ingredient[grep("whip.*cream", ingredients$ingredient, ignore.case = TRUE)] <- "whipping cream"

# cachaca
# ghee
# sake
# mirin
# clove
# baking soda
# preserved lemon

# Add features based on ingredients

features$cachaca <- addFeature("cachaca")
features$tortillas <- addFeature("tortilla")
features$dashi <- addFeature("dashi")
features$salsa <- addFeature("salsa")
features$gochujang <- addFeature("gochu")
features$kimchi <- addFeature(kimchi)
features$shaoxing_wine <- addFeature("shao")
features$masala <- addFeature("masala")
features$curry_paste <- addFeature("curry paste")

ingredients$ingredient[grep("fermented black beans", ingredients$ingredient, ignore.case = TRUE)] <- "douchi"
features$black_beans <- addFeature("black beans")

features$five_spice_powder <- addFeature("five.*spice")
features$nori <- addFeature("nori")
features$avocado <- addFeature("avocado")
features$andouille <- addFeature("andouille")
features$mustard_seed <- addFeature("mustard seed")

ingredients$ingredient[grep("parm.gian", ingredients$ingredient, ignore.case = TRUE)] <- "parmesan"
features$parmesan <- addFeature("parmesan")

ingredients$ingredient[grep("creole", ingredients$ingredient, ignore.case = TRUE)] <- "cajun-creole seasoning"
features$cajun-creole_seasoning <- addFeature("cajun")

features$turmeric <- addFeature("tu.*meric")
features$hoisin_sauce <- addFeature("hoisin")
features$couscous <- addFeature("couscous")

ingredients$ingredient[grep("cumin ?seed", ingredients$ingredient, ignore.case = TRUE)] <- "cumSeed"
features$cumin_seed <- addFeature("cumSeed")
features$cumin <- addFeature("cumin")

features$buttermilk <- addFeature("buttermilk")

ingredients$ingredient[grep("light soy sauce", ingredients$ingredient, ignore.case = TRUE)] <- "light/dark soy sauce"
features$light_dark_soy_sauce <- addFeature("dark soy sauce")
features$soy_sauce <- addFeature("soy sauce")

features$oyster_sauce <- addFeature("oyster.*sauc")

ingredients$ingredient[grep("pesto", ingredients$ingredient, ignore.case = TRUE)] <- "pesto"
features$pesto <- addFeature("pesto")

ingredients$ingredient[grep("thai basil", ingredients$ingredient, ignore.case = TRUE)] <- "asian leaves"
ingredients$ingredient[grep("asian basil", ingredients$ingredient, ignore.case = TRUE)] <- "asian leaves"
ingredients$ingredient[grep("holy basil", ingredients$ingredient, ignore.case = TRUE)] <- "asian leaves"
ingredients$ingredient[grep("anise basil", ingredients$ingredient, ignore.case = TRUE)] <- "asian leaves"
features$asian_basil <- addFeature("asian leaves")
features$basil <- addFeature("basil")

features$pecans <- addFeature("pecan")
features$feta <- addFeature("feta")
features$sour_cream <- addFeature("sour cream")
features$jalapenos <- addFeature("jalapeno")
features$poblanos <- addFeature("poblano")
features$cornmeal <- addFeature("corn.*meal")
features$lemongrass <- addFeature("lemon.?grass")

ingredients$ingredient[grep("diced tomato.*chili", ingredients$ingredient, ignore.case = TRUE)] <- "diced tomatoes and chilies"
features$green_chilies <- addFeature("green chil")

features$chili_powder <- addFeature("chili powder")
features$curry_powder <- addFeature("curry powder")

ingredients$ingredient[grep("cilantro stem", ingredients$ingredient, ignore.case = TRUE)] <- "cilRoot"
ingredients$ingredient[grep("cilantro root", ingredients$ingredient, ignore.case = TRUE)] <- "cilRoot"
features$cilantro <- addFeature("cilantro")

ingredients$ingredient[grep("dry white wine", ingredients$ingredient, ignore.case = TRUE)] <- "white wine"
features$white_wine <- addFeature("white wine", exact = TRUE)

ingredients$ingredient[grep("Italian parsley", ingredients$ingredient, ignore.case = TRUE)] <- "flat leat parsley"
ingredients$ingredient[grep("sesame.*oil", ingredients$ingredient, ignore.case = TRUE)] <- "sesame oil"
ingredients$ingredient[grep("starch", ingredients$ingredient, ignore.case = TRUE)] <- "starch"
ingredients$ingredient[grep("fish sauce", ingredients$ingredient, ignore.case = TRUE)] <- "fish sauce"
ingredients$ingredient[grep("beets", ingredients$ingredient, ignore.case = TRUE)] <- "beets"
    
# Exploratory plots
#boxplot comparisons
#pairwise plot
#clusters