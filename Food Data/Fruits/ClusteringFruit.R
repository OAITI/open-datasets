
# This notebook was created by your friendly Omniacs at Omni Analytics Innovative Technologies Initiative
# You can visit us at oaiti.org
# Follow us on twitter @omnianalytics

#Load Libraries
library(dplyr)
library(cluster)
library(ggdendro)
library(ggplot2)
library(RColorBrewer)

# Read in fruit data
# Property of and Compiled by Omni Analytics Group, LLC - http://omnianalytics.io
fruit<-structure(list(Name = structure(c(40L, 1L, 2L, 3L, 4L, 5L, 6L, 
                                         7L, 9L, 8L, 11L, 12L, 13L, 14L, 15L, 17L, 16L, 18L, 19L, 20L, 
                                         21L, 22L, 23L, 24L, 25L, 44L, 26L, 27L, 28L, 29L, 42L, 30L, 31L, 
                                         32L, 33L, 34L, 36L, 35L, 37L, 38L, 39L, 41L, 43L, 10L), .Label = c("Apricot", 
                                                                                                            "Avocado", "Banana", "Blackberry", "Blueberry", "Boysenberry", 
                                                                                                            "Cantaloupe", "Cherimoya", "Cherry", "Cranberry", "Date", "Elderberry", 
                                                                                                            "Fig", "Goji berry", "Gooseberry", "Grapefruit", "Green Grapes", 
                                                                                                            "Guava", "Huckleberry", "Kiwi fruit", "Kumquat", "Lemon", "Lime", 
                                                                                                            "Lychee", "Mango", "Mulberry", "Nectarine", "Olive", "Orange", 
                                                                                                            "Papaya", "Passionfruit", "Peach", "Pear", "Persimmon", "Pineapple", 
                                                                                                            "Plum", "Pomegranate", "Quince", "Raspberry", "Red Apple", "Strawberry", 
                                                                                                            "Tangerine", "Ugli fruit", "Watermelon"), class = "factor"), 
                      FruitGroup = structure(c(3L, 3L, 3L, 3L, 1L, 3L, 1L, 3L, 
                                               3L, 2L, 2L, 3L, 2L, 3L, 3L, 3L, 3L, 3L, 1L, 2L, 2L, 3L, 3L, 
                                               3L, 2L, 3L, 2L, 3L, 3L, 3L, 3L, 2L, 2L, 3L, 3L, 3L, 3L, 2L, 
                                               2L, 3L, 1L, 1L, 3L, 1L), .Label = c("aggregate", "multiple", 
                                                                                   "simple"), class = "factor"), Fruittype = structure(c(7L, 
                                                                                                                                         3L, 1L, 1L, 3L, 1L, 1L, 6L, 3L, 6L, 1L, 1L, 3L, 1L, 1L, 1L, 
                                                                                                                                         2L, 6L, 1L, 1L, 5L, 5L, 5L, 4L, 4L, 6L, 3L, 3L, 3L, 5L, 5L, 
                                                                                                                                         6L, 1L, 3L, 7L, 2L, 3L, 5L, 1L, 7L, 3L, 1L, 5L, 1L), .Label = c("berry", 
                                                                                                                                                                                                         "citrus", "drupe", "fleshy", "hesperidium", "pepos", "pome"
                                                                                                                                         ), class = "factor"), Shape = structure(c(6L, 6L, 5L, 1L, 
                                                                                                                                                                                   6L, 6L, 6L, 6L, 6L, 4L, 3L, 6L, 3L, 3L, 3L, 3L, 6L, 3L, 6L, 
                                                                                                                                                                                   3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 6L, 3L, 6L, 6L, 5L, 3L, 6L, 
                                                                                                                                                                                   5L, 3L, 6L, 3L, 6L, 5L, 2L, 2L, 5L, 6L), .Label = c("curved", 
                                                                                                                                                                                                                                       "heart", "oval", "ovate", "pear", "round"), class = "factor"), 
                      Color = structure(c(9L, 5L, 2L, 10L, 7L, 7L, 4L, 2L, 8L, 
                                          2L, 1L, 7L, 9L, 9L, 2L, 2L, 5L, 2L, 7L, 1L, 5L, 10L, 2L, 
                                          1L, 5L, 2L, 7L, 5L, 2L, 5L, 5L, 10L, 7L, 6L, 2L, 5L, 7L, 
                                          1L, 8L, 10L, 8L, 8L, 3L, 8L), .Label = c("brown", "green", 
                                                                                   "lime", "maroon", "orange", "peach", "purple", "red", "red ", 
                                                                                   "yellow"), class = "factor"), VisibleSeeds = structure(c(2L, 
                                                                                                                                            2L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 1L, 2L, 2L, 2L, 
                                                                                                                                            2L, 2L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 2L, 2L, 2L, 2L, 
                                                                                                                                            2L, 2L, 2L, 2L, 2L, 2L, 1L, 2L, 2L, 1L, 1L, 2L, 2L), .Label = c("not-visible", 
                                                                                                                                                                                                            "visible"), class = "factor"), PeelingRequired = structure(c(1L, 
                                                                                                                                                                                                                                                                         1L, 2L, 2L, 1L, 1L, 1L, 2L, 1L, 2L, 2L, 1L, 2L, 1L, 2L, 1L, 
                                                                                                                                                                                                                                                                         1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 2L, 2L, 
                                                                                                                                                                                                                                                                         2L, 2L, 1L, 1L, 2L, 1L, 2L, 2L, 1L, 1L, 1L, 2L, 1L), .Label = c("no", 
                                                                                                                                                                                                                                                                                                                                         "yes"), class = "factor"), ServingSize = structure(c(5L, 
                                                                                                                                                                                                                                                                                                                                                                                              18L, 15L, 5L, 7L, 7L, 7L, 10L, 7L, 7L, 19L, 1L, 17L, 14L, 
                                                                                                                                                                                                                                                                                                                                                                                              2L, 7L, 12L, 5L, 14L, 4L, 5L, 16L, 5L, 1L, 13L, 10L, 2L, 
                                                                                                                                                                                                                                                                                                                                                                                              5L, 14L, 5L, 6L, 7L, 1L, 5L, 5L, 5L, 17L, 9L, 5L, 5L, 8L, 
                                                                                                                                                                                                                                                                                                                                                                                              11L, 5L, 3L), .Label = c("1 cup", "1 cup ", "1 cup choped", 
                                                                                                                                                                                                                                                                                                                                                                                                                       "1 large", "1 medium", "1 medium ", "1/2 cup", "1/2 cup ", 
                                                                                                                                                                                                                                                                                                                                                                                                                       "1/2 cup chopped", "1/2 cup cubed", "1/2 cup sliced", "1/2 medium", 
                                                                                                                                                                                                                                                                                                                                                                                                                       "1/3 medium", "1/4 cup", "1/5 medium", "1medium", "2 medium", 
                                                                                                                                                                                                                                                                                                                                                                                                                       "2 meduim", "5 Dates"), class = "factor"), FruitWeight_g = c(138L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    70L, 35L, 118L, 72L, 73L, 72L, 80L, 73L, 78L, 42L, 145L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    100L, 28L, 150L, 77L, 128L, 55L, 28L, 91L, 19L, 58L, 67L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    190L, 83L, 77L, 140L, 136L, 34L, 131L, 123L, 70L, 236L, 98L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    166L, 168L, 132L, 78L, 154L, 92L, 62L, 83L, 122L, 110L), 
                      Calories = c(72, 34, 58, 105, 31, 41, 35, 27, 46, 58, 117, 
                                   106, 74, 23, 66, 53, 41, 37, 10, 56, 13, 17, 20, 125, 54, 
                                   23, 60.2, 60, 39, 62, 20, 27, 229, 38, 96, 118, 61, 35, 105, 
                                   52, 32, 27, 45, 51), Fiber_g = c(3.3, 1.4, 2.4, 3.1, 3.8, 
                                                                    1.7, 4, 0.7, 1.5, 1.8, 3.3, 10, 2.9, 2, 6, 0.7, 1.4, 3, 0, 
                                                                    3.1, 1, 1.6, 1.9, 2, 1.5, 0.3, 2, 2.3, 1.1, 3.1, 1.1, 1.3, 
                                                                    25, 1.5, 5.1, 6, 1.8, 1, 0.9, 1.7, 4, 1.7, 2, 5), VitaminA_mg = c(75, 
                                                                                                                                      1348, 51, 76, 154, 39, 0.035, 2706, 46, 0, 4, 0.48, 142, 
                                                                                                                                      1.39, 0.24, 51, 1187, 343, 0.12, 159, 0.21, 13, 34, 0, 631, 
                                                                                                                                      438, 0.019, 452, 135, 295, 1840, 766, 1.65, 319, 38, 2733, 
                                                                                                                                      455, 40, 166, 37, 20, 10, 0, 0.036), VitaminC_mg = c(6, 7, 
                                                                                                                                                                                           3, 10, 15, 7, 2.05, 30, 5, 9, 0, 52.2, 2, 5.4, 41.6, 8, 44, 
                                                                                                                                                                                           126, 0.8, 68, 8.3, 31, 20, 136, 23, 6, 51, 7, 0, 70, 20, 
                                                                                                                                                                                           43, 70.8, 7, 7, 13, 13, 13, 9, 14, 16, 49, 70, 14.6), Potassium_mg = c(148, 
                                                                                                                                                                                                                                                                  181, 175, 422, 117, 56, 91.5, 214, 161, 210, 272, 56.6, 232, 
                                                                                                                                                                                                                                                                  235, 297, 147, 178, 229, 0, 302, 35.3, 80, 68, 325, 129, 
                                                                                                                                                                                                                                                                  86, 272, 273, 3, 237, 261, 180, 821, 186, 198, 270, 207, 
                                                                                                                                                                                                                                                                  97, 399, 181, 93, 127, 0, 93.5), Folate_µg = c(4, 6, 31, 
                                                                                                                                                                                                                                                                                                                  24, 18, 4, 41.55, 17, 3, 14, 8, 8.7, 6, 0, 9, 2, 13, 27, 
                                                                                                                                                                                                                                                                                                                  0, 35, 3.2, 6, 5, 26.6, 12, 2, 8.4, 7, 0, 39, 36, 27, 33, 
                                                                                                                                                                                                                                                                                                                  4, 12, 13, 7, 9, 9, 3, 13, 20, 26, 11)), .Names = c("Name", 
                                                                                                                                                                                                                                                                                                                                                                      "FruitGroup", "Fruittype", "Shape", "Color", "VisibleSeeds", 
                                                                                                                                                                                                                                                                                                                                                                      "PeelingRequired", "ServingSize", "FruitWeight_g", "Calories", 
                                                                                                                                                                                                                                                                                                                                                                      "Fiber_g", "VitaminA_mg", "VitaminC_mg", "Potassium_mg", "Folate_µg"
                                                                                                                                                                                                                                                                                                                  ), class = "data.frame", row.names = c(NA, -44L))

# View the data
fruit

# Clean the row names and perform a little feature engineering by calculating the Fiber, Vitamin C and Potassium content per gram
cleaned_fruit <- fruit %>%
  select(Name, FruitGroup, Fruittype, Shape,
         Color, VisibleSeeds, PeelingRequired,
         Weight = FruitWeight_g, Calories, Fiber = Fiber_g,
         VitaminA = VitaminA_mg, VitaminC = VitaminC_mg,
         Potassium = Potassium_mg, Folate = Folate_µg) %>%
  mutate(Fiber = Fiber / Weight,
         VitaminC = VitaminC / Weight,
         Potassium = Potassium / Weight) %>%
  select(-Weight)

# Ensure the row names are correct
rownames(cleaned_fruit) <- cleaned_fruit$Name

# Calculate the similarity matrix based on both qualitative and quantitative characteristics
mydist <- daisy(cleaned_fruit[,-1], metric = "gower")

#Perfor Hierarchical Clustering via complete linkage
myclust <- hclust(mydist, "complete")
mydendro <- as.dendrogram(myclust)

# Plot the final result
plot(myclust)
rect.hclust(myclust, k = 9, border = "red")

# Let's make this a little prettier by using the A2R package code
source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R")
op = par(bg = "#EFEFEF")
A2Rplot(myclust, k = 9, boxes = FALSE, col.up = "gray50", 
        col.down = c("#FF6B6B", brewer.pal(8,"Set2")))


