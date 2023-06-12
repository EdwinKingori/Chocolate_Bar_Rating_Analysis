#Loading packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(selectr)
library(readr)
library(skimr)

#importing file
flavors_df <- read_csv("flavors_of_cacao.csv")
View(flavors_df)
colnames(flavors_df)
glimpse(flavors_df)

#Data_Cleaning
#Cleaning Column names
flavors_df <- janitor::clean_names(flavors_df)
colnames(flavors_df)

#Summarizing Data
#using the n-distinct() to check the number of unique IDs 
n_distinct(flavors_df$company_maker_if_known)

#Replacing first column with Brand
flavors_df %>%
  rename(Brand = `company_maker_if_known`)

trimmed_flavors_df <- flavors_df %>%
  select(rating, `cocoa_percent`, `company_location`)
head(trimmed_flavors_df)

#Finding the mean value of Rating
trimmed_flavors_df <- flavors_df %>%
  summarize(mean(rating))
View(trimmed_flavors_df)

#Filtering highly rated chocolate bars
best_flavors <- flavors_df %>%
  filter(Rating >= 3.9, `Cocoa
Percent` >= 75)
View(best_flavors)

#Plotting
#Revealing the locations that produce the highest rated chocolate bars
ggplot(data = flavors_df) +
  geom_bar(mapping = aes(x = `company_location`, fill= rating))+
  labs


ggplot(data = trimmed_flavors_df) +
  geom_point(mapping = aes(x = `Cocoa\nPercent`, y = Rating)) +
  labs(title = "trimmed_flavors")