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

#Using summary to get comprehensive data on rating
flavors_df%>%
  select(rating)%>%
  summary()

#Finding the mean value of Rating
trimmed_flavors_df <- flavors_df %>%
  summarize(mean(rating))
View(trimmed_flavors_df)

#Replacing first column with Brand
flavors_df %>%
  rename(Brand = `company_maker_if_known`)

trimmed_flavors_df <- flavors_df %>%
  select(rating, `cocoa_percent`, `company_location`, bean_type)
View(trimmed_flavors_df)



#Filtering highly rated chocolate bars from the trimmed data frame
best_flavors <- trimmed_flavors_df %>%
  filter(rating >= 3.9, `cocoa_percent` >= 75,!is.na(bean_type)& bean_type != "")
View(best_flavors)

#Visualizations
#Exploring the produce of chocolate bars per location
ggplot(data = flavors_df) +
  geom_bar(mapping = aes(x = `company_location`, fill= rating))+
  labs(title="Exploring the production of chocolate bars")+
  theme(plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(color = 'black',  fill = NA),
        axis.text.x = element_text(angle =90, hjust = 1))

#Exploring the ratings per year
#Insights: 
    #The geom_smooth() function is added with method = "lm" to include a trend line. This line represents the overall trend in ratings over the years. Adjust the method and parameters as needed to fit the desired model.

    #The x-axis label is set to "Review Date" and the y-axis label is set to "Rating" using the labs() function.

    #The scale_x_continuous() function is used to set the x-axis breaks to a sequence of years. Adjust the by parameter as needed to control the spacing of the breaks.

    #The geom_text() function is added to display annotations for the average rating per year. It calculates the mean rating for each year using the aggregate() function and displays the rounded rating value on the plot. Adjust the positioning (vjust) and formatting (round()) of the annotations as desired.

ggplot(data = flavors_df, aes(x=review_date, y = rating, color = cocoa_percent))+
  geom_point()+
  geom_jitter()+
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add a trend line
  labs(title = "Ratings per year", x = "Review Date", y = "Rating") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(color = 'black', fill = NA)) +
  scale_x_continuous(breaks = seq(min(flavors_df$review_date), max(flavors_df$review_date), by = 1)) +  # Set x-axis breaks
  geom_text(data = aggregate(rating ~ review_date, flavors_df, FUN = mean),
            aes(label = round(rating, 2)), vjust = -1)  # Add average rating annotations

ggplot(data = flavors_df, aes(x=review_date, y = rating, color = cocoa_percent))+
  geom_point()+
  geom_jitter()+
  labs(title = "Ratings per year", x = "Review Date", y = "Rating") +
  theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(color = 'black', fill = NA))

#Exploring the cocoa_percentage and rating 
ggplot(data = flavors_df, aes(x = cocoa_percent, y = rating, color = company_location))+
  geom_point()+
  labs(title = "Cocoa_Percent vs Rating", x = "Cocoa_Percent", y = "Rating")+
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(color = 'black', fill = NA),
        axis.text.x = element_text(angle = 60, hjust =1)) 