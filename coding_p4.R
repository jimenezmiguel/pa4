library(tidyverse)
library(ggplot2)

library(tidyverse)
library(ggplot2)


fricatives <- read_csv ("data_raw/fricatives.csv")
fri <- fricatives

summary(fri)

## try again to load the data in the presentation
library(tidyverse)
library(ggplot2)


fricatives <- read_csv ("data_raw/fricatives.csv")

summary(fricatives)



#create plot with data_reception

fricatives %>%
  ggplot(., aes(x=s_cog + sh_cog, y = s_skewness + sh_skewness)) +
           geom_point()

##Make a boxplot of center of gravity 
##as a function of phoneme. 
###In another slide, plot skewness as a function of phoneme. 
##Use a statistical transformation (i.e., not a boxplot)

fricatives <- read_csv ("data_raw/fricatives.csv")

fricatives %>%
  df. <- as.data.frame(fricatives)
par(las = 1) # all axis labels horizontal
boxplot(df., main = "Ni idea", horizontal = TRUE)
   

#boxplot again
fricatives %>%
  boxplot(s_cog ~ s_skewness, data = fricatives)


#delete a column from a dataframe 
fricatives <- read_csv ("data_raw/fricatives.csv")


fri_1 <- select(fricatives, -c(obs))

fri_1 %>&=%

  
## Codigo de colega para data warngling y cambiar los campos
  
  
fricatives <- read_csv ("data_raw/fricatives.csv")

Tidy_Fricatives <- fricatives  %>%  
  pivot_longer(c("s_cog", "s_skewness", "sh_cog", "sh_skewness"), names_to = "class", values_to = "hz") %>% 
  separate("class", into = c("phoneme", "metric"), sep = "_")

#esto funciona y lo dejo
 final_tidy <- pivot_wider(Tidy_Fricatives,  names_from = metric, values_from = hz)
 
  ## Boxplot final , skewness as a function of phoneme
 boxplot(skewness ~ phoneme, data = final_tidy)

  ## Boxplot cog as a function of phoneme
 
 boxplot(cog ~ phoneme, data = final_tidy)
 
 ## plot with skenewss as a funciton of phoneme
final_tidy %>%
plot(density (skewness))

## another plot with the older dataframe
fricatives %>%
 ggplot(., aes (x = s_skewness, y= sh_skewness)) +
 geom_point() +
 geom_smooth(method = lm)
       

#transoform de data, add column with log of skewnewss

fri_log <- mutate( fri_1, s_skew_log = log(s_skewness), sh_skew_log = log(sh_skewness))


#transoform de data, add column with log10 of skewnewss

fri_log10 <- mutate( fri_1, s_skew_log10 = log10(s_skewness), sh_skew_log10 = log10(sh_skewness))

#another ggplot

fricatives %>%
  ggplot(., aes(x=s_cog + sh_cog, y = s_skewness + sh_skewness)) +
  geom_point()

         

#logaritmic transformation of skewness
 #DID NOT WORK

library(dplyr)
fri_1 %>% mutate_each(funs(round(.,3)), B126_log:H251_log)

#ANOTHER TRY AT BOXPLOT
fri_1 %>%
boxplot(s_skewness ~ s_cog, data = fri_1,
        varwidth = TRUE, log10 = "y", las = 1)

# Add a title
title("s_cog as a funciton of s_skewness")



## Saving my tidy data
write.csv (final_tidy, "data_tidy/data_tidy.csv")



##Fit a model that examines center of gravity as a function of skewness for the [s] segments (hint: you will have to transform the data). Make a table of the model summary.

fricatives <- read_csv ("data_raw/fricatives.csv")

mod <- lm (formula = s_cog ~ s_skewness, data = fricatives)
summary(mod)


## make a scatterplot of the model
##Make a scatter plot that illustrates the relationship in (8).


attach(mod)
plot(mod)
