
library(tigerstats)
library(knitr)
library(tidyverse)
library(tidymodels)


#install.packages("corrplot") 
#library(randomForest)
#if (!require("libraryname")) install.packages("libraryname")

# Web Scraping
install.packages("data.table")
library(data.table)
library(XML)
pages<-c(1:15)
urls <- rbindlist(lapply(pages,function(x){
  url <- paste ("https://www.kaggle.com/yasserh/bike-sharing-dataset?select=day.csv")
  data.frame(url)
}),fill=TRUE)

joblocations <- rbindlist (apply(urls , 1, function(url) {
  doc <- htmlParse(url)
  locations <- getNodeSet(doc,'//*[@id="site-content"]/div[3]/div[2]/div[3]/div[2]/div/div[2]/div/div[3]/div[7]/span[22]')
  data.frame (sapply(locations ,function(x) { xmlValue(x)}))
}),fill=TRUE)


# data-import
library(readxl)
Bik <- read_excel("C:/Users/bader/Desktop/Bik/Bik.xlsx")
View(Bik)

# Data Analysis 
str(Bik)

# data-wrangling and cleaning
#colnames(Bik)
#  [1] "instant"    "dteday"     "season"     "yr"         "mnth"       "holiday"   
#  [7] "weekday"    "workingday" "weathersit" "temp"       "atemp"      "hum"       
# [13] "windspeed"  "casual"     "registered" "cnt"  

Bik_v2 <- Bik %>%
  dplyr::select(-instant, 
         -dteday, 
         -yr, 
         -weathersit, 
         -atemp,
         -casual, 
         -registered)

  # mutate(sunday = ifelse(weekday == 0, 1, 0))

Bik_v2$season <- as.factor(Bik_v2$season)
Bik_v2$mnth <- as.factor(Bik_v2$mnth)
Bik_v2$holiday <- as.factor(Bik_v2$holiday)
Bik_v2$weekday <- as.factor(Bik_v2$weekday)
Bik_v2$workingday <- as.factor(Bik_v2$workingday)

#automatic download of the wrangled dataset
write_csv(Bik_v2, "data_wrangled_kaggle_bikes_dataset.csv")


#Plot for  Weekdays

ggplot(Bik_v2, 
       aes(x = as.factor(weekday), 
           y = cnt,
           color = as.factor(weekday))) +
  geom_point() +
  geom_boxplot() + 
  labs(
    title = "Number of Rentals by Weekday",
    x = "Day of the Week",
    y = "Number of Rentals",
    color = "Day of the Week",
    caption = "Based on BoomBikes 2018 Kaggle data https://www.kaggle.com/yasserh/bike-sharing-dataset"
  ) + #optional change to the title, x, y, legend, and captions in the plot
  theme_classic() + 
  scale_color_discrete(name = "Day of the Week", 
                       labels = c("Sunday", "Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")) + 
  scale_color_brewer(palette = "Set1") + #optional change to the color palette where you state the name of the color palette that you want to use 
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())


#GGPlot for Season

ggplot(Bik_v2, aes(x = season, y = cnt, group = season)) + geom_boxplot() 


#ggPlot for Month

ggplot(Bik_v2, aes(x = mnth, y = cnt, group = mnth)) + geom_boxplot()


#GGPlot for Holiday

ggplot(Bik_v2, aes(x = holiday, y = cnt, group = holiday)) + geom_boxplot()


#Plot for Week day

ggplot(Bik_v2, aes(x = weekday, y = cnt, group = weekday)) + geom_boxplot()

#ggPlot for Workingday

ggplot(Bik_v2, aes(x = workingday, y = cnt, group = workingday)) + geom_boxplot()


#Plot for Hum (Humidity)

ggplot(Bik_v2, aes(x = hum, y = cnt)) + 
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "black")

#Plot for Wrind Speed

ggplot(Bik_v2, aes(x = windspeed, y = cnt)) + 
  geom_point(color = "purple") +
  geom_smooth(method = "lm", se = FALSE, color = "black")


#Plot for Temp (Temperature)

ggplot(Bik_v2, aes(x = temp, y = cnt)) + 
  geom_point(color = "darkgreen") + 
  geom_smooth(method = "lm", se = FALSE, color = "black")

#cor_matrix <- cor(Bik_v2)
#corrplot(cor_matrix, order = "hclust", tl.col = "black", tl.srt = 45, addCoef.col = 'black', method = "square")


#begin with splitting the dataset into training and test datasets using initial_split() 
set.seed(28)
bi_split <- initial_split(Bik_v2, 
                          prop = 0.75, #The proportion of data to be retained for modeling/analysis
                          strata = cnt) #Numeric strata are binned into quartiles

bi_split

bi_train <- training(bi_split)
bi_train

bi_test <- testing(bi_split)
bi_test
                          

# Feature Engineering

bi_rec <- recipe(cnt ~ temp + hum + windspeed, data = bi_train) %>%
  step_corr(temp, hum, windspeed, threshold = 0.9) %>% #see correlation plot above
  #step_dummy(season, mnth, holiday, weekday, workingday) %>% #dummy encoding on categorical variables
  prep() #estimate the required parameters to be applied to other datasets

bi_test <- bi_rec %>%
  bake(testing(bi_split)) #apply trained preprocessing recipe

bi_train <- juice(bi_rec) #extract transformed training set

bi_rec

 # summary

summary(bi_rec)
 
# Test set
bi_test
# Train set
bi_train


# Model Training
#initialize the model
lm_model <- linear_reg() %>% #functionality of algorithm is imported here, parameters might be specified here 
  set_engine('lm') %>%  #algorithm itself, links to package that the algorithms
  set_mode('regression') #mode ex. classification, regression, xgboost, etc. 


lm_fit_temp1 <- lm_model %>% 
  fit(cnt ~ temp,
      data = bi_train)


lm_fit_temp2 <- lm_model %>%
  fit(cnt ~ temp + windspeed, #plus sign for another variable, : sign for interactions 
      data = bi_train)


lm_fit_temp3 <- lm_model %>%
  fit(cnt ~ temp + windspeed + hum, #plus sign for another variable, : sign for interactions 
      data = bi_train)

glance(lm_fit_temp1)
glance(lm_fit_temp2)
glance(lm_fit_temp3)


#this is an alternative to using tidymodels
lm(cnt ~ temp, data = bi_train)
summary(lm(cnt ~ temp, data = bi_train))

tidy(lm_fit_temp1)
tidy(lm_fit_temp2)
tidy(lm_fit_temp3)





# Model Testing

bi_pred <- predict(lm_fit_temp1, bi_test) #creates a numerical vector
bi_pred2 <- predict(lm_fit_temp2, bi_test) 
bi_pred3 <- predict(lm_fit_temp3, bi_test) 

results <- bi_test %>%
  bind_cols(bi_pred) %>% #can not use mutate with a vector
  mutate(residuals = cnt - .pred) #can also use residuals() on an lm() object

results2 <- bi_test %>%
  bind_cols(bi_pred2) %>% #can not use mutate with a vector
  mutate(residuals = cnt - .pred) #can also use residuals() on an lm() object

results3 <- bi_test %>%
  bind_cols(bi_pred3) %>% #can not use mutate with a vector
  mutate(residuals = cnt - .pred) #can also use residuals() on an lm() object

head(results2)


#Model Evaluation using Residuals metric

p <- ggplot(results3, aes(x = temp, y = cnt)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # regression line 
  geom_segment(aes(xend = temp, yend = .pred), alpha = .2) + #creates gray lines
  geom_point(aes(color = abs(residuals), size = abs(residuals))) +  # size of the points
  scale_color_continuous(low = "green", high = "red") +             # colour of the points mapped to residual size - green smaller, red larger
  guides(color = FALSE, size = FALSE) +     # Size legend removed
  geom_point(aes(y = .pred), shape = 1) + #added points with clear center 
  theme_bw() +
  labs(
    x = "Temperature", 
    y = "Number of Rentals",
    title = "Residuals Plot",
  )

p

ggplot(results, aes(x = residuals)) + geom_histogram() #check distribution of residuals, should be normally distributed


# Model Evaluation using RMSE

results %>% rmse(truth = cnt, estimate = .pred)

#calculate by hand using rmse <- sqrt(mean(residuals^2)), residuals = predicted value - actual value

bi_rmse <- data.frame(model = c("temp only", "temp + windspeed", "temp + windspeed + hum"), 
                      rmse = c(1513, 1482, 1412)) 

ggplot(bi_rmse, aes(x = model, y = rmse, color = model)) + 
  geom_point(size=3) + 
  geom_segment(aes(x=model, 
                   xend=model, 
                   y=0, 
                   yend=rmse)) +
  coord_flip() + 
  scale_color_brewer(palette = "Dark2") +
  labs(color = "Model",
       y = "RMSE",
       x = "Model Evaluated",
       title = "Root Mean Square Error (RMSE) Plot")


# Model Evaluation using R-squared  
results %>% rsq(truth = cnt, estimate = .pred) #rsquared - temperature explains 38% of the variation within the data, used adjusted for multiple variables

ggplot(results, aes(x = cnt, y = .pred)) + 
  geom_point() +
  geom_abline(color = 'blue', linetype = 2) + #reference line
  labs(title = 'R-Squared Plot',
       y = 'Predicted Counts', 
       x = 'Actual Counts')

ggplot(results3, aes(x = cnt, y = .pred)) + 
  geom_point() +
  geom_abline(color = 'blue', linetype = 2) + #reference line
  labs(title = 'R-Squared Plot',
       y = 'Predicted Counts', 
       x = 'Actual Counts')

