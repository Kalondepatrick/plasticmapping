#-----------------        Loading libraries      -------------------------------#

library(readr)
library(tidyverse)
library(lattice)
library(dplyr)
require(Hmisc)
library(MASS)

#-----------------        Helper function 1      -------------------------------#

DataCleaning <- function(data, dir = "") {
  #set's working directory to dir 
  if(!(dir == "")){setwd(dir)}
  raw <- read_csv(data) 
  
  #----- Remove unnecessary columns i.e. time stamp, examples and retain the remainder
  data = raw[,5:18]
  
  #----- Rename column names
  
  data = data %>% 
    rename(question1 = '(1) Given a task to draw outlines around objects that are present in the picture below, would you be able to differentiate different objects and draw outlines?') %>% 
    rename(confidence1 = 'From the image above (image 1), how confident are you that you can draw outlines around different objects ?') %>%
    
    rename(question2 = '(2) Given a task to draw outlines around objects that are present in the picture below, would you be able to differentiate different objects and draw outlines?') %>%
    rename(confidence2 = 'From the image above (image 2), how confident are you that you can draw outlines around different objects ?') %>%
    
    rename(question3 = '(3) Given a task to draw outlines around objects that are present in the picture below, how confident are you that you can be able to differentiate different objects and draw outlines?') %>%
    rename(confidence3 = 'From the image above (image 3), how confident are you that you can draw outlines around different objects ?') %>%
    
    rename(question4 = '(4) Given a task to draw outlines around objects that are present in the picture below, how confident are you that you can be able to differentiate different objects and draw outlines?') %>%
    rename(confidence4 = 'From the image above (image 4), how confident are you that you can draw outlines around different objects ?') %>%
    
    rename(question5 = '(5) Given a task to draw outlines around objects that are present in the picture below, how confident are you that you can be able to differentiate different objects and draw outlines?') %>%
    rename(confidence5 = 'From the image above (image 5), how confident are you that you can draw outlines around different objects ?') %>%
    
    rename(question6 = '(6) Given a task to draw outlines around objects that are present in the picture below, how confident are you that you can be able to differentiate different objects and draw outlines?') %>%
    rename(confidence6 = 'From the image above (image 6), how confident are you that you can draw outlines around different objects ?') %>%
    
    rename(question7 = '(7) Given a task to draw outlines around objects that are present in the picture below, how confident are you that you can be able to differentiate different objects and draw outlines?') %>%
    rename(confidence7 = 'From the image above (image 7), how confident are you that you can draw outlines around different objects ?') 
  
  #----- Restructure the data from wide format to long
  
  # First create an ID field
  
  data = tibble::rowid_to_column(data, "ID")
  
  #First separate the responses to each question
  
  question1 = data[,c("ID","question1", "confidence1" )]
  
  question2 = data[,c("ID","question2", "confidence2" )]
  question3 = data[,c("ID","question3", "confidence3" )]
  question4 = data[,c("ID","question4", "confidence4" )]
  question5 = data[,c("ID","question5", "confidence5" )]
  question6 = data[,c("ID","question6", "confidence6" )]
  question7 = data[,c("ID","question7", "confidence7" )]
  
  #Add a column for question 
  
  question1$question = 1
  question2$question = 2
  question3$question = 3
  question4$question = 4
  question5$question = 5
  question6$question = 6
  question7$question = 7
  
  #------- Rename 
  question1 = question1 %>% 
    rename(visibility = 'question1') %>% 
    rename(confidence = 'confidence1') 
  
  question2 = question2 %>% 
    rename(visibility = 'question2') %>% 
    rename(confidence = 'confidence2') 
  
  question3 = question3 %>% 
    rename(visibility = 'question3') %>% 
    rename(confidence = 'confidence3') 
  
  question4 = question4 %>% 
    rename(visibility = 'question4') %>% 
    rename(confidence = 'confidence4') 
  
  question5 = question5 %>% 
    rename(visibility = 'question5') %>% 
    rename(confidence = 'confidence5') 
  
  question6 = question6 %>% 
    rename(visibility = 'question6') %>% 
    rename(confidence = 'confidence6') 
  
  question7 = question7 %>% 
    rename(visibility = 'question7') %>% 
    rename(confidence = 'confidence7') 
  
  final = rbind(question1,question2, question3, question4, question5, question6, question7)
  
  #----- Add other data properties or values
  #Populate heights
  final$height[final$question == 1] <- 40
  final$height[final$question == 2] <- 50
  final$height[final$question == 3] <- 70
  final$height[final$question == 4] <- 60
  final$height[final$question == 5] <- 20
  final$height[final$question == 6] <- 30
  final$height[final$question == 7] <- 10
  
  # Add Ground Sampling distance (GSD)
  
  final$GSD[final$height == 10] <- 0.25
  final$GSD[final$height == 20] <- 0.55
  final$GSD[final$height == 30] <- 0.82
  final$GSD[final$height == 40] <- 1.09
  final$GSD[final$height == 50] <- 1.36
  final$GSD[final$height == 60] <- 1.64
  final$GSD[final$height == 70] <- 1.91
  
  # Handling question 6
  final = final[!(final$question == 6),]
  
  # Remove rows with NA's
  final = na.omit(final)
  
  # The GSD's are from Pix4D Captyre - the software that was used for mapping.
  # I will replace these values with the actual values that were recorded after image processing in Pix4D (I have the files on my other computer)
  
  
  return(final)
}

#--------------- Application of data cleaning procedure  ---------------------#
#Please change the working directory accordingly
cleaned = DataCleaning("inputs/responses.csv", "~/Documents/GISFolder/Projects/StCloud/mappingplastics")
str(b)

#--------------- Summaries  ---------------------#


#-----------------        Helper function 2      -------------------------------#

summaries.plots <- function(cleaned) {
  
  # Create a function for calculation of mode (for categorical data - confidence levels)
  find_mode <- function(x) {
    u <- unique(x)
    tab <- tabulate(match(x, u))
    u[tab == max(tab)]
  }
  
  plotting_data = cleaned %>%
    group_by(height, GSD) %>%
    summarize(
      prop = sum(visibility=="Yes"),
      mode_conf = find_mode(confidence),
      mean_conf = mean(as.numeric(confidence)),
      sd_conf = sd(as.numeric(confidence))
    )
  return(plotting_data)
}

plotting_data = summaries.plots(cleaned)

#--------------- Plots  ---------------------#

#---- Plot for visibility
visibility = plotting_data %>%
  ggplot(aes(x = as.numeric(height), y = prop)) +
  geom_point() +
  geom_line()+
  xlab("Height") + ylab("Visual observers who could see plastics")

#---- Plot for confidence
confidence = plotting_data %>%
  ggplot(aes(x = as.numeric(height), y = mean_conf)) +
  geom_point() +
  geom_line()+
  xlab("Height") + ylab("Confidence")

visibility
confidence

#---------------        Modelling      -------------------------#

#univariate binomial regression model (for visibility) 

glm.fit <- glm(confidence ~ height, data = cleaned, family = binomial)
summary(glm.fit)
summary.table1 <- coef(summary(glm.fit))
summary.table1

# Ordinal regression model (for confidence)
cleaned$confidence = factor(cleaned$confidence, ordered=TRUE,
                         levels = c(1,2,3,4,5))

model2.conf <- polr(confidence ~ height, data = cleaned, Hess=TRUE)
summary(model2.conf)  
summary_table <- coef(summary(model2.conf))
pval <- pnorm(abs(summary_table[, "t value"]),lower.tail = FALSE)* 2
summary_table <- cbind(summary_table, "p value" = round(pval,3))
summary_table
ci <- confint(model2.conf)
exp(cbind(OR = coef(model2.conf), ci))

#Interpletation ???????

# Relationship between visibility and confidence

### Other standing questions
# (Question 1) Handling question 6 (30 meters flight height)
#As you can see on line 122, I just ommited the values

# (Question 2)Scaling confidence from 1-5 to a different range i.e. 0 - 100 % 
#### In the graph, should we include a measure of variability i.e. the standard deviation ???


#Visual observers expressed confidence in deliniating only in images where plastics were visible