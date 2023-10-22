
#Library where the data set is located. Within this library the penguins_raw data will be used
library(palmerpenguins) 

#dplyr library allows for SQL-like commands to be used to filter data
library(dplyr)

#rpart is the library used in the development of the classifcation trees
library(rpart)
library(rpart.plot)

#ggplot2 is the library used in the plotting of graphs.
library(ggplot2)


# This first section is to remove the variables that will not be used in this analysis. 

penguin_cleanup <- select(penguins_raw, -'Individual ID', -'Date Egg', 
                          -studyName, -'Delta 15 N (o/oo)',-'Delta 13 C (o/oo)', 
                          -Comments, -'Clutch Completion', -Stage, )


#This section is the renaming of the variables in the code to make the later processing easier. 

penguin_rename <- rename (penguin_cleanup, Culmen_Depth = 'Culmen Depth (mm)',
                            Culmen_Length = 'Culmen Length (mm)',
                            Body_Mass = 'Body Mass (g)', Flipper_Length = 'Flipper Length (mm)')

#This is to remove the instances of N/A values in the data set

penguin_final <- na.omit(penguin_rename)


#This section uses the mutate feature to change the values of Sex, Species and Island.
#Sex - 1 = Male, 2 = Female
#Species - 1 = Adelie, 2 = Gentoo, 3 = Chinstrap
#Island - 1 = Biscoe, 2 = Dream, 3 = Torgensen

penguin_mutate <- penguin_final %>%
mutate(Sex=replace(Sex, Sex== 'MALE', 1),
       Sex=replace(Sex, Sex== 'FEMALE', 2),
       Species=replace(Species, Species== 'Adelie Penguin (Pygoscelis adeliae)', 1), 
       Species=replace(Species, Species== 'Gentoo penguin (Pygoscelis papua)', 2),
       Species=replace(Species, Species=='Chinstrap penguin (Pygoscelis antarctica)', 3),
       Island=replace(Island, Island=='Biscoe', 1),
       Island=replace(Island, Island=='Dream', 2), 
       Island=replace(Island, Island=='Torgersen', 3)
       %>% as.data.frame())
        

#Changes the data from characters to integars, this will be useful later in in the development.

penguin_mutate$Sex <- as.numeric(as.character(penguin_mutate$Sex))
penguin_mutate$Species <- as.numeric(as.character(penguin_mutate$Species))



#This sections creates training and test data for my machine learning algorithm to be developed and tested
#Here the data has been split 80% training and 20% testing. 

penguins_samples <- floor(0.8 * nrow(penguin_mutate))

set.seed(123)
penguin_train <- sample(seq_len(nrow(penguin_mutate)), size = penguins_samples)

training <- penguin_mutate[penguin_train, ]
testing <- penguin_mutate[-penguin_train, ]

head(penguin_mutate)        
       

#This is the function that takes 3 variables as a parameter and allows me to place
#this into the rpart building of a classification tree. 

penguin_variables <- function(variable1, variable2, variable3) {
  variable1 + variable2 + variable3
}


#Development of rpart classification trees.
#Multiple trees have been developed to analys the results.

class1 <- rpart(Species~Body_Mass+Culmen_Depth+Flipper_Length, data = training, method = 'class')
rpart.plot(class1)

class2 <- rpart(Species~Body_Mass+Sex+Flipper_Length, data = training, method = 'class')
rpart.plot(class2)

class3 <- rpart(Island$.~Culmen_Length+Culmen_Depth+Flipper_Length, data = training, method = 'class')
rpart.plot(class3)

class4 <- rpart(Species~penguin_variables(Culmen_Length, Culmen_Depth, Flipper_Length), data = training, method = 'class')
rpart.plot(class4)

class5 <- rpart(Species~penguin_variables(Body_Mass, Culmen_Depth, Flipper_Length), data = training, method = 'class')
rpart.plot(class5)



#Prediction of the test data
predict_test_data <-predict(class3, testing, type = 'class')

prediction <- table(testing$Island$., predict_test_data)

plot(prediction)

#Accuracy Test from https://www.guru99.com/r-decision-trees.html

accuracy_Test <- sum(diag(prediction)) / sum(prediction)
print(accuracy_Test)




#Scatter graphs of the different penguins based on island and species. Multiple graphs have been created to analyse results.  

qplot(Culmen_Length, Body_Mass, data=training, colour = Species, size=I(3))

qplot(Culmen_Length, Culmen_Depth, data=training, colour=Species, size=I(3))

qplot(Flipper_Length, Culmen_Length, data=training, colour=Species, size=I(3))

qplot(Body_Mass, Flipper_Length, data=training, colour=Island$., size=I(3))



