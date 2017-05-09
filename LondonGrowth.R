#CPLN 675 - Final Project - Estimating land development predictive model

#In this run through, the goal is to take both land cover and other open data to forecast
#new development in 2011. We will then simulate some new population for 2020 and use our 
#model to forecast for 2020.

#We model the spatial lag of development and a particular
#feature of the built environment. In this case, a proposed train station.

#We will focus on two new feature engineering techniques - spatial lag and variable interactions.


library(ggplot2)
library(AppliedPredictiveModeling)
library(randomForest)
library(popbio)
library(caret)
library(pscl)
library(ROCR)
library(ggmap)
library(pROC)
library(reshape2)
library(grid)
library(gridExtra)
library(RColorBrewer)
library(ggthemes)


#turn off scientific notations
options(scipen=999)

#set you working directory
setwd("~/Documents/UPenn/UPENN MCP/Semester 4/Land Modelling/London/")
ldn <- read.csv("london_3.csv")
#ldn <- subset(london, select = c())
names(ldn)
str()
#table(ldn$id_ward)

summary(ldn$popde)
# Cleaning some data
summary(ldn$avgneighpop11)
ldn$avgneighpop01 <- ifelse(ldn$avgneighpop01 < 0, 0, ldn$avgneighpop01)
ldn$avgneighpop11 <- ifelse(ldn$avgneighpop11 < 0, 0, ldn$avgneighpop11)

# New Variable - Developable areas in 2001 - where pop density is below the 3rd Quantile of Pop density
summary(ldn$popdens01)
summary(ldn$popdens11)
ldn$devds01 <- ifelse(ldn$popdens01 > 61, 1, 0 )
ldn$devds11 <- ifelse(ldn$popdens11 > 68, 1, 0 )
ldn$popdens_0111 <- (ldn$popdens11 - ldn$popdens01)
ldn$popdens_1120 <- (ldn$pop2018/ldn$Hectares) - ldn$popdens11

#Change in Pop
ldn$pop_0111 <- ldn$pop2011 - ldn$pop2001
ldn$pop_1118 <- ldn$pop2018 - ldn$pop2011


# New Variable - Housing density change
ldn$houseDen_01 <- ldn$house2001 / ldn$Hectares
ldn$houseDen_11 <- ldn$house2011 / ldn$Hectares


#Using Ward names instead of ward id's to colour for better visualization 
# more distinct/diverging colour distribution
ggplot(ldn, aes(X,Y)) + geom_point(aes(colour=Ward))  +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.background = element_rect(fill = NA),
        legend.position = "none",
        aspect.ratio = 1) 


#how about the spatial lag of development
ggplot(ldn, aes(X,Y)) + geom_point(aes(colour= avgneighpop01)) +
        theme(axis.text = element_blank(),
              axis.ticks = element_blank(),
              axis.title = element_blank(),
              legend.direction = "horizontal",
              legend.position = c(0.7, 0.9),
              legend.background = element_blank(),
              panel.background = element_rect(fill = NA),
              aspect.ratio = 1) +
              scale_color_distiller(type = 'seq')

#Lets see the change in density
ggplot(london, aes(X,Y)) + geom_point(aes(colour= (pop2018 - pop2011))) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.direction = "horizontal",
        legend.position = c(0.7, 0.9),
        legend.background = element_blank(),
        panel.background = element_rect(fill = NA),
        aspect.ratio = 1) +
        scale_color_distiller(type = 'seq')


#Spatial change in pop density relative to their median for the year per ward
summary(ldn$devds01)
ggplot(ldn, aes(X,Y)) + geom_point(aes(colour= as.factor(devds11 - devds01))) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.direction = "horizontal",
        legend.position = c(0.7, 0.9),
        legend.background = element_blank(),
        panel.background = element_rect(fill = NA),
        aspect.ratio = 1) 




#Spatial change in pop density across 2001-11
summary(ldn$popdens_0111)
ggplot(ldn, aes(X,Y)) + geom_point(aes(colour= popdens_0111)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.direction = "horizontal",
        legend.position = c(0.7, 0.9),
        legend.background = element_blank(),
        panel.background = element_rect(fill = NA),
        aspect.ratio = 1) +
        scale_color_distiller(type = 'seq')


#Mapping Euclidean distance from new development proposed
ggplot(ldn, aes(X,Y)) + geom_point(aes(colour= as.factor(transit))) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.direction = "horizontal",
        legend.position = c(0.8,0.92),
        legend.title = element_blank(),
        legend.background = element_blank(),
        panel.background = element_rect(fill = NA),
        aspect.ratio = 1)  +
        scale_color_brewer(type = 'div', palette = "Set2")


# Check for developed land 
ggplot(ldn, aes(X,Y)) + geom_point(aes(colour= as.factor(devnew12) )) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.direction = "vertical",
        legend.title = element_text("Development 2011"),
        legend.position = c(0.8,0.92),
        legend.background = element_blank(),
        panel.background = element_rect(fill = NA),
        aspect.ratio = 1) 

# New Variable - Areas abv and below avg pop density
# Lower density may denote scope for more development
ggplot(ldn, aes(X,Y)) + geom_point(aes(colour= as.factor(d))) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.direction = "vertical",
        legend.position = c(0.8,0.92),
        legend.background = element_blank(),
        panel.background = element_rect(fill = NA),
        aspect.ratio = 1) +
        scale_color_brewer(name= "Scope for Densification 2011", 
                     palette = "Set3", 
                     labels= c("Abv Avg Density", "Below Avg Density"))


#developmed areas 2001
ggplot(ldn, aes(X,Y)) + geom_point(aes(colour= as.factor(devnew00))) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.direction = "vertical",
        legend.position = c(0.8,0.92),
        legend.background = element_blank(),
        panel.background = element_rect(fill = NA),
        aspect.ratio = 1) + 
        scale_color_brewer(name= "Developed Land 2001", palette= "Set2")

#developmed areas 2011
ggplot(ldn, aes(X,Y)) + geom_point(aes(colour= as.factor(devnew12))) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.direction = "vertical",
        legend.position = c(0.1,0.2),
        legend.background = element_blank(),
        panel.background = element_rect(fill = NA),
        aspect.ratio = 1) + 
        scale_color_brewer(name= "Developed Land 2011", palette= "Set2")

#New development 2001-11 by subtracting the two
ggplot(ldn, aes(X,Y)) + geom_point(aes(colour= as.factor(devnew12- devnew00))) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.direction = "vertical",
        legend.position = c(0.8,0.92),
        legend.background = element_blank(),
        panel.background = element_rect(fill = NA),
        aspect.ratio = 1) + 
        scale_color_brewer(name= "Developed Land 2001-11")

#New development areas 2001-11
ggplot(ldn, aes(X,Y)) + geom_point(aes(colour= as.factor(ch0012r))) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.direction = "vertical",
        legend.position = c(0.8,0.92),
        legend.background = element_blank(),
        panel.background = element_rect(fill = NA),
        aspect.ratio = 1) +
        scale_color_brewer(name= "New Development 2001-11", 
                           palette = "Set3", 
                           labels= c("No Change", "New Development"))


#New development Proposed
ggplot(ldn, aes(X,Y)) + geom_point(aes(colour= as.factor(ldn$bromley_TO))) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.direction = "vertical",
        legend.position = c(0.8,0.92),
        legend.background = element_blank(),
        panel.background = element_rect(fill = NA),
        aspect.ratio = 1) +
        scale_color_brewer(name= "New Rail line & Development", 
                     palette = "Set3", 
                     labels= c("No Change", "New Development"))


#Race - White
ggplot(ldn, aes(X,Y)) + geom_point(aes(colour= house2001)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        legend.background = element_blank(),
        panel.background = element_rect(fill = NA),
        aspect.ratio = 1) +
        scale_color_continuous_tableau()

#Distance to tube
ggplot(ldn, aes(X,Y)) + geom_point(aes(colour= ldn$disttube)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        legend.background = element_blank(),
        panel.background = element_rect(fill = NA),
        aspect.ratio = 1) +
        scale_color_continuous_tableau()


# into Factors
ldn$watr00 <- as.factor(ldn$watr00)
ldn$watr12 <- as.factor(ldn$watr12)
ldn$green00 <- as.factor(ldn$green00)   
ldn$green12 <- as.factor(ldn$green12)
ldn$devnew00 <- as.factor(ldn$devnew00)
ldn$devnew12 <- as.factor(ldn$devnew12)
ldn$ch0012r <- as.factor(ldn$ch0012r)  
ldn$bromley_TO <- as.factor(ldn$bromley_TO)
ldn$devds11 <- as.factor(ldn$devds11)

# Count of Cells that developed (without changing to numeric)
nrow(subset(ldn, ldn$ch0012r == 1))

# As a percent
nrow(subset(ldn, ldn$ch0012r == 1)) * 100 / nrow(ldn)

# Groups of Variables
ldnVariables_Pop <- ldn[,c(15,18,20,27)]
ldnVariables_PopDensity <- ldn[,c(15,30,31)]
ldnVariables_Housing <- ldn[,c(15,54,55)]
ldnVariables_Race_noOther <- ldn[,c(15,43,44,45)]
ldnVariables_Race_Other <- ldn[,c(15,46)]

#melt and plot to analyze
meltPop <- melt(ldnVariables_Pop, id= 'ch0012r')
meltPopDensity <- melt(ldnVariables_PopDensity, id= 'ch0012r')
meltPopHousing <- melt(ldnVariables_Housing, id= 'ch0012r')
meltRace_noOther <- melt(ldnVariables_Race_noOther, id= 'ch0012r')
meltRace_Other <- melt(ldnVariables_Race_Other, id= 'ch0012r')


#Plots
ggplot(meltPop, aes(as.factor(ch0012r), value)) + 
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean", colour="orange", fill="pink") +
  facet_wrap(~variable) + theme_fivethirtyeight() + theme(legend.position = "none")

  
ggplot(meltPopDensity, aes(as.factor(ch0012r), value)) + 
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean", colour="orange", fill="pink") +
  facet_wrap(~variable) + theme_fivethirtyeight() + theme(legend.position = "none")
  
ggplot(meltPopHousing, aes(as.factor(ch0012r), value)) + 
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean", colour="orange", fill="pink") +
  facet_wrap(~variable) + theme_fivethirtyeight() + theme(legend.position = "none")

ggplot(meltRace_noOther, aes(as.factor(ch0012r), value)) + 
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean", colour="orange", fill="pink") +
  facet_wrap(~variable) + theme_fivethirtyeight() + theme(legend.position = "none")

ggplot(meltRace_Other, aes(as.factor(ch0012r), value)) + 
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean", colour="orange", fill="pink") +
  facet_wrap(~variable) + theme_fivethirtyeight() + theme(legend.position = "none")


#building the  models

#create training and tests sets using half of the data
nrow(ldn)
set.seed(7900)
trainIndex <- createDataPartition(ldn$watr00, p = .5,
                                  list = FALSE,
                                  times = 1)
datTrain <- ldn[ trainIndex,]
datTest  <- ldn[-trainIndex,]
nrow(datTrain)
nrow(datTest)
names(datTest) 


#The first model will start with the land cover types at baseline
model1 <- glm(ch0012r ~ watr00 + green00 + devnew00, 
              family="binomial"(link="logit"), data = datTrain)
summary(model1)
pR2(model1)

# Significant but does not explain much as the McFaddden is quite low

#The Second model - we use population density as a predictor
model2 <- glm(ch0012r ~ watr00 + green00 + popdens01 + devnew00, 
              family="binomial"(link="logit"), data = datTrain)
summary(model2)
pR2(model2)

names(datTrain)
#Third model - we use race and almost everything else we got .. not much seems to be working- crazy!!
model3 <- glm(ch0012r ~  chioth01 + blck01 + Ave_pop200  + (popdens01 / houseDen_01) + 
                         (popdens01 * popdens_0111) + house2001  ,
              family="binomial"(link="logit"), data = datTrain)
summary(model3)
pR2(model3)

#Fourth (more like 204 th) model - Trying some more feature engineering
names(ldn)
model4 <- glm(ch0012r ~ watr00 +  (devnew00/Hectares) + chioth01 + blck01 + 
                pop2011 + (popdens01 / houseDen_01) + (popdens01 * disttube) +
                (popdens_0111/popdens01) + house2001 ,
              family="binomial"(link="logit"), data = datTrain)
summary(model4)
pR2(model4)



#Lets take it for a ride
ClassProbs <- predict(model4, datTest, type="response")

#lets look at the distribution of the probabilities
hist(ClassProbs) #try and find the right threshold to say what will develop because 0.5 will not give approoriate ans

#put these into a data frame so we can compare probabilities with actual dat land
testProbs <- data.frame(Class = datTest$ch0012r,Probs = ClassProbs)
head(testProbs)
summary(testProbs)


#plot the distrubtion of predictied probabilities for each binary output.
testProbsPlot <- ggplot(testProbs, aes(x = Probs, fill=Class)) + geom_density() +
  facet_grid(Class ~ .) + xlab("Probability") + geom_vline(xintercept = .5) + theme_fivethirtyeight()

testProbsPlot

#lets create a cutoff threshhold
testProbs$predClass  = ifelse(testProbs$Probs > .15 ,1,0)
head(testProbs)

#how many 1's?
table(testProbs$predClass)

#lets do a confusion matrix
confusionMatrix(testProbs$Class ,testProbs$predClass)

#Don't forget all the other goodness of fit metrics including spatial cv.
#lets do it again but with 100 cv folds

ctrl <- trainControl(method = "cv", number=100, savePredictions = TRUE)

cvFit <- train(ch0012r ~ watr00 + green00 +  (devnew00/Hectares) +  + chioth01 + blck01 + Ave_pop200  + 
                 (popdens01 / houseDen_01) + (popdens01 * disttube) +
                 ( popdens_0111/popdens01) + house2001 ,
               family="binomial"(link="logit"), data = datTrain, method="glm", trControl = ctrl)
cvFit

#let's plot the accuracy
hist(cvFit$resample[,1], xlim=c(0, 1))

#ROC Curve
pred <- prediction( testProbs$Probs, testProbs$Class)
perf <- performance(pred,"tpr","fpr")
plot(perf)
abline(a=0, b= 1)

#------------
#lets map our result
#before we do that we have to predict for the whole dataset
names(ldn)
ClassProbs2 <- predict(model4, ldn, type="response")

#Now join our predicted probabilities with the xy.
#lets join them using a column bind or cbind to the testProbs data frame
ProbsforMap <- as.data.frame(cbind(ClassProbs2, ldn$ch0012r, ldn$X, ldn$Y, ldn$devnew12 ))

#create an outcome with a threshold of .2
ProbsforMap$predClass  = ifelse(ProbsforMap$ClassProbs2 > .15 ,1,0)
head(ProbsforMap)

sum(ProbsforMap$predClass)/nrow(ProbsforMap)


#let clean up the column names just for the x and y columns. 
colnames(ProbsforMap) <- c("ClassProbs2", "CHANGE01_1", "X", "Y","developed_2011", "predClass")
head(ProbsforMap)


#lets plot the predicted probabilities
ggplot(ProbsforMap, aes(X,Y)) + geom_point(aes(colour=ClassProbs2 * developed_2011)) + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.direction = "horizontal",
        legend.position = c(0.8,0.92),
        legend.background = element_blank(),
        panel.background = element_rect(fill = NA)) + 
        scale_color_continuous_tableau(name="Probability")

  
#lets plot the classified probabilities
predicted_Development <- ggplot(ProbsforMap, aes(X,Y)) + 
  geom_point(aes(colour=as.factor(predClass))) + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        legend.background = element_blank(),
        panel.background = element_rect(fill = NA),
        aspect.ratio = 1) +
  scale_color_brewer(palette = "Set4")


actual_Development <- ggplot(ldn, aes(X,Y)) + 
  geom_point(aes(colour=as.factor(CHANGE01_1))) + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.position = "vertical",
        legend.background = element_blank(),
        panel.background = element_rect(fill = NA),
        aspect.ratio = 1) +
  scale_color_brewer(palette = "Set4")


grid.arrange(actual_Development,predicted_Development)




##################################################---------------------------------------


names(ldn)

# Building a model that uses 2011 data to train and Predicts for 2020

#Selecting only those fields that are used in making the model
#Using only those that are available for both 2010 and 2020 

data2020 <- ldn[,c(1:6,10,14,15,28,31,41,42,53,56,57,60,62,25)]
data2011 <- ldn[,c(1:6,9,13,15,29,30,36,37,53,56,57,59,61,20)]

#Check what all is selected 
names(data2011)
names(data2020)


# GIving them common names in the same order (not sure if order matters - but just for safety)
names(data2011) <- c("FID","Id","OBJECTID","Ward","GSS_Code","Hectares","Wtr", "Dvlpmnt", "Chnge_Ftr","HH", "Pp_Dnsty", "Blck", "Chn_Others", "Dst_Tube", "X", "Y", "Pp_Dnsty_Chng", "Housing_Den","Future_pop" )
names(data2020) <- c("FID","Id","OBJECTID","Ward","GSS_Code","Hectares","Wtr", "Dvlpmnt", "Chnge_Ftr", "HH","Pp_Dnsty", "Blck", "Chn_Others", "Dst_Tube", "X", "Y", "Pp_Dnsty_Chng", "Housing_Den","Future_pop" )
names(data2011)
names(data2020)

# Build Model using renamed attributes (Common Names)

model_future <- glm(Chnge_Ftr ~ Wtr +  (Dvlpmnt/Hectares) + Chn_Others + Blck + 
                 Future_pop + (Pp_Dnsty / Housing_Den) + (Pp_Dnsty * Dst_Tube) +
                  (Pp_Dnsty_Chng / Pp_Dnsty) + HH  ,
                      family="binomial"(link="logit"), data = data2011)

summary(model_future)
pR2(model_future)


#USe the model to predict with new Data for 2020
Predict_Prob <- predict(model_future, data2020, type = "response")
hist(Predict_Prob)

summary(Predict_Prob)

# Data frame to analyze the probabilities
Probmap <- as.data.frame(Predict_Prob)
Probmap$Change <- data2020$Chnge_Ftr 
Probmap$X <- data2020$X
Probmap$Y <- data2020$Y
Probmap$Dev <- data2020$Dvlpmnt
Probmap$Clsfd <- ifelse(Predict_Prob > 0.09, 1, 0) 


head(Probmap)

# Probability Map

#lets plot the predicted probabilities
ggplot(Probmap, aes(X,Y)) + geom_point(aes(colour=Predict_Prob * Dev)) + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.direction = "vertical",
        legend.position = c(0.9,0.2),
        legend.background = element_blank(),
        panel.background = element_rect(fill = NA)) + 
        scale_color_continuous_tableau(name="Probability",
                                       palette = "Orange")



#lets plot the classified probabilities
ggplot(Probmap, aes(X,Y)) + 
  geom_point(aes(colour=as.factor(Clsfd))) + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.direction = "vertical",
        legend.position = c(0.8,0.92),
        legend.background = element_blank(),
        panel.background = element_rect(fill = NA),
        aspect.ratio = 1) +
        scale_color_brewer(palette = "Pastel1",
                           name="Predicted Urban Development 2020",
                           labels = c("No Change", "New Development"))
