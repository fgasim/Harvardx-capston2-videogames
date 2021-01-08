# This code was written by Fidan Gasim as part of the Capstone Project 
# for the HardvardX Data Science Program. 

# Global Ops, Packages, Libraries ####

# -> Set global options ####
options(repos="https://cran.rstudio.com")
options(timeout=10000, digits=10, pillar.sigfigs=100)

# -> Install packages ####

list.of.packages <- c("caret", "data.table", "devtools", "dplyr", 
                      "DT", "ggplot2", "ggthemes", "h2o", "irlba", 
                      "kableExtra", "knitr", "lubridate", "Matrix.utils", 
                      "purrr", "RColorBrewer","scales", "tidyr", "tidyverse", 
                      "splitstackshape", "ggrepel", "tinytex","tree", "gam", "gridExtra", 
                      "matrixStats", "GGally", "corrplot", "polycor", "lsr","readr","e1071","randomForest",
                      "glmnet","ade4","FSelector","naniar","kernlab")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# -> Load libraries ####
library(tidyverse)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(caret)
library(data.table)
library(knitr) #A General-Purpose Package for Dynamic Report Generation in R
library(kableExtra) #to build common complex tables and manipulate table styles
library(lubridate)
library(Matrix.utils) #Data.frame-Like Operations on Sparse and Dense Matrix Objects.
library(DT) #provides an R interface to the JavaScript library DataTables.
library(RColorBrewer) #Provides color schemes for maps (and other graphics) 
library(ggthemes) #Some extra themes, geoms, and scales for 'ggplot2'.
library(scales)
library(purrr)
library(devtools)
library(ggrepel)
library(splitstackshape)
library(tree)
library(gam)
library(gridExtra)
library(matrixStats)
library(GGally)
library(corrplot)
library(lares)
library(polycor)
library(lsr)
library(Boruta)
library(e1071)
library(randomForest)
library(glmnet)
library(ade4)
library(FSelector)
library(naniar)
library(klaR)
library(kernlab)
library(tinytex)


####### Data Preprocessing #######

# Download and import dataset 

urlfile<-"https://raw.githubusercontent.com/fgasim/Harvardx-capstone2-videogames/main/Video_Games.csv"
videogames<-read.csv(urlfile, 
                     header = TRUE,
                     na.strings=c("","N/A","NA"))

# Initial exploration of the dataset

class(videogames) # type of dataset

dim(videogames) # no. of rows and columns in the dataset

str(videogames) # structure of the dataset - data types of each column

head(videogames) # first few rows of the dataset

summary(videogames) # summary statistics of the dataset

# -> Data Transformation #### 
# Transform variables, remove variables we won't be using, add new variables

# Remove regional sales columns (we are only interested in Global Sales)

videogames<-videogames%>%dplyr::select(-c("EU_Sales","JP_Sales","NA_Sales","Other_Sales"))

# Rename columns for ease of use

videogames<-videogames%>%
  rename("Year" = Year_of_Release)

# Create new column for the age of the game (number of years since release)

videogames<-videogames%>%mutate(Age=2020-(Year))

# Create new column - Combine platforms by manufacturer (to reduce number of categories)

nintendo<-c("3DS","DS","GB","GBA","N64","GC", "NES","SNES","Wii","WiiU")
sony<-c("PS","PS2","PSP","PS3","PS4","PSV")
sega<-c("GEN","SCD","DC","GG")
microsoft<-c("XB","X360", "XOne")
other<-c("2006","3DO","NG","PCFX","TG16")

Platform_Type<-function(x){
  if (x %in% sony == TRUE) {return('Sony')}
  else if(x %in% microsoft == TRUE) {return('Microsoft')}
  else if(x %in% nintendo == TRUE) {return('Nintendo')}
  else if(x %in% sega == TRUE) {return('Sega')}
  else{return('OTHER')}
}

videogames$Platform_Type<-sapply(videogames$Platform, Platform_Type)

# Convert non-numeric values to numeric

videogames$Year<-as.numeric(as.character(videogames$Year))
videogames$User_Count<-as.numeric(as.character(videogames$User_Count))
videogames$User_Score<-as.numeric(as.character(videogames$User_Score))
videogames$Critic_Count<-as.numeric(as.character(videogames$Critic_Count))
videogames$Critic_Score<-as.numeric(as.character(videogames$Critic_Score))

# Rescale the User Score column to match Critic Score column

videogames$User_Score<- as.numeric(as.character(videogames$User_Score)) *10

# Convert categorical variables to factors - works better for statistical modeling 

videogames[sapply(videogames, is.character)] <- 
  lapply(videogames[sapply(videogames, is.character)], as.factor)

str(videogames) #check whether the categorical variables were converted

# Check all the levels of each factor to understand them better

for (x in names(videogames)) {
  if (is.factor(videogames[[x]]))
  {print(paste (x,":",levels(videogames[[x]])))}
}

# -> Data Splitting ####

# Split the videogames dataset into training and test sets 

# Test set will be 20% of video games dataset
set.seed(1, sample.kind = "Rounding") 
test_index <- createDataPartition(y=videogames$Global_Sales, times = 1, p = 0.2, list = FALSE)
vgTrain <- videogames[-test_index,]
vgTest <- videogames[test_index,]

rm(test_index)
nrow(vgTrain) #check number of observations
nrow(vgTest) #check number of observations 

# -> Data Cleaning ####

# Identifying and dealing with missing values 

apply(vgTrain,2,function(x){sum(is.na(x))})

apply(vgTrain,2, function(x){length(which(is.na(x)))/dim(vgTrain)[1]}) # by percentage

gg_miss_var(vgTrain) # missing values across entire datset

# Visualize dataset with missing values 

gg_miss_fct(x = vgTrain, fct = Year) # missing values by Year

gg_miss_fct(x = vgTrain, fct = Platform_Type) # missing values by Platform Manufacturer

vgTrain%>%group_by(Platform_Type,Year)%>%     # check game releases for Sega 
  filter(Platform_Type=="Sega")%>%
  summarize(count=n())

# Remove all games released before 1999 (year Metacritic was founded) - check if this would help get rid of NAs

temp<-vgTrain%>%filter(Year>1999 & Year<=2016)

gg_miss_fct(x = temp, fct = Year) # many missing values still remain betweem 1999 and 2016

gg_miss_fct(x = temp, fct = Platform_Type) # many missing values still remain across platforms - Sega specifically

rm(temp) # remove dataset as we won't be using it for analysis 

# Remove all rows with missing Name, Year, Genre, Publisher (small percentage of NAs)
# This won't harm the data much

vgTrain<-vgTrain%>% drop_na(c(Name, Genre, Publisher, Year))

apply(vgTrain,2,function(x){sum(is.na(x))})    #still have many NAs in some columns

# Create new dataframe with no NAs - to be used for visualization purposes

vgTrain2<-vgTrain[complete.cases(vgTrain),] 

apply(vgTrain2,2,function(x){sum(is.na(x))}) # confirm that there are no missing values 

nrow(vgTrain2) # number of observations remaining in dataset

##### Data Visualization #######

# 'In this section, we will be analyzing the dataset further to gain better understanding of all the variables. 
# In some sections we will be using the dataframe with NAs (vgTrain), 
# in others we will be using the dataframe with no NAs (vgTrain2).'

# Number of Unique Video Games in dataset

n_distinct(vgTrain$Name) 

# some games appear more than once as they're released across multiple platforms

# -> Global Sales #### 

# Distribution of Global Sales 

vgTrain%>%
  group_by(Global_Sales)%>%
  summarize(n=n())%>%
  ggplot(aes(x=Global_Sales, y=n))+
  geom_bar(stat="identity", color="darkgreen")+
  ylim(0, 250)+
  scale_x_continuous(labels=c(0,5,10,15,20,25,30),
                     breaks=c(0,5,10,15,20,25,30),
                     limits=c(0,30))+
  labs(x="Global Sales (millions of units)",
       y="Number of Video Games", 
       title="Distribution of Global Sales")

mean(vgTrain$Global_Sales<1.0)     #percentage of games selling less than 1 million

# Top 10 games by Global Sales

vgTrain%>%group_by(Name)%>%
  summarize(sales=sum(Global_Sales))%>%
  top_n(10)%>%
  ggplot(aes(x=reorder(Name, sales), y=sales))+
  geom_bar(stat="identity", width = 0.8, fill="darkgreen")+
  labs(x="Game", y="Global Sales (units sold)", 
       title= "Highest Selling Games (Top 10)")+
  coord_flip()


# -> Platform ####

# Number of Unique Platforms in dataset

n_distinct(vgTrain$Platform)

# Global Sales by Platform

vgTrain%>%group_by(Platform)%>%
  summarize(sales=sum(Global_Sales))%>%
  top_n(15)%>%
  ggplot(aes(x=reorder(Platform,sales), y=sales))+
  geom_bar(stat="identity", width=0.8, fill="darkgreen")+
  labs(x="Platform", y="Global Sales (millions of units)", 
       title= "Distribution of Global Sales by Platform (Top 15)")+
  coord_flip()

# Game Releases by Platform

vgTrain%>%
  group_by(Platform)%>%
  summarize(n=n())%>%
  top_n(15)%>%
  ggplot(aes(x=reorder(Platform,n), y=n))+
  geom_bar(stat="identity", width = 0.8, fill="darkgreen")+
  labs(x="Platform", y="Count", 
       title= "Game Releases by Platform (Top 15)")+
  coord_flip()

# Global Sales by Platform Type 

vgTrain%>%group_by(Platform_Type)%>%
  summarize(sales=sum(Global_Sales))%>%
  ggplot(aes(x=reorder(Platform_Type,sales), y=sales))+
  geom_bar(stat="identity", width=0.8, fill="darkgreen")+
  labs(x="Platform Type", 
       y="Global Sales (millions of units)", 
       title= "Distribution of Global Sales by Platform Type")+
  coord_flip()

# Game Releases by Platform Type

vgTrain%>%
  group_by(Platform_Type)%>%
  summarize(n=n())%>%
  ggplot(aes(x=reorder(Platform_Type,n), y=n))+
  geom_bar(stat="identity", width = 0.8, fill="darkgreen")+
  labs(x="Platform", y="Count", 
       title= "Game Releases by Platform Type")+
  coord_flip()

# -> Genre #####

# Global Sales by Genre

vgTrain%>%group_by(Genre)%>%
  summarize(sales=sum(Global_Sales))%>%
  arrange(desc(sales))%>%
  top_n(10)%>%
  ggplot(aes(x=reorder(Genre, sales), y=sales))+
  geom_bar(stat="identity", width=0.8, fill="darkgreen")+
  labs(x="Genre", y="Global Sales (millions of units)", 
       title= "Distribution of Global Sales by Genre")+
  coord_flip()

# Game Releases by Genre

vgTrain%>%
  group_by(Genre)%>%
  summarize(n=n())%>%
  ggplot(aes(x=reorder(Genre,n), y=n))+
  geom_bar(stat="identity", width = 0.8, fill="darkgreen")+
  labs(x="Genre", y="Count", title= "Game Releases by Genre")+
  coord_flip()

# -> Publisher ####

# Global Sales by Publisher (top 15)

vgTrain%>%group_by(Publisher)%>%
  summarize(sales=sum(Global_Sales))%>%
  top_n(15)%>%
  ggplot(aes(x=reorder(Publisher,sales), y=sales))+
  geom_bar(stat="identity", width=0.8, fill="darkgreen")+
  labs(x="Publisher",
       y="Global Sales (units sold)", 
       title= "Distribution of Global Sales by Publisher (Top 15)")+
  coord_flip()

# Game Releases by Publisher (top 15)

vgTrain%>%
  group_by(Publisher)%>%
  summarize(n=n())%>%
  top_n(15)%>%
  ggplot(aes(x=reorder(Publisher,n), y=n))+
  geom_bar(stat="identity", width = 0.8, fill="darkgreen")+
  labs(x="Publisher", y="Count", title= "Game Releases by Publisher (Top 15)")+
  coord_flip()

# -> Developer ####

# Global Sales by Developer (top 15)

vgTrain%>%group_by(Developer)%>%
  summarize(sales=sum(Global_Sales))%>%
  arrange(desc(sales))%>%
  top_n(15)

# Lot of missing values in "Developer" column so we will use the vgTrain2 dataframe instead

vgTrain2%>%group_by(Developer)%>%
  summarize(sales=sum(Global_Sales))%>%
  top_n(15)%>%
  ggplot(aes(x=reorder(Developer, sales), y=sales))+
  geom_bar(stat="identity", width = 0.8, fill="darkgreen")+
  labs(x="Developer",
       y="Global Sales (units sold)", 
       title= "Distribution of Global Sales by Developer (Top 15)")+
  coord_flip()

# Game Releases by Developer (top 15)

vgTrain2%>%
  group_by(Developer)%>%
  summarize(n=n())%>%
  top_n(15)%>%
  ggplot(aes(x=reorder(Developer,n), y=n))+
  geom_bar(stat="identity", width = 0.8, fill="darkgreen")+
  labs(x="Developer", y="Count", 
       title= "Game Releases by Developer (Top 15)")+
  coord_flip()

total_sales<-sum(vgTrain$Global_Sales)

vgTrain%>%
  group_by(Developer)%>%
  summarize(percentage_sales=sum(Global_Sales)/total_sales)%>%
  arrange(desc(percentage_sales))%>%
  top_n(10)

# -> Rating ####

# Global Sales by Age Rating

vgTrain%>%group_by(Rating)%>%
  summarize(sales=sum(Global_Sales))%>%
  arrange(desc(sales))

# Lot of missing values in "Rating" column, so we will use dataframe vgTrain2 for visualization instead

vgTrain2%>%group_by(Rating)%>%
  summarize(sales=sum(Global_Sales))%>%
  ggplot(aes(x=reorder(Rating, sales), y=sales))+
  geom_bar(stat="identity", width = 0.8, fill="darkgreen")+
  labs(x="Age Rating", y="Global Sales", 
       title= "Distribution of Global Sales by Rating")+
  coord_flip()

# Game Releases by Rating 

vgTrain2%>%
  group_by(Rating)%>%
  summarize(n=n())%>%
  ggplot(aes(x=reorder(Rating,n), y=n))+
  geom_bar(stat="identity", width = 0.8, fill="darkgreen")+
  labs(x="Rating", y="Count", title= "Game Releases by Rating")+
  coord_flip()

# -> Critic Score & User Score ####

# Distribution of Critic Score 

vgTrain2%>%
  ggplot(aes(x=Critic_Score))+
  geom_bar(fill="darkgreen")+
  scale_x_continuous(breaks=seq(0,100,10),
                     labels=seq(0,100,10))

mean(vgTrain2$Critic_Score)

# Distribution of User Score

vgTrain2%>%
  ggplot(aes(x=User_Score))+
  geom_bar(bins=10, fill="darkgreen")+
  scale_x_continuous(breaks=seq(0,100,10),
                     labels=seq(0,100,10))

mean(vgTrain2$User_Score)

# Top 10 games by average Critic Score 
# using vgTrain2 since vgTrain has many missing values in Critic Score column

vgTrain2%>%group_by(Name)%>%
  summarize(avg=mean(Critic_Score))%>%
  arrange(desc(avg))%>%
  top_n(10)

# Top 10 games by average User Score
# using vgTrain2 sinve vgTrain has many missing values in User Score column

vgTrain2%>%group_by(Name)%>%
  summarize(avg=mean(User_Score))%>%
  arrange(desc(avg))%>%
  top_n(10)

# Plot - Critic Score vs. User Score

vgTrain2%>%
  ggplot(aes(Critic_Score,User_Score))+
  geom_point(color="orange")+
  geom_smooth(color="black")+
  labs(x="Critic Score", y="User Score", 
       title="Relationship between Critic Score and User Score")

cor(vgTrain2$User_Score,vgTrain2$Critic_Score)

# Plot - Critic Score vs. Global Sales

criticeffect<-vgTrain2%>%
  ggplot(aes(x=Critic_Score, y=Global_Sales))+
  geom_point(color="orange")+
  geom_smooth(color="black")+
  xlim(0,100)+
  ylim(0,40)+
  labs(x="Critic Score", y="Global Sales", 
       title="Critic Score vs Global Sales")

# Plot - User Score vs. Global Sales

usereffect<-vgTrain2%>%
  ggplot(aes(x=User_Score, y=Global_Sales))+
  geom_point(color="orange")+
  geom_smooth(color="black")+
  xlim(0,100)+
  ylim(0,40)+
  labs(x="User Score", y="Global Sales", 
       title="User Score vs Global Sales")

# Plot - Effect of User Score vs Critic Score on Global Sales

grid.arrange(criticeffect,usereffect)

# Average Critic Scores by Genre

vgTrain2%>%group_by(Genre)%>%
  summarize(critic_score=mean(Critic_Score))%>%
  arrange(desc(critic_score))

# Average User Scores by Genre

vgTrain2%>%group_by(Genre)%>%
  summarize(user_score=mean(User_Score))%>%
  arrange(desc(user_score))

# Plot - Critic Scores by Genre

critic_genre<-vgTrain2%>%
  ggplot(aes(y=Critic_Score, color=Genre))+
  geom_boxplot()

# Plot - User Scores by Genre

user_genre<-vgTrain2%>%
  ggplot(aes(y=User_Score, color=Genre))+
  geom_boxplot()

# User Scores vs Critic Scores by Genre 

grid.arrange(user_genre,critic_genre, nrow=1)

# -> Critic Count & User Count ####

# Top 10 most rated games by critics (Critic Count)
# using vgTrain2 since vgTrain has many missing values in Critic Count column

vgTrain2%>%group_by(Name)%>%
  summarize(count=sum(Critic_Count))%>%
  arrange(desc(count))%>%
  top_n(10)

# Top 10 most rated games by users (User Count)
# using vgTrain2 since vgTrain has many missing values in User Count column

vgTrain2%>%group_by(Name)%>%
  summarize(count=sum(User_Count))%>%
  arrange(desc(count))%>%
  top_n(10)

# Plot - Critic Count vs User Count

vgTrain2%>%
  ggplot(aes(Critic_Count,User_Count))+
  geom_point(color="orange")+
  geom_smooth(color="black")+
  labs(x="Critic Count", y="User Count", 
       title="Relationship between Critic Count and User Count")

cor(vgTrain2$Critic_Count,vgTrain2$User_Count)

# Plot - Critic Count vs. Global Sales

criticcount<-vgTrain2%>%
  ggplot(aes(x=Critic_Count, y=Global_Sales))+
  geom_point(color="orange")+
  geom_smooth(color="black")+
  labs(x="Critic Count", y="Global Sales", 
       title="Critic Count vs Global Sales")

# Plot - User Count vs. Global Sales

usercount<-vgTrain2%>%
  ggplot(aes(x=User_Count, y=Global_Sales))+
  geom_point(color="orange")+
  geom_smooth(color="black")+
  labs(x="User Count", y="Global Sales", 
       title="User Count vs Global Sales")

# Plot - Effect of User Count vs Critic Count on Global Sales

grid.arrange(criticcount,usercount)

# Correlations between the count variables and global sales 

cor(vgTrain2$Critic_Count,vgTrain2$Global_Sales)
cor(vgTrain2$User_Count,vgTrain2$Global_Sales)

# Plot - relationship between Critic Score and Critic Count

vgTrain2 %>% 
  ggplot(aes(Critic_Score, Critic_Count)) + 
  geom_point(color="orange")+
  geom_smooth(color="black")

cor(vgTrain2$Critic_Score,vgTrain2$Critic_Count)

# Plot - relationship between User Score and User Count

vgTrain2%>% 
  ggplot(aes(User_Score, User_Count)) + 
  geom_point(color="orange")+
  geom_smooth(color="black")

cor(vgTrain2$User_Score,vgTrain2$User_Count)

# -> Year & Age ####

# Plot - Game Releases per Year 

vgTrain%>%
  group_by(Year)%>%
  filter(Year>1985 & Year<=2016)%>%
  summarize(count=n())%>%
  ggplot(aes(x=Year, y=count))+
  geom_bar(stat="identity", fill="darkgreen")+
  scale_x_continuous(breaks=1985:2016)+
  theme(axis.text.x = element_text(angle = 90))+
  labs(x= "Year", y="Number of Games", 
       title= "Distribution of Game Releases by Year")

# Plot - Global Sales per Year 

vgTrain%>%
  group_by(Year,Global_Sales)%>%
  filter(Year>1985 & Year<=2016)%>%
  summarize(count=n())%>%
  ggplot(aes(x=Year, y=Global_Sales))+
  geom_bar(stat="identity", fill="darkgreen")+
  scale_x_continuous(breaks=1985:2016)+
  theme(axis.text.x = element_text(angle = 90))+
  labs(x= "Year", y="Number of Games", 
       title= "Distribution of Global Sales by Year")

# Plot - Released Games per Year and Platform Type

games1<-vgTrain%>%
  ggplot(aes(x=Year))+
  geom_line(aes(y=..count.., color=Platform_Type), stat="bin", bins=30)+
  labs(x= "Year", y="Number of Games", 
       title= "Distribution of Game Releases by Year")

# Plot - Global Sales per Year and Platform Type

games2<-vgTrain%>%
  group_by(Platform_Type, Year)%>%
  summarize(sales=sum(Global_Sales))%>%
  ggplot(aes(x=Year))+
  geom_line(aes(y=sales, color=Platform_Type))+
  labs(x= "Year", y="Global Sales (units sold)", 
       title= "Distribution of Game Sales by Year")

grid.arrange(games1,games2)

# Average age of games in dataset
mean(vgTrain$Age) 

# Global Sales by Age of the Game 

vgTrain%>%
  ggplot(aes(x=Age, y=Global_Sales))+
  geom_bar(stat="identity", width=0.8, fill="darkgreen")+
  xlim(0,30)+
  labs(x="Age of the Game", 
       y="Global Sales (millions of units)", 
       title= "Distribution of Global Sales by Age")

cor(vgTrain$Global_Sales,vgTrain$Age)   #correlation between sales and age of the game

# Feature Selection ####

# In this section, we will aim to select the most important features to feed into our machine learning algorithms. 
# Having too many features increases the models' complexity, making them run too slowly, or produce suboptimal results. 

# Remove columns we won't be using for feature selection
# Name column has too many categories 
# Year column is redundant with Age 

vgTrain<-vgTrain%>%dplyr::select(-c("Name","Year")) #update vgTrain dataset (with NAs)
colnames(vgTrain)

vgTrain2<-vgTrain2%>%dplyr::select(-c("Name","Year")) #update vgTrain2 dataset (no NAs)
colnames(vgTrain2)

# For feature selection to work effectively, we will be using the dataset with no missing values (vgTrain2)
# We will first start by looking at the correlations between features and with the target variable 

### Correlation 1 - numeric features only
set.seed(1234)
num_cols <- dplyr::select_if(vgTrain2, is.numeric) 
cor(num_cols) 

# None of the numeric features are highly correlated with each other so we won't remove any at this stage

# check which numeric features are the most correlated with target variable "Global_Sales"
cor(num_cols, vgTrain2$Global_Sales) 

corr_var(num_cols,
         Global_Sales)

# Plot 1 - Correlation Matrix (numeric features) 

corrplot(cor(num_cols),method='number',
         tl.cex=0.6,
         number.cex=0.7,
         mar = c(1, 1, 1, 1),
         title="Correlation Matrix - numeric features")

### Correlation 2 
# Correlation matrix for a dataframe with mixed variable types 
# function via author: Srikanth KS (talegari)

cor2 <-function(df){
  
  stopifnot(inherits(df, "data.frame"))
  stopifnot(sapply(df, class) %in% c("integer"
                                     , "numeric"
                                     , "factor"
                                     , "character"))
  
  cor_fun <- function(pos_1, pos_2){
    
    # both are numeric
    if(class(df[[pos_1]]) %in% c("integer", "numeric") &&
       class(df[[pos_2]]) %in% c("integer", "numeric")){
      r <- stats::cor(df[[pos_1]]
                      , df[[pos_2]]
                      , use = "pairwise.complete.obs"
      )
    }
    
    # one is numeric and other is a factor/character
    if(class(df[[pos_1]]) %in% c("integer", "numeric") &&
       class(df[[pos_2]]) %in% c("factor", "character")){
      r <- sqrt(
        summary(
          stats::lm(df[[pos_1]] ~ as.factor(df[[pos_2]])))[["r.squared"]])
    }
    
    if(class(df[[pos_2]]) %in% c("integer", "numeric") &&
       class(df[[pos_1]]) %in% c("factor", "character")){
      r <- sqrt(
        summary(
          stats::lm(df[[pos_2]] ~ as.factor(df[[pos_1]])))[["r.squared"]])
    }
    
    # both are factor/character
    if(class(df[[pos_1]]) %in% c("factor", "character") &&
       class(df[[pos_2]]) %in% c("factor", "character")){
      r <- lsr::cramersV(df[[pos_1]], df[[pos_2]], simulate.p.value = TRUE)
    }
    
    return(r)
  } 
  
  cor_fun <- Vectorize(cor_fun)
  
  # now compute corr matrix
  corrmat <- outer(1:ncol(df)
                   , 1:ncol(df)
                   , function(x, y) cor_fun(x, y)
  )
  
  rownames(corrmat) <- colnames(df)
  colnames(corrmat) <- colnames(df)
  
  return(corrmat)
}

vg_cor <- cor2(vgTrain2)

# Plot 2 - Correlation between numeric and categorical features 
set.seed(1234)

corrplot(vg_cor,
         method='number',
         tl.cex=0.6,
         number.cex=0.4,
         mar = c(1, 1, 1, 1),
         title="Correlation Matrix with All Features")

### Boruta function
set.seed(1234)

boruta_output <- Boruta(Global_Sales ~ ., 
                        data=vgTrain2, doTrace=0)  

# Get significant variables including tentatives
boruta_sig <- getSelectedAttributes(boruta_output, withTentative = TRUE)
print(boruta_sig)  

# Do a tentative rough fix
roughFixMod <- TentativeRoughFix(boruta_output)
boruta_sig <- getSelectedAttributes(roughFixMod)
print(boruta_sig)

# Variable Importance Scores
imps <- attStats(roughFixMod)
imps2 = imps[imps$decision != 'Rejected', c('meanImp', 'decision')]
head(imps2[order(-imps2$meanImp), ])  # descending sort

# Plot variable importance
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")  

# Recursive Feature Elimination (RFE)

# Convert categorical variables to character 
# RFE cannot handle categorical predictors with more than 53 categories 
vgTrain2$Developer<-as.character(vgTrain2$Developer) 
vgTrain2$Publisher<-as.character(vgTrain2$Publisher)

set.seed(1234)

# define the control using a random forest selection function
rfe_ctrl <- rfeControl(functions=rfFuncs, method="cv", number=10)

# run the RFE algorithm
rfe_results <- rfe(vgTrain2[,-4], 
               vgTrain2$Global_Sales, 
               sizes=c(1:11), 
               rfeControl=rfe_ctrl)

# summarize the results
print(rfe_results)

# list the chosen features
predictors(rfe_results)

# plot the results
plot(rfe_results, type=c("g", "o"))

# Create table of Feature Selections results - only list the top 5 for each method

fsresults<-data_frame(
  Corr1=c("CriticCount","UserCount","CriticScore","UserScore","Age"),
  Corr2=c("Developer","Publisher","CriticCount","UserCount","CriticScore"),
  Boruta=c("Platform","UserCount","Rating", "CriticCount","Age"),
  RFE=c("UserCount","Platform","CriticCount","CriticScore","Age"),
)
print(fsresults)

# Based on these results, we will be removing features: 
# User Score, Publisher, Developer, Rating, Genre, Platform Type

# Data Preprocessing for Modeling ####

# We will work with the training set that has no NAs 
# since data imputation for such a large number of observations could skew the results

# Select subset of features for modeling (based on Feature Selection) 
vgTrain2<-vgTrain2%>%
  dplyr::select(-c(User_Score,Publisher,Developer,Rating,Genre,Platform_Type)) 
colnames(vgTrain2) #check column names
nrow(vgTrain2) #check number of observations

train_set<-vgTrain2   #rename to train set

# Check if any NAs in datasets
apply(vgTrain2,2,function(x){sum(is.na(x))})

# Center and scale numeric features 

preProcValues <- preProcess(train_set, method = c("center", "scale"))
train_set <- predict(preProcValues, train_set)

# Dummify categorical variables in train set

t_dummy <- dummyVars(" ~ .", data = train_set)
train_dummy <- data.frame(predict(t_dummy, newdata = train_set))
head(train_dummy)

######## Modeling #########

# Model Evaluation Functions

# Define Root Mean Squared Error (RMSE) - loss function
RMSE <- function(true_sales, predicted_sales){
  sqrt(mean((true_sales - predicted_sales)^2))
}

# Set control parameters - run algorithms using 10-fold cross validation repeated 3 times

trControl <- trainControl(method = "repeatedcv",
                          number = 10,
                          repeats=3,
                          savePredictions="final")

metric <- "RMSE"

# -> Linear Regression ####

# Linear Regression Model 

set.seed(1234)

fit_lr<-train(Global_Sales~.,
              data=train_dummy,
              method="lm",
              metric=metric,
              trControl=trControl)

# Predict on training set
train_lr<-predict(fit_lr,train_dummy)

# Training error
RMSE(train_set$Global_Sales, train_lr)

# Validation error
rmse_lr<-fit_lr$results['RMSE']
rmse_lr

# Check variable importance
varImp(fit_lr)

# Create a table of results to compare models' performance 
rmse_results <- tibble(Method = "Linear Regression", 
                       "Training Error" = RMSE(train_set$Global_Sales, train_lr),
                       "Validation Error" = rmse_lr$RMSE)
rmse_results

# -> Elastic Net Regression ####
set.seed(1234)

fit_glmnet<-train(Global_Sales~.,
                  data=train_dummy,
                  method="glmnet",
                  metric=metric,
                  trControl=trControl,
                  tuneLength=10)

# Predict on training set
train_glmnet<-predict(fit_glmnet,train_dummy)

# Training error
RMSE(train_set$Global_Sales, train_glmnet)

# Validation error
rmse_glmnet<-fit_glmnet$results[row.names(fit_glmnet$bestTune),'RMSE']
rmse_glmnet

# Check variable importance
varImp(fit_glmnet)

# Update results table
rmse_results <- bind_rows(rmse_results, 
                          tibble(Method = "Elastic Net", 
                                 "Training Error" = RMSE(train_set$Global_Sales, train_glmnet),
                                 "Validation Error" = rmse_glmnet))
rmse_results

# -> Support Vector Machine ####
set.seed(1234)

fit_svm<-train(Global_Sales~.,
               data=train_dummy,
               method="svmRadial",
               metric=metric,
               trControl=trControl)

# Predict on training set
train_svm<-predict(fit_svm,train_dummy)

# Training error
RMSE(train_set$Global_Sales, train_svm)

# test_set error
rmse_svm<-fit_svm$results[row.names(fit_svm$bestTune),'RMSE']
rmse_svm

# Check variable importance
varImp(fit_svm)

# Update results table

rmse_results <- bind_rows(rmse_results, 
                          tibble(Method = "SVM", 
                                 "Training Error" = RMSE(train_set$Global_Sales, train_svm),
                                 "Validation Error" = rmse_svm))
rmse_results

# -> Random Forest ##### 

# Random Forest using caret package 

# Rf Model 1 - Run the model with default parameters 
set.seed(1234)

fit_rf1 <- train(Global_Sales~.,
                 data = train_dummy,    
                 method = "rf",
                 metric=metric,
                 trControl = trControl)

fit_rf1

# Predict on training set
train_rf1<-predict(fit_rf1,train_dummy)

# Training error
RMSE(train_set$Global_Sales, train_rf1)

# Validation error
rmse_rf1<-fit_rf1$results[row.names(fit_rf1$bestTune),'RMSE']
rmse_rf1

# Check variable importance
varImp(fit_rf1)

# Update results table

rmse_results <- bind_rows(rmse_results, 
                          tibble(Method = "RandomForest 1", 
                                 "Training Error" = RMSE(train_set$Global_Sales, train_rf1),
                                 "Validation Error"= rmse_rf1))
rmse_results

## RF Model 2 - with tuned parameters

# Fine tune parameters - search for best mtry, maxnodes, ntrees 
# These parameters have the biggest impact on the model's performance and accuracy

set.seed(1234)

# Search for best mtry
tuneGrid <- expand.grid(.mtry = c(1: 10)) #Construct a vector with values from 3:10

rf_mtry <- train(Global_Sales~.,
                 data = train_dummy,
                 method = "rf",
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 importance = TRUE,
                 nodesize = 14,
                 ntree = 300)
print(rf_mtry)

rf_mtry$bestTune$mtry # best value of mtry

best_mtry <- rf_mtry$bestTune$mtry #store best mtry for final model
best_mtry

# Search for best maxnodes
# Code inspired by: https://www.guru99.com/r-random-forest-tutorial.html

set.seed(1234)

store_maxnode <- list() #The results of the model will be stored in this list
tuneGrid <- expand.grid(.mtry = best_mtry) #Use the best value of mtry
for (maxnodes in c(10: 25)) {   #Compute the model with different values of maxnodes from 10:25.
  set.seed(1234)
  rf_maxnode <- train(Global_Sales~.,
                      data = train_dummy,
                      method = "rf",
                      tuneGrid = tuneGrid,
                      trControl = trControl,
                      importance = TRUE,
                      nodesize = 14,
                      maxnodes = maxnodes,
                      ntree = 300)
  current_iteration <- toString(maxnodes)   #Store as a string variable the value of maxnode.
  store_maxnode[[current_iteration]] <- rf_maxnode   #Save the result of the model in the list.
}
results_maxnode <- resamples(store_maxnode)  #Arrange the results of the model
summary(results_maxnode)  #summarize results

# Search for best ntree - same methodology as maxnode

set.seed(1234)

store_maxtrees <- list()
for (ntree in c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000)) {
  set.seed(1234)
  rf_maxtrees <- train(Global_Sales~.,
                       data = train_dummy,
                       method = "rf",
                       tuneGrid = tuneGrid,
                       trControl = trControl,
                       importance = TRUE,
                       nodesize = 14,
                       maxnodes = 24,
                       ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)

# Train new model with tuned parameteres

set.seed(1234)

fit_rf2 <- train(Global_Sales~.,
                 train_dummy,
                 method = "rf",
                 tuneGrid = tuneGrid,   # best mtry
                 trControl = trControl,
                 importance = TRUE,
                 nodesize = 14,
                 ntree = 600,   # best ntree
                 maxnodes = 24,
                 metric=metric)  # best maxnodes

# Predict on training set
train_rf2<-predict(fit_rf2,train_dummy)

# Training error
RMSE(train_set$Global_Sales, train_rf2)

# Validation error 
rmse_rf2<-fit_rf2$results[row.names(fit_rf2$bestTune),'RMSE']
rmse_rf2

# Check variable importance
varImp(fit_rf2)

# Update results table

rmse_results <- bind_rows(rmse_results, 
                          tibble(Method = "RandomForest 2", 
                                 "Training Error"= RMSE(train_set$Global_Sales, train_rf2),
                                 "Validation Error" = rmse_rf2))

# Compare models - Plot results from all models

rmse_results<-as.data.frame(rmse_results)
rmse_results

results <- resamples(list(LR=fit_lr, ENET=fit_glmnet, 
                          SVM=fit_svm, RF1=fit_rf1, RF2=fit_rf2))
summary(results)
dotplot(results)

##### Final Validation #####

# Preprocessing of training set

# Feature Selection
vgTest<-vgTest%>%
  dplyr::select(-c(User_Score,Publisher,Developer,Rating,Genre,Platform_Type,Name,Year))  
colnames(vgTest) #check column names
nrow(vgTest) #check number of observations

# Check if any NAs in datasets
apply(vgTest,2,function(x){sum(is.na(x))})

# Remove NAs
vgTest<-vgTest[complete.cases(vgTest),] 
nrow(vgTest)

# Rename to test set
test_set<-vgTest      

# Center and scale numeric features 
test_set<-predict(preProcValues, test_set)

# Dummify categorical variables in test set 
t2_dummy <- dummyVars(" ~ .", data = test_set)
test_set_dummy <- data.frame(predict(t2_dummy, newdata = test_set))
head(test_set_dummy)

# Baseline Model - Guessing Global Sales based on the mean

set.seed(1234)

# Mean of observed Global Sales
mu<-mean(train_set$Global_Sales)
mu

# Predict all unknown ratings with overall mean 

naive_rmse <- RMSE(test_set$Global_Sales, mu)
naive_rmse

rmse_results <- bind_rows(rmse_results,
                          tibble(Method = "Baseline", 
                                 RMSE = RMSE(test_set$Global_Sales, mu)))
rmse_results

# Final model - Random Forest with default parameters

set.seed(1234)

# Train the final model using all available data 
fit_final <- train(Global_Sales~.,
                   train_dummy,
                   method = "rf",
                   tuneGrid = tuneGrid,   # best mtry
                   importance = TRUE,
                   nodesize = 14,
                   ntree = 600,   # best ntree
                   maxnodes = 24,
                   metric=metric)  # best maxnodes
fit_final

# Predicting on test set 
test_final<- predict(fit_final, test_set_dummy) 

# Check model performance
RMSE(test_final, test_set_dummy$Global_Sales)

# Check variable importance
varImp(fit_final)

# Update results table

rmse_results <- bind_rows(rmse_results, 
                          tibble(Method = "Final Model - RF", 
                                 RMSE = RMSE(test_final,test_set_dummy$Global_Sales)))
rmse_results




