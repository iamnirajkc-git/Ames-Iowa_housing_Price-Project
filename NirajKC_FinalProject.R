#step1:Import the required libraries.
library(dplyr)
library(ggplot2) 
options(scipen = 999)
install.packages("GGally")
library(GGally)
library(caTools)
install.packages("Metrics")
library(Metrics)

#step 2: Load the data set
setwd("/Users/nirajkc/Desktop")
sales= read.csv("AmesHousing.csv")
head(sales)
#step 3.Check the structure of the dataset 
str(sales)
sort(colSums(is.na(sales)))
sum(is.na(sales$Garage.Area))
sales$Garage.Area[is.na(sales$Garage.Area)]<-mean(sales$Garage.Area,na.rm=TRUE)
sum(is.na(sales$Mas.Vnr.Area))
sales$Mas.Vnr.Area[is.na(sales$Mas.Vnr.Area)]<-mean(sales$Mas.Vnr.Area,na.rm=TRUE)
sum(is.na(sales$Garage.Area))
sum(is.na(sales$Mas.Vnr.Area))

#step 4: Checking the summary 
summary(sales)
#exploring some of the numeric variables
summary(sales[c("price", "area","Mas.Vnr.Area","Garage.Area", "X1st.Flr.SF")]) 
#For categorical variable, we use table() function
table(sales$Overall.Qual) 
table(sales$Year.Built) 
table(sales$Exter.Qual) 
table(sales$Neighborhood) 


# Histogram of prices
hist(sales$price,
     col = "red", breaks   = 50)
#Bar plot of the house built year
counts <- table(sales$Year.Built)
barplot(counts, main = "When were the most houses built?",
        xlab = "Year",
        ylab = "Number of houses",
        col ="violet")
#condition of the houses
barplot(table(sales$Overall.Cond), 
        main = "In what condition are the most houses on the market?", 
        xlab = "condition of houses",
        ylab = "Number of houses",
        col = "Orange")

#prices per neighborhood
neighbourhoods = tapply(sales$price, sales$Neighborhood, median)
neighbourhoods = sort(neighbourhoods, decreasing = TRUE)
dotchart(neighbourhoods, 
         bg = "purple1",
         xlab="Median price of a house",
         main = "Which neighborhood is the most expensive to buy a house in?")
#External quality oh house vs Price
externalQual = tapply(sales$price, sales$Exter.Qual, median)
externalQual = sort(externalQual, decreasing = TRUE)
dotchart(externalQual, 
         bg = "purple1",
         xlab="median price of a house",
         main = "Which external quality of house determine the price?")



# firstly, I need to pick only numerical variables
numeric = sales %>% select(where(is.numeric)) 
ggcorr(numeric)

#correlation coefficients between variables and prices
cor(sales$Enclosed.Porch, sales$price)
cor(sales$price, sales$PID)
cor(sales$price, sales$Overall.Qual)
cor(sales$price, sales$area)
cor(sales$price, sales$X1st.Flr.SF) #first floor sq.ft
cor(sales$price, sales$Full.Bath)
cor(sales$price, sales$TotRms.AbvGrd) #Total rooms above ground
cor(sales$price, sales$Year.Built)
cor(sales$price, sales$Mas.Vnr.Area)


# Let's see the correlation matrix, selecting only  important variables which have negative and postive relationship

new_sales = numeric %>% select(price, area, Open.Porch.SF, Wood.Deck.SF, Garage.Area, 
                               Garage.Cars, Garage.Yr.Blt, Fireplaces, TotRms.AbvGrd, Half.Bath, Full.Bath,
                               Bsmt.Full.Bath, Garage.Area, X2nd.Flr.SF, X1st.Flr.SF, BsmtFin.SF.1, 
                               Mas.Vnr.Area, Year.Built, Year.Remod.Add, Overall.Qual, Lot.Area, 
                               Lot.Frontage, Enclosed.Porch, PID)
ggcorr(new_sales, size = 3)

    
#Step 5: splitting the data into training and test
set.seed(2)
sample <- sample.split(sales, SplitRatio = 0.7)
train  <- subset(sales, sample == TRUE)
test   <- subset(sales, sample == FALSE)
dim(train)
dim(test)

#	Step 6: Separate the test labels from the test data
test_label <- test[ , 4]
test_label

#Step 7: Train the model. 
model = lm(price ~  Overall.Qual  + area + X1st.Flr.SF + Garage.Area + Mas.Vnr.Area + Year.Built + Exter.Qual+ Full.Bath , data = train)
summary(model)

#	Step 8: Make predictions
pred <- predict(model, test)
pred

#	Step 9: Compare the predicted and actual values. 
plot(test_label, type = "l", lty=1.8,  col="red") #red color for test price
lines(pred, type="l", col="blue")#blue color for predicted price

#Accuracy of the model(root mean square error)
sqrt(mean((pred-test_label)^2))


