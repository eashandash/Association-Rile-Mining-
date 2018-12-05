## Importing packages

# This R environment comes with all of CRAN and many other helpful packages preinstalled.
# You can see which packages are installed by checking out the kaggle/rstats docker image: 
# https://github.com/kaggle/docker-rstats

library(tidyverse) # metapackage with lots of helpful functions

## Running code

# In a notebook, you can run a single code cell by clicking in the cell and then hitting 
# the blue arrow to the left, or by clicking in the cell and pressing Shift+Enter. In a script, 
# you can run code by highlighting the code you want to run and then clicking the blue arrow
# at the bottom of this window.

## Reading in files

# You can access files from datasets you've added to this kernel in the "../input/" directory.
# You can see the files added to this kernel by running the code below. 

list.files(path = "../input")


library(dplyr) #data manipulation
library(readr) #input/output
library(data.table) #data manipulation
library(stringr) #string manipulation
library(caret)  #model evaluation (confusion matrix)
library(tibble) #data wrangling
library("ROSE") #over/under sampling
library("randomForest") #random forest model building
library(pROC) #ROC plots
library("MLmetrics") #Normalized Gini
library(cowplot)
library(arules)
library(arulesViz)
library(methods)


data<- read.csv('../input/data.csv')
head(data)
mydata <- data %>%
            select(Country,InvoiceDate, CustomerID)

missing_values <- as.data.frame(colSums(is.na(mydata)))
missing_values["var"] <- row.names(missing_values)
missing_values =  melt( missing_values, id.vars="var", value.name="no_of_NA")
missing_values <- missing_values[missing_values$no_of_NA > 0, ]
missing_values

row.has.na <- apply(mydata, 1, function(x){any(is.na(x))})
final.filtered <- mydata[!row.has.na,]
head(final.filtered)


final.filtered$CustomerID <- as.factor(final.filtered$CustomerID)

tData <- as(final.filtered, "transactions")

#FrequentItems_con <- eclat (tData, parameter = list(supp = 0.005, maxlen = 15)) # calculates support for frequent items
rules_con <- apriori (tData, parameter = list(supp = 0.00001, conf = 0.1)) # Min Support as 0.01, confidence as 0.8.
rules_conf_con <- sort (rules_con, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
# show the support, lift and confidence for all rules

inspect(head(rules_conf_con,150))

#FP growth starts
library("rCBA")

new_df <- data %>%
            select(Quantity,UnitPrice , Country)
new_df <- new_df %>%
              filter(Country %in% c('Australia', 'Belgium'))
head(new_df)


train <- sapply(new_df,as.factor)
train <- data.frame(train, check.names=FALSE)


head(train)

txns <- as(train,"transactions")

rules = rCBA::fpgrowth(txns, support=0.03, confidence=0.03, maxLength=3, consequent="Country",
           parallel=FALSE)

head(inspect(rules))







           





















