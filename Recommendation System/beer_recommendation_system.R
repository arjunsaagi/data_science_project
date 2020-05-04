
##Set working directory
#setwd("P:/Work/Analysis/UpGrade/Domain Elective/Assigment")

##Load library 
#install.packages(recommenderlab)
library(recommenderlab)

#install.packages(tidyverse)
library(tidyverse)

#install.packages(ggplot)
library(ggplot2)

##Load csv file containing beer data 
beer_data <- read.csv("beer_data.csv", stringsAsFactors = F)

##check the str
str(beer_data)

##View data
View(beer_data)


##duplicate check
##check if data is unique by beer_beerid and review_profilename
 beer_data %>% 
  group_by(beer_beerid,review_profilename) %>% 
  summarise(TotalCount =n()) %>%
  filter(TotalCount > 1) %>%
   head()

##1409 record are duplicate 
##check data for few sample 
beer_data %>%
  filter(beer_beerid==7 & review_profilename=='BEERchitect')

beer_data %>%
  filter(beer_beerid==10 & review_profilename=='ABOWMan')

beer_data %>%
  filter(beer_beerid==17 & review_profilename=='granger10') 

##same user have given multiple rating for same beer_id, 
##let consider max rating as valid and remove other entry

##Arrange the data by beer_beerid,review_profilename,desc(review_overall) for creating rank
beer_data <- beer_data %>% arrange(beer_beerid,review_profilename,desc(review_overall))

##Create new flag column by grouping beer_beerid,review_profilename
beer_data <- beer_data %>%
  group_by(beer_beerid,review_profilename) %>%
  mutate(flag = row_number())


##keep the record where flag ==1 and select only 3 columns
beer_data <- beer_data %>% 
  filter(flag==1) %>% 
  select(beer_beerid,review_profilename,review_overall)

##check for duplicate again.
beer_data %>% 
  group_by(beer_beerid,review_profilename) %>% 
  summarise(TotalCount =n()) %>%
  filter(TotalCount > 1) %>%
  head()
##No record found

##Check for null value
sum(is.na(beer_data))
##No record found

##check unique beer count
length(unique(beer_data$beer_beerid))

##40308 are unique beer

##check unique profile_name
length(unique(beer_data$review_profilename))

##22498 are unique profile



#######################1. Data preparation ###############

##1.1 Choose only those beers that have at least N number of reviews

##check number of review per beer
review_per_beer <-beer_data %>% group_by(beer_beerid) %>%
  summarise(TotalCount = n()) %>%
  arrange(desc(TotalCount))

summary(review_per_beer$TotalCount)
##Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##1.00    1.00    2.00   11.81    5.00  987.00 

##Check number of beer having rating more than by 11 user

Unique_beer_filter <- review_per_beer %>%
                      filter(TotalCount>11) 

nrow(Unique_beer_filter)

##6150 out of 40308 beer have been rated by more than 11 user   
##Lets select Mean value which 11, 
##let keep only beer data which have been rated by more than 11 users
##So Value of N is 12

##filter the beer_data which should contain only beer_id present in Unique_beer_filter
beer_data_filtered <- beer_data %>%
  inner_join(Unique_beer_filter, by="beer_beerid") %>%
  select(review_profilename,beer_beerid,review_overall)

##Please note that order of column is  user/item/rating which will help later to create realratingMatrix

##check number of row left
nrow(beer_data_filtered)
##392245

##check few rows
head(beer_data_filtered)

##1.2 Convert this data frame to a "realratingMatrix" 
##before you build your collaborative filtering models

##check the object type
str(beer_data_filtered)
##convert to data frame first
beer_data_filtered <- as.data.frame(beer_data_filtered)

##convert to realRatingMatrix
beer_data_filtered.matrix<- as(beer_data_filtered,"realRatingMatrix")

## check object type 
class(beer_data_filtered.matrix)



##################2. Data Exploration ###############

##2.1 Determine how similar the first ten users are 
##    with each other and visualise it
set.seed(1)

similar_users <- similarity(beer_data_filtered.matrix[1:10,],
                            method = "cosine",
                            which = "users")


#Similarity matrix
as.matrix(similar_users)

#Visualise similarity matrix
image(as.matrix(similar_users), main = "User similarity")

##5th user is simmilar to 1st, 2nd and 8th user vice versa 
##1st and 2nd is simillar to each other
##1st and 8th user is  simillar to each other

##2.2 Compute and visualise the similarity between the first 10 beers

similar_beers <- similarity(beer_data_filtered.matrix[,1:10 ],
                            method = "cosine",
                            which = "items")

#Similarity matrix
as.matrix(similar_beers)

#Visualise similarity matrix
image(as.matrix(similar_beers), main = "Beer similarity")

## 2nd and 10th beer is simillar to each other

##2.3 What are the unique values of ratings?

unique(getRatings(beer_data_filtered.matrix))
## Unique values are 4.0, 3.5, 4.5, 3.0, 2.5, 1.0, 5.0, 2.0, 1.5

##2.4 Visualise the rating values and notice:
#The average beer ratings

col_mean<-  colMeans(beer_data_filtered.matrix)
qplot(col_mean) + stat_bin(binwidth = 1) + ggtitle("Distribution of beer rating")
  
##Most of bear got average rating of 4, there is no rating for 1, 

#The average user ratings
row_mean<-  rowMeans(beer_data_filtered.matrix)
qplot(row_mean) + stat_bin(binwidth = 1) + ggtitle("Distribution of user rating")

##Most of user gave average rating of 4,

#The average number of ratings given to the beers
col_Counts<-  colCounts(beer_data_filtered.matrix)
qplot(col_Counts) + stat_bin(binwidth = 30) + ggtitle("Distribution of beer rating count")

##Look like avg number of rating given to beer are near to 2800

#The average number of ratings given by the users
row_Counts<-  rowCounts(beer_data_filtered.matrix)
qplot(row_Counts) + stat_bin(binwidth = 30) + ggtitle("Distribution of user rating count")

##Look like avg number of rating given by user are near to 15900


################3. Recommendation Models #############
##3.1 Divide your data into training and testing datasets

##Let also filter the data such that 
beer_data_filtered.matrix <- beer_data_filtered.matrix[rowCounts(beer_data_filtered.matrix) > 50,colCounts(beer_data_filtered.matrix) > 50] 

which_train <- sample(x = c(TRUE, FALSE), size = nrow(beer_data_filtered.matrix), replace = TRUE, prob = c(0.8, 0.2))
beer_data_filtered.matrix_train <- beer_data_filtered.matrix[which_train, ] 
beer_data_filtered.matrix_test <- beer_data_filtered.matrix[!which_train, ] 


##3.2 Build IBCF and UBCF models

IBCF_model <- Recommender(data = beer_data_filtered.matrix_train, method = "IBCF", parameter = list(k = 10))

n_recommended <- 5

recc_predicted <- predict(object = IBCF_model, newdata = beer_data_filtered.matrix, n = n_recommended) 
recc_predicted
class(recc_predicted)
slotNames(recc_predicted)

recc_predicted@items$giblet 

UBCF_model <- Recommender(data = beer_data_filtered.matrix_train, method = "UBCF") 
UBCF_model

recc_predicted <- predict(object = UBCF_model,  newdata = beer_data_filtered.matrix_test, n = n_recommended)

recc_matrix <- sapply(recc_predicted@items, function(x){   colnames(beer_data_filtered.matrix)[x] }) 
dim(recc_matrix)

recc_matrix[, c("giblet")]

# run algorithms, predict next n movies
results <- evaluate(scheme, algorithms, n=c(1, 3, 5, 10, 15,20))
class(results)
results
# Draw ROC curve
plot(results, annotate = 1:4, legend="topleft")


percentage_training <- 0.8
min(rowCounts(beer_data_filtered.matrix)) 

items_to_keep <- 35
rating_threshold <- 3 
n_eval <- 1 
eval_sets <- evaluationScheme(data = beer_data_filtered.matrix, method = "split"
                              , train = percentage_training
                              , given = items_to_keep
                              , goodRating = rating_threshold, k = n_eval) 
eval_sets
getData(eval_sets, "known")
getData(eval_sets, "unknown")

getData(eval_sets, "train")
getData(eval_sets, "known")

model_to_evaluate <- "IBCF" 
model_parameters <- NULL

eval_recommender <- Recommender(data = getData(eval_sets, "train")
                                , method = model_to_evaluate
                                , parameter = model_parameters)

models_to_evaluate <- list(  IBCF_cos = list(name = "IBCF", param = list(method =    "cosine")),   IBCF_cor = list(name = "IBCF", param = list(method =    "pearson")),   UBCF_cos = list(name = "UBCF", param = list(method =    "cosine")),   UBCF_cor = list(name = "UBCF", param = list(method =    "pearson")),   random = list(name = "RANDOM", param=NULL) )

n_recommendations <- c(1, 5, seq(10, 100, 10))

list_results <- evaluate(x = eval_sets, method = models_to_evaluate, n = n_recommendations) 
class(list_results) 

plot(list_results, annotate = 1, legend = "topleft") +title("ROC curve")
