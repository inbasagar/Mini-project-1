#library importing
library(recommenderlab)
library(reshape2)
library(reshape)
library(plyr)
library(data.table)
library(ggplot2)
library(DT)


#retriving data
movie_data<-read.csv("C:/Users/Inbasagar/Documents/R project/movies.csv",stringsAsFactors =FALSE)
rating_data<-read.csv("C:/Users/Inbasagar/Documents/R project/ratings.csv")

str(movie_data)
str(rating_data)

datatable(movie_data)
datatable(rating_data)

movie_genre<-as.data.frame(movie_data$genres,stringsAsFactors = FALSE)
movie_genre2 <- as.data.frame(tstrsplit(movie_genre[,1], "[|]", 
                                     type.convert=TRUE), 
                           stringsAsFactors=FALSE)
colnames(movie_genre2)<-c(1:10)
list_genre<-c("Action", "Adventure", "Animation", "Children", 
               "Comedy", "Crime","Documentary", "Drama", "Fantasy",
               "Film-Noir", "Horror", "Musical", "Mystery","Romance",
               "Sci-Fi", "Thriller", "War", "Western")
genre_mat1<-matrix(0,10330,18)
genre_mat1[1,]<-list_genre
colnames(genre_mat1) <- list_genre

for (index in 1:nrow(movie_genre2)) {
  for (col in 1:ncol(movie_genre2)) {
    gen_col=which(genre_mat1[1,]== movie_genre2[index,col])
    genre_mat1[index+1,gen_col]<-1
    }
  
}
str(genre_mat1)
genre_mat2<-as.data.frame(genre_mat1[-1,],stringsAsFactors = FALSE)
for (col in 1:ncol(genre_mat2)) {
  genre_mat2[,col]<-as.integer(genre_mat2[,col])
} 
str(genre_mat2)

#head(movie_data)

searchmovie<-cbind(movie_data[,1:2],genre_mat2[])
head(searchmovie)

ratingmatrix<-dcast(rating_data,userId~movieId,value.var="rating",na.rm=FALSE)
ratingmatrix<-as.matrix(ratingmatrix[,-1])
ratingmatrix<-as(ratingmatrix,"realRatingMatrix")
#recommendation

recommendation_model<-recommenderRegistry$get_entries()
names(recommendation_model) 

lapply(recommendation_model,"[[","description") 
#we will use cf

recommendation_model$IBCF_realRatingMatrix$parameters

#lets check similarity
similarity_mat<-similarity(ratingmatrix[1:4,],method = "cosine",which="users")
as.matrix(similarity_mat)
image(as.matrix(similarity_mat),main="user's similarity") 

movie_similarity<-similarity(ratingmatrix[,1:4],method = "cosine",which="items")
as.matrix(movie_similarity)
image(as.matrix(movie_similarity),main="movie similarity") 

#rating values
rating_values<-as.vector(ratingmatrix@data)
unique(rating_values)

#how much rating as count os numbers
table_rating<-table(rating_values)
table_rating

#most viewd movies visualization

movie_views<-colCounts(ratingmatrix)
table_views<-data.frame(movie=names(movie_views),views=movie_views)
table_views<-table_views[order(table_views$views,decreasing =TRUE),]
table_views$title<-NA
for (index in 1:10325) 
{table_views[index,3]<-as.character(subset(movie_data,movie_data$movieId==table_views[index,1])$title)

}
table_views[1:6,]

#plotting this data
ggplot(table_views[1:6,],aes(x=title,y=views))+
  geom_bar(stat = "identity",fill="steelblue")+
  theme(axis.text.x=element_text(angle = 45,hjust = 1))+
  ggtitle("total views of the top films")

#heatmap of rating matrix
#u can enter matrix likr 1:30 or 1:668 ur choice
image(ratingmatrix[1:30,1:30],axes=FALSE,main="30X30 heatmap")

#lots of sparse data

#now we will
#1.select usefull data
#2.normalize it
#3.binarize it

#you have seen the rating dataset ok so what do you think how many user needs to
#rate a movie to be useful lets say 50

movie_ratings<-ratingmatrix[rowCounts(ratingmatrix)>50,colCounts(ratingmatrix)>50]
movie_ratings

minimum_movies<-quantile(rowCounts(movie_ratings),0.98)
minimun_user<-quantile(colCounts(movie_ratings),0.98)
#quantile isThe word comes from the word quantity. In simple terms, a quantile is where a sample is divided into equal-sized, adjacent,
image(movie_ratings[rowCounts(movie_ratings)>minimum_movies,colCounts(movie_ratings)>minimun_user],main="heatmap of the top users and movies")

#aveerage rating /user
average_ratings<-rowMeans(movie_ratings)
ggplot(average_ratings,fill=I("black"),col=I("blue"))+
  ggtitle("distribution of average rating per user")

#normalize data
normalize_ratings<-normalize(movie_ratings)
sum(rowMeans(normalize_ratings)>0.000001)

#heatmap of normalized data
image(normalize_ratings [rowCounts(normalize_ratings)>minimum_movies,colCounts(normalize_ratings)>minimun_user])

binary_minimum_movies <- quantile(rowCounts(movie_ratings), 0.95)
binary_minimum_users <- quantile(colCounts(movie_ratings), 0.95)
#movies_watched <- binarize(movie_ratings, minRating = 1)

good_rated_films <- binarize(movie_ratings, minRating = 3)
image(good_rated_films[rowCounts(movie_ratings) > binary_minimum_movies,
                       colCounts(movie_ratings) > binary_minimum_users],
      main = "Heatmap of the top users and movies")

#collaborative filtering system
sampled_data<-sample(x=c(TRUE,FALSE),size = nrow(movie_ratings),replace=TRUE,prob = c(0.8,0.2))
training_data<-movie_ratings[sampled_data,]
testing_data<-movie_ratings[sampled_data,]

#recommendation system
 
recommendation_system<-recommenderRegistry$get_entries(dataType="realRatingMatrix")
recommendation_system$IBCF_realRatingMatrix$parameters

recommen_model<-Recommender(data=training_data,method="IBCF",parameter=list(k=30))

recommen_model

class(recommen_model)
info<-getModel(recommen_model)
class(info$sim)


#we will carry out the sum of rows and colms with similarity of the objs above


#lets recommend

top_recommendations<-5
predicted_recommendations<-predict(object = recommen_model,
                                   newdata=testing_data,
                                   n=top_recommendations)
predicted_recommendations


user1<-predicted_recommendations@items[[1]]
movie_user1<-predicted_recommendations@itemLabels[user1]
movie_user2<-movie_user1
for (index in 1:10) {
  movie_user2[index]<-as.character(subset(movie_data,
                                   movie_data$movieId==movie_user1[index])$title)
  
}
movie_user2

