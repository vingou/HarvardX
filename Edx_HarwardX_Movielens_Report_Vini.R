## ------------------------------------------------------------------------
# checks if the packages have been previously installed. if not, install them. 
list.of.packages <- c("lubridate", "caret","dplyr","tidyverse","kableExtra","tidyr","stringr","forcats","ggplot2","stringr","RSQLite","sqldf","corrplot","gridExtra","grid")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


## ----echo = FALSE, include=FALSE-----------------------------------------
library(lubridate)
library(caret)
library(dplyr)
library(tidyverse)
library(kableExtra)
library(tidyr)
library(stringr)
library(forcats)
library(ggplot2)
library(stringr)
library(RSQLite)
library(sqldf)
library(corrplot)
library(gridExtra)
library(grid)




## ----RMSE_function1, echo = TRUE-----------------------------------------
RMSE <- function(predicted_ratings, true_ratings){
  sqrt(mean((predicted_ratings - true_ratings)^2))
}


## ----EDX, include=FALSE--------------------------------------------------
################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
     semi_join(edx, by = "movieId") %>%
     semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)


## ----summary, echo = FALSE-----------------------------------------------

summary(edx)


## ------------------------------------------------------------------------
edx <- mutate(edx, year_rated = year(as_datetime(timestamp)))



## ---- echo = FALSE-------------------------------------------------------
edx %>% summarize(Users = n_distinct(userId),
              Movies = n_distinct(movieId)) 



## ----rating_distribution, echo = FALSE-----------------------------------
edx %>%
  ggplot(aes(rating)) +
  geom_histogram(binwidth = 0.25, color = "black") +
  scale_x_discrete(limits = c(seq(0.5,5,0.5))) +
  scale_y_continuous(breaks = c(seq(0, 2500000, 500000))) +
  ggtitle("Rating distribution")
  


## ----echo = FALSE--------------------------------------------------------
rating_cnt_per_member <- edx %>% group_by(userId) %>% summarise(rating_cnt=n())
rating_cnt_per_member <- rating_cnt_per_member %>% filter(rating_cnt>10 & rating_cnt < 500)
ggplot(rating_cnt_per_member, aes(x=rating_cnt))+geom_histogram(binwidth = 20)+xlab("Moving rating count")+ylab ("number of users")+ggtitle("Distribution of count of number of ratings by users")


## ----number_of_ratings_per_movie, echo = TRUE----------------------------
edx %>%
count(movieId) %>%
ggplot(aes(n)) +
geom_histogram(bins = 30, color = "black") +
scale_x_log10() +
xlab("Number of ratings") +
  ylab("Number of movies") +
ggtitle("Number of ratings per movie")


## ----movies_rated_per_year-----------------------------------------------

movies_per_year <- edx %>%
  select(movieId, year_rated) %>% # select columns 
  group_by(year_rated) %>% # group by year
  summarise(count = n()) %>%  # count movies per year
 arrange(year_rated)
  
movies_per_year %>%
  ggplot(aes(x = year_rated, y = count)) +
  geom_line()
 



## ----echo = FALSE , include=FALSE----------------------------------------

dat <-  edx %>% separate_rows(genres, sep ="\\|")
head(dat)

genre_count_by_movieId <- dat %>% group_by(movieId, genres) %>% summarize(n = n())
head(genre_count_by_movieId)

number_of_genres <- dat %>% group_by(genres) %>% summarize(n = n())
number_of_genres

genre_list <- number_of_genres$genres
genre_list



temp <- dat %>%
  group_by(genres) %>%
  summarize(n=n()) %>%
  ungroup() %>%
  mutate(sumN = sum(n), percentage = n/sumN) %>%
  arrange(-percentage)

temp %>%
  ggplot(aes(reorder(genres, percentage), percentage, fill= percentage)) +
  geom_bar(stat = "identity") + coord_flip() +
  labs(y = "Percentage", x = "Genre") +
  ggtitle("Distribution of Genres by Percent Rated")


## ----number_ratings_given_by_users, echo = TRUE, fig.height=4, fig.width=5----
edx %>%
count(userId) %>%
ggplot(aes(n)) +
geom_histogram(bins = 30, color = "black") +
scale_x_log10() +
xlab("Number of ratings") + 
ylab("Number of users") +
ggtitle("Number of ratings given by users")


## ----Mean_movie_ratings_given_by_users, eval=FALSE, include=FALSE--------
## edx %>%
##   group_by(userId) %>%
##   filter(n() >= 1000) %>%
##   summarize(b_u = mean(rating)) %>%
##   ggplot(aes(b_u)) +
##   geom_histogram(bins = 30, color = "black") +
##   xlab("Mean rating") +
##   ylab("Number of users") +
##   ggtitle("Mean movie ratings given by users") +
##   scale_x_discrete(limits = c(seq(0.5,5,0.5))) +
##   theme_light()
## 


## ------------------------------------------------------------------------

#Is there a correlation

#Number of movie ratings per movie
n_movies_ratings <- edx %>% group_by(movieId) %>% summarize(n = n())

#Average Movie Rating for each movie
avg_movie_rat <- edx %>% group_by(movieId) %>% summarize(avg_m_r = mean(rating))

#Create correlation data
cor_dat <- edx %>% select(rating, movieId, userId, year_rated)  %>%
  left_join(n_movies_ratings, by = "movieId") %>%
  left_join(avg_movie_rat, by = 'movieId')
temp <- cor_dat %>% select(one_of("rating", "movieId", "userId", "year_rated"
                                               )) %>% as.matrix()
M <- cor(temp, use = "pairwise.complete.obs")

corrplot(M, method = "circle", order = "hclust")




## ------------------------------------------------------------------------
get_cor <- function(df){
  m <- cor(df$x, df$y);
  eq <- substitute(italic(r) == cor, list(cor = format(m, digits = 2)))
  as.character(as.expression(eq));
}

#Number of ratings vs average movie ratings
cor_dat %>%
  ggplot(aes(n, avg_m_r)) + stat_bin_hex(bins = 50) + scale_fill_distiller(palette = "Spectral") +
  stat_smooth(method = "glm",  size = 1) +
  annotate("text", x = 20000, y = 2.5, label = get_cor(data.frame(x = cor_dat$n, y = cor_dat$avg_m_r)), 
           parse = TRUE, size = 7) + ylab("Average Movie Ratings") + xlab("Number of Ratings") 


## ---- echo = TRUE--------------------------------------------------------
mu <- mean(edx$rating)
mu_result <- tibble(Method = "Average movie rating ",  mu)
mu_result %>% knitr::kable()


## ----naive_rmse, echo = TRUE---------------------------------------------
model_1_rmse <- RMSE(validation$rating, mu)




## ----rmse_results1, echo = TRUE------------------------------------------
rmse_results <- tibble(method = "Model I - Naive Model", RMSE = model_1_rmse)
kable(rmse_results) %>%
  kable_styling(bootstrap_options = c("striped",  full_width = F))



## ----Number_of_movies_with_the computed_b_i, echo = TRUE, fig.height=3, fig.width=4----
movie_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))
movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"),
ylab = "Number of movies", main = "Number of movies with the computed b_i")


## ----predicted_ratings, echo = TRUE--------------------------------------
predicted_ratings <- mu +  validation %>%
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)
model_2_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                         tibble(method="Movie II - Effect model",  
                                     RMSE = model_2_rmse ))

kable(rmse_results) %>%
  kable_styling(bootstrap_options = c("striped",  full_width = F))


## ---- echo = TRUE--------------------------------------------------------
edx %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black")


## ----user_avgs, echo = TRUE----------------------------------------------
user_avgs <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))
  


## ----model_3_rmse--------------------------------------------------------
predicted_ratings <- validation%>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)
model_3_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Model III - Movie and user effect model",  
                                     RMSE = model_3_rmse))

kable(rmse_results) %>%
  kable_styling(bootstrap_options = c("striped",  full_width = F))




## ----lambdas, echo = TRUE------------------------------------------------
lambdas <- seq(0, 5, 0.25)
model_4_rmse <- sapply(lambdas, function(l){
  
  mu <- mean(edx$rating)
  
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- 
    edx %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, edx$rating))
})


## ----plot_lambdas, echo = TRUE-------------------------------------------
qplot(lambdas, model_4_rmse)  


## ----min_lambda, echo = TRUE---------------------------------------------
  best_lambda <- lambdas[which.min(model_4_rmse)]
best_lambda


## ----rmse_results4,  results='asis'--------------------------------------
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Model  IV - Regularized movie and user effect model",  
                                     RMSE = min(model_4_rmse)))
kable(rmse_results) %>%
  kable_styling(bootstrap_options = c("striped",  full_width = F))



## ------------------------------------------------------------------------
mu <- mean(validation$rating)
l <- best_lambda

b_i <- validation %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n() + l))
  
b_u <- validation %>%
    left_join(b_i, by='movieId') %>% 
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n() +l))
  
predicted_ratings <- validation %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i +  b_u) %>% .$pred
RMSE(predicted_ratings, validation$rating)


## ----rmse_results_all, echo = FALSE--------------------------------------

rmse_results <- bind_rows(rmse_results,
                          data_frame(method=
                            "Model  IV - Regularized movie and user effect model on VALIDATION SET",  
                            RMSE = RMSE(predicted_ratings, validation$rating)))

kable(rmse_results) %>%
  kable_styling(bootstrap_options = c("striped",  full_width = F))



## ------------------------------------------------------------------------
print("Operating System:")
version

