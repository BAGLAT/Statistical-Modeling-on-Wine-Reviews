install.packages("ISLR")
install.packages("SmartEDA")
library(dplyr)
library(ggplot2)
library(tidyverse)
library("ISLR")
library("SmartEDA")

##Reading of the data
Data<- read.csv("winemag-data-130k-v2.csv")
Data<-filter(Data, country == "US")
colSums(is.na(Data))
Data <- na.omit(Data)
glimpse(Data)
Data.describe()
typeof(Data)
ExpData(data=Data,type=2)
Data <- Data%>%extract(title,'year',"(20\\d\\d)",convert = T,remove = F)%>%mutate(year=ifelse(year<1900,NA,year))

Data$wordcount <- sapply(gregexpr("\\S+", Data$description), length)
summary(Data$wordcount)
Data$description[which(Data$wordcount == 135)]
Data$description[which(Data$wordcount == 3)]
ggplot(data = Data, aes(x= wordcount))+
  geom_histogram(binwidth = 3)+
  labs(x = "Word Count", y= "Frequency", title = "Distribution of word count of description")
cor(Data$points, Data$wordcount)

wine_dataset_US<-select (Data,-c(country, taster_twitter_handle, designation, description, title, X))

######encoding

encode_ordinal <- function(x, order = unique(x)) {
  x <- as.numeric(factor(x, levels = order, exclude = NULL))
  #x
}

wine_dataset_US$province <- encode_ordinal(wine_dataset_US[["province"]])
wine_dataset_US$region_1<-encode_ordinal(wine_dataset_US[["region_1"]])
wine_dataset_US$region_2<-encode_ordinal(wine_dataset_US[["region_2"]])
wine_dataset_US$variety<-encode_ordinal(wine_dataset_US[["variety"]])
wine_dataset_US$winery<-encode_ordinal(wine_dataset_US[["winery"]])
wine_dataset_US$taster_name<-encode_ordinal(wine_dataset_US[["taster_name"]])
wine_dataset_US$year<-encode_ordinal(wine_dataset_US[["year"]])


wine_dataset_US$points<-as.numeric(wine_dataset_US[["points"]])
wine_dataset_US$wordcount<-as.numeric(wine_dataset_US[["wordcount"]])
wine_dataset_US$reviewcount<-as.numeric(wine_dataset_US[["reviewcount"]])
wine_dataset_US$price<-as.numeric(wine_dataset_US[["price"]])


glimpse(wine_dataset_US)
sapply(wine_dataset_US, class)

##correlation matrix
cor(subset(wine_dataset_US, select=c(points,price,province,region_1,region_2,taster_name,variety,winery)))
m<-cor(subset(wine_dataset_US, select=c(points,price,province,region_1,region_2,taster_name,variety,winery,wordcount,year,reviewcount)))
cor(subset(wine_dataset_US, select=c(points,wordcount,year,reviewcount)))

##plot correlation of two variables wordcount and price
p1<- wine_dataset_US %>%
  filter(!is.na(price)) %>%
  ggplot(aes(x = log(price), y = points))+
  geom_point(size = 2,alpha = .3)+
  geom_smooth(method = 'lm')+
  annotate('text', x = 2, y = 100, label = 'Cor = .453')+
  labs(title = 'Price vs Points Awarded' )

p2<- wine_dataset_US %>%
  filter(!is.na(price)) %>%
  ggplot(aes(x = wordcount, y = points))+
  geom_point(seize = 2, alpha = .3)+
  geom_smooth(method = 'lm')+
  annotate('text', x = 2, y= 100, label = 'Cor = .637')+
  labs(title = "Description word Length vs points")

grid.arrange(p1, p2)

##plot dependency of quality of words on their ratings
mean_point<- mean(wine_dataset_US$points) %>% round(2)

tib1<- wine_dataset_US %>%
  filter(str_detect(word, "[:alpha:]+")) %>%
  group_by(word) %>%
  summarize(mean = mean(points), num = n(), sd = sd(points)) %>%
  filter(num > 50) %>%
  arrange(desc(mean)) %>% head(14)

tib2<- wine_dataset_US %>%
  filter(str_detect(word, "[:alpha:]+")) %>%
  group_by(word) %>%
  summarize(mean = mean(points), num = n(), sd = sd(points)) %>%
  filter(num > 50) %>%
  arrange(mean) %>% head(14)

full<- rbind(tib1, tib2)

full %>%
  mutate( num_words = str_to_title(paste0(word ," ", '(',num,')'))) %>%
  ggplot(aes(x = reorder(num_words,mean), y = mean))+
  geom_point(aes(colour = mean > mean_point), show.legend = F, size = 3)+
  geom_hline(yintercept = mean_point, lty = 3)+
  geom_segment(aes(y = mean_point, x = num_words, yend = mean, xend = num_words, colour = mean> mean_point),
               show.legend = F, linetype = 1, size = 1.3)+
  coord_flip()+
  scale_colour_manual(values = c('red','green'))+
  theme_minimal()+
  labs(x = '',
       y= "Average Points",
       title = 'What words are used Vs Ratings',
       subtitle = 'Frequency of words in bracket'
  )


##plot overall correlation
library(corrplot)
library(RColorBrewer)
corrplot(m, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))

ggplot(data = wine_dataset_US, aes(x=wordcount, y=points))+
  geom_point()

ggplot(wine_dataset_US, aes(x = points,
                            y = wordcount)) +
  geom_point(position = "jitter", alpha = 0.1) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Number of Words in Description Against Wine Rating",
       x = "Rating",
       y = "Words in description",
       caption = "Points score is from 80 - 100")

##to check distribution of points
ggplot(data = wine_dataset_US, aes(x= points, colour = I('black'), fill = I('#d9d209')))+
  geom_histogram(binwidth = 1)+
  labs(x = "Points", y= "Frequency", title = "Distribution of points")

#wine_dataset_US[, .N, by=.("Points", Bin)]
install.packages('sqldf') 
library(sqldf)
countdata<-sqldf("SELECT  Points, COUNT(*) AS Count FROM wine_dataset_US GROUP BY Points")
wine_dataset_US$revcount<-countdata$Count(where())
# #install.packages("PerformanceAnalytics", dependencies = TRUE)
# library("PerformanceAnalytics")
# wine_dataset_US1<-select (wine_dataset_US,c(points,price,province,taster_name,variety,winery,wordcount,year))
# chart.Correlation(wine_dataset_US1, histogram=TRUE, pch=19)

##linear regression model 1
#lm_model1 <- lm(points ~ price+province+region_1+region_2+taster_name+year+variety+winery+wordcount, data = wine_dataset_US)
lm_model1 <- lm(points ~ price+province+region_1+region_2+taster_name+variety+winery, data = wine_dataset_US)
summary(lm_model1)
plot(lm_model1)

##linear regression model 2
lm_model2 <- lm(points ~ log(price)+province+taster_name+year+variety+winery+log(wordcount)+region_1+region_2+reviewcount, data = wine_dataset_US)
summary(lm_model2)
AIC(lm_model1)
step_AIC_backward <- step(lm_model1)

##linear regression model 3
lm_model3 <- lm(points ~ (log(wordcount)+log(price))^2+variety+taster_name*province+winery+year, data = wine_dataset_US)
summary(lm_model3)
plot(lm_model3)



##Applying stepwise linear regression (AIC forward)
step_AIC_forward <-step(lm(points~1, data = wine_dataset_US), direction = "forward", scope =list(upper = lm_model1))
summary(step_AIC_forward)
step_AIC_forward

step_AIC_backward <- step(lm_model1)
summary(step_AIC_backward)


lm3 <- lm(points ~ log(price), data = wine_dataset_US)
summary(lm3)
lm4 <- lm(points ~ province, data = Data)
summary(lm4)
lm5 <- lm(points ~ region_1, data = wine_dataset_US)
summary(lm5)
lm6 <- lm(points ~ region_2, data = wine_dataset_US)
summary(lm6)
lm7 <- lm(points ~ I(taster_name^2), data = wine_dataset_US)
summary(lm7)
lm8 <- lm(points ~ year, data = wine_dataset_US)
summary(lm8)
lm9 <- lm(points ~ variety, data = wine_dataset_US)
summary(lm9)
lm10 <- lm(points ~ I(winery^2), data = wine_dataset_US)
summary(lm10)
lm11 <- lm(points ~ log(wordcount), data = wine_dataset_US)
summary(lm11)







install.packages("corrplot")
library(corrplot)
library(RColorBrewer)
M <-cor(subset(wine_dataset_US, select=c(points,wordcount,price,variety,taster_name,province,winery,year)))
corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))

