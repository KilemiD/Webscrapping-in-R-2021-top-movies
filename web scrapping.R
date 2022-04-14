library(rvest)
library(readr)

#specifying the url
url=("https://www.imdb.com/search/title/?title_type=feature&release_date=2021-01-01,2021-12-31&count=100&ref_=adv_prv"
      )
url2=("https://www.imdb.com/search/title/?title_type=feature&release_date=2020-01-01,2020-12-31&count=100&start=101&ref_=adv_nxt")

#reading the html code from the website
webpage=read_html(url)

#title
rank <- html_nodes(webpage,".text-primary")%>%
  html_text()
head(rank)
#converting rank to numeric
rank_data=as.numeric(rank)
head(rank_data)

#getting the title
title=html_nodes(webpage,".lister-item-header a")%>%
  html_text()
head(title)
length(title)
#description
description=html_nodes(webpage,".ratings-bar+ .text-muted")%>%
  html_text()
head(description)
length(description)
#preprocessing, removing '\n'
description_data=gsub("\n","",description)
head(description_data)
length(description)
#movie runtime
runtime=html_nodes(webpage,".runtime")%>%
  html_text()
head(runtime)
length(runtime)
#removing minute and converting it to numerical
runtime_data=gsub(" min","",runtime)
runtime_data=as.numeric(runtime_data)
head(runtime_data)
length(runtime_data)
#genre
genre=html_nodes(webpage,".genre")%>%
  html_text()
head(genre)
length(genre)
#preprocessing, removing n
genre_data=gsub("\n","",genre)
#taking only the first genre
genre_data=gsub(",.*","",genre_data)
#converting each genre from text to factor
genre_data=as.factor(genre_data)
head(genre_data)
length(genre_data)
#IMDB rating section
rating=html_nodes(webpage,".ratings-imdb-rating strong")%>%
  html_text()
head(rating)
length(rating)
#converting rating to numerical
rating_data=as.numeric(rating)
head(rating_data)
length(rating_data)
#votes
votes=html_nodes(webpage,".sort-num_votes-visible span:nth-child(2)")%>%
  html_text()
head(votes)

#removing commas
votes_data=gsub(",","",votes)
#converting to numeric
votes_data=as.numeric(votes_data)
head(votes_data)
length(votes_data)
#movie directors
directors=html_nodes(webpage,".text-muted+ p a:nth-child(1)")%>%
  html_text()
head(directors)

#converting to factors
directors_data=as.factor(directors)
length(directors_data)
#actors
actors=html_nodes(webpage,".lister-item-content .ghost+ a")%>%
  html_text()
head(actors)
length(actors)
#converting to factors
actors_data=as.factor(actors)
head(actors_data)
length(actors_data)
#getting the metascore data
metascore=html_nodes(webpage,".metascore")%>%
  html_text()
head(metascore)
length(metascore)
View(metascore)
#preprocessing remove extra spaces
metascore_data=gsub(" ","",metascore)
View(metascore_data)
#check the length
length(metascore_data)
as.data.frame(metascore_data)

for (i in c(31,39,50,51,73,82,86,87,94)){
  a=metascore_data[1:(i-1)]
  
  b=metascore_data[i:length(metascore_data)]
  
  metascore_data=append(a,list("NA"))
  
  metascore_data=append(metascore_data,b)
}

#converting metascore to numerical
metascore_data=as.numeric(metascore_data)
head(metascore_data)
View(metascore_data)
length(metascore_data)
summary(metascore_data)
as.data.frame(metascore_data)
#gross earnings
gross=html_nodes(webpage,".ghost~ .text-muted+ span")%>%
  html_text()
head(gross)
length(gross)
#removing m and $ signs
gross_data=gsub("M","",gross)
gross_data=substring(gross_data,2,6)
head(gross_data)
#checking length
length(gross_data)
View(gross_data)
#filling lacking movies with NA
for (i in c(28,52,54,62,67,75,78,82,87,92)){
  a=gross_data[1:(i-1)]
  
  b=gross_data[i:length(gross_data)]
  
  gross_data=append(a,list("NA"))
  
  gross_data=append(gross_data,b)
}

#converting gross to numerical
gross_data=as.numeric(gross_data)
length(gross_data)
summary(gross_data)

#combininig all lists to form a data frame
movies2021_df=data.frame(Rank=rank_data,
                     Title=as.factor(title),
                     Description=as.factor(description_data),
                     Runtime=runtime_data,
                     Genre=genre_data,
                     Rating=rating_data,
                     Metascore=metascore_data,
                     Votes=votes_data,
                     Director=directors_data,
                     Actor=actors_data
                     )
str(movies2021_df)
View(movies2021_df)
#saving a csv file
setwd("C:/Users/John/OneDrive/webscraping in r")
write.csv(movies2021_df,"movies 2021.csv",row.names = F)


#analyzing the data
setwd("C:/Users/John/OneDrive/webscraping in r")
library(ggplot2)
movies_df=read.csv("movies 2021.csv",stringsAsFactors = T)
qplot(data=movies_df,Runtime,fill=Genre,bins=30)
summary(movies_df)
#continuing
ggplot(movies_df,aes(x=Runtime,y=Rating))+
  geom_point(aes(size=Votes,col=Genre))

#plot 3
ggplot(movies_df,aes(x=Runtime,y=Gross_Earning))+
  geom_point(aes(size=Rating,col=Genre))


