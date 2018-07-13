library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(highcharter)
library(ggplot2)
library(corrplot)

library(arules)
library(arulesViz)
#reading data----------------


movies1 = read_lines("../data/movies.dat") %>% 
  str_replace_all("::", "\036") %>%
  paste(collapse = "\n") %>%
  read_delim(delim = "\036", escape_double = F, trim_ws = T,
             col_names = c('MovieID', 'Title', 'Genres'))




tag = read_delim("../data/tags.dat", "::",
                 col_names = c("UserID", NA, "MovieID", NA,"Tag",NA, "Timestamp"))

tag %>% select(1,3,5,7) -> tag

rating = read_delim("../data/ratings.dat", "::",
                    col_names = c("UserID", NA, "MovieID", NA,"Rating", NA, "Timestamp")
)

rating %>% select(1,3,5,7) -> rating


#1---------------


#mahboob tarin film : ---------
rating %>% 
  select(MovieID, Rating) %>% 
  group_by(MovieID) %>% 
  summarise(averageRating = mean(Rating), number = n()) %>% 
  filter(number > 10000) %>% 
  arrange(desc(averageRating)) %>% 
  head(1) %>% 
  full_join(movies1) %>% 
  drop_na() %>% 
  select(Title, averageRating)


#bishtarin nazarat-------------
tag %>% 
  group_by(MovieID) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  head(1) %>% 
  full_join(movies1) %>% 
  drop_na() %>% 
  select(Title)

#manfoor tarin film-------------
rating %>% 
  select(MovieID, Rating) %>% 
  group_by(MovieID) %>% 
  summarise(averageRating = mean(Rating), number = n()) %>% 
  filter(number > 10000) %>% 
  arrange((averageRating)) %>% 
  head(1) -> most_hated_movie_title

movies1 %>% 
  filter(MovieID == most_hated_movie_title$MovieID)

tag %>% 
  filter(MovieID == most_hated_movie_title$MovieID) -> comments_about_the_most_hated_movie

library(wordcloud2)
comments_about_the_most_hated_movie %>% group_by(Tag) %>% summarise(n = n()) %>%
  arrange(desc(n)) %>% head(200) -> hated_comments
wordcloud2(frequnct_dickens_words, size = 0.5, shape = "star")

#tedadeh film haye sakhte shode dar har sal----------
movies1 %>% 
  mutate(year = str_replace(str_replace(str_extract(Title, "\\(\\d+\\)"), "\\)", ""), "\\(", "")) -> movies

movies %>% 
  group_by(year) %>% 
  summarise(count = n()) %>% 
  drop_na() %>% 
  arrange(desc(count)) %>% 
  kable()


kable(dt) %>%
  kable_styling("striped", full_width = F) %>%
  row_spec(0, angle = -45)

#dar har sali mardom be che genri alaghe daran-----------

separate_rows(movies, Genres, sep = "\\|") -> movies
rating %>% 
  group_by(MovieID) %>% 
  summarise(averageRating = mean(Rating), count = n()) %>%
  filter(count > 5000) %>% 
  full_join(movies) %>% 
  select(year, Genres, averageRating) %>% 
  group_by(Genres, year) %>% 
  summarise(averageRating = mean(averageRating, na.rm = T)) %>% 
  drop_na() %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(rank = rank(-averageRating, ties.method = "first")) %>% 
  filter(rank == 1) %>% 
  arrange(year) %>% 
  select(-rank) 

#2-----------
#نمودار ستونی تعداد فیلم های هر ژانر----------

movies %>% 
  group_by(Genres) %>% 
  summarise(count = n()) %>% 
  filter(count > 1) %>% 
  drop_na() %>% 
  hchart(hcaes(x = Genres, y = count, color = Genres), type = "column")

#. نمودار همبستگی ژانرها------------



movies %>% 
  mutate(hasGenre = 1) %>% 
  group_by(Genres) %>% 
  mutate(count = n()) %>% 
  filter(count > 1) %>% 
  drop_na() %>% 
  ungroup() %>% 
  select(-count) %>% 
  spread(Genres, hasGenre, fill = 0) %>% 
  select(-MovieID, -year, -Title) %>% 
  cor(method = c("pearson", "kendall", "spearman")) %>% 
  corrplot( order = "hclust", 
           tl.col = "black", tl.srt = 45)

#متوسط امتیاز به هر ژانر ------------


rating %>% 
  group_by(MovieID) %>% 
  summarise(averageRating = mean(Rating)) %>% 
  full_join(movies) %>% 
  select(averageRating, Genres) %>% 
  group_by(Genres) %>% 
  filter(n() > 1) %>% 
  summarise(averageRating = mean(averageRating)) %>% 
  drop_na()
  
#ت. دوران طلایی فیلم سازی ------------

rating %>% 
  group_by(MovieID) %>% 
  summarise(averageRating = mean(Rating)) %>% 
  full_join(movies) %>% 
  select(year, averageRating) %>% 
  group_by(year) %>% 
  summarise(count = n(), averageRating = mean(averageRating, na.rm = T)) %>% 
  drop_na() %>% 
  arrange(desc(count)) %>% 
  head(20) %>% 
  slice(which.max(averageRating))





#3-----------
library(stopwords)
movies %>% 
  mutate(Title = str_replace_all(Title,"\\(\\d+\\)", "" )) %>% 
  mutate(Title = str_replace_all(Title,"[[:punct:]]", "")) %>% 
  separate_rows(Title, sep = " ") %>% 
  select(Title) %>%
  group_by(Title) %>% 
  summarise(count = n()) %>%
  filter(count > 30 & !str_to_lower(Title) %in% stopwords::stopwords() & Title > "a" & !str_to_lower(Title) %in% stopwords("fr")) %>% 
  wordcloud2(size = 0.8, shape = "star")
  


#4----------
rating %>% 
  full_join(movies1) %>% 
  filter(Title == "Castle in the Sky (Tenkû no shiro Rapyuta) (1986)") -> castle


rating %>% 
  sample_frac(0.4) %>% 
  inner_join(movies1) %>% 
  rbind(castle) %>% 
  group_by(UserID) %>% 
  summarise(movies = list(c(Title))) -> basket


new <- as(basket$movies, "transactions")


grules <- apriori (data=new, parameter=list (supp=0.001,conf = 0.15,minlen=2), 
                  appearance = list(default="rhs",lhs="Castle in the Sky (Tenkû no shiro Rapyuta) (1986)"),
                  control = list (verbose=F)) # those who bought 'milk' also bought..

rules_conf <- sort (grules, by="lift", decreasing=TRUE) # 'high-confidence' rules.
inspect((rules_conf)[1:30])

grules <- apriori (data=new, parameter=list (supp=0.001,conf = 0.15,minlen=2), 
                   appearance = list(default="rhs",lhs="Cast Away (2000)"),
                   control = list (verbose=F)) # those who bought 'milk' also bought..

rules_conf <- sort (grules, by="lift", decreasing=TRUE) # 'high-confidence' rules.
inspect((rules_conf)[1:30])


grules <- apriori (data=new, parameter=list (supp=0.001,conf = 0.15,minlen=2), 
                   appearance = list(default="rhs",lhs="Memento (2000)"),
                   control = list (verbose=F)) # those who bought 'milk' also bought..

rules_conf <- sort (grules, by="lift", decreasing=TRUE) # 'high-confidence' rules.
inspect((rules_conf)[1:30])














