library(ggplot2)
library(highcharter)
library(gutenbergr)
library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)
library("ggthemes")

#Dickens, Charles

sentiments %>% filter(lexicon == "nrc") -> sentiments
popular_books <- c(
  "The Pickwick Papers",
  "Oliver Twist",
  "Nicholas Nickleby",
  "The Old Curiosity Shop",
  "Barnaby Rudge: A Tale of the Riots of 'Eighty",
  "Martin Chuzzlewit",
  "Dombey and Son",
  "David Copperfield",
  "Bleak House",
  "Hard Times",
  "Little Dorrit",
  "A Tale of Two Cities",
  "Our Mutual Friend",
  "The Mystery of Edwin Drood"
)

gutenberg_metadata %>% filter(author == "Dickens, Charles" &
                                has_text &
                                language == "en" &
                                title %in% popular_books) %>% select(gutenberg_id, title) -> dickens_book_titles

dickensList = list()
Dbooks = list()
for (i in 1:nrow(dickens_book_titles)) {
  book = gutenberg_download(dickens_book_titles$gutenberg_id[i])
  Dbooks[[i]] <- book
  wbook = book$text %>% unlist %>%
    str_replace_all("\'", "") %>%
    str_replace_all("[[:punct:]]", "") %>%
    str_split(pattern = "\\s") %>%
    unlist() %>%
    table() %>%
    as.data.frame(StrinsAsFactor = F)
  
  wbook = wbook %>%
    mutate(Book_title = dickens_book_titles$title[i])
  dickensList[[i]] = wbook
  
}


dickens = bind_rows(dickensList)
colnames(dickens) <- c("word", "count", "Book_title")
#selecting 14 books

dickens %>% filter(!str_to_lower(word) %in% stop_words$word &
                     word != "") %>%
  arrange(desc(count)) %>%
  mutate(proper = !word %in% str_to_lower(word)) -> dickens_books_words

#1---------------
dickens_books_words %>% group_by(word) %>% summarise(n = sum(count)) %>%
  arrange(desc(n)) %>% head(20) -> frequnct_dickens_words

frequnct_dickens_words %>% hchart(hcaes(x = word, y = n), type = "column") %>% hc_add_theme(hc_theme_sandsignika())

#2-------------
library(wordcloud2)
dickens_books_words %>% group_by(word) %>% summarise(n = sum(count)) %>%
  arrange(desc(n)) %>% head(200) -> frequnct_dickens_words
wordcloud2(frequnct_dickens_words, size = 0.3, figPath = "dickens.png")


#3---------------

dickens_books_words %>% filter(proper &
                                 word != "Miss" & word != "Sir") %>%
  group_by(Book_title) %>% mutate(wordRank = rank(-count, ties.method = "first")) %>%
  filter(wordRank <= 5) -> book_characters
book_characters %>% group_by(Book_title) %>%
  hchart(hcaes(x = word,
               y = count,
               group = Book_title),
         type = "column",
         size = 3) %>%
  hc_add_theme(hc_theme_sandsignika())

#4---------
sentiments -> word_emotion
dickens_books_words %>% filter(str_to_lower(word) %in% word_emotion$word) -> dickens_words_emotions
dickens_words_emotions %>% mutate(word = str_to_lower(word)) -> dickens_words_emotions
full_join(dickens_words_emotions, word_emotion) -> dickens_words_emotions
dickens_words_emotions %>% select(word, count, Book_title, sentiment) -> dickens_words_emotions


dickens_words_emotions %>% group_by(Book_title, sentiment, word) %>% summarise(count = mean(count)) -> dickens_words_emotions
dickens_words_emotions %>% filter(sentiment == "positive") -> pos_emotions
pos_emotions %>% group_by(Book_title) %>%
  mutate(rank = rank(-count, ties.method = "first")) %>% filter(rank <= 20) -> pos_emotions
pos_emotions %>% filter(rank <= 20) -> pos_emotions
dickens_words_emotions %>% filter(sentiment == "negative") -> neg_emotions
neg_emotions %>% group_by(Book_title) %>%
  mutate(rank = rank(-count, ties.method = "first")) %>% filter(rank <= 20) -> neg_emotions
full_join(neg_emotions, pos_emotions) -> dickens_words_emotions
for (i in 1:length(popular_books)) {
  dickens_words_emotions %>% filter(Book_title == popular_books[i]) -> temp_book
  print(
    ggplot(data = temp_book , aes(x = word , y = count)) + geom_bar(stat = "identity", aes(fill = sentiment)) +
      theme_solarized(light = FALSE) +
      scale_colour_solarized("red") +
      ggtitle(popular_books[i]) +
      theme(axis.text.x = element_text(
        angle = 45,
        vjust = 1,
        hjust = 1
      ))
  )
}
#5--------------
lesmiserables <- gutenberg_download(48731:48735)


lesmiserables %>%
  unnest_tokens(word, text) -> words_lesmiserables
words_lesmiserables %>% filter(!str_to_lower(word) %in% stop_words$word) -> words_lesmiserables

portion <- ceiling(as.numeric(nrow(words_lesmiserables) / 200))

words_lesmiserables %>% mutate(part = round(row_number(word) / (portion), 0)) -> words_lesmiserables
words_lesmiserables %>% filter(word %in% sentiments$word) -> words_lesmiserables

full_join(words_lesmiserables, sentiments) %>% select(word, part, sentiment) %>%
  filter(sentiment == "positive" |
           sentiment == "negative") -> words_lesmiserables

words_lesmiserables %>% group_by(part, sentiment) %>%
  summarise(count = n()) %>% mutate(count = ifelse(sentiment == "positive", count , -1 * count)) %>% 
  hchart(hcaes(x = part, y = count, group = sentiment), type = "column") %>%
  hc_add_theme(hc_theme_sandsignika())

words_lesmiserables %>% group_by(sentiment) %>%
  summarise(count = n()) %>% hchart(hcaes(x = sentiment, y = count, color = sentiment), type = "column") %>%
  hc_add_theme(hc_theme_sandsignika())

#6----------

Dbooks %>% .[[1]] %>%
  unnest_tokens(word, text) -> Dbooks_ordered
Dbooks_ordered %>% mutate(Book_title = dickens_book_titles$title[1]) -> Dbooks_ordered
for (i in 2:length(popular_books)) {
  Dbook_ordered_temp = Dbooks[[i]] %>%
    unnest_tokens(word, text)
  Dbook_ordered_temp %>% mutate(Book_title = dickens_book_titles$title[i]) -> Dbook_ordered_temp
  rbind(Dbook_ordered_temp, Dbooks_ordered) -> Dbooks_ordered
}
Dbooks_ordered %>% mutate(nextWord = "") -> Dbooks_ordered
Dbooks_ordered %>% anti_join(stop_words) -> Dbooks_ordered_1
Dbooks_ordered_1 %>% mutate(nextWord = lead(word)) %>%
  group_by(word, nextWord) %>% summarise(count = n()) -> Dbooks_double

Dbooks_double %>% arrange(desc(count)) %>%
  head(30) %>% mutate(words = paste(word, nextWord, sep = " ")) %>%
  hchart(hcaes(x = words , y = count), type = "column")  %>%
  hc_add_theme(hc_theme_sandsignika())


#7---------
Dbooks_ordered %>% mutate(nextWord = lead(word)) %>%
  filter(word %in% c("She", "she", "He", "he")) %>%
  group_by(nextWord) %>% summarise(count = n()) %>%
  arrange(desc(count)) %>% head(20) %>%
  hchart(hcaes(nextWord, y = count), type = "column") %>%
  hc_add_theme(hc_theme_sandsignika())

#8--------

roman_numbers <- unlist(as.character(as.roman(1:200)))
bind_rows(Dbooks) -> dickens_books_lines

dickens_books_lines %>% group_by(gutenberg_id) %>% 
  filter(row_number() >= 100) -> dickens_books_no_content


original_books <- dickens_books_no_content %>%
  group_by(gutenberg_id) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",ignore_case = TRUE)) )) %>%
  ungroup() %>% filter(chapter > 0)



#farz bar inke chapter chapter joda kardim

dickens_unigram <- original_books %>% 
  unnest_tokens(word, text) %>% filter(!word %in% stop_words$word)
  

dickens_bigram <- original_books %>%  unnest_tokens(bigram, text, token = "ngrams", n = 2)
bigrams_separated <- dickens_bigram %>%  separate(bigram, c("word1", "word2"), sep = " ")
bigrams_separated %>%  filter(!(word1 %in% stop_words$word)) %>%  filter(!(word2 %in% stop_words$word)) -> bigrams_separated

bigrams_united <- bigrams_separated %>%  unite(bigram, word1, word2, sep = " ")


original_books %>%  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%  
  filter(!word1 %in% stop_words$word,         
         !word2 %in% stop_words$word,         
         !word3 %in% stop_words$word)  %>% drop_na() -> tgram_seprated

tgram_united <- tgram_seprated %>%  unite(tgram, word1, word2, word3, sep = " ")

dickens_unigram %>% group_by(chapter, gutenberg_id) %>% 
  mutate(numberOfwords = n()) -> dickens_unigram

dickens_unigram %>% filter(word != "NA") %>% group_by(gutenberg_id, chapter, word) %>% 
  summarise(count = n() / mean(numberOfwords)) %>% mutate(rank = rank(-count , ties.method = "first"))-> unigram_distribution

bigrams_united %>% filter(bigram != "NA NA") %>% group_by(chapter, gutenberg_id) %>% 
  mutate(numberOfwords = n()) -> bigrams_united

bigrams_united %>% group_by(gutenberg_id, chapter, bigram) %>% 
  summarise(count = n() / mean(numberOfwords)) %>% mutate(rank = rank(-count , ties.method = "first")) -> bigram_distribution


unigram_distribution %>% filter(gutenberg_id == 564 , chapter == 4) %>% arrange(desc(count)) %>% head(30) %>% 
  ggplot(aes(x = word , y = count)) + geom_bar(stat = "identity", aes(fill = word)) +  theme_solarized(light = FALSE) +
  scale_colour_solarized("red") +
  theme(axis.text.x = element_text(
    angle = 45,
    vjust = 1,
    hjust = 1
  ))

unigram_distribution %>% filter(gutenberg_id == 766 , chapter == 10)%>% arrange(desc(count)) %>% head(30) %>% 
  ggplot(aes(x = word , y = count)) + geom_bar(stat = "identity", aes(fill = word)) +  theme_solarized(light = FALSE) +
  scale_colour_solarized("red") +
  theme(axis.text.x = element_text(
    angle = 45,
    vjust = 1,
    hjust = 1
  ))


unigram_distribution %>% filter(gutenberg_id == 917 , chapter == 15)%>% arrange(desc(count)) %>% head(30) %>% 
  ggplot(aes(x = word , y = count)) + geom_bar(stat = "identity", aes(fill = word)) +  theme_solarized(light = FALSE) +
  scale_colour_solarized("red") +
  theme(axis.text.x = element_text(
    angle = 45,
    vjust = 1,
    hjust = 1
  ))

bigram_distribution %>% filter(gutenberg_id == 564 , chapter == 4)%>% arrange(desc(count)) %>% head(30) %>% 
  hchart(hcaes(x = bigram, y = count), type = "column")%>%
  hc_add_theme(hc_theme_sandsignika())



bigram_distribution %>% filter(gutenberg_id == 766 , chapter == 10)%>% arrange(desc(count)) %>% head(30) %>% 
  hchart(hcaes(x = bigram, y = count), type = "column")%>%
  hc_add_theme(hc_theme_sandsignika())


bigram_distribution %>% filter(gutenberg_id == 917 , chapter == 15)%>% arrange(desc(count)) %>% head(30) %>% 
  hchart(hcaes(x = bigram, y = count), type = "column")%>%
  hc_add_theme(hc_theme_sandsignika())

#9-----------
library(janeaustenr) 
original_books <- austen_books() %>%  group_by(book) %>%
  mutate(linenumber = row_number(),         
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",ignore_case = TRUE)))) %>%  ungroup()

  

austin_unigram <- original_books %>% 
  unnest_tokens(word, text) %>% filter(!word %in% stop_words$word)


austin_bigram <- original_books %>%  unnest_tokens(bigram, text, token = "ngrams", n = 2)
bigrams_separated <- austin_bigram %>%  separate(bigram, c("word1", "word2"), sep = " ")
bigrams_separated %>%  filter(!(word1 %in% stop_words$word)) %>%  filter(!(word2 %in% stop_words$word)) -> bigrams_separated

bigrams_united <- bigrams_separated %>%  unite(bigram, word1, word2, sep = " ")


austin_unigram %>% group_by(chapter, book) %>% 
  mutate(numberOfwords = n()) -> austin_unigram

austin_unigram %>% filter(word != "NA") %>% group_by(book, chapter, word) %>% 
  summarise(count = n() / mean(numberOfwords)) %>% mutate(rank = rank(-count , ties.method = "first"))-> unigram_distribution

bigrams_united %>% filter(bigram != "NA NA") %>% group_by(chapter, book) %>% 
  mutate(numberOfwords = n()) -> bigrams_united

bigrams_united %>% group_by(book, chapter, bigram) %>% 
  summarise(count = n() / mean(numberOfwords)) %>% mutate(rank = rank(-count , ties.method = "first")) -> bigram_distribution

unigram_distribution %>% filter(book == "Sense & Sensibility", chapter == 10) %>% arrange(desc(count)) %>% head(30) %>% 
  ggplot(aes(x = word , y = count)) + geom_bar(stat = "identity", aes(fill = word))+  theme_solarized(light = FALSE) +
  scale_colour_solarized("red") +
  theme(axis.text.x = element_text(
    angle = 45,
    vjust = 1,
    hjust = 1
  ))


unigram_distribution %>% filter(book == "Pride & Prejudice", chapter == 15) %>% arrange(desc(count)) %>% head(30) %>% 
  ggplot(aes(x = word , y = count)) + geom_bar(stat = "identity", aes(fill = word))+  theme_solarized(light = FALSE) +
  scale_colour_solarized("red") +
  theme(axis.text.x = element_text(
    angle = 45,
    vjust = 1,
    hjust = 1
  ))


unigram_distribution %>% filter(book == "Emma", chapter == 20) %>% arrange(desc(count)) %>% head(30) %>% 
  ggplot(aes(x = word , y = count)) + geom_bar(stat = "identity", aes(fill = word))+  theme_solarized(light = FALSE) +
  scale_colour_solarized("red") +
  theme(axis.text.x = element_text(
    angle = 45,
    vjust = 1,
    hjust = 1
  ))

bigram_distribution %>% filter(book == "Sense & Sensibility", chapter == 10) %>% arrange(desc(count)) %>% head(30) %>% 
  hchart(hcaes(x = bigram,  y = count) , type = "column") %>%
  hc_add_theme(hc_theme_sandsignika())

bigram_distribution %>% filter(book == "Pride & Prejudice", chapter == 3) %>% arrange(desc(count)) %>% head(30) %>% 
  hchart(hcaes(x = bigram,  y = count) , type = "column") %>%
  hc_add_theme(hc_theme_sandsignika())

bigram_distribution %>% filter(book == "Emma", chapter == 20) %>% arrange(desc(count)) %>% head(30) %>% 
  hchart(hcaes(x = bigram,  y = count) , type = "column") %>%
  hc_add_theme(hc_theme_sandsignika())

#10-------------
install.packages("Ngram")

bigrams_separated <- dickens_bigram %>%  separate(bigram, c("word1", "word2"), sep = " ")
bigrams_separated %>%  filter(!(word1 %in% stop_words$word)) %>%  filter(!(word2 %in% stop_words$word)) -> bigrams_separated
bigrams_united <- bigrams_separated %>%  unite(bigram, word1, word2, sep = " ")

bigrams_united %>% filter(bigram != "NA NA") %>% group_by(chapter, gutenberg_id) %>% 
  mutate(numberOfwords = n()) -> bigrams_united


bigrams_united %>% group_by(gutenberg_id, chapter, bigram) %>% 
  summarise(count = n() / mean(numberOfwords)) %>% mutate(rank = rank(-count , ties.method = "first")) -> bigram_distribution


bigrams_separated_a <- austin_bigram %>%  separate(bigram, c("word1", "word2"), sep = " ")
bigrams_separated_a %>%  filter(!(word1 %in% stop_words$word)) %>%  filter(!(word2 %in% stop_words$word)) -> bigrams_separated_a

bigrams_united_a <- bigrams_separated_a %>%  unite(bigram, word1, word2, sep = " ")


bigrams_united_a %>% filter(bigram != "NA NA") %>% group_by(chapter, book) %>% 
  mutate(numberOfwords = n()) -> bigrams_united_a

bigrams_united_a %>% group_by(book, chapter, bigram) %>% 
  summarise(count = n() / mean(numberOfwords)) %>% mutate(rank = rank(-count , ties.method = "first")) -> bigram_distribution_a

bigrams_united %>% filter(gutenberg_id != 730 & bigram != "NA NA") %>% 
  group_by(bigram) %>% summarise(count = n()) %>% arrange(desc(count)) %>% head(30) -> important_words_d
  

bigrams_united_a %>% filter(book != "Pride & Prejudice", bigram != "NA NA") %>% 
  group_by(bigram) %>% summarise(count = n()) %>% arrange(desc(count)) %>% head(30) -> important_words_a

bigram_distribution %>% filter(gutenberg_id != 730) %>% 
  filter(bigram %in% important_words_d$bigram) %>% select(bigram, count) %>% 
  spread(key = bigram, value = count) -> train_d
train_d[is.na(train_d)] <- 0
train_d$is_dickens <- 1

train_d %>% group_by() %>%  select(-gutenberg_id) %>% select(-chapter) -> train_d

bigram_distribution_a %>% filter(book != "Pride & Prejudice") %>% 
  filter(bigram %in% important_words_a$bigram) %>% select(bigram , count) %>% 
  spread(key = bigram , value = count) -> train_a
train_a[is.na(train_a)] <- 0
train_a$is_dickens <- 0

full_join(train_a, train_d) -> train
train[is.na(train)] <- 0

train %>% group_by() %>%  select(-book) %>% select(-chapter) -> train

library(h2o)
h2o.init()
htrain = as.h2o(train)
colnames(train) %>% .[-31] -> colnames_train
hglm = h2o.glm(y = "is_dickens", x= colnames_train,
               training_frame = htrain, family="binomial")


bigram_distribution %>% filter(gutenberg_id == 730) %>% 
  filter(bigram %in% important_words_d$bigram) %>% select(bigram, count) %>%
  spread(key = bigram, value = count) -> test_d

bigram_distribution_a %>% filter(book == "Pride & Prejudice") %>% 
  filter(bigram %in% important_words_a$bigram) %>% select(bigram, count) %>% 
  spread(key = bigram, value = count) -> test_a

full_join(test_a , test_d) %>% group_by() %>% select(-book) %>% select(-chapter) %>% select(-gutenberg_id) -> test
test[is.na(test)] <- 0


setdiff(colnames_train, colnames(test)) -> x

cbind(test, setNames( lapply(x, function(x) x=NA), x) ) -> test
test[is.na(test)] <- 0
htest <- as.h2o(test)



predict <- as.data.frame(h2o.predict(hglm, htest))
cbind(test, predict %>% select(predict)) -> test

test$answer <- integer(45)
test$answer[17:45] <- 1
#khata 
uncorrect <- sum(test$predict != test$answer)
print(uncorrect * 100 / nrow(test))

chisq.test(bigram_distribution %>% select(count), bigram_distribution_a %>% select(count))

austin_unigram %>% filter(word != "NA") %>% group_by(book, chapter, word) %>% 
  summarise(count = n() / mean(numberOfwords)) %>% mutate(rank = rank(-count , ties.method = "first"))-> unigram_distribution_a
chisq.test(unigram_distribution %>% select(count), unigram_distribution_a %>% select(count))
