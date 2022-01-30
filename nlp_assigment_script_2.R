#NLP Application - Inspect 4 quarters of Data

library(tidyverse)
library(Rgraphviz)
library(tm)
library(lexicon)

#setwd("C:/Users/Nicoletta/Downloads/")


getwd()

#Save Documents in your working directory

#Load Raw Data
raw_data_q4 <- read_file(file = "gs_q421.txt")
raw_data_q3 <- read_file(file = "gs_q321.txt")
raw_data_q2 <- read_file(file = "gs_q221.txt")
raw_data_q1 <- read_file(file = "gs_q121.txt")

#create a tibble with four documents (4 quarters)
conf_call_df <- tibble(q4 = raw_data_q4, q3 =  raw_data_q3, q2 = raw_data_q2, q1 = raw_data_q1)

#Generate corpus and inspect

corpus <- iconv(conf_call_df)
corpus <- Corpus(VectorSource(conf_call_df))
inspect(corpus[1])

#Clean corpus by creating a function to clean unwanted data from corpus - adding custom words

#Removing Recurring names such as company name and executives
company_name <-"Goldman Sachs"
ceo_name <-"David Solomon"
cfo_name <- "Stephen Scherr"
co_cfo_name <- "Denis Coleman"
ir_name <- "Carey Halio"
co_ir_name <- "Heather Kennedy Miner"

execs_names <- c(company_name, ceo_name, cfo_name, ir_name, co_ir_name)

# Remove analysts names from Q&A session
analysts_names <- c("Glenn Schorr", "Steven Chubak", "Mike Carrier", "Betsy Graseck", 
                    "Mike Mayo", "Brennan Hawken", "Devin Ryan", "Gerard Cassidy", "Jeremy Sigee")

clean_f <-function(corpus){
  corpus <- tm_map(corpus, tolower)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeWords, c(stopwords("en"), execs_names, analysts_names, "'s", "david"))
  return(corpus)
}

#Apply function to the corpus of Data
clean_corpus <- clean_f(corpus)

#Create TDM from our clean corpus
tdm <- TermDocumentMatrix(clean_corpus)

mat_tdm <- as.matrix(tdm)
mat_tdm[1:20, 1:4]

#Sum the frequency of the tdm word count, sort and plot the top results
word_freq <- rowSums(as.matrix(all_tdm_tf))
word_freq <- sort(word_freq, decreasing = TRUE)

# Check top 20 most frequent words in the TDM
barplot(word_freq[1:20], col = "tan", las = 2, main = "Top 20 Frequent Words")

##############################################################################
#Create tibble object with the TDM Matrix (keep)
terms <- rownames(mat_tdm)

tibble_tdm <- tibble(terms = terms, Q4 = mat_tdm[,1], Q3 = mat_tdm[,2], Q2 = mat_tdm[,3], Q1 = mat_tdm[,4])

#Check frequency of Growth and Revenue as very important terms
tibble_tdm %>% filter(terms %in% c("growth", "revenue"))

#Load Bing lexicon
bing <- tidytext::get_sentiments("bing")

levs <- c("Q1", "Q2", "Q3", "Q4")

tibble_join <- tibble_tdm %>% gather(quarter, frequency, - terms) %>% inner_join(bing, by = c("terms" = "word")) %>% mutate(quarter = as.factor(quarter))

tibble_join %>% group_by(quarter, sentiment) %>% summarise(magnitude =  sum(frequency)) %>% ungroup() %>% 
  ggplot( aes(fill = sentiment, y = magnitude, x = quarter)) + 
  geom_bar(position="fill", stat="identity") + 
  labs(title = "Goldman Sachs 2021 Earnings Call Sentiment - Bing", x = "Quarter", y = "Magnitude", fill = "Sentiment") + 
  theme_minimal()

tibble_join %>% group_by(quarter) %>% filter( sentiment == "positive") %>% 
  top_n(wt = frequency, 10) %>% 
  ggplot(aes( x = terms, y = frequency)) + 
  geom_bar() + 
  facet_wrap(~quarter)

# Loughran & Mcdonald Lexicon
# This dictionary was first presented in the Journal of Finance and has been widely used in the finance domain ever since.

#Loughran & Mcdonald lexicon

loughran_mcdonald <- lexicon::hash_sentiment_loughran_mcdonald %>% as_tibble() %>% 
  mutate(sentiment = ifelse(y == 1, "positive", "negative")) %>% select(terms = x, sentiment)

# To be Noted that now the recognized words are much less - around 12% with a lot less volatility in between quarters
tibble_join <- tibble_tdm %>% gather(quarter, frequency, - terms) %>% inner_join(loughran_mcdonald, by = "terms") %>% mutate(quarter = as.factor(quarter))

tibble_join %>% group_by(quarter, sentiment) %>% summarise(magnitude =  sum(frequency)) %>% ungroup() %>% 
  ggplot( aes(fill = sentiment, y = magnitude, x = quarter)) + 
  geom_bar(position="fill", stat="identity") + 
  labs(title = "Goldman Sachs 2021 Earnings Call Sentiment - L&M", x = "Quarter", y = "Magnitude", fill = "Sentiment")

#Top 10 most frequent positive and negative L&M words in my TDM
tibble_join %>% group_by(terms, sentiment) %>% summarize(term_sum = sum(frequency)) %>% ungroup() %>% filter(sentiment == "positive") %>%
  arrange(desc(term_sum)) %>% top_n(10) %>% ggplot(aes(x = reorder(terms,term_sum), y = term_sum)) + 
  geom_col(fill = "blue") + 
  labs(title = "Goldman Sachs Earnings Calls - L&M Top 10 Positive terms", x = "terms", y = "frequency") + 
  theme_minimal()

tibble_join %>% group_by(terms, sentiment) %>% summarize(term_sum = sum(frequency)) %>% ungroup() %>% filter(sentiment == "negative") %>%
  arrange(desc(term_sum)) %>% top_n(10) %>% ggplot(aes(x = reorder(terms,term_sum), y = term_sum)) + 
  geom_col(fill = "red") + 
  labs(title = "Goldman Sachs Earnings Calls - L&M Top 10 Negative terms", x = "terms", y = "frequency") + 
  theme_minimal()

# Create a dendrogram which we will be using to find similarities among quarters and can thene integrate with our Sentiment analysis (would work better with a wider dataset maybe covering the past 5 years)

#Creates the DocumentTermMatrix, which is exactly like transposing the TermDocumentMatrix

dtm <- DocumentTermMatrix(clean_corpus)

#Does a dendrogram
distMatrix <-dist(t(as.matrix(mat_tdm)),
                  method="euclidean")

groups <-hclust(distMatrix , method="ward.D")

head(groups)

plot(groups)


# Plot Semanthic similarity of the quarters according to Growth and revenue, which helps us understand the level of bearishness/bullishness of 
# GS executives with a better degree of confidence

tibble_tdm %>% filter(terms %in% c("growth", "revenue")) %>% gather(quarter, value, -terms) %>% spread(terms, value) %>% 
  ggplot(aes( x = growth, y = revenue, col = quarter)) + geom_point(size = 4) + 
  theme_minimal()

