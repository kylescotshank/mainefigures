#################################
#
# Library load and setup 
#
#################################

library(tidyverse)
library(tidytext)
library(viridis)
library(purrr)
library(tibble)
library(stringr)
library(readxl)
library(scales)
library(patchwork)
library(readtext)
library(lubridate)
library(forcats)
library(pdftools)
library(tm)
library(igraph)
library(ggraph)
library(topicmodels)
library(textclean)

#################################
#
# Load data (new .doc format)
#
#################################

setwd("/Users/KSHAN006/OneDrive - The Hershey Company/Desktop/barharbor_council_minutes/new_doc")

new_doc_data <- list.files(pattern = "*.doc") %>%
  map_df(~ readtext(.))

cleaned_new_docs <- new_doc_data %>%
  mutate(date = ymd(substr(doc_id, 3,10)),
         year = year(date),
         month = month(date))

#################################
#
# Load data (old .doc format)
#
#################################

setwd("/Users/KSHAN006/OneDrive - The Hershey Company/Desktop/barharbor_council_minutes/old_doc")

old_doc_data <- list.files(pattern = "*.doc") %>%
  map_df(~ readtext(.))

cleaned_old_docs <- old_doc_data %>%
  mutate(date = ymd(substr(doc_id, 3,10)),
         year = year(date),
         month = month(date))

#################################
#
# Load data (.pdf format, 2022-2023)
#
#################################


directory <- "/Users/KSHAN006/OneDrive - The Hershey Company/Desktop/barharbor_council_minutes/pdfs"

pdfs <- paste(directory, "/", list.files(directory, pattern = "*.pdf"), sep = "")
pdf_names <- list.files(directory, pattern = "*.pdf")
pdf_texts <- map(pdfs, pdftools::pdf_text)

pdf_data_raw <- tibble(document = pdf_names, text = pdf_texts)

cleaned_pdfs <- pdf_data_raw %>%
  mutate(doc_id = document, 
         date = mdy(substr(doc_id, 2,9)),
         date = ymd(date),
         year = year(date),
         month = month(date)) %>%
  select(doc_id, text, date, year, month) %>%
  unnest(text)

#################################
#
# Join Data!
#
#################################

cnd <- as_tibble(cleaned_new_docs)
cod <- as_tibble(cleaned_old_docs)
cpdf <- as_tibble(cleaned_pdfs)

working_data <- cnd %>%
  rbind(cod) %>%
  rbind(cpdf)


#################################
#
# Most common single word
#
#################################

doc_words <- working_data %>% 
  unnest_tokens(word, text) %>%
  ## remove number strings
  filter(!grepl('[0-9]', word)) %>%
  ## remove long underscores
  filter(!grepl('([_])', word)) %>%
  ## remove stop words
  anti_join(stop_words) %>% 
  group_by(doc_id, year) %>% 
  count(word, sort = TRUE) %>%
  ungroup()


doc_words %>%
  count(word, sort = TRUE) %>%
  mutate(word = reorder(word,n)) %>%
  top_n(20) %>% 
  ggplot(aes(x = word, y = n)) + 
  geom_col(col=I("black")) + 
  labs(x = NULL) + 
  coord_flip() + 
  theme_bw() + 
  ggtitle("Most Common Words in B.H. Council Minutes",
          subtitle = "2002 - 2023")


#################################
#
# Make some process stop words,
# also explore stemming!
#
#################################

bh_stop_words <- tibble(word = c("council","minutes","bar","harbor",
                                 "p.m","councilors","councilor","attendance",
                                 "call","adjournment","motion","moved","passed",
                                 "discussion","agenda","special","ms","adopt",
                                 "adjourn","session","meeting","adoption",
                                 "business","comments","action","regular","review",
                                 "requested","warrant","approval","authorize",
                                 "approve","item","excused","request",
                                 "attendance","ms","special","adoption","adopt",
                                 "call","ordinance","paid","paul","reed","bills",
                                 "brechlin","minutolo","shank","caines","hochman",
                                 "dobbs","peacock","valerie","paradis","blancato",
                                 "weir","scott","val","lou","germain","coston",
                                 "vote","friedmann","goldthwait","nominated",
                                 "nominate","yfriedmann","ygoldthwait","yhochman",
                                 "ypeacock","yfriedmannyminutolo","paul","enoch",
                                 "ydobbs","dana","st","coughy","cough","peacocky",
                                 "nay","yay","yes","no","gary","eveland","clark",
                                 "stivers","burt","barker","greenlee","peter","david",
                                 "disney","valenti","andre","veilleux","schloss","jordan",
                                 "sandy","albert","ii","iii","iv","v","vi","vii","viii",
                                 "ix","bowden","robert","garland","julia","robert",
                                 "christopher","walsh","liz","graves","sarah","gilbert",
                                 "cornell","knight","james","smith","kevin","sutherland",
                                 "beathany","leavitt","stephen","wagner","lynn","kenison",
                                 "freshley","beard","donald","seward","plumb","noonan",
                                 "ycoston","ron", "linscott","hochmany","friedmanny","jim"))

doc_words %>% 
  anti_join(bh_stop_words) %>%
  count(word, sort = TRUE) %>%
  mutate(word = reorder(word,n)) %>%
  top_n(20) %>% 
  ggplot(aes(x = word, y = n)) + 
  geom_col(col=I("black")) + 
  labs(x = NULL) + 
  coord_flip() + 
  theme_bw() + 
  ggtitle("Most Common Words in B.H. Council Minutes",
          subtitle = "2002 - 2023, common process words removed")


#################################
#
# TF-IDF
#
#################################

doc_words_cleaned <- doc_words %>%
  anti_join(bh_stop_words)

total_words <- doc_words_cleaned %>% 
  group_by(doc_id) %>% 
  summarize(total = sum(n))

combined_words <- left_join(doc_words_cleaned, total_words)

doc_tf_idf <- combined_words %>%
  bind_tf_idf(word, doc_id, n)

doc_tf_idf %>%
  select(word, tf_idf) %>%
  top_n(20, tf_idf) %>% 
  ggplot(aes(x = reorder(word, tf_idf), y = tf_idf)) + 
  geom_col(col=I("black")) + 
  labs(x = NULL) + 
  coord_flip() + 
  theme_bw() + 
  ggtitle("Most Important Words in B.H. Council Minutes",
          subtitle = "2002 - 2023, common process words removed")


#################################
#
# Bigrams + Bigrams TF-IDf
#
#################################

bigram_words <- working_data %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram))

bigrams_separated <- bigram_words %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% bh_stop_words$word) %>%
  filter(!word2 %in% bh_stop_words$word) %>%
  filter(!grepl('[0-9]', word1)) %>%
  filter(!grepl('[0-9]', word2)) 

bigram_counts <- bigrams_filtered %>% 
  group_by(year) %>%
  count(word1, word2, sort = TRUE)

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ") 

bigrams_tf_idf <- bigrams_united %>%
  count(year, bigram) %>%
  bind_tf_idf(bigram, year, n)

bigrams_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  group_by(year) %>%
  slice_max(tf_idf, n = 5) %>%
  ungroup() %>%
  mutate(year = as.factor(year),
         bigram = reorder_within(bigram, tf_idf, year)) %>%
  ggplot(aes(tf_idf, bigram, fill = year)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ year, ncol = 4, scales = "free") +
  labs(x = "tf-idf of bigram", y = NULL) + 
  theme_bw() +
  scale_y_reordered() + 
  ggtitle("Most Important Bigrams in B.H. Council Minutes",
          subtitle = "2002 - 2023, common process words removed")

#################################
#
# Trigrams + Trigrams TF-IDf
#
#################################

trigram_words <- working_data %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  filter(!is.na(trigram))

trigrams_separated <- trigram_words %>%
  separate(trigram, c("word1", "word2","word3"), sep = " ")

trigrams_filtered <- trigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word1 %in% bh_stop_words$word) %>%
  filter(!word2 %in% bh_stop_words$word) %>%
  filter(!word3 %in% bh_stop_words$word) %>% 
  filter(!grepl('[0-9]', word1)) %>%
  filter(!grepl('[0-9]', word2)) %>%
  filter(!grepl('[0-9]', word3))

trigram_counts <- trigrams_filtered %>% 
  group_by(year) %>%
  count(word1, word2, word3, sort = TRUE)

trigrams_united <- trigrams_filtered %>%
  unite(trigram, word1, word2, word3, sep = " ") 

trigrams_tf_idf <- trigrams_united %>%
  count(year, trigram) %>%
  bind_tf_idf(trigram, year, n)

trigrams_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  group_by(year) %>%
  slice_max(tf_idf, n = 5) %>%
  ungroup() %>%
  mutate(year = as.factor(year),
         bigram = reorder_within(trigram, tf_idf, year)) %>%
  ggplot(aes(tf_idf, bigram, fill = year)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ year, ncol = 4, scales = "free") +
  labs(x = "tf-idf of trigram", y = NULL) + 
  theme_bw() +
  scale_y_reordered() + 
  ggtitle("Most Important Trigrams in B.H. Council Minutes",
          subtitle = "2002 - 2023, common process words removed")
  

#################################
#
# Topic Model Section!
#
#################################

library(textmineR)

textcleaner <- function(x){
  x <- as.character(x)
  
  x <- x %>%
    str_to_lower() %>%  # convert all the string to low alphabet
    replace_contraction() %>% # replace contraction to their multi-word forms
    replace_internet_slang() %>% # replace internet slang to normal words
    replace_emoji() %>% # replace emoji to words
    replace_emoticon() %>% # replace emoticon to words
    replace_hash(replacement = "") %>% # remove hashtag
    replace_word_elongation() %>% # replace informal writing with known semantic replacements
    replace_number(remove = T) %>% # remove number
    replace_date(replacement = "") %>% # remove date
    replace_time(replacement = "") %>% # remove time
    str_remove_all(pattern = "[[:punct:]]") %>% # remove punctuation
    str_remove_all(pattern = "[^\\s]*[0-9][^\\s]*") %>% # remove mixed string n number
    str_squish() %>% # reduces repeated whitespace inside a string.
    str_trim() # removes whitespace from start and end of string
  
  return(as.data.frame(x))
  
}

topic_model_working_data <- working_data %>%
  group_split(year)

#################################
#
# 2002
#
#################################

cleaned_2002 <- textcleaner(topic_model_working_data[[1]]$text)
cleaned_2002 <- cleaned_2002 %>% 
  mutate(id = rownames(cleaned_2002))

# crete dtm
set.seed(2002)

dtm_r_2002 <- CreateDtm(doc_vec = cleaned_2002$x,
                     doc_names = cleaned_2002$id,
                     ngram_window = c(1,2),
                     stopword_vec = c(stopwords("en"),bh_stop_words$word),
                     verbose = F)

dtm_r_2002 <- dtm_r_2002[,colSums(dtm_r_2002)>2]

lda_model_2002 <- FitLdaModel(dtm = dtm_r_2002,
                         k = 20, # number of topic
                         iterations = 500,
                         burnin = 180,
                         alpha = 0.1,beta = 0.05,
                         optimize_alpha = T,
                         calc_likelihood = T,
                         calc_coherence = T,
                         calc_r2 = T)

lda_model_2002$top_terms <- GetTopTerms(phi = lda_model_2002$phi,M = 15)
lda_model_2002$prevalence <- colSums(lda_model_2002$theta)/sum(lda_model_2002$theta)*100

lda_model_2002$summary <- data.frame(topic = rownames(lda_model_2002$phi),
                                coherence = round(lda_model_2002$coherence,3),
                                prevalence = round(lda_model_2002$prevalence,3),
                                top_terms = apply(lda_model_2002$top_terms,2,function(x){paste(x,collapse = ", ")}))

lda_model_2002_summary <- lda_model_2002$summary %>%
  `rownames<-`(NULL)

lda_model_2002_summary

lda_model_2002_summary %>% pivot_longer(cols = c(coherence,prevalence)) %>%
  ggplot(aes(x = factor(topic,levels = unique(topic)), y = value, group = 1)) +
  geom_point() + geom_line() +
  facet_wrap(~name,scales = "free_y",nrow = 2) +
  theme_minimal() +
  labs(title = "Best topics by coherence and prevalence score",
       subtitle = "2002 BH Town Council Meeting Minutes",
       x = "Topics", y = "Value")


lda_model_2002_summary %>% 
  arrange(desc(coherence)) %>%
  slice(1:5)

#################################
#
# 2003
#
#################################

cleaned_2003 <- textcleaner(topic_model_working_data[[2]]$text)
cleaned_2003 <- cleaned_2003 %>% 
  mutate(id = rownames(cleaned_2003))

# crete dtm
set.seed(2003)

dtm_r_2003 <- CreateDtm(doc_vec = cleaned_2003$x,
                        doc_names = cleaned_2003$id,
                        ngram_window = c(1,2),
                        stopword_vec = c(stopwords("en"),bh_stop_words$word),
                        verbose = F)

dtm_r_2003 <- dtm_r_2003[,colSums(dtm_r_2003)>2]

lda_model_2003 <- FitLdaModel(dtm = dtm_r_2003,
                              k = 20, # number of topic
                              iterations = 500,
                              burnin = 180,
                              alpha = 0.1,beta = 0.05,
                              optimize_alpha = T,
                              calc_likelihood = T,
                              calc_coherence = T,
                              calc_r2 = T)

lda_model_2003$top_terms <- GetTopTerms(phi = lda_model_2003$phi,M = 15)
lda_model_2003$prevalence <- colSums(lda_model_2003$theta)/sum(lda_model_2003$theta)*100

lda_model_2003$summary <- data.frame(topic = rownames(lda_model_2003$phi),
                                     coherence = round(lda_model_2003$coherence,3),
                                     prevalence = round(lda_model_2003$prevalence,3),
                                     top_terms = apply(lda_model_2003$top_terms,2,function(x){paste(x,collapse = ", ")}))

lda_model_2003_summary <- lda_model_2003$summary %>%
  `rownames<-`(NULL)

lda_model_2003_summary %>% 
  arrange(desc(coherence)) %>%
  slice(1:5)

#################################
#
# 2004
#
#################################

cleaned_2004 <- textcleaner(topic_model_working_data[[3]]$text)
cleaned_2004 <- cleaned_2004 %>% 
  mutate(id = rownames(cleaned_2004))

# crete dtm
set.seed(2004)

dtm_r_2004 <- CreateDtm(doc_vec = cleaned_2004$x,
                        doc_names = cleaned_2004$id,
                        ngram_window = c(1,2),
                        stopword_vec = c(stopwords("en"),bh_stop_words$word),
                        verbose = F)

dtm_r_2004 <- dtm_r_2004[,colSums(dtm_r_2004)>2]

lda_model_2004 <- FitLdaModel(dtm = dtm_r_2004,
                              k = 20, # number of topic
                              iterations = 500,
                              burnin = 180,
                              alpha = 0.1,beta = 0.05,
                              optimize_alpha = T,
                              calc_likelihood = T,
                              calc_coherence = T,
                              calc_r2 = T)

lda_model_2004$top_terms <- GetTopTerms(phi = lda_model_2004$phi,M = 15)
lda_model_2004$prevalence <- colSums(lda_model_2004$theta)/sum(lda_model_2004$theta)*100

lda_model_2004$summary <- data.frame(topic = rownames(lda_model_2004$phi),
                                     coherence = round(lda_model_2004$coherence,3),
                                     prevalence = round(lda_model_2004$prevalence,3),
                                     top_terms = apply(lda_model_2004$top_terms,2,function(x){paste(x,collapse = ", ")}))

lda_model_2004_summary <- lda_model_2004$summary %>%
  `rownames<-`(NULL)

lda_model_2004_summary %>% 
  arrange(desc(coherence)) %>%
  slice(1:5)

#################################
#
# 2005
#
#################################

cleaned_2005 <- textcleaner(topic_model_working_data[[4]]$text)
cleaned_2005 <- cleaned_2005 %>% 
  mutate(id = rownames(cleaned_2005))

# crete dtm
set.seed(2005)

dtm_r_2005 <- CreateDtm(doc_vec = cleaned_2005$x,
                        doc_names = cleaned_2005$id,
                        ngram_window = c(1,2),
                        stopword_vec = c(stopwords("en"),bh_stop_words$word),
                        verbose = F)

dtm_r_2005 <- dtm_r_2005[,colSums(dtm_r_2005)>2]

lda_model_2005 <- FitLdaModel(dtm = dtm_r_2005,
                              k = 20, # number of topic
                              iterations = 500,
                              burnin = 180,
                              alpha = 0.1,beta = 0.05,
                              optimize_alpha = T,
                              calc_likelihood = T,
                              calc_coherence = T,
                              calc_r2 = T)

lda_model_2005$top_terms <- GetTopTerms(phi = lda_model_2005$phi,M = 15)
lda_model_2005$prevalence <- colSums(lda_model_2005$theta)/sum(lda_model_2005$theta)*100

lda_model_2005$summary <- data.frame(topic = rownames(lda_model_2005$phi),
                                     coherence = round(lda_model_2005$coherence,3),
                                     prevalence = round(lda_model_2005$prevalence,3),
                                     top_terms = apply(lda_model_2005$top_terms,2,function(x){paste(x,collapse = ", ")}))

lda_model_2005_summary <- lda_model_2005$summary %>%
  `rownames<-`(NULL)


lda_model_2005_summary %>% 
  arrange(desc(coherence)) %>%
  slice(1:5)

#################################
#
# 2006
#
#################################

cleaned_2006 <- textcleaner(topic_model_working_data[[5]]$text)
cleaned_2006 <- cleaned_2006 %>% 
  mutate(id = rownames(cleaned_2006))

# crete dtm
set.seed(2006)

dtm_r_2006 <- CreateDtm(doc_vec = cleaned_2006$x,
                        doc_names = cleaned_2006$id,
                        ngram_window = c(1,2),
                        stopword_vec = c(stopwords("en"),bh_stop_words$word),
                        verbose = F)

dtm_r_2006 <- dtm_r_2006[,colSums(dtm_r_2006)>2]

lda_model_2006 <- FitLdaModel(dtm = dtm_r_2006,
                              k = 20, # number of topic
                              iterations = 500,
                              burnin = 180,
                              alpha = 0.1,beta = 0.05,
                              optimize_alpha = T,
                              calc_likelihood = T,
                              calc_coherence = T,
                              calc_r2 = T)

lda_model_2006$top_terms <- GetTopTerms(phi = lda_model_2006$phi,M = 15)
lda_model_2006$prevalence <- colSums(lda_model_2006$theta)/sum(lda_model_2006$theta)*100

lda_model_2006$summary <- data.frame(topic = rownames(lda_model_2006$phi),
                                     coherence = round(lda_model_2006$coherence,3),
                                     prevalence = round(lda_model_2006$prevalence,3),
                                     top_terms = apply(lda_model_2006$top_terms,2,function(x){paste(x,collapse = ", ")}))

lda_model_2006_summary <- lda_model_2006$summary %>%
  `rownames<-`(NULL)


lda_model_2006_summary %>% 
  arrange(desc(coherence)) %>%
  slice(1:5)

#################################
#
# 2007
#
#################################

cleaned_2007 <- textcleaner(topic_model_working_data[[6]]$text)
cleaned_2007 <- cleaned_2007 %>% 
  mutate(id = rownames(cleaned_2007))

# crete dtm
set.seed(2007)

dtm_r_2007 <- CreateDtm(doc_vec = cleaned_2007$x,
                        doc_names = cleaned_2007$id,
                        ngram_window = c(1,2),
                        stopword_vec = c(stopwords("en"),bh_stop_words$word),
                        verbose = F)

dtm_r_2007 <- dtm_r_2007[,colSums(dtm_r_2007)>2]

lda_model_2007 <- FitLdaModel(dtm = dtm_r_2007,
                              k = 20, # number of topic
                              iterations = 500,
                              burnin = 180,
                              alpha = 0.1,beta = 0.05,
                              optimize_alpha = T,
                              calc_likelihood = T,
                              calc_coherence = T,
                              calc_r2 = T)

lda_model_2007$top_terms <- GetTopTerms(phi = lda_model_2007$phi,M = 15)
lda_model_2007$prevalence <- colSums(lda_model_2007$theta)/sum(lda_model_2007$theta)*100

lda_model_2007$summary <- data.frame(topic = rownames(lda_model_2007$phi),
                                     coherence = round(lda_model_2007$coherence,3),
                                     prevalence = round(lda_model_2007$prevalence,3),
                                     top_terms = apply(lda_model_2007$top_terms,2,function(x){paste(x,collapse = ", ")}))

lda_model_2007_summary <- lda_model_2007$summary %>%
  `rownames<-`(NULL)


lda_model_2007_summary %>% 
  arrange(desc(coherence)) %>%
  slice(1:5)

#################################
#
# 2008
#
#################################

cleaned_2008 <- textcleaner(topic_model_working_data[[7]]$text)
cleaned_2008 <- cleaned_2008 %>% 
  mutate(id = rownames(cleaned_2008))

# crete dtm
set.seed(2008)

dtm_r_2008 <- CreateDtm(doc_vec = cleaned_2008$x,
                        doc_names = cleaned_2008$id,
                        ngram_window = c(1,2),
                        stopword_vec = c(stopwords("en"),bh_stop_words$word),
                        verbose = F)

dtm_r_2008 <- dtm_r_2008[,colSums(dtm_r_2008)>2]

lda_model_2008 <- FitLdaModel(dtm = dtm_r_2008,
                              k = 20, # number of topic
                              iterations = 500,
                              burnin = 180,
                              alpha = 0.1,beta = 0.05,
                              optimize_alpha = T,
                              calc_likelihood = T,
                              calc_coherence = T,
                              calc_r2 = T)

lda_model_2008$top_terms <- GetTopTerms(phi = lda_model_2008$phi,M = 15)
lda_model_2008$prevalence <- colSums(lda_model_2008$theta)/sum(lda_model_2008$theta)*100

lda_model_2008$summary <- data.frame(topic = rownames(lda_model_2008$phi),
                                     coherence = round(lda_model_2008$coherence,3),
                                     prevalence = round(lda_model_2008$prevalence,3),
                                     top_terms = apply(lda_model_2008$top_terms,2,function(x){paste(x,collapse = ", ")}))

lda_model_2008_summary <- lda_model_2008$summary %>%
  `rownames<-`(NULL)


lda_model_2008_summary %>% 
  arrange(desc(coherence)) %>%
  slice(1:5)

#################################
#
# 2009
#
#################################

cleaned_2009 <- textcleaner(topic_model_working_data[[8]]$text)
cleaned_2009 <- cleaned_2009 %>% 
  mutate(id = rownames(cleaned_2009))

# crete dtm
set.seed(2009)

dtm_r_2009 <- CreateDtm(doc_vec = cleaned_2009$x,
                        doc_names = cleaned_2009$id,
                        ngram_window = c(1,2),
                        stopword_vec = c(stopwords("en"),bh_stop_words$word),
                        verbose = F)

dtm_r_2009 <- dtm_r_2009[,colSums(dtm_r_2009)>2]

lda_model_2009 <- FitLdaModel(dtm = dtm_r_2009,
                              k = 20, # number of topic
                              iterations = 500,
                              burnin = 180,
                              alpha = 0.1,beta = 0.05,
                              optimize_alpha = T,
                              calc_likelihood = T,
                              calc_coherence = T,
                              calc_r2 = T)

lda_model_2009$top_terms <- GetTopTerms(phi = lda_model_2009$phi,M = 15)
lda_model_2009$prevalence <- colSums(lda_model_2009$theta)/sum(lda_model_2009$theta)*100

lda_model_2009$summary <- data.frame(topic = rownames(lda_model_2009$phi),
                                     coherence = round(lda_model_2009$coherence,3),
                                     prevalence = round(lda_model_2009$prevalence,3),
                                     top_terms = apply(lda_model_2009$top_terms,2,function(x){paste(x,collapse = ", ")}))

lda_model_2009_summary <- lda_model_2009$summary %>%
  `rownames<-`(NULL)


lda_model_2009_summary %>% 
  arrange(desc(coherence)) %>%
  slice(1:5)

#################################
#
# 2010
#
#################################

cleaned_2010 <- textcleaner(topic_model_working_data[[9]]$text)
cleaned_2010 <- cleaned_2010 %>% 
  mutate(id = rownames(cleaned_2010))

# crete dtm
set.seed(2010)

dtm_r_2010 <- CreateDtm(doc_vec = cleaned_2010$x,
                        doc_names = cleaned_2010$id,
                        ngram_window = c(1,2),
                        stopword_vec = c(stopwords("en"),bh_stop_words$word),
                        verbose = F)

dtm_r_2010 <- dtm_r_2010[,colSums(dtm_r_2010)>2]

lda_model_2010 <- FitLdaModel(dtm = dtm_r_2010,
                              k = 20, # number of topic
                              iterations = 500,
                              burnin = 180,
                              alpha = 0.1,beta = 0.05,
                              optimize_alpha = T,
                              calc_likelihood = T,
                              calc_coherence = T,
                              calc_r2 = T)

lda_model_2010$top_terms <- GetTopTerms(phi = lda_model_2010$phi,M = 15)
lda_model_2010$prevalence <- colSums(lda_model_2010$theta)/sum(lda_model_2010$theta)*100

lda_model_2010$summary <- data.frame(topic = rownames(lda_model_2010$phi),
                                     coherence = round(lda_model_2010$coherence,3),
                                     prevalence = round(lda_model_2010$prevalence,3),
                                     top_terms = apply(lda_model_2010$top_terms,2,function(x){paste(x,collapse = ", ")}))

lda_model_2010_summary <- lda_model_2010$summary %>%
  `rownames<-`(NULL)


lda_model_2010_summary %>% 
  arrange(desc(coherence)) %>%
  slice(1:5)

#################################
#
# 2011
#
#################################

cleaned_2011 <- textcleaner(topic_model_working_data[[10]]$text)
cleaned_2011 <- cleaned_2011 %>% 
  mutate(id = rownames(cleaned_2011))

# crete dtm
set.seed(2011)

dtm_r_2011 <- CreateDtm(doc_vec = cleaned_2011$x,
                        doc_names = cleaned_2011$id,
                        ngram_window = c(1,2),
                        stopword_vec = c(stopwords("en"),bh_stop_words$word),
                        verbose = F)

dtm_r_2011 <- dtm_r_2011[,colSums(dtm_r_2011)>2]

lda_model_2011 <- FitLdaModel(dtm = dtm_r_2011,
                              k = 20, # number of topic
                              iterations = 500,
                              burnin = 180,
                              alpha = 0.1,beta = 0.05,
                              optimize_alpha = T,
                              calc_likelihood = T,
                              calc_coherence = T,
                              calc_r2 = T)

lda_model_2011$top_terms <- GetTopTerms(phi = lda_model_2011$phi,M = 15)
lda_model_2011$prevalence <- colSums(lda_model_2011$theta)/sum(lda_model_2011$theta)*100

lda_model_2011$summary <- data.frame(topic = rownames(lda_model_2011$phi),
                                     coherence = round(lda_model_2011$coherence,3),
                                     prevalence = round(lda_model_2011$prevalence,3),
                                     top_terms = apply(lda_model_2011$top_terms,2,function(x){paste(x,collapse = ", ")}))

lda_model_2011_summary <- lda_model_2011$summary %>%
  `rownames<-`(NULL)


lda_model_2011_summary %>% 
  arrange(desc(coherence)) %>%
  slice(1:5)

#################################
#
# 2012
#
#################################

cleaned_2012 <- textcleaner(topic_model_working_data[[10]]$text)
cleaned_2012 <- cleaned_2012 %>% 
  mutate(id = rownames(cleaned_2012))

# crete dtm
set.seed(2012)

dtm_r_2012 <- CreateDtm(doc_vec = cleaned_2012$x,
                        doc_names = cleaned_2012$id,
                        ngram_window = c(1,2),
                        stopword_vec = c(stopwords("en"),bh_stop_words$word),
                        verbose = F)

dtm_r_2012 <- dtm_r_2012[,colSums(dtm_r_2012)>2]

lda_model_2012 <- FitLdaModel(dtm = dtm_r_2012,
                              k = 20, # number of topic
                              iterations = 500,
                              burnin = 180,
                              alpha = 0.1,beta = 0.05,
                              optimize_alpha = T,
                              calc_likelihood = T,
                              calc_coherence = T,
                              calc_r2 = T)

lda_model_2012$top_terms <- GetTopTerms(phi = lda_model_2012$phi,M = 15)
lda_model_2012$prevalence <- colSums(lda_model_2012$theta)/sum(lda_model_2012$theta)*100

lda_model_2012$summary <- data.frame(topic = rownames(lda_model_2012$phi),
                                     coherence = round(lda_model_2012$coherence,3),
                                     prevalence = round(lda_model_2012$prevalence,3),
                                     top_terms = apply(lda_model_2012$top_terms,2,function(x){paste(x,collapse = ", ")}))

lda_model_2012_summary <- lda_model_2012$summary %>%
  `rownames<-`(NULL)


lda_model_2012_summary %>% 
  arrange(desc(coherence)) %>%
  slice(1:5)

#################################
#
# 2013
#
#################################

cleaned_2013 <- textcleaner(topic_model_working_data[[11]]$text)
cleaned_2013 <- cleaned_2013 %>% 
  mutate(id = rownames(cleaned_2013))

# crete dtm
set.seed(2013)

dtm_r_2013 <- CreateDtm(doc_vec = cleaned_2013$x,
                        doc_names = cleaned_2013$id,
                        ngram_window = c(1,2),
                        stopword_vec = c(stopwords("en"),bh_stop_words$word),
                        verbose = F)

dtm_r_2013 <- dtm_r_2013[,colSums(dtm_r_2013)>2]

lda_model_2013 <- FitLdaModel(dtm = dtm_r_2013,
                              k = 20, # number of topic
                              iterations = 500,
                              burnin = 180,
                              alpha = 0.1,beta = 0.05,
                              optimize_alpha = T,
                              calc_likelihood = T,
                              calc_coherence = T,
                              calc_r2 = T)

lda_model_2013$top_terms <- GetTopTerms(phi = lda_model_2013$phi,M = 15)
lda_model_2013$prevalence <- colSums(lda_model_2013$theta)/sum(lda_model_2013$theta)*100

lda_model_2013$summary <- data.frame(topic = rownames(lda_model_2013$phi),
                                     coherence = round(lda_model_2013$coherence,3),
                                     prevalence = round(lda_model_2013$prevalence,3),
                                     top_terms = apply(lda_model_2013$top_terms,2,function(x){paste(x,collapse = ", ")}))

lda_model_2013_summary <- lda_model_2013$summary %>%
  `rownames<-`(NULL)


lda_model_2013_summary %>% 
  arrange(desc(coherence)) %>%
  slice(1:5)


#################################
#
# 2014
#
#################################

cleaned_2014 <- textcleaner(topic_model_working_data[[12]]$text)
cleaned_2014 <- cleaned_2014 %>% 
  mutate(id = rownames(cleaned_2014))

# crete dtm
set.seed(2014)

dtm_r_2014 <- CreateDtm(doc_vec = cleaned_2014$x,
                        doc_names = cleaned_2014$id,
                        ngram_window = c(1,2),
                        stopword_vec = c(stopwords("en"),bh_stop_words$word),
                        verbose = F)

dtm_r_2014 <- dtm_r_2014[,colSums(dtm_r_2014)>2]

lda_model_2014 <- FitLdaModel(dtm = dtm_r_2014,
                              k = 20, # number of topic
                              iterations = 500,
                              burnin = 180,
                              alpha = 0.1,beta = 0.05,
                              optimize_alpha = T,
                              calc_likelihood = T,
                              calc_coherence = T,
                              calc_r2 = T)

lda_model_2014$top_terms <- GetTopTerms(phi = lda_model_2014$phi,M = 15)
lda_model_2014$prevalence <- colSums(lda_model_2014$theta)/sum(lda_model_2014$theta)*100

lda_model_2014$summary <- data.frame(topic = rownames(lda_model_2014$phi),
                                     coherence = round(lda_model_2014$coherence,3),
                                     prevalence = round(lda_model_2014$prevalence,3),
                                     top_terms = apply(lda_model_2014$top_terms,2,function(x){paste(x,collapse = ", ")}))

lda_model_2014_summary <- lda_model_2014$summary %>%
  `rownames<-`(NULL)


lda_model_2014_summary %>% 
  arrange(desc(coherence)) %>%
  slice(1:5)

#################################
#
# 2015
#
#################################

cleaned_2015 <- textcleaner(topic_model_working_data[[13]]$text)
cleaned_2015 <- cleaned_2015 %>% 
  mutate(id = rownames(cleaned_2015))

# crete dtm
set.seed(2015)

dtm_r_2015 <- CreateDtm(doc_vec = cleaned_2015$x,
                        doc_names = cleaned_2015$id,
                        ngram_window = c(1,2),
                        stopword_vec = c(stopwords("en"),bh_stop_words$word),
                        verbose = F)

dtm_r_2015 <- dtm_r_2015[,colSums(dtm_r_2015)>2]

lda_model_2015 <- FitLdaModel(dtm = dtm_r_2015,
                              k = 20, # number of topic
                              iterations = 500,
                              burnin = 180,
                              alpha = 0.1,beta = 0.05,
                              optimize_alpha = T,
                              calc_likelihood = T,
                              calc_coherence = T,
                              calc_r2 = T)

lda_model_2015$top_terms <- GetTopTerms(phi = lda_model_2015$phi,M = 15)
lda_model_2015$prevalence <- colSums(lda_model_2015$theta)/sum(lda_model_2015$theta)*100

lda_model_2015$summary <- data.frame(topic = rownames(lda_model_2015$phi),
                                     coherence = round(lda_model_2015$coherence,3),
                                     prevalence = round(lda_model_2015$prevalence,3),
                                     top_terms = apply(lda_model_2015$top_terms,2,function(x){paste(x,collapse = ", ")}))

lda_model_2015_summary <- lda_model_2015$summary %>%
  `rownames<-`(NULL)


lda_model_2015_summary %>% 
  arrange(desc(coherence)) %>%
  slice(1:5)


#################################
#
# 2016
#
#################################

cleaned_2016 <- textcleaner(topic_model_working_data[[14]]$text)
cleaned_2016 <- cleaned_2016 %>% 
  mutate(id = rownames(cleaned_2016))

# crete dtm
set.seed(2016)

dtm_r_2016 <- CreateDtm(doc_vec = cleaned_2016$x,
                        doc_names = cleaned_2016$id,
                        ngram_window = c(1,2),
                        stopword_vec = c(stopwords("en"),bh_stop_words$word),
                        verbose = F)

dtm_r_2016 <- dtm_r_2016[,colSums(dtm_r_2016)>2]

lda_model_2016 <- FitLdaModel(dtm = dtm_r_2016,
                              k = 20, # number of topic
                              iterations = 500,
                              burnin = 180,
                              alpha = 0.1,beta = 0.05,
                              optimize_alpha = T,
                              calc_likelihood = T,
                              calc_coherence = T,
                              calc_r2 = T)

lda_model_2016$top_terms <- GetTopTerms(phi = lda_model_2016$phi,M = 15)
lda_model_2016$prevalence <- colSums(lda_model_2016$theta)/sum(lda_model_2016$theta)*100

lda_model_2016$summary <- data.frame(topic = rownames(lda_model_2016$phi),
                                     coherence = round(lda_model_2016$coherence,3),
                                     prevalence = round(lda_model_2016$prevalence,3),
                                     top_terms = apply(lda_model_2016$top_terms,2,function(x){paste(x,collapse = ", ")}))

lda_model_2016_summary <- lda_model_2016$summary %>%
  `rownames<-`(NULL)


lda_model_2016_summary %>% 
  arrange(desc(coherence)) %>%
  slice(1:5)


#################################
#
# 2017
#
#################################

cleaned_2017 <- textcleaner(topic_model_working_data[[15]]$text)
cleaned_2017 <- cleaned_2017 %>% 
  mutate(id = rownames(cleaned_2017))

# crete dtm
set.seed(2017)

dtm_r_2017 <- CreateDtm(doc_vec = cleaned_2017$x,
                        doc_names = cleaned_2017$id,
                        ngram_window = c(1,2),
                        stopword_vec = c(stopwords("en"),bh_stop_words$word),
                        verbose = F)

dtm_r_2017 <- dtm_r_2017[,colSums(dtm_r_2017)>2]

lda_model_2017 <- FitLdaModel(dtm = dtm_r_2017,
                              k = 20, # number of topic
                              iterations = 500,
                              burnin = 180,
                              alpha = 0.1,beta = 0.05,
                              optimize_alpha = T,
                              calc_likelihood = T,
                              calc_coherence = T,
                              calc_r2 = T)

lda_model_2017$top_terms <- GetTopTerms(phi = lda_model_2017$phi,M = 15)
lda_model_2017$prevalence <- colSums(lda_model_2017$theta)/sum(lda_model_2017$theta)*100

lda_model_2017$summary <- data.frame(topic = rownames(lda_model_2017$phi),
                                     coherence = round(lda_model_2017$coherence,3),
                                     prevalence = round(lda_model_2017$prevalence,3),
                                     top_terms = apply(lda_model_2017$top_terms,2,function(x){paste(x,collapse = ", ")}))

lda_model_2017_summary <- lda_model_2017$summary %>%
  `rownames<-`(NULL)


lda_model_2017_summary %>% 
  arrange(desc(coherence)) %>%
  slice(1:5)


#################################
#
# 2018
#
#################################

cleaned_2018 <- textcleaner(topic_model_working_data[[16]]$text)
cleaned_2018 <- cleaned_2018 %>% 
  mutate(id = rownames(cleaned_2018))

# crete dtm
set.seed(2018)

dtm_r_2018 <- CreateDtm(doc_vec = cleaned_2018$x,
                        doc_names = cleaned_2018$id,
                        ngram_window = c(1,2),
                        stopword_vec = c(stopwords("en"),bh_stop_words$word),
                        verbose = F)

dtm_r_2018 <- dtm_r_2018[,colSums(dtm_r_2018)>2]

lda_model_2018 <- FitLdaModel(dtm = dtm_r_2018,
                              k = 20, # number of topic
                              iterations = 500,
                              burnin = 180,
                              alpha = 0.1,beta = 0.05,
                              optimize_alpha = T,
                              calc_likelihood = T,
                              calc_coherence = T,
                              calc_r2 = T)

lda_model_2018$top_terms <- GetTopTerms(phi = lda_model_2018$phi,M = 15)
lda_model_2018$prevalence <- colSums(lda_model_2018$theta)/sum(lda_model_2018$theta)*100

lda_model_2018$summary <- data.frame(topic = rownames(lda_model_2018$phi),
                                     coherence = round(lda_model_2018$coherence,3),
                                     prevalence = round(lda_model_2018$prevalence,3),
                                     top_terms = apply(lda_model_2018$top_terms,2,function(x){paste(x,collapse = ", ")}))

lda_model_2018_summary <- lda_model_2018$summary %>%
  `rownames<-`(NULL)


lda_model_2018_summary %>% 
  arrange(desc(coherence)) %>%
  slice(1:5)



#################################
#
# 2019
#
#################################

cleaned_2019 <- textcleaner(topic_model_working_data[[17]]$text)
cleaned_2019 <- cleaned_2019 %>% 
  mutate(id = rownames(cleaned_2019))

# crete dtm
set.seed(2019)

dtm_r_2019 <- CreateDtm(doc_vec = cleaned_2019$x,
                        doc_names = cleaned_2019$id,
                        ngram_window = c(1,2),
                        stopword_vec = c(stopwords("en"),bh_stop_words$word),
                        verbose = F)

dtm_r_2019 <- dtm_r_2019[,colSums(dtm_r_2019)>2]

lda_model_2019 <- FitLdaModel(dtm = dtm_r_2019,
                              k = 20, # number of topic
                              iterations = 500,
                              burnin = 180,
                              alpha = 0.1,beta = 0.05,
                              optimize_alpha = T,
                              calc_likelihood = T,
                              calc_coherence = T,
                              calc_r2 = T)

lda_model_2019$top_terms <- GetTopTerms(phi = lda_model_2019$phi,M = 15)
lda_model_2019$prevalence <- colSums(lda_model_2019$theta)/sum(lda_model_2019$theta)*100

lda_model_2019$summary <- data.frame(topic = rownames(lda_model_2019$phi),
                                     coherence = round(lda_model_2019$coherence,3),
                                     prevalence = round(lda_model_2019$prevalence,3),
                                     top_terms = apply(lda_model_2019$top_terms,2,function(x){paste(x,collapse = ", ")}))

lda_model_2019_summary <- lda_model_2019$summary %>%
  `rownames<-`(NULL)


lda_model_2019_summary %>% 
  arrange(desc(coherence)) %>%
  slice(1:5)



#################################
#
# 2020
#
#################################

cleaned_2020 <- textcleaner(topic_model_working_data[[18]]$text)
cleaned_2020 <- cleaned_2020 %>% 
  mutate(id = rownames(cleaned_2020))

# crete dtm
set.seed(2020)

dtm_r_2020 <- CreateDtm(doc_vec = cleaned_2020$x,
                        doc_names = cleaned_2020$id,
                        ngram_window = c(1,2),
                        stopword_vec = c(stopwords("en"),bh_stop_words$word),
                        verbose = F)

dtm_r_2020 <- dtm_r_2020[,colSums(dtm_r_2020)>2]

lda_model_2020 <- FitLdaModel(dtm = dtm_r_2020,
                              k = 20, # number of topic
                              iterations = 500,
                              burnin = 180,
                              alpha = 0.1,beta = 0.05,
                              optimize_alpha = T,
                              calc_likelihood = T,
                              calc_coherence = T,
                              calc_r2 = T)

lda_model_2020$top_terms <- GetTopTerms(phi = lda_model_2020$phi,M = 15)
lda_model_2020$prevalence <- colSums(lda_model_2020$theta)/sum(lda_model_2020$theta)*100

lda_model_2020$summary <- data.frame(topic = rownames(lda_model_2020$phi),
                                     coherence = round(lda_model_2020$coherence,3),
                                     prevalence = round(lda_model_2020$prevalence,3),
                                     top_terms = apply(lda_model_2020$top_terms,2,function(x){paste(x,collapse = ", ")}))

lda_model_2020_summary <- lda_model_2020$summary %>%
  `rownames<-`(NULL)


lda_model_2020_summary %>% 
  arrange(desc(coherence)) %>%
  slice(1:5)



#################################
#
# 2021
#
#################################

cleaned_2021 <- textcleaner(topic_model_working_data[[19]]$text)
cleaned_2021 <- cleaned_2021 %>% 
  mutate(id = rownames(cleaned_2021))

# crete dtm
set.seed(2021)

dtm_r_2021 <- CreateDtm(doc_vec = cleaned_2021$x,
                        doc_names = cleaned_2021$id,
                        ngram_window = c(1,2),
                        stopword_vec = c(stopwords("en"),bh_stop_words$word),
                        verbose = F)

dtm_r_2021 <- dtm_r_2021[,colSums(dtm_r_2021)>2]

lda_model_2021 <- FitLdaModel(dtm = dtm_r_2021,
                              k = 20, # number of topic
                              iterations = 500,
                              burnin = 180,
                              alpha = 0.1,beta = 0.05,
                              optimize_alpha = T,
                              calc_likelihood = T,
                              calc_coherence = T,
                              calc_r2 = T)

lda_model_2021$top_terms <- GetTopTerms(phi = lda_model_2021$phi,M = 15)
lda_model_2021$prevalence <- colSums(lda_model_2021$theta)/sum(lda_model_2021$theta)*100

lda_model_2021$summary <- data.frame(topic = rownames(lda_model_2021$phi),
                                     coherence = round(lda_model_2021$coherence,3),
                                     prevalence = round(lda_model_2021$prevalence,3),
                                     top_terms = apply(lda_model_2021$top_terms,2,function(x){paste(x,collapse = ", ")}))

lda_model_2021_summary <- lda_model_2021$summary %>%
  `rownames<-`(NULL)


lda_model_2021_summary %>% 
  arrange(desc(coherence)) %>%
  slice(1:5)


#################################
#
# 2022
#
#################################

cleaned_2022 <- textcleaner(topic_model_working_data[[20]]$text)
cleaned_2022 <- cleaned_2022 %>% 
  mutate(id = rownames(cleaned_2022))

# crete dtm
set.seed(2022)

dtm_r_2022 <- CreateDtm(doc_vec = cleaned_2022$x,
                        doc_names = cleaned_2022$id,
                        ngram_window = c(1,2),
                        stopword_vec = c(stopwords("en"),bh_stop_words$word),
                        verbose = F)

dtm_r_2022 <- dtm_r_2022[,colSums(dtm_r_2022)>2]

lda_model_2022 <- FitLdaModel(dtm = dtm_r_2022,
                              k = 20, # number of topic
                              iterations = 500,
                              burnin = 180,
                              alpha = 0.1,beta = 0.05,
                              optimize_alpha = T,
                              calc_likelihood = T,
                              calc_coherence = T,
                              calc_r2 = T)

lda_model_2022$top_terms <- GetTopTerms(phi = lda_model_2022$phi,M = 15)
lda_model_2022$prevalence <- colSums(lda_model_2022$theta)/sum(lda_model_2022$theta)*100

lda_model_2022$summary <- data.frame(topic = rownames(lda_model_2022$phi),
                                     coherence = round(lda_model_2022$coherence,3),
                                     prevalence = round(lda_model_2022$prevalence,3),
                                     top_terms = apply(lda_model_2022$top_terms,2,function(x){paste(x,collapse = ", ")}))

lda_model_2022_summary <- lda_model_2022$summary %>%
  `rownames<-`(NULL)


lda_model_2022_summary %>% 
  arrange(desc(coherence)) %>%
  slice(1:5)



#################################
#
# 2023
#
#################################

cleaned_2023 <- textcleaner(topic_model_working_data[[21]]$text)
cleaned_2023 <- cleaned_2023 %>% 
  mutate(id = rownames(cleaned_2023))

# crete dtm
set.seed(2023)

dtm_r_2023 <- CreateDtm(doc_vec = cleaned_2023$x,
                        doc_names = cleaned_2023$id,
                        ngram_window = c(1,2),
                        stopword_vec = c(stopwords("en"),bh_stop_words$word),
                        verbose = F)

dtm_r_2023 <- dtm_r_2023[,colSums(dtm_r_2023)>2]

lda_model_2023 <- FitLdaModel(dtm = dtm_r_2023,
                              k = 20, # number of topic
                              iterations = 500,
                              burnin = 180,
                              alpha = 0.1,beta = 0.05,
                              optimize_alpha = T,
                              calc_likelihood = T,
                              calc_coherence = T,
                              calc_r2 = T)

lda_model_2023$top_terms <- GetTopTerms(phi = lda_model_2023$phi,M = 15)
lda_model_2023$prevalence <- colSums(lda_model_2023$theta)/sum(lda_model_2023$theta)*100

lda_model_2023$summary <- data.frame(topic = rownames(lda_model_2023$phi),
                                     coherence = round(lda_model_2023$coherence,3),
                                     prevalence = round(lda_model_2023$prevalence,3),
                                     top_terms = apply(lda_model_2023$top_terms,2,function(x){paste(x,collapse = ", ")}))

lda_model_2023_summary <- lda_model_2023$summary %>%
  `rownames<-`(NULL)


lda_model_2023_summary %>% 
  arrange(desc(coherence)) %>%
  slice(1:5)

#################################
#
# Combine Yearly Models
#
#################################

lda_2002_top5 <- lda_model_2002_summary %>% 
  mutate(year = '2002') %>%
  arrange(desc(coherence)) %>%
  slice(1:5)

lda_2003_top5 <- lda_model_2003_summary %>% 
  mutate(year = '2003') %>%
  arrange(desc(coherence)) %>%
  slice(1:5)

lda_2004_top5 <- lda_model_2004_summary %>% 
  mutate(year = '2004') %>%
  arrange(desc(coherence)) %>%
  slice(1:5)

lda_2005_top5 <- lda_model_2005_summary %>% 
  mutate(year = '2005') %>%
  arrange(desc(coherence)) %>%
  slice(1:5)

lda_2006_top5 <- lda_model_2006_summary %>% 
  mutate(year = '2006') %>%
  arrange(desc(coherence)) %>%
  slice(1:5)

lda_2007_top5 <- lda_model_2007_summary %>% 
  mutate(year = '2007') %>%
  arrange(desc(coherence)) %>%
  slice(1:5)

lda_2008_top5 <- lda_model_2008_summary %>% 
  mutate(year = '2008') %>%
  arrange(desc(coherence)) %>%
  slice(1:5)

lda_2009_top5 <- lda_model_2009_summary %>% 
  mutate(year = '2009') %>%
  arrange(desc(coherence)) %>%
  slice(1:5)

lda_2010_top5 <- lda_model_2010_summary %>% 
  mutate(year = '2010') %>%
  arrange(desc(coherence)) %>%
  slice(1:5)

lda_2011_top5 <- lda_model_2011_summary %>% 
  mutate(year = '2011') %>%
  arrange(desc(coherence)) %>%
  slice(1:5)

lda_2012_top5 <- lda_model_2012_summary %>% 
  mutate(year = '2012') %>%
  arrange(desc(coherence)) %>%
  slice(1:5)

lda_2013_top5 <- lda_model_2013_summary %>% 
  mutate(year = '2013') %>%
  arrange(desc(coherence)) %>%
  slice(1:5)

lda_2014_top5 <- lda_model_2014_summary %>% 
  mutate(year = '2014') %>%
  arrange(desc(coherence)) %>%
  slice(1:5)

lda_2015_top5 <- lda_model_2015_summary %>% 
  mutate(year = '2015') %>%
  arrange(desc(coherence)) %>%
  slice(1:5)

lda_2016_top5 <- lda_model_2016_summary %>% 
  mutate(year = '2016') %>%
  arrange(desc(coherence)) %>%
  slice(1:5)

lda_2017_top5 <- lda_model_2017_summary %>% 
  mutate(year = '2017') %>%
  arrange(desc(coherence)) %>%
  slice(1:5)

lda_2018_top5 <- lda_model_2018_summary %>% 
  mutate(year = '2018') %>%
  arrange(desc(coherence)) %>%
  slice(1:5)

lda_2019_top5 <- lda_model_2019_summary %>% 
  mutate(year = '2019') %>%
  arrange(desc(coherence)) %>%
  slice(1:5)

lda_2020_top5 <- lda_model_2020_summary %>% 
  mutate(year = '2020') %>%
  arrange(desc(coherence)) %>%
  slice(1:5)

lda_2021_top5 <- lda_model_2021_summary %>% 
  mutate(year = '2021') %>%
  arrange(desc(coherence)) %>%
  slice(1:5)

lda_2022_top5 <- lda_model_2022_summary %>% 
  mutate(year = '2022') %>%
  arrange(desc(coherence)) %>%
  slice(1:5)

lda_2023_top5 <- lda_model_2023_summary %>% 
  mutate(year = '2023') %>%
  arrange(desc(coherence)) %>%
  slice(1:5)

all_years_top5_topics <- rbind(lda_2002_top5,
                               lda_2003_top5,
                               lda_2004_top5,
                               lda_2005_top5,
                               lda_2006_top5,
                               lda_2007_top5,
                               lda_2008_top5,
                               lda_2009_top5,
                               lda_2010_top5,
                               lda_2011_top5,
                               lda_2012_top5,
                               lda_2013_top5,
                               lda_2014_top5,
                               lda_2015_top5,
                               lda_2016_top5,
                               lda_2017_top5,
                               lda_2018_top5,
                               lda_2019_top5,
                               lda_2020_top5,
                               lda_2021_top5,
                               lda_2022_top5,
                               lda_2023_top5)

View(all_years_top5_topics)


setwd("/Users/KSHAN006/OneDrive - The Hershey Company/Desktop/barharbor_council_minutes/")
write_csv(all_years_top5_topics,"all_years_top5_topics.csv")  


final_topics <- read.csv("all_years_top5_topics_ammended.csv")

#################################
#
# Most Prevalent Topic
#
#################################

final_topics %>%
  mutate(overall_topic = topic.1) %>%
  filter(overall_topic != 'ignore') %>%
  select(overall_topic,year,prevalence) %>%
  group_by(year) %>%
  slice_max(prevalence, n = 1) %>%
  ungroup() %>%
  ggplot(aes(x = year, y = prevalence, fill = overall_topic)) + 
  geom_bar(stat="identity", position="dodge", color = I("black")) + 
  geom_text(aes(label = overall_topic), hjust=-0.05, color="black") +
  theme_bw() + 
  theme(legend.position="left") + 
  xlab("") + ylab("Topic Prevalence") + 
  theme(legend.position = "none") +
  coord_flip() + 
  scale_y_continuous(limits = c(0,20)) + 
  scale_x_reverse() + 
  ggtitle("Most Prevalent Discussion Topic of Bar Harbor Town Council",
          subtitle = "By Year, 2002-2023, obtained from Town Council Meeting Minutes") + 
  scale_fill_viridis_d(option = "F")
  
#################################
#
# Top 5 Most Prevalent By Year
#
#################################


final_topics %>%
  mutate(overall_topic = topic.1) %>%
  filter(overall_topic != 'ignore') %>%
  select(overall_topic,year,prevalence) %>%
  group_by(year, overall_topic) %>%
  slice_max(prevalence) %>%
  ungroup() %>%
  mutate(year = as.factor(year),
         overall_topic = reorder_within(overall_topic, prevalence, year)) %>%
  ggplot(aes(x = reorder(overall_topic, prevalence), y = prevalence, fill = year)) + 
  geom_col(color = I("black")) + 
  theme_bw() + 
  facet_wrap(~year, ncol = 5, scales = "free") +
  coord_flip() + 
  theme(legend.position = "none") + 
  scale_x_reordered() + 
  xlab("Overall Prevalence") + ylab("") + 
  ggtitle("Most Prevalent Discussion Topics of Bar Harbor Town Council",
          subtitle = "By Year, 2002-2023") + 
  scale_fill_viridis_d(option = "B")
  