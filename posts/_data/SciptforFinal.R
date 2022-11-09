### READ IN THE DATA

#write csv has been commented out due to it continously trying to save an "updated version" in Git. 

reddit_data <- read.csv(here::here("posts", "_data", "loveisblindjapan.csv"))

twitter1 <- read.csv(here::here("posts", "_data", "tweets.csv"))

twitter2 <- read.csv(here::here("posts", "_data", "tweets#.csv"))

reddit <- subset(reddit_data, select = c("body", "created_utc")) 

reddit$created_utc <- as.Date.POSIXct(reddit$created_utc)

reddit <- reddit %>% 
  select(text = body, 
         date = created_utc)
# remove deleted or removed comments by moderators of the subreddit (ones that only contain [deleted] or [removed])
reddit <- reddit %>% 
  filter(!text == '[deleted]') %>% 
  filter(!text == '[removed]')

#remove counting column
twitter1 <- twitter1 %>% select(!c(X, User))
twitter2 <- twitter2 %>% select(!c(X, User))

twitter <- merge(twitter1, twitter2, by=c('Tweet','Tweet', 'Date', 'Date'),all=T, ignore_case =T)
#write.csv(twitter, here::here("posts", "_data", "twitter.csv") , all(T) )

names(twitter) <- tolower(names(twitter))
twitter <- twitter %>% 
  rename_at('tweet', ~ 'text', 
            'Date' ~ 'date')
twitter$date <- as.Date(strftime(twitter$date, format="%Y-%m-%d"))

# remove duplicate tweets
twitter <- twitter %>% distinct(text, date, .keep_all = TRUE)

#check for duplicate tweets
twitter %in% unique(twitter[ duplicated(twitter)]) 

# Twiter Lemmitized
twitter_corpus <- subset(twitter, detect_language(twitter) == "en")
twitter_corpus <- corpus(twitter_corpus)
twitter_corpus <- twitter_corpus[!is.na(twitter_corpus)]
twittersummary <- summary(twitter_corpus)
twitter_corpus <- trimws(gsub("[[:digit:]]{1,4}-[[:digit:]]{1,4}-[[:digit:]]{1,4}", "", twitter_corpus))

mystopwords <- c("love is blind japan", "#loveisbindjapan", "#LoveIsBlindJapan","Love Is Blind Japan","Love is Blind Japan", "Love Is Blind: Japan", "#loveisblind", "ラブイズブラインドjapan", "#ラブイズブラインドjapan", "loveisblind", "#loveisblind2", "blind:japan", "blind", "show")

twitter_corpus_tokens <- tokens(twitter_corpus, 
                                remove_punct = T,
                                remove_numbers = T,
                                remove_symbols = T,
                                remove_url = T) %>% 
  tokens_tolower() %>% 
  tokens_remove(pattern = phrase(mystopwords), valuetype = 'fixed') %>% 
  tokens_select(pattern = stopwords("en"), selection = "remove")

twitter_lemmitized <- tokens_replace(twitter_corpus_tokens, 
                                     pattern = lexicon::hash_lemmas$token, 
                                     replacement = lexicon::hash_lemmas$lemma)

# Reddit Lemmitized

reddit_corpus <- subset(reddit, detect_language(reddit) == "en")
reddit_corpus <- corpus(reddit_corpus)
reddit_corpus <- reddit_corpus[!is.na(reddit_corpus)]
redditsummary <- summary(reddit_corpus)

reddit_corpus <- trimws(gsub("[[:digit:]]{1,4}-[[:digit:]]{1,4}-[[:digit:]]{1,4}", "", reddit_corpus))

reddit_corpus_tokens <- tokens(reddit_corpus, 
                               remove_punct = T,
                               remove_numbers = T, 
                               remove_symbols = T,
                               remove_url = T) %>% 
  tokens_tolower() %>% 
  tokens_select(pattern = stopwords("en"), selection = "remove")

reddit_lemmitized <- tokens_replace(reddit_corpus_tokens, 
                                    pattern = lexicon::hash_lemmas$token, 
                                    replacement = lexicon::hash_lemmas$lemma)

### DICTIONARY METHOD

#Twitter NRC

twitterDfm_nrc <- dfm(tokens(twitter_lemmitized,
                             remove_punct = TRUE),
                      tolower = TRUE) %>%
  dfm_lookup(data_dictionary_NRC)

tdf_nrc <- convert(twitterDfm_nrc, to = "data.frame")
tdf_nrc$polarity <- (tdf_nrc$positive - tdf_nrc$negative)/(tdf_nrc$positive + tdf_nrc$negative)
tdf_nrc$polarity[which((tdf_nrc$positive + tdf_nrc$negative) == 0)] <- 0

twitter_corpus_dfm <- twitter_lemmitized %>% 
  dfm() %>% 
  dfm_remove(stopwords('english')) %>% 
  dfm_trim(min_termfreq = 30, verbose = FALSE)

# Reddit NRC

redditDfm_nrc <- dfm(tokens(reddit_lemmitized,
                            remove_punct = TRUE),
                     tolower = TRUE) %>%
  dfm_lookup(data_dictionary_NRC)

rdf_nrc <- convert(redditDfm_nrc, to = "data.frame")
rdf_nrc$polarity <- (rdf_nrc$positive - rdf_nrc$negative)/(rdf_nrc$positive + rdf_nrc$negative)
rdf_nrc$polarity[which((rdf_nrc$positive + rdf_nrc$negative) == 0)] <- 0

reddit_corpus_dfm <- reddit_lemmitized %>% 
  dfm() %>% 
  dfm_remove(stopwords('english')) %>% 
  dfm_trim(min_termfreq = 30, verbose = FALSE)

## EMOTIONS BY DATE

twitter_emotions <- twitter_emotions %>% 
  filter(word_count > 100)

reddit_emotions <- reddit_emotions %>% 
  filter(word_count > 100)

anger <- ggplot() +
  geom_smooth(data = reddit_emotions, aes(x = date, y = anger_percent, color="Reddit")) + geom_smooth(data = twitter_emotions, aes(x = date, y = anger_percent, color = "Twitter")) +
  scale_color_manual(name='Social Medias',
                     breaks=c('Reddit', 'Twitter'),
                     values=c('Reddit'='red', 'Twitter'='blue')) +
  labs(title="Twitter vs Reddit anger percentage by date")

anticipation <- ggplot() +
  geom_smooth(data = reddit_emotions, aes(x = date, y = anticipation_percent, color="Reddit")) + geom_smooth(data = twitter_emotions, aes(x = date, y = anticipation_percent, color = "Twitter")) +
  scale_color_manual(name='Social Medias',
                     breaks=c('Reddit', 'Twitter'),
                     values=c('Reddit'='red', 'Twitter'='blue')) +
  labs(title="Twitter vs Reddit anticipation percentage by date")

disgust <- ggplot() +
  geom_smooth(data = reddit_emotions, aes(x = date, y = disgust_percent, color="Reddit")) + geom_smooth(data = twitter_emotions, aes(x = date, y = disgust_percent, color = "Twitter")) +
  scale_color_manual(name='Social Medias',
                     breaks=c('Reddit', 'Twitter'),
                     values=c('Reddit'='red', 'Twitter'='blue')) +
  labs(title="Twitter vs Reddit disgust percentage by date")

fear <- ggplot() +
  geom_smooth(data = reddit_emotions, aes(x = date, y = fear_percent, color="Reddit")) + geom_smooth(data = twitter_emotions, aes(x = date, y = fear_percent, color = "Twitter")) +
  scale_color_manual(name='Social Medias',
                     breaks=c('Reddit', 'Twitter'),
                     values=c('Reddit'='red', 'Twitter'='blue')) +
  labs(title="Twitter vs Reddit fear percentage by date")

joy <- ggplot() +
  geom_smooth(data = reddit_emotions, aes(x = date, y = joy_percent, color="Reddit")) + geom_smooth(data = twitter_emotions, aes(x = date, y = joy_percent, color = "Twitter")) +
  scale_color_manual(name='Social Medias',
                     breaks=c('Reddit', 'Twitter'),
                     values=c('Reddit'='red', 'Twitter'='blue')) +
  labs(title="Twitter vs Reddit joy percentage by date")

sadness <- ggplot() +
  geom_smooth(data = reddit_emotions, aes(x = date, y = sadness_percent, color="Reddit")) + geom_smooth(data = twitter_emotions, aes(x = date, y = sadness_percent, color = "Twitter")) +
  scale_color_manual(name='Social Medias',
                     breaks=c('Reddit', 'Twitter'),
                     values=c('Reddit'='red', 'Twitter'='blue')) +
  labs(title="Twitter vs Reddit sadness percentage by date")

surprise <- ggplot() +
  geom_smooth(data = reddit_emotions, aes(x = date, y = surprise_percent, color="Reddit")) + geom_smooth(data = twitter_emotions, aes(x = date, y = surprise_percent, color = "Twitter")) +
  scale_color_manual(name='Social Medias',
                     breaks=c('Reddit', 'Twitter'),
                     values=c('Reddit'='red', 'Twitter'='blue')) +
  labs(title="Twitter vs Reddit surprise percentage by date")

trust <- ggplot() +
  geom_smooth(data = reddit_emotions, aes(x = date, y = trust_percent, color="Reddit")) + geom_smooth(data = twitter_emotions, aes(x = date, y = trust_percent, color = "Twitter")) +
  scale_color_manual(name='Social Medias',
                     breaks=c('Reddit', 'Twitter'),
                     values=c('Reddit'='red', 'Twitter'='blue')) +
  labs(title="Twitter vs Reddit trust percentage by date")


negative <- ggplot() +
  geom_smooth(data = reddit_emotions, aes(x = date, y = negative_percent, color="Reddit")) + geom_smooth(data = twitter_emotions, aes(x = date, y = negative_percent, color = "Twitter")) +
  scale_color_manual(name='Social Medias',
                     breaks=c('Reddit', 'Twitter'),
                     values=c('Reddit'='red', 'Twitter'='blue')) +
  labs(title="Twitter vs Reddit negativeness percentage by date")

positive <- ggplot() +
  geom_smooth(data = reddit_emotions, aes(x = date, y = positive_percent, color="Reddit")) + geom_smooth(data = twitter_emotions, aes(x = date, y = positive_percent, color = "Twitter")) +
  scale_color_manual(name='Social Medias',
                     breaks=c('Reddit', 'Twitter'),
                     values=c('Reddit'='red', 'Twitter'='blue')) +
  labs(title="Twitter vs Reddit positveness percentage by date")


twitter_reddit_sentiment <- ggplot() +
  geom_smooth(data = reddit_emotions, aes(x = date, y = polarity, color="Reddit")) + geom_smooth(data = twitter_emotions, aes(x = date, y = polarity, color = "Twitter")) + 
  scale_color_manual(name='Social Medias',
                     breaks=c('Reddit', 'Twitter'),
                     values=c('Reddit'='red', 'Twitter'='blue')) +
  labs(title="Twitter vs Reddit Average Dictionary Sentiment")

## WORD CLOUDS

twitterwordcloud <- textplot_wordcloud(twitter_corpus_dfm, max_words=200, color="blue")
redditwordcloud <- textplot_wordcloud(reddit_corpus_dfm, max_words=200, color="blue")

### TEXT PLOTS

## Reddit

rsmaller_dfm <- dfm_trim(reddit_corpus_dfm, min_termfreq = 100)

# create fcm from dfm
rsmaller_fcm <- fcm(rsmaller_dfm)

rmyFeatures <- names(topfeatures(rsmaller_fcm, 30))

# retain only those top features as part of our matrix
reven_smaller_fcm <- fcm_select(rsmaller_fcm, pattern = rmyFeatures, selection = "keep")

# compute size weight for vertices in network
rsize <- log(colSums(reven_smaller_fcm))

# create plot
textplot_network(reven_smaller_fcm, vertex_size = rsize / max(rsize) * 3)

## TWITTER

tsmaller_dfm <- dfm_trim(twitter_corpus_dfm, min_termfreq = 100)

# create fcm from dfm
smaller_fcm <- fcm(tsmaller_dfm)

myFeatures <- names(topfeatures(smaller_fcm, 30))

# retain only those top features as part of our matrix
even_smaller_fcm <- fcm_select(smaller_fcm, pattern = myFeatures, selection = "keep")

# compute size weight for vertices in network
size <- log(colSums(even_smaller_fcm))

# create plot
textplot_network(even_smaller_fcm, vertex_size = size / max(size) * 3)






### STM MODELING
k <- 25
rModel <- dfm(reddit_lemmitized)

docvars(reddit_lemmitized) <- rModel

rModel$polarity <- rdf_nrc$polarity

rModel <- stm(rModel,
              K = k,
              prevalence = ~ polarity,
              max.em.its = 1000,
              seed = 1234,
              init.type = "Spectral")

#labelTopics(rModel)
plot(rModel, type = "summary")

# get the words
rTopicNames <- labelTopics(rModel, n=4)$frex

# set up an empty vector
rTopicLabels <- rep(NA, k)

# set up a loop to go through the topics and collapse the words to a single name
for (i in 1:k){
  rTopicLabels[i] <- paste(rTopicNames[i,], collapse = "_")
}

# print the names
rTopicLabels

# estimate effects
rmodelEffects <- estimateEffect(formula = 1:k ~ polarity,
                                stmobj = rModel,
                                metadata = reddit_lemmitized)

# plot effects
rRows <- 2
par(mfrow = c(rRows, 3), bty = "n", lwd = 2)
for (i in 1:k){
  plot.estimateEffect(rmodelEffects,
                      covariate = "polarity",
                      xlim = c(-.25, .25),
                      model = rModel,
                      topics = rmodelEffects$topics[i],
                      method = "difference",
                      cov.value1 = 1,
                      cov.value2 = 0, 
                      main = rTopicLabels[i],
                      printlegend = F,
                      linecol = "grey26",
                      labeltype = "custom",
                      verbose.labels = F,
                      custom.labels = c(""))
  par(new = F)
}

k <- 25
### TWITTER
tModel <- dfm(twitter_lemmitized)

docvars(twitter_lemmitized) <- tModel

tModel$polarity <- tdf_nrc$polarity

tModel <- stm(tModel,
              K = k,
              prevalence = ~ polarity,
              max.em.its = 1000,
              seed = 1234,
              init.type = "Spectral")

labelTopics(tModel)
plot(tModel, type = "summary")

# get the words
tTopicNames <- labelTopics(tModel, n=4)$frex

# set up an empty vector
tTopicLabels <- rep(NA, k)

# set up a loop to go through the topics and collapse the words to a single name
for (i in 1:k){
  tTopicLabels[i] <- paste(tTopicNames[i,], collapse = "_")
}

# print the names
tTopicLabels

# estimate effects
tmodelEffects <- estimateEffect(formula = 1:k ~ polarity,
                                stmobj = tModel,
                                metadata = twitter_lemmitized)

# plot effects
tRows <- 2
par(mfrow = c(tRows, 3), bty = "n", lwd = 2)
for (i in 1:k){
  plot.estimateEffect(tmodelEffects,
                      covariate = "polarity",
                      xlim = c(-.25, .25),
                      model = tModel,
                      topics = tmodelEffects$topics[i],
                      method = "difference",
                      cov.value1 = 1,
                      cov.value2 = 0, 
                      main = tTopicLabels[i],
                      printlegend = F,
                      linecol = "grey26",
                      labeltype = "custom",
                      verbose.labels = F,
                      custom.labels = c(""))
  par(new = F)
}