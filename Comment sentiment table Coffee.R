
library(dplyr)
library(tm)
library(udpipe)
library(lattice)
library(tidytext)


#take text column
Tweets <- select(coffee_backup_tweets, 'text')

# pre-processing:
Tweets$text <- gsub("'", "", Tweets$text)  # remove apostrophes
Tweets$text <- gsub("[[:punct:]]", " ", Tweets$text)  # replace punctuation with space
Tweets$text <- gsub("[[:cntrl:]]", " ", Tweets$text)  # replace control characters with space
Tweets$text <- gsub("^[[:space:]]+", "", Tweets$text) # remove whitespace at beginning of documents
Tweets$text<- gsub("[[:space:]]+$", "", Tweets$text) # remove whitespace at end of documents


#rename column and add ID
Tweets$ID <- seq.int(nrow(Tweets))


#run through udpipe
UdTweets <- Tweets %>% rename(doc_id = ID, text = text) %>% udpipe("english")

#graph most common types of words
stats <- txt_freq(UdTweets$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = stats, col = "cadetblue", 
         main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", 
         xlab = "Freq")

#graph most common nouns
stats <- subset(UdTweets, upos %in% c("NOUN")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most occurring nouns", xlab = "Freq")


U2 <- subset(UdTweets, upos == "ADJ" 
             | upos == "ADV"
             | upos == "NOUN"
             | upos == "VERB")


#get bing sentiment dictionary, with +1 for positive, -1 otherwise (negative)
sent_dict = get_sentiments("bing") %>% mutate(sentiment = ifelse(sentiment == "positive", 1, -1))

# Get a sentiment dictionary lexicon 
load(file("https://github.com/sborms/sentometrics/raw/master/data-raw/valence-raw/valShifters.rda"))

#words with positive and negative sentiment
polarity_terms <- rename(sent_dict, term = word, polarity = sentiment)

#words which negate other words (e.g. cant, arent)
polarity_negators <- subset(valShifters$valence_en, t == 1)$x

#words which amplify others (e.g. absolutely)
polarity_amplifiers <- subset(valShifters$valence_en, t == 2)$x

#words which deamplify others (e.g. barely)
polarity_deamplifiers <- subset(valShifters$valence_en, t == 3)$x

# Do sentiment analysis based on the lexicon with polarity amplifiers and deamplifiers
sentiments <- txt_sentiment(U2, term = "lemma", 
                            polarity_terms = polarity_terms,
                            polarity_negators = polarity_negators, 
                            polarity_amplifiers = polarity_amplifiers,
                            polarity_deamplifiers = polarity_deamplifiers)
sentiments <- sentiments$data


## Use cbind_dependencies to add the parent token to which the keyword is linked
reasons <- sentiments %>% 
  cbind_dependencies() %>%
  select(doc_id, lemma, token, upos, sentiment_polarity, token_parent, lemma_parent, upos_parent, dep_rel) %>%
  filter(sentiment_polarity < 0)# sentiment_polarity > 0)
head(reasons)

reasons <- filter(reasons, dep_rel %in% "amod")

#take lemma and lemma parrent from reasons and count numbers of lemmas, then arrange by decending order
word_cooccurences <- reasons %>% 
  group_by(lemma, lemma_parent) %>%
  summarise(cooc = n()) %>%
  arrange(-cooc)


library(shiny)
library(DT)
shinyApp(
  ui = fluidPage(DTOutput('tbl')),
  server = function(input, output) {
    output$tbl = renderDT(
      word_cooccurences
    )
  }
)
