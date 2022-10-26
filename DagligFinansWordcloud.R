require(rvest)
require(dplyr)
require(ggplot2)
require(ggthemes)
require(tibble)
require(SentimentAnalysis)
require(tm)
require(slam)
require(udpipe)
require(tidytext)
require(wordcloud)
require(textrank)

e24link <- "https://e24.no"
e24page <- read_html(e24link)

# Load in titles from e24
e24title <- e24page %>% 
  html_nodes(".title") %>% 
  html_text()
e24title

# Load in finansavisen
finansavisenlink <- "https://www.finansavisen.no/"
finansavisenPage <- read_html(finansavisenlink)

# Load in titles from finansavisen
finansavisen <- finansavisenPage %>%  
  html_nodes(".c-latest-articles-desktop__feed__link__title") %>% 
  html_text()
finansavisen

# Loading in borsen dagbladet website
borsenDagbladetLink <- "https://borsen.dagbladet.no/"
borsenDagbladetPage <- read_html(borsenDagbladetLink)

# Loading in titles from borsen Dagbladet website
borsenDagbladet <- borsenDagbladetPage %>%  
  html_nodes(".dynamic-sizing") %>% 
  html_text()

borsenDagbladet


dagensNaringslivLink <- "https://www.dn.no/"
dnLink <- read_html(dagensNaringslivLink)

dagensNaringsliv <- dnLink %>% 
  html_nodes(".dre-item__title--font-sharp") %>% 
  html_text()
dagensNaringsliv 
  
# Load the norwegian udpipe model
tagger <- udpipe_load_model("norwegian-bokmaal-ud-2.1-20180111.udpipe")

# Create a udpipe dataframe of finasavisen
finansavisenWords <- 
  udpipe_annotate(object = tagger,
                  x = finansavisen) %>% 
  as_tibble()

  
# Create a udpipe dataframe of e24
e24words <- udpipe_annotate(object = tagger,
                         x = e24title) %>% 
  as_tibble()

# Creating a udpipe dataframe for borsen dagbladet
borsenDagbladetWords <- udpipe_annotate(object = tagger,
                                        x = borsenDagbladet) %>% 
  as_tibble()

# Creating a udpipe dataframe for dagens næringsliv
dagensNaringslivWords <- udpipe_annotate( object = tagger,
                                          x = dagensNaringsliv) %>% 
  as_tibble()

# Select e24 sentence id and sentence
sentences <- e24words %>% 
  select("sentence_id","sentence") %>% 
  unique()

# Filter out noun and adj word
terminology <- e24words %>% 
  filter(upos %in% c("NOUN", "ADJ")) %>% 
  select("sentence_id", "lemma")


# Combine all news journals to dataframe
df <- rbind(finansavisenWords,
            e24words, 
            borsenDagbladetWords, 
            dagensNaringslivWords)


# Select sentence_id and sentence
sentences <- df %>% 
  select("doc_id","sentence_id","sentence") %>% 
  unique()


# Selecting sentence_id and lemma
term <- df %>% 
  filter(upos %in% c("NOUN","ADJ")) %>% 
  select("sentence_id","lemma")

# Counting lemma
term_count <- term %>% 
  count(term$lemma)

# Arranging in descending order
term_count %>% 
  arrange(-term_count$n) -> term_count

# Filtering out words with less than 3 characthers
term_count %>% 
  filter(nchar(`term$lemma`) > 3) -> term_count


require(RColorBrewer)
require(wordcloud2)
pal2 <- brewer.pal(6,"Dark2")

# Daily wordcloud for 4 financial newsjournals in Norway
wordcloud(words = term_count$`term$lemma`,
          freq = term_count$n,
          min.freq = 2,
          max.words = 100,
          col= pal2,
          scale = c(3,.25),
          random.order = FALSE)


