# IMDB Sentiment Analysis with Visualizations
# --------------------------------------------------
# 1. Load packages
library(dplyr)
library(stringr)
library(tm)
library(tidyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)

# 2. Read dataset
df_imdb <- read.csv('IMDB Dataset.csv', stringsAsFactors = FALSE)

# 3. Basic cleaning: lowercase & keep alphanumerics
df_imdb <- df_imdb %>%
  mutate(review_clean = tolower(review),
         review_clean = str_replace_all(review_clean, '[^a-z ]', ' '),
         review_clean = str_squish(review_clean),
         review_length = nchar(review_clean))

# 4. Top common words per sentiment
stop_words <- stopwords('en')
token_df <- df_imdb %>%
  rowwise() %>%
  mutate(word_list = strsplit(review_clean, ' ')) %>%
  unnest(word_list) %>%
  ungroup() %>%
  rename(word = word_list) %>%
  filter(word != '', !(word %in% stop_words), nchar(word)>2)

pos_top <- token_df %>% filter(sentiment=='positive') %>% count(word, sort=TRUE) %>% pull(n, name=word)
neg_top <- token_df %>% filter(sentiment=='negative') %>% count(word, sort=TRUE) %>% pull(n, name=word)

# 5. Visualize top 20 words per sentiment
vec_to_df <- function(v){data.frame(word=names(v), freq=as.numeric(v))}
pos_top20_df <- vec_to_df(head(pos_top,20))
neg_top20_df <- vec_to_df(head(neg_top,20))
ggplot(pos_top20_df, aes(x=reorder(word,freq), y=freq))+geom_col(fill='steelblue')+coord_flip()+theme_minimal()+labs(title='Top 20 Positive Words', x='Word', y='Freq')
ggplot(neg_top20_df, aes(x=reorder(word,freq), y=freq))+geom_col(fill='firebrick')+coord_flip()+theme_minimal()+labs(title='Top 20 Negative Words', x='Word', y='Freq')

# 6. Word clouds
set.seed(123)
par(mfrow=c(1,2))
wordcloud(names(pos_top), pos_top, max.words=100, colors=brewer.pal(8,'Dark2'), scale=c(3,0.5))
title('Positive Reviews')
wordcloud(names(neg_top), neg_top, max.words=100, colors=brewer.pal(8,'Reds'), scale=c(3,0.5))
title('Negative Reviews')
par(mfrow=c(1,1))

# 7. Distinctive words via log-odds
word_counts <- token_df %>% count(sentiment, word)
totals <- word_counts %>% group_by(sentiment) %>% summarise(total=sum(n))
V <- n_distinct(word_counts$word)
word_counts_wide <- word_counts %>% pivot_wider(names_from=sentiment, values_from=n, values_fill=0)
pos_total <- totals$total[totals$sentiment=='positive']
neg_total <- totals$total[totals$sentiment=='negative']
word_counts_wide <- word_counts_wide %>% mutate(lor = log(((positive+1)/(pos_total+V))/((negative+1)/(neg_total+V))))
top_pos <- word_counts_wide %>% arrange(desc(lor)) %>% slice_head(n=15)
top_neg <- word_counts_wide %>% arrange(lor) %>% slice_head(n=15)
ggplot(top_pos, aes(x=reorder(word,lor), y=lor))+geom_col(fill='steelblue')+coord_flip()+theme_minimal()+labs(title='Distinctive Positive Words',x='Word',y='Log Odds')
ggplot(top_neg, aes(x=reorder(word,-lor), y=-lor))+geom_col(fill='firebrick')+coord_flip()+theme_minimal()+labs(title='Distinctive Negative Words',x='Word',y='Log Odds')

# 8. Review length distribution
ggplot(df_imdb, aes(review_length))+geom_histogram(bins=50, fill='darkslateblue', alpha=0.7)+theme_minimal()+labs(title='Review Length Distribution',x='Length',y='Count')
ggplot(df_imdb, aes(sentiment, review_length, fill=sentiment))+geom_boxplot(alpha=0.7)+scale_fill_manual(values=c('negative'='firebrick','positive'='steelblue'))+theme_minimal()+labs(title='Review Length by Sentiment',x='Sentiment',y='Length')
