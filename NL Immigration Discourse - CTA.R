install.packages("tm")
install.packages("tidytext")
install.packages("tidyverse")
install.packages("RedditExtractoR")
install.packages("dplyr")
install.packages("word2vec")


library(tm)
library(tidytext)
library(tidyverse)
library(RedditExtractoR)
library(dplyr)
library(word2vec)

keywords <- c("immigratie", "asielzoekers","vluchtelingen", "azc", "buitenlanders")

threads_all <- lapply(keywords, function(kw) {
  find_thread_urls(
    keywords = kw, 
    subreddit = "nederlands", 
    sort_by = "relevance",
    period = "year"
  )
}) %>%
  bind_rows() %>%
  distinct(url, .keep_all = TRUE)  # remove duplicate threads

thread_data <- get_thread_content(threads_all$url)
comments <- thread_data$comments
immigratie_comment <- comments$comment

immigratie_comment <- immigratie_comment[!is.na(immigratie_comment) & nchar(trimws(immigratie_comment)) > 0]

writeLines(immigratie_comment, "textminingimmigrants.txt")

#here I ran the code again with the other subreddit r/nederland as I encountered repeated errors while trying to loop the process of extraction from r/nederland and r/nederlands

text1 <- readLines("textminingimmigrants.txt")
text2 <- readLines("textminingimmigrants2.txt") #the name of the .txt file containing comments from r/nederland
merged <- c(text1, text2)
writeLines(merged, "merged.txt")

# 1. LOAD TEXT
text <- readLines("merged.txt", encoding = "UTF-8") %>% paste(collapse = " ")

# 2. LOAD STOPWORDS
custom_stopwords <- readLines("stopwords-nl.txt", encoding = "UTF-8")
all_stopwords    <- unique(c(stopwords("dutch"), custom_stopwords))

# 3. LOAD NRC LEXICON
nrc_lexicon_dutch_clean <- read.delim("Dutch-NRC-EmoLex.txt", 
                                      header = TRUE, sep = "\t") %>%
  select(Dutch.Word, anger:trust) %>%
  filter(rowSums(.[2:11]) > 0) %>%
  mutate(Dutch.Word = tolower(Dutch.Word))

# 4. DEFINE TARGET WORDS
target_words <- c("asielzoeker", "vluchteling", "azc", "statushouder",
                  "gezinshereniging", "arbeidsmigrant", "expat", "immigratie",
                  "immigrant", "buitenlander", "migrant", "derdelander", 
                  "nieuwkomer") 

# EXTRACT SENTENCES WITH DIRECT TARGET WORD MENTIONS
# FROM RAW TEXT (before cleaning/stemming)

# Step 1: Split raw text into sentences first
raw_sentence_df <- data.frame(text = text, 
                              stringsAsFactors = FALSE) %>%
  mutate(text = tolower(text)) %>%                    # lowercase only
  unnest_sentences(sentence, text) %>%
  mutate(
    sentence_id  = row_number(),
    sentence_key = paste("doc1", sentence_id, sep = "_")
  )

cat("Total sentences in corpus:", nrow(raw_sentence_df), "\n")

# Step 2: Filter to sentences with EXACT target word mentions
target_sentences <- raw_sentence_df %>%
  filter(str_detect(sentence, 
                    paste0("\\b(", paste(target_words, collapse = "|"), ")\\b")))
# \\b = word boundary, ensures exact word match, e.g. won't match "immigranten" when looking for "immigrant"

cat("Sentences with target word mentions:", nrow(target_sentences), "\n")

# Step 3: Clean ONLY the filtered sentences for NRC matching
target_sentences_clean <- target_sentences %>%
  mutate(sentence_clean = sentence %>%
           str_replace_all("http[s]?://\\S+|www\\.\\S+", "") %>%  # remove URLs
           str_replace_all("[^a-z ]", "") %>%                      # remove punctuation/numbers
           str_replace_all(paste(all_stopwords, collapse = "\\b|\\b"), "") %>%  # stopwords
           str_squish()                                            # clean whitespace
  )

# Step 4: Tokenize and score emotions
sentence_emotions <- target_sentences_clean %>%
  unnest_tokens(Dutch.Word, sentence_clean) %>%
  inner_join(nrc_lexicon_dutch_clean, by = "Dutch.Word",
             relationship = "many-to-many") %>%
  group_by(sentence_key) %>%
  summarise(across(anger:trust, sum), .groups = "drop")

cat("Sentences with emotion matches:", nrow(sentence_emotions), "\n")

h1a_summary <- sentence_emotions %>%
  summarise(
    total_positive  = sum(positive),
    total_negative  = sum(negative)
  ) %>%
  mutate(
    total           = total_positive + total_negative,
    prop_positive   = round(total_positive / total, 2),
    prop_negative   = round(total_negative / total, 2),
    ratio_neg_pos   = round(total_negative / total_positive, 2)
  )

print(h1a_summary)

# ---- Visualisation ----
h1a_summary %>%
  select(prop_positive, prop_negative) %>%
  pivot_longer(
    everything(),
    names_to  = "affect",
    values_to = "proportion"
  ) %>%
  mutate(
    affect = recode(
      affect,
      prop_positive = "Positive",
      prop_negative = "Negative"
    )
  ) %>%
  ggplot(aes(x = affect, y = proportion, fill = affect)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = scales::percent(proportion, accuracy = 0.1)),
            vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("Negative" = "violet", "Positive" = "skyblue")) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15)),
    labels = scales::percent
  ) +
  labs(
    title    = "Negative vs Positive Affect in Immigrant-Related Sentences",
    x        = NULL,
    y        = "Proportion"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title      = element_text(face = "bold", size = 13),
    plot.caption    = element_text(color = "grey60", size = 9),
    axis.text       = element_text(size = 11, face = "bold")
  )

#chi-test
chi_h1a <- chisq.test(
  x = c(h1a_summary$total_negative, h1a_summary$total_positive),  # observed
  p = c(0.5, 0.5)                          # expected equal split
)

print(chi_h1a)

h1b_summary <- sentence_emotions %>%
  summarise(across(anger:trust, sum)) %>%
  select(anger, anticipation, disgust, fear, joy, sadness, surprise, trust) %>%
  pivot_longer(everything(), names_to = "emotion", values_to = "count") %>%
  mutate(proportion = count / sum(count)) %>%       # convert to proportion of all emotions
  arrange(desc(proportion))

print(h1b_summary)

# ---- Visualisation ----
h1b_summary %>%
  mutate(
    cna_highlight = emotion %in% c("anger", "fear"),
    emotion       = str_to_title(emotion)
  ) %>%
  ggplot(aes(x = reorder(emotion, proportion), y = proportion, fill = cna_highlight)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = scales::percent(proportion, accuracy = 0.1)),
            hjust = -0.2, size = 3.5) +
  coord_flip() +
  scale_fill_manual(
    values = c("FALSE" = "skyblue", "TRUE" = "violet"),
    breaks = c("TRUE", "FALSE"),
    labels = c("CNA fight/flight (anger & fear)", "Other emotions")
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15)),
    labels = scales::percent
  ) +
  labs(
    x        = NULL,
    y        = "Proportion of Emotion Words",
    fill     = NULL,
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title      = element_text(face = "bold", size = 13),
    plot.subtitle   = element_text(color = "grey40", size = 11),
    plot.caption    = element_text(color = "grey60", size = 9),
    axis.text       = element_text(size = 11)
  )


total_emotion_words <- sentence_emotions %>%
  summarise(across(anger:trust, sum)) %>%
  rowSums()


# Get fear count from proportion
fear_count <- round(h1b_summary %>% 
                      filter(emotion == "fear") %>% 
                      pull(proportion) * total_emotion_words)

# Expected proportion if all 8 emotions equally distributed
expected_prop <- 1/8

# Binomial test
binom_fear <- binom.test(
  x           = fear_count,          # observed fear count
  n           = total_emotion_words,  # total emotion words
  p           = expected_prop,        # expected under null
  alternative = "greater"             # fear > expected
)

print(binom_fear)

# Do the same for anger
anger_count <- round(h1b_summary %>% 
                       filter(emotion == "anger") %>% 
                       pull(proportion) * total_emotion_words)

binom_anger <- binom.test(
  x           = anger_count,
  n           = total_emotion_words,
  p           = expected_prop,
  alternative = "greater"
)

print(binom_anger)


# Get fear count from proportion
fear_count <- round(h1b_summary %>% 
                      filter(emotion == "fear") %>% 
                      pull(proportion) *)

# Expected proportion if all 8 emotions equally distributed
expected_prop <- 1/8

# Binomial test
binom_fear <- binom.test(
  x           = fear_count,          # observed fear count
  n           = total_emotion_words,  # total emotion words
  p           = expected_prop,        # expected under null
  alternative = "greater"             # fear > expected
)

print(binom_fear)

# Do the same for anger
anger_count <- round(h1b_summary %>% 
                       filter(emotion == "anger") %>% 
                       pull(proportion) * total_emotion_words)

binom_anger <- binom.test(
  x           = anger_count,
  n           = total_emotion_words,
  p           = expected_prop,
  alternative = "greater"
)

print(binom_anger)


#h2a

negative_sentences <- sentence_emotions %>%
  filter(negative > 0) %>%
  mutate(
    fight  = anger,
    flight = fear,
    cna_cluster = case_when(
      fight == 0 & flight == 0 ~ "no_cna_signal",
      fight > flight           ~ "fight",
      flight > fight           ~ "flight",
      fight == flight          ~ "equal"
    )
  )


cna_signal_test <- negative_sentences %>%
  mutate(has_cna = ifelse(cna_cluster == "no_cna_signal", 
                          "no_cna_signal", 
                          "cna_signal")) %>%
  count(has_cna)

print(cna_signal_test)

signal_n    <- cna_signal_test %>% filter(has_cna == "cna_signal")    %>% pull(n)
no_signal_n <- cna_signal_test %>% filter(has_cna == "no_cna_signal") %>% pull(n)

chi_signal <- chisq.test(
  x = c(signal_n, no_signal_n),
  p = c(0.5, 0.5)
)

chi_signal 

data.frame(
  category   = c("CNA Signal", "No CNA Signal"),
  n          = c(signal_n, no_signal_n)
) %>%
  mutate(proportion = n / sum(n)) %>%
  ggplot(aes(x = category, y = proportion, fill = category)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = scales::percent(proportion, accuracy = 0.1)),
            vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("CNA Signal"    = "violet",
                               "No CNA Signal" = "honeydew4")) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15)),
    labels = scales::percent
  ) +
  labs(
    title    = "CNA Signal vs No Signal in Negative Immigration Sentences",
    y        = "Proportion of Sentences",
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title      = element_text(face = "bold", size = 13),
    axis.text       = element_text(size = 11, face = "bold")
  )






cna_distribution <- negative_sentences %>%
  count(cna_cluster) %>%
  mutate(proportion = round(n / sum(n), 2))



library(ggplot2)

#Exploratory H2a

fight_n  <- cna_distribution %>% filter(cna_cluster == "fight")  %>% pull(n)
flight_n <- cna_distribution %>% filter(cna_cluster == "flight") %>% pull(n)

chi_ff <- chisq.test(
  x = c(fight_n, flight_n),
  p = c(0.5, 0.5)
)

cat("\n--- Chi-square: Fight vs Flight ---\n")
cat("Fight:  n =", fight_n,
    " proportion =", round(fight_n / (fight_n + flight_n), 3), "\n")
cat("Flight: n =", flight_n,
    " proportion =", round(flight_n / (fight_n + flight_n), 3), "\n")
cat("X²(1) =", round(chi_ff$statistic, 2),
    " p =", round(chi_ff$p.value, 4), "\n")

# ============================================================
# VISUALISE ALL FOUR CLUSTERS WITH STATS
# ============================================================
cna_distribution %>%
  mutate(
    cna_cluster = recode(cna_cluster,
                         "fight"         = "Fight",
                         "flight"        = "Flight",
                         "equal"         = "Equal",
                         "no_cna_signal" = "No CNA Signal"
    )
  ) %>%
  ggplot(aes(x = reorder(cna_cluster, proportion),
             y = proportion,
             fill = cna_cluster)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = scales::percent(proportion, accuracy = 0.1)),
            hjust = -0.2, size = 4) +
  coord_flip() +
  scale_fill_manual(values = c(
    "Fight"         = "violet",
    "Flight"        = "skyblue2",
    "Equal"         = "brown3",
    "No CNA Signal" = "honeydew4"
  )) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15)),
    labels = scales::percent
  ) +
  labs(
    title    = "H2: CNA Cluster Distribution in Negative Immigration Sentences",
    x        = NULL,
    y        = "Proportion of Sentences",
    fill     = NULL,
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title      = element_text(face = "bold", size = 13),
    plot.subtitle   = element_text(color = "grey40", size = 9),
    plot.caption    = element_text(color = "grey60", size = 9),
    axis.text       = element_text(size = 11, face = "bold")
  )

#WORD EMBEDDINGS 
library(word2vec)
library(tidyverse)

# ============================================================
# 1. PREPARE TEXT FROM NEGATIVE SENTENCES ONLY
# ============================================================

# Get sentence keys from negative sentences
negative_keys <- negative_sentences %>% pull(sentence_key)

# Extract raw text of negative sentences
negative_text <- target_sentences %>%
  filter(sentence_key %in% negative_keys) %>%
  pull(sentence) %>%
  paste(collapse = " ") %>%
  tolower() %>% 
  str_replace_all("[0-9]+", " ") %>%
  str_replace_all("[[:punct:]]", " ") %>% 
  str_replace_all("http[s]?://\\S+|www\\.\\S+", "") %>% 
  str_replace_all("[^a-z ]", "") %>%
  str_squish()

negative_text_vector <- unlist(strsplit(negative_text, " "))

cat("Total words in negative corpus:", length(negative_text_vector), "\n")

# ============================================================
# 2. BUILD SKIP-GRAM MODEL
# ============================================================
set.seed(99)

model_skip_negative <- word2vec(
  x         = negative_text_vector,
  type      = "skip-gram",
  dim       = 50,
  window    = 5,
  iter      = 15,
  min_count = 3
)

# ============================================================
# 3. GET NEAREST NEIGHBOURS PER TARGET WORD
# ============================================================
skipgram_results <- lapply(target_words, function(term) {
  tryCatch(
    predict(model_skip_negative, newdata = term,
            type = "nearest", top_n = 20),
    error = function(e) {
      message("Not in vocabulary: ", term)
      return(NULL)
    }
  )
}) %>%
  set_names(target_words)

# Print per target word
for (term in target_words) {
  cat("\n=============================\n")
  cat("Term:", term, "\n")
  cat("=============================\n")
  if (!is.null(skipgram_results[[term]])) {
    print(skipgram_results[[term]])
  } else {
    cat("Not in vocabulary\n")
  }
}

# Extract only the associated words per keyword
skipgram_clean <- lapply(skipgram_results, function(x) {
  
  if (!is.null(x)) {
    rownames(x)
  } else {
    NA
  }
  
})

# Convert list into dataframe
skipgram_table <- as.data.frame(skipgram_clean)

# View table
print(skipgram_table)

# Save table
write.csv(
  skipgram_table,
  "skipgram_words_by_keyword.csv",
  row.names = FALSE
)
