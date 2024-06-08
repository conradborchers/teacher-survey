library(tidyverse)
library(binom)

d <- read_csv('survey-data-nov14-23.csv') %>% 
  janitor::clean_names() %>% 
  slice(3:n()) # headers

# Relevant questions for topic modeling on "values":
# q161, q159, q124, q167, q187_228, q125, q173, q176, q80

d %>%
  select(q161, q159, q124, q167, q187_228, q125, q173, q176, q80)

# Load necessary libraries
library(tidyverse)
library(tidytext)
library(tm)
library(topicmodels)
library(ggplot2)
library(LDAvis)
library(servr)

# Select and combine the columns
selected_columns <- d %>% 
  select(q161, q159, q124, q167, q187_228, q125, q173, q176, q80) %>%
  unite("text", everything(), sep = " ", na.rm = TRUE)

# Tokenize and create a Document-Term Matrix (DTM)
text_df <- selected_columns %>%
  unnest_tokens(word, text)

# Remove stop words
data("stop_words")
text_df <- text_df %>%
  anti_join(stop_words)

# Create a DTM
dtm <- text_df %>%
  count(document = row_number(), word) %>%
  cast_dtm(document, word, n)

# Perform Latent Dirichlet Allocation (LDA)
lda_model <- LDA(dtm, k = 3, control = list(seed = 1234))
topics <- tidy(lda_model, matrix = "beta")

# Visualize the topics with ggplot2
top_terms <- topics %>%
  group_by(topic) %>%
  top_n(30, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)  # Arrange by topic first, then by -beta within each topic

ggplot(top_terms, aes(reorder(term, beta), beta, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top Terms in Each Topic",
       x = "Term",
       y = "Beta")

# Optional: Interactive visualization with LDAvis
json_lda <- createJSON(
  phi = posterior(lda_model)$terms,
  theta = posterior(lda_model)$topics,
  doc.length = rowSums(as.matrix(dtm)),
  vocab = colnames(dtm),
  term.frequency = colSums(as.matrix(dtm))
)

serVis(json_lda, out.dir = 'vis', open.browser = TRUE)

