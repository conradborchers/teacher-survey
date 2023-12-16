library(tidyverse)

d <- read_csv('survey-data-nov14-23.csv') %>% 
  janitor::clean_names() %>% 
  slice(3:n()) # headers

# Descriptions
refs <- read_csv('survey-data-nov14-23.csv', skip = 1) %>% names()

get_column_by_desc <- function(d, refs, s='foo'){
  return(names(d)[which(refs %>% str_detect(s))])
}

get_desc_by_column <- function(d, refs, cols){
  ii <- which(names(d) %in% cols)
  return(refs[ii])
}

get_choice_by_description <- function(s, split='-', take=1) {
  split_result <- str_split(s, split)[[1]]  # Accessing the first element of the split result
  return(split_result[take])  # Returning the specified element based on 'take'
}

cols <- get_column_by_desc(d, refs, "Rank the aspects you'd be more interested when reflecting on your teaching practice")

choices <- get_desc_by_column(d, refs, cols) %>% map_chr(~get_choice_by_description(., split='-', take=2))

d_items <- d %>% 
  select(all_of(cols)) %>% 
  `colnames<-`(choices) %>% 
  janitor::clean_names() %>% 
  select(-matches('^other')) %>% 
  mutate_all(as.numeric)

# Avg rank imputation assuming 3 items have been ranked out of ncol. Can make this a variable later in routine fun
rank_of_unselected <- mean(4:ncol(d_items))

d_items[is.na(d_items)] <- rank_of_unselected

d_items['response_id'] <- d$response_id

d_items <-  d_items %>% 
  pivot_longer(!response_id, names_to = 'item', values_to = 'rank')


# Function to calculate median
calculate_median <- function(data) {
  median(data)
}

# Number of bootstrap samples
num_bootstraps <- 1000

# Bootstrap function
bootstrap_median <- function(x) {
  bootstrapped_medians <- replicate(num_bootstraps, calculate_median(sample(x, replace = TRUE)))
  ci_lower <- quantile(bootstrapped_medians, 0.025)
  ci_upper <- quantile(bootstrapped_medians, 0.975)
  return(data.frame(
    median_rank = median(x),
    lower_ci = ci_lower,
    upper_ci = ci_upper
  ))
}

# Group by 'item' and perform bootstrapping
result <- d_items %>%
  group_by(item) %>%
  summarize(bootstrapped_results = list(bootstrap_median(rank)))

# Output the results
print(result$bootstrapped_results)
d_items

# Alternative route is computing binomial CIs for how often solution was chosen as top 3
d_items <- d_items %>% 
  mutate(chosen = rank >=3)


calculate_binomial_ci <- function(data) {
  success_count <- sum(data)
  total_count <- length(data)
  binom_ci <- binom.confint(success_count, total_count) %>% 
    filter(method=='exact')
  return(data.frame(
    proportion_chosen = mean(data),
    lower_ci = binom_ci$lower[1],
    upper_ci = binom_ci$upper[1]
  ))
}
library(binom)

# Group by 'item' and calculate binomial CI for 'chosen' variable
result_with_binomial_ci <- d_items %>%
  group_by(item) %>%
  summarize(
    bootstrapped_results = list(bootstrap_median(rank)),
    binomial_results = list(calculate_binomial_ci(chosen))
  )

# Output the results
print(result_with_binomial_ci$binomial_results)
