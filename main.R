library(tidyverse)
library(binom)

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

calculate_binomial_ci <- function(data) {
  success_count <- sum(data)
  total_count <- length(data)
  binom_ci <- binom.confint(success_count, total_count) %>% 
    filter(method=='exact')
  res_df <- data.frame(
    proportion_chosen = mean(data),
    lower_ci = binom_ci$lower[1],
    upper_ci = binom_ci$upper[1]
  )
  ans <- paste(round(res_df$proportion_chosen, 3), round(res_df$lower_ci, 3), round(res_df$upper_ci, 3))
  return(ans)
}

item_analysis <- function(d, refs, s = 'foo', n_choices=3, choice_split_take=2) {
  cols <- get_column_by_desc(d, refs, s)
  
  choices <- get_desc_by_column(d, refs, cols) %>% map_chr(~get_choice_by_description(., split='-', take=choice_split_take))
  
  d_items <- d %>% 
    select(all_of(cols)) %>% 
    `colnames<-`(choices) %>% 
    janitor::clean_names() %>% 
    select(-matches('^other')) %>% 
    mutate_all(as.numeric)
  
  rank_of_unselected <- mean((n_choices+1):ncol(d_items))
  
  d_items[is.na(d_items)] <- rank_of_unselected
  
  d_items['response_id'] <- d$response_id
  
  d_items <-  d_items %>% 
    pivot_longer(!response_id, names_to = 'item', values_to = 'rank') %>% 
    mutate(chosen = rank <= n_choices)
  
  res <- d_items %>%
    group_by(item) %>%
    summarize(
      chosen_prop_ci = calculate_binomial_ci(chosen)
    )
  return(res %>% arrange(desc(chosen_prop_ci)))
}

item_analysis(d, refs, s = "Rank the factors you selected in the previous question, with one ", n_choices=3)
item_analysis(d, refs, s = "the aspects you'd be more interested when reflecting on your teaching practice, with one", n_choices=3)
item_analysis(d, refs, s = "rank the kind of information about your students you would be more interested in accessing when reflecting ", n_choices=3, choice_split_take=4)
item_analysis(d, refs, s = "Rank the following methods for collecting students’ information ", n_choices=3)
item_analysis(d, refs, s = "Rank the kind of information about teachers", n_choices=3, choice_split_take=4)
item_analysis(d, refs, s = "Several sources for collecting teacher data", n_choices=5)
item_analysis(d, refs, s = "Rank your preferences about the people you would like to have", n_choices=2, choice_split_take = 10)
item_analysis(d, refs, s = "Rank your preferences to keep track of your reflection sessions", n_choices=3, choice_split_take = 10)
