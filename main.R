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

return_column <- function(d, refs, s = 'foo') {
  cols <- get_column_by_desc(d, refs, s)
  return(d %>% select(all_of(cols)))
}

return_rankings <- function(d, refs, s = 'foo', n_choices=3, choice_split_take=2) {
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

  return(d_items)
}

# What data (teacher vs. student)

d_what_students <- return_rankings(d, refs, s = "Several sources for collecting data from your students ", n_choices=4)
d_what_teachers <- return_rankings(d, refs, s = "Several sources for collecting teacher data ", n_choices=5)

d_what_students$item %>% table
d_what_teachers$item %>% table

d_what_students %>% 
  group_by(item) %>% 
  summarize(median_rank = median(rank), iqr_rank = IQR(rank))

d_what_teachers %>% 
  group_by(item) %>% 
  summarize(median_rank = median(rank), iqr_rank = IQR(rank))

p1 <- d_what_students %>%
  mutate(rank = floor(rank)) %>%
  count(item, rank) %>%
  mutate(item = case_when(
    str_detect(item, 'location') ~ 'location',
    str_detect(item, 'behavior') ~ 'log data',
    TRUE ~ item
  )) %>% 
  ggplot(aes(x = item, y = n, fill = as.factor(rank))) +  # Setting x, y, and fill aesthetics
  geom_bar(stat = "identity") +  # Creating a bar plot with the counts
  labs(x = "Item", y = "Count", fill = "Rank") +  # Adding labels
  ggtitle("Student Data") +  # Adding a title
  theme_minimal() +   # Applying a minimal theme (you can customize this further)
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p2 <- d_what_teachers %>%
  mutate(rank = floor(rank)) %>%
  count(item, rank) %>%
  mutate(item = case_when(
    str_detect(item, 'location') ~ 'location',
    str_detect(item, 'behavior') ~ 'log data',
    str_detect(item, 'stress') ~ 'phyiological data',
    TRUE ~ item
  )) %>% 
  ggplot(aes(x = item, y = n, fill = as.factor(rank))) +  # Setting x, y, and fill aesthetics
  geom_bar(stat = "identity") +  # Creating a bar plot with the counts
  labs(x = "Item", y = "Count", fill = "Rank") +  # Adding labels
  ggtitle("Teacher Data") +  # Adding a title
  theme_minimal() +   # Applying a minimal theme (you can customize this further)
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

gridExtra::grid.arrange(p1, p2, ncol=2)

# Equalizing
d_what_teachers_eq <- d_what_teachers %>% 
  mutate(item = case_when(
    str_detect(item, 'location') ~ 'location',
    str_detect(item, 'behavior') ~ 'log data',
    str_detect(item, 'stress') ~ 'physiological data',
    TRUE ~ item
  )) %>% 
  arrange(response_id, rank) %>% 
  filter(item != 'physiological data') %>% 
  group_by(response_id) %>% 
  mutate(rank = rank(rank)) %>% # Re-rank for standardization
  ungroup()

d_what_students_eq <- d_what_students %>% 
  mutate(item = case_when(
    str_detect(item, 'location') ~ 'location',
    str_detect(item, 'behavior') ~ 'log data',
    str_detect(item, 'stress') ~ 'physiological data',
    TRUE ~ item
  )) 

d_acceptance_test <- inner_join(
d_what_teachers_eq %>% select(-chosen),
d_what_students_eq %>% select(-chosen),
by=c('response_id', 'item'),
suffix=c('_teacher', '_student')
)

d_audio <- d_acceptance_test %>% filter(item=='audio')
wilcox.test(d_audio$rank_student, d_audio$rank_teacher, paired = TRUE)
median(d_audio$rank_student);median(d_audio$rank_teacher) 

tmp <- d_acceptance_test %>% filter(item=='video')
wilcox.test(tmp$rank_student, tmp$rank_teacher, paired = TRUE)
median(tmp$rank_student);median(tmp$rank_teacher) 

tmp <- d_acceptance_test %>% filter(item=='location')
wilcox.test(tmp$rank_student, tmp$rank_teacher, paired = TRUE)
median(tmp$rank_student);median(tmp$rank_teacher) 

tmp <- d_acceptance_test %>% filter(item=='log data')
wilcox.test(tmp$rank_student, tmp$rank_teacher, paired = TRUE)
median(tmp$rank_student);median(tmp$rank_teacher) 

# With whom would they be willing to share?
d_share <- return_column(d, refs, s = "with whom") %>% 
  select(-matches('text')) %>% 
  `colnames<-`(c('share_student', 'share_teacher'))

get_desc_by_column(d, refs, names(d_share))

d_share_student <- d_share %>% 
  select(share_student) %>% 
  mutate(teacher = paste('participant', 1:n(), sep='-')) %>% 
  mutate(share_student = str_split(share_student, ',')) %>% 
  unchop(share_student) %>% 
  mutate(selected=TRUE) %>% 
  complete(share_student, teacher, fill = list(selected=FALSE)) %>% 
  filter(!str_detect(share_student, 'Other \\(please|specify\\)')) 

d_share_teacher <- d_share %>% 
  select(share_teacher) %>% 
  mutate(teacher = paste('participant', 1:n(), sep='-')) %>% 
  mutate(share_teacher = str_split(share_teacher, ',')) %>% 
  unchop(share_teacher) %>% 
  mutate(selected=TRUE) %>% 
  complete(share_teacher, teacher, fill = list(selected=FALSE)) %>% 
  filter(!str_detect(share_teacher, 'Other \\(please|specify\\)')) 

d_share_student_agg <- d_share_student %>%
  group_by(share_student) %>%
  summarize(
    share_perc = mean(selected)
  ) %>% 
  ungroup() %>% 
  arrange(share_perc)

d_share_teacher_agg <- d_share_teacher %>%
  group_by(share_teacher) %>%
  summarize(
    share_perc = mean(selected)
  ) %>% 
  ungroup() %>% 
  arrange(share_perc)

out <- inner_join(d_share_student_agg, d_share_teacher_agg, 
           by=c('share_student'='share_teacher'), 
           suffix = c("_student", "_teacher")) %>% 
  mutate(share_perc_student = round(100*share_perc_student, 2)) %>% 
  mutate(share_perc_teacher = round(100*share_perc_teacher, 2)) %>% 
  as.data.frame()

compare_freqs <- function(v1, v2){
  p0 <- mean(v1)
  p_value <- binom.test(sum(v2), length(v2), p=p0)$p.value
  return(p_value)
}

for (element in d_share_teacher$share_teacher %>% unique()){
  print(element)
  v1 <- d_share_teacher %>% filter(share_teacher==element) %>% pull(selected)
  v2 <- d_share_student %>% filter(share_student==element) %>% pull(selected)
  print(compare_freqs(v1, v2))
}
d_share_teacher
d_share_student


out
out %>% write_csv('tmp.csv')

d_share_teacher

coalesce_columns <- function(d_items, suffix) {
  col_names_without_suffix <- gsub(paste0(suffix, "$"), "", names(d_items))
  for (col in col_names_without_suffix) {
    d_items[col] <- d_items %>% select(matches(col)) %>% coalesce(!!!.)
  }
  d_items <- d_items[,col_names_without_suffix]
  return(d_items)
}

item_analysis <- function(d, refs, s = 'foo', n_choices=3, choice_split_take=2) {
  cols <- get_column_by_desc(d, refs, s)
  
  choices <- get_desc_by_column(d, refs, cols) %>% map_chr(~get_choice_by_description(., split='-', take=choice_split_take))
  
  d_items <- d %>% 
    select(all_of(cols)) %>% 
    `colnames<-`(choices) %>% 
    janitor::clean_names() %>% 
    select(-matches('^other')) %>% 
    mutate_all(as.numeric) %>% 
    coalesce_columns("_2")
  
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

# RQ 1

item_analysis(d, refs, s = "Rank the factors you selected in the previous question, with one ", n_choices=3)
item_analysis(d, refs, s = "the aspects you'd be more interested when reflecting on your teaching practice, with one", n_choices=3)
item_analysis(d, refs, s = "rank the kind of information about your students you would be more interested in accessing when reflecting ", n_choices=3, choice_split_take=4)
item_analysis(d, refs, s = "Rank the following methods for collecting students’ information ", n_choices=3)
item_analysis(d, refs, s = "Several sources for collecting teacher data", n_choices=5)
item_analysis(d, refs, s = "Rank your preferences about the people you would like to have", n_choices=2, choice_split_take = 10)
item_analysis(d, refs, s = "Rank your preferences to keep track of your reflection sessions", n_choices=3, choice_split_take = 10)



# Revise
item_analysis(d, refs, s = "Rank the kind of information about teachers", n_choices=3, choice_split_take=4)

