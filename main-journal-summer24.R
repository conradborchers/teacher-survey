library(tidyverse)
library(binom)

d <- read_csv('survey-data-nov14-23.csv') %>% 
  janitor::clean_names() %>% 
  slice(3:n()) # headers
#
convert_range_to_numeric_center <- function(range_string) {
  if (grepl("More than", range_string)) {
    return(as.numeric(gsub("[^0-9.]+", "", range_string)) + 1)
  } else if (grepl("Less than", range_string)) {
    return(as.numeric(gsub("[^0-9.]+", "", range_string)) - 1)
  } else {
    range_string <- range_string %>% str_remove(' years')
    bounds <- as.numeric(unlist(strsplit(range_string, " - ")))
    center <- mean(bounds, na.rm = TRUE)
    return(center)
  }
}

d$q1_31 %>% sapply(convert_range_to_numeric_center) %>% as.numeric() %>% mean(na.rm=TRUE)

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
  cat("columns are: ", cols, '\n')
  descs <- get_desc_by_column(d, refs, cols)
  cat("descriptions are: ", descs, '\n')
  return(d %>% select(all_of(cols)))
}

return_rankings <- function(d, refs, s = 'foo', n_choices=3, choice_split_take=2) {
  cols <- get_column_by_desc(d, refs, s)
  cat("columns are: ", cols, '\n')
  descs <- get_desc_by_column(d, refs, cols)
  cat("descriptions are: ", descs, '\n')
  
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

# Factors that prevent teachers from reflecting

d$q5_145 %>% map(str_split, ',') 
d$q5_36_text %>% table() %>% names()

d_rank <- d %>% 
  filter(!is.na(q5_145)) %>%
  select(q5_145) %>%
  separate(q5_145, into=c('p1', 'p2', 'p3'), sep=',') %>%
  mutate(response_id = 1:n()) %>%
  pivot_longer(!response_id) %>%
  mutate(rank = name %>% str_extract('[0-9]+') %>% as.numeric()) %>%
  select(-name) %>%
  rename(option=value) %>%
  complete(response_id, option, fill = list(rank=6.5)) %>% # 9 options. neutral rank 4:9 is 6.5
  mutate(is_top_3 = rank<6.5)

d_plot <- d_rank %>%
  mutate(option = ifelse(
    str_detect(option, 'Other'), 'Other', option
  )) %>%
  group_by(option) %>%
  summarize(
    mean_rank = mean(rank),
    median_rank = median(rank),
    prob_top_3 = mean(is_top_3)
    ) %>%
  ungroup() %>%
  arrange(desc(prob_top_3)) %>%
  mutate(perc_top_3 = round(100*prob_top_3, 1))

d_plot %>% select(option, perc_top_3)

p1 <- ggplot(d_plot, aes(x = reorder(option, perc_top_3), y = perc_top_3)) +
  geom_bar(stat = "identity") +
  labs(title = "Reasons Teachers Do Not Reflect",
       x = "",
       y = "% Inclusion in Top 3 Choices",
       caption = "What are the factors that prevent you from reflecting on your teaching practice?") +
  theme_minimal() +
  coord_flip()  

# Factors that motivate reflection

d_rank <- return_rankings(d, refs, s = "your main reasons for reflecting on the teaching practices", n_choices=3)

d_plot <- d_rank %>%
  mutate(item = ifelse(
    str_detect(item, 'Other'), 'Other', item
  )) %>%
  mutate(item = case_when(
    str_detect(item, 'opportuniti') ~ "Finding Opportunities for Improvement",
    str_detect(item, 'goal') ~ "Setting Goals for My Teaching Practice",
    str_detect(item, 'measure') ~ "Measuring My Teaching Performance",
    str_detect(item, 'informal') ~ "Getting Informal Feedback",
    str_detect(item, 'evidence') ~ "Having Evidence of My Progress",
    str_detect(item, '_formal_feedback') ~ "Getting Formal Feedback"
  )) %>%
  group_by(item) %>%
  summarize(
    mean_rank = mean(rank),
    median_rank = median(rank),
    prob_top_3 = mean(chosen)
  ) %>%
  ungroup() %>%
  arrange(desc(prob_top_3)) %>%
  mutate(perc_top_3 = round(100*prob_top_3, 1))

d_plot %>% select(item, perc_top_3)

p2 <- ggplot(d_plot, aes(x = reorder(item, perc_top_3), y = perc_top_3)) +
  geom_bar(stat = "identity") +
  labs(title = "Motivation for Reflection",
       x = "",
       y = "% Inclusion in Top 3 Choices",
       caption = "What would be your main reasons for reflecting on your teaching practices?") +
  theme_minimal() +
  coord_flip() 

png("rq1-aied25.png", width = 760*10, height = 380*10, res = 72*10)
gridExtra::grid.arrange(p1, p2, ncol=2)
dev.off()

## Differnetial analysis

# Experience split
s='how long have you worked as a teacher'
cols <- get_column_by_desc(d, refs, s)
cat("columns are: ", cols, '\n')
d['experience_years'] <- d$q4_34 %>% sapply(convert_range_to_numeric_center) %>% as.numeric()
d['high_experience'] <- ifelse(d$experience_years<median(d$experience_years, na.rm=TRUE), 0, 1)
median(d$experience_years, na.rm=TRUE)

d_rank_tmp <- d %>% 
  filter(!is.na(q5_145)) %>%
  filter(!is.na(high_experience)) %>%
  select(q5_145, high_experience) %>%
  separate(q5_145, into=c('p11', 'p2', 'p3'), sep=',') %>%
  mutate(response_id = 1:n()) 

d_rank <- d_rank_tmp %>%
  select(-high_experience) %>%
  pivot_longer(cols = -c(response_id)) %>%
  mutate(rank = name %>% str_extract('[0-9]+') %>% as.numeric()) %>%
  select(-name) %>%
  rename(option=value) %>%
  complete(response_id, option, fill = list(rank=6.5)) %>% # 9 options. neutral rank 4:9 is 6.5
  mutate(is_top_3 = rank<6.5) %>%
  left_join(d_rank_tmp %>% select(response_id, high_experience), by='response_id')

d_plot <- d_rank %>%
  group_by(option, high_experience) %>%
  summarize(
    mean_rank = mean(rank),
    median_rank = median(rank),
    prob_top_3 = mean(is_top_3),
    ci_lower = binom.test(sum(is_top_3), n(), conf.level = 0.95)$conf.int[1],
    ci_upper = binom.test(sum(is_top_3), n(), conf.level = 0.95)$conf.int[2]
  ) %>%
  ungroup() %>%
  arrange(desc(prob_top_3)) %>%
  mutate(perc_top_3 = round(100*prob_top_3, 1)) %>%
  mutate(ci_lower = round(100*ci_lower, 1)) %>%
  mutate(ci_upper = round(100*ci_upper, 1)) 

d_plot %>%
  select(option, high_experience, prob_top_3, ci_lower, ci_upper) %>%
  pivot_wider(names_from = high_experience, values_from = c('prob_top_3', 'ci_lower', 'ci_upper')) %>%
  select(option, matches('_1'), matches('_0'))

# Tests manually for "Not enough time for reflection" and
# "Difficulties in keeping track of student's performance" and 
d_rank %>%
  group_by(option, high_experience) %>%
  summarize(
    prob_top_3 = mean(is_top_3),
    n_top_3 = sum(is_top_3),
    n = n()
  )
binom.test(7, 58, p = 15/41) # Not enough time < .001
binom.test(16, 58, p = 6/41) # Keeping track < .01
# All other were p > .06
 
d_plot %>% 
  mutate(option = ifelse(
    option=='Not enough time for reflection', 'Not enough time for reflection***',
    option
  )) %>% 
  mutate(option = ifelse(
    option=="Difficulties in keeping track of student's performance", 
    "Difficulties in keeping track of student's performance**",
    option
  )) %>% 
  mutate(
    high_experience = ifelse(high_experience==1, 'Experienced', 'Novice')
  ) %>% 
ggplot(aes(x = reorder(option, perc_top_3), y = perc_top_3)) +
  stat_summary(geom = "bar", fun = mean, position = "dodge") +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), position = "dodge") + 
  geom_errorbar(stat='summary', width=.2) + 
  labs(title = "Reasons Teachers Do Not Reflect",
       x = "Reason",
       y = "% Inclusion in Top 3 Choices",
       caption = "What are the factors that prevent you from reflecting on your teaching practice?") +
  theme_minimal() +
  coord_flip() + 
  facet_wrap(~high_experience)

# Motivation

d_rank <- return_rankings(d, refs, s = "your main reasons for reflecting on the teaching practices", n_choices=3) %>%
  left_join(d %>% select(response_id, high_experience), by='response_id')

d_plot <- d_rank %>%
  filter(!is.na(high_experience)) %>% 
  group_by(item, high_experience) %>%
  summarize(
    mean_rank = mean(rank),
    median_rank = median(rank),
    prob_top_3 = mean(chosen),
    ci_lower = binom.test(sum(chosen), n(), conf.level = 0.95)$conf.int[1],
    ci_upper = binom.test(sum(chosen), n(), conf.level = 0.95)$conf.int[2]
  ) %>%
  ungroup() %>%
  arrange(desc(prob_top_3)) %>%
  mutate(perc_top_3 = round(100*prob_top_3, 1)) %>%
  mutate(ci_lower = round(100*ci_lower, 1)) %>%
  mutate(ci_upper = round(100*ci_upper, 1)) 

d_plot %>%
  select(item, high_experience, prob_top_3, ci_lower, ci_upper) %>%
  pivot_wider(names_from = high_experience, values_from = c('prob_top_3', 'ci_lower', 'ci_upper')) %>%
  select(item, matches('_1'), matches('_0'))

# to_get_formal_feedback
# to_get_informal_feedback
d_rank %>%
  filter(!is.na(high_experience)) %>% 
  group_by(item, high_experience) %>%
  summarize(
    prob_top_3 = mean(chosen),
    n_top_3 = sum(chosen),
    n = n()
  )
binom.test(27, 60, p = 13/42) # get informal feedback < .05
binom.test(9, 60, p = 11/42) # formal feedback p = .055
# All other  p > 0.5

d_plot %>% 
  mutate(item = ifelse(
    item=='to_get_informal_feedback', 'to_get_informal_feedback*',
    item
  )) %>% 
  mutate(item = ifelse(
    item=="to_get_formal_feedback", 
    "to_get_formal_feedback (p = .055)",
    item
  )) %>% 
  mutate(
    high_experience = ifelse(high_experience==1, 'Experienced', 'Novice')
  ) %>% 
  ggplot(aes(x = reorder(item, perc_top_3), y = perc_top_3)) +
  stat_summary(geom = "bar", fun = mean, position = "dodge") +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), position = "dodge") + 
  geom_errorbar(stat='summary', width=.2) + 
  labs(title = "Reasons For Reflection",
       x = "Reason",
       y = "% Inclusion in Top 3 Choices",
       caption = "What would be your main reasons for reflecting on your teaching practices?") +
  theme_minimal() +
  coord_flip() + 
  facet_wrap(~high_experience)

# Dependency analysis RQ2  ECTEL based on experience

# No differential data acceptance based on experience

d_what_students <- return_rankings(d, refs, s = "Several sources for collecting data from your students ", n_choices=4)
d_what_teachers <- return_rankings(d, refs, s = "Several sources for collecting teacher data ", n_choices=5)

d_what_students_eq <- d_what_students %>% 
  mutate(item = case_when(
    str_detect(item, 'location') ~ 'location',
    str_detect(item, 'behavior') ~ 'log data',
    str_detect(item, 'stress') ~ 'physiological data',
    TRUE ~ item
  )) 

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

d_acceptance_test <- inner_join(
  d_what_teachers_eq %>% select(-chosen),
  d_what_students_eq %>% select(-chosen),
  by=c('response_id', 'item'),
  suffix=c('_teacher', '_student')
)

d_posthoc <- d_acceptance_test %>% 
  left_join(d %>% select(response_id, high_experience), by='response_id') %>%
  mutate(chosen_motivate = ifelse(high_experience==1, TRUE, FALSE))  %>% # To be renamed
  filter(!is.na(chosen_motivate))

median(d_posthoc$rank_student[d_posthoc$chosen_motivate & d_posthoc$item=='audio'])
median(d_posthoc$rank_student[!d_posthoc$chosen_motivate & d_posthoc$item=='audio'])

median(d_posthoc$rank_student[d_posthoc$chosen_motivate & d_posthoc$item=='video'])
median(d_posthoc$rank_student[!d_posthoc$chosen_motivate & d_posthoc$item=='video'])

median(d_posthoc$rank_student[d_posthoc$chosen_motivate & d_posthoc$item=='log data'])
median(d_posthoc$rank_student[!d_posthoc$chosen_motivate & d_posthoc$item=='log data'])

mean(d_posthoc$rank_student[d_posthoc$chosen_motivate & d_posthoc$item=='location'])
mean(d_posthoc$rank_student[!d_posthoc$chosen_motivate & d_posthoc$item=='location'])

wilcox.test(d_posthoc$rank_student[d_posthoc$chosen_motivate & d_posthoc$item=='audio'], 
            d_posthoc$rank_student[!d_posthoc$chosen_motivate & d_posthoc$item=='audio'], paired = FALSE)
wilcox.test(d_posthoc$rank_student[d_posthoc$chosen_motivate & d_posthoc$item=='video'], 
            d_posthoc$rank_student[!d_posthoc$chosen_motivate & d_posthoc$item=='video'], paired = FALSE)
wilcox.test(d_posthoc$rank_student[d_posthoc$chosen_motivate & d_posthoc$item=='log data'], 
            d_posthoc$rank_student[!d_posthoc$chosen_motivate & d_posthoc$item=='log data'], paired = FALSE)
wilcox.test(d_posthoc$rank_student[d_posthoc$chosen_motivate & d_posthoc$item=='location'], 
            d_posthoc$rank_student[!d_posthoc$chosen_motivate & d_posthoc$item=='location'], paired = FALSE)


wilcox.test(d_posthoc$rank_teacher[d_posthoc$chosen_motivate & d_posthoc$item=='audio'], 
            d_posthoc$rank_teacher[!d_posthoc$chosen_motivate & d_posthoc$item=='audio'], paired = FALSE)
wilcox.test(d_posthoc$rank_teacher[d_posthoc$chosen_motivate & d_posthoc$item=='video'], 
            d_posthoc$rank_teacher[!d_posthoc$chosen_motivate & d_posthoc$item=='video'], paired = FALSE)
wilcox.test(d_posthoc$rank_teacher[d_posthoc$chosen_motivate & d_posthoc$item=='log data'], 
            d_posthoc$rank_teacher[!d_posthoc$chosen_motivate & d_posthoc$item=='log data'], paired = FALSE)
wilcox.test(d_posthoc$rank_teacher[d_posthoc$chosen_motivate & d_posthoc$item=='location'], 
            d_posthoc$rank_teacher[!d_posthoc$chosen_motivate & d_posthoc$item=='location'], paired = FALSE)

# Data sharing?

d_share <- return_column(d, refs, s = "with whom") %>% 
  select(-matches('text')) %>% 
  `colnames<-`(c('share_student', 'share_teacher')) %>%
  cbind(., d %>% select(high_experience))

get_desc_by_column(d, refs, names(d_share))

d_share_student <- d_share %>% 
  select(share_student, high_experience) %>% 
  mutate(teacher = paste('participant', 1:n(), sep='-')) %>% 
  mutate(share_student = str_split(share_student, ',')) %>% 
  unchop(share_student) %>% 
  mutate(selected=TRUE) %>% 
  complete(share_student, teacher, fill = list(selected=FALSE)) %>% 
  arrange(teacher, high_experience) %>%
  fill(high_experience, .direction='down') %>% 
  filter(!str_detect(share_student, 'Other \\(please|specify\\)')) 

d_share_teacher <- d_share %>% 
  select(share_teacher, high_experience) %>% 
  mutate(teacher = paste('participant', 1:n(), sep='-')) %>% 
  mutate(share_teacher = str_split(share_teacher, ',')) %>% 
  unchop(share_teacher) %>% 
  mutate(selected=TRUE) %>% 
  complete(share_teacher, teacher, fill = list(selected=FALSE)) %>% 
  arrange(teacher, high_experience) %>%
  fill(high_experience, .direction='down') %>% 
  filter(!str_detect(share_teacher, 'Other \\(please|specify\\)')) 

d_share_student_agg <- d_share_student %>%
  group_by(share_student, high_experience) %>%
  summarize(
    shareperc = mean(selected),
    nselect = sum(selected),
    nn = n(),
    cilower = binom.test(sum(selected), n(), conf.level = 0.95)$conf.int[1],
    ciupper = binom.test(sum(selected), n(), conf.level = 0.95)$conf.int[2]
  ) %>% 
  ungroup() %>% 
  arrange(share_student)

d_share_teacher_agg <- d_share_teacher %>%
  group_by(share_teacher) %>%
  summarize(
    shareperc = mean(selected),
    nselect = sum(selected),
    nn = n(),
    cilower = binom.test(sum(selected), n(), conf.level = 0.95)$conf.int[1],
    ciupper = binom.test(sum(selected), n(), conf.level = 0.95)$conf.int[2]
  ) %>% 
  ungroup() %>% 
  arrange(shareperc)

out <- inner_join(d_share_student_agg, d_share_teacher_agg, 
                  by=c('share_student'='share_teacher'), 
                  suffix = c("_student", "_teacher")) %>% 
  mutate(shareperc_student = round(100*shareperc_student, 2)) %>% 
  mutate(shareperc_teacher = round(100*shareperc_teacher, 2)) %>% 
  mutate(cilower_student = round(100*cilower_student, 2)) %>% 
  mutate(ciupper_student = round(100*ciupper_student, 2)) %>% 
  mutate(cilower_teacher = round(100*cilower_teacher, 2)) %>% 
  mutate(ciupper_teacher = round(100*ciupper_teacher, 2)) %>% 
  as.data.frame()

d_plot <- out %>%
  #mutate(share_student) %>% 
  pivot_longer(cols = c("shareperc_student", "cilower_student", 
           "ciupper_student", "shareperc_teacher", "cilower_teacher", 
           "ciupper_teacher")) %>%
  separate(name, into=c('metric', 'context'), sep='_') %>%
  pivot_wider(names_from=metric, values_from=value)

# Novice, Experience contrast
# student_data, principal not significant
# student data, subject colleague
# student data, coaching expert
# student data, subject coordinator
# student data, colleages from same year
d_plot %>%
  filter(context=='student') %>%
  select(share_student, high_experience, matches('^n'), -matches('teacher'))
binom.test(35, 66, p = 30/50) # student_data, principal, p>.2
binom.test(28, 66, p = 16/50) # student_data, subject colleague p = .085
binom.test(18, 66, p = 9/50) # student data, coaching expert, p = .055
binom.test(21, 66, p = 9/50) # student data, subject coordinator, p = .006
binom.test(16, 66, p = 5/50)# student data, colleagues from same year, p < .001
# All other  p > 0.5


d_plot %>% 
  rename(item = share_student) %>%
  mutate(
    high_experience = ifelse(high_experience==1, 'Experienced', 'Novice')
  ) %>% 
  mutate(
    context = ifelse(context=='student', 'Student Data', 'Teacher Data')
  ) %>% 
  ggplot(aes(x = reorder(item, shareperc), y = shareperc)) +
  stat_summary(geom = "bar", fun = mean, position = "dodge") +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = cilower, ymax = ciupper), position = "dodge") + 
  geom_errorbar(stat='summary', width=.2) + 
  labs(title = "Data Sharing Acceptance",
       x = "Data Item",
       y = "% Acceptance",
       caption = "") +
  theme_minimal() +
  coord_flip() + 
  facet_wrap(~context+high_experience)

# Experience ~ Analytics preferences (EC-TEL 24 RQ1)

# Teacher analytics preferences of teachers

coalesce_columns <- function(d_items, suffix) {
  col_names_without_suffix <- gsub(paste0(suffix, "$"), "", names(d_items))
  for (col in col_names_without_suffix) {
    d_items[col] <- d_items %>% select(matches(col)) %>% coalesce(!!!.)
  }
  d_items <- d_items[,col_names_without_suffix]
  return(d_items)
}

item_analysis <- function(d, refs, s = 'foo', n_choices=3, choice_split_take=2, return_d_items=FALSE) {
  cols <- get_column_by_desc(d, refs, s)
  cat("columns are: ", cols, '\n')
  descs <- get_desc_by_column(d, refs, cols)
  cat("descriptions are: ", descs, '\n')
  
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
  
  if (return_d_items)
    return(d_items)
  
  res <- d_items %>%
    group_by(item) %>%
    summarize(
      chosen_prop_ci = calculate_binomial_ci(chosen),
      nchosen = sum(chosen),
      nn = n(),
      median_rank = median(rank),
      iqr_rank = IQR(rank),
      mean_rank = mean(rank),
      sd_rank = sd(rank)
    )
  return(res %>% arrange(desc(chosen_prop_ci)))
}

d_1 <- d %>%
  filter(high_experience==1) %>%
  item_analysis(refs, s = "the aspects you'd be more interested when reflecting on your teaching practice, with one", n_choices=3) %>%
  select(item, chosen_prop_ci, nn, nchosen) %>%
  separate(chosen_prop_ci, into=c('p', 'cilower', 'ciupper'), sep=' ') %>%
  mutate(p=as.numeric(p), cilower=as.numeric(cilower), ciupper=as.numeric(ciupper)) %>%
  mutate(high_experience='Expert')

d_2 <- d %>%
  filter(high_experience==0) %>%
  item_analysis(refs, s = "the aspects you'd be more interested when reflecting on your teaching practice, with one", n_choices=3) %>%
  select(item, chosen_prop_ci, nn, nchosen) %>%
  separate(chosen_prop_ci, into=c('p', 'cilower', 'ciupper'), sep=' ') %>%
  mutate(p=as.numeric(p), cilower=as.numeric(cilower), ciupper=as.numeric(ciupper)) %>%
  mutate(high_experience='Novice')

d_plot <- rbind(d_1, d_2) %>%
  arrange(item)

# my_monitoring_of_student_progress_and_learning
# my_teaching_methods_what_you_do_in_classroom
# student_discipline_and_behavior
d_plot %>%
  select(item, high_experience, nn, nchosen, p)
binom.test(20, 60, p = 18/42) # my_monitoring_of_student_progress_and_learning p = .152
binom.test(25, 60, p = 11/42) # my_teaching_methods_what_you_do_in_classroom, p=.012
binom.test(12, 60, p = 13/42) # student_discipline_and_behavior,  p = .070

d_plot %>% 
  ggplot(aes(x = reorder(item, p), y = p)) +
  stat_summary(geom = "bar", fun = mean, position = "dodge") +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = cilower, ymax = ciupper), position = "dodge") + 
  geom_errorbar(stat='summary', width=.2) + 
  labs(title = "Teacher Analytics Preferences",
       x = "Analytics",
       y = "% Top 3 Rank",
       caption = "") +
  theme_minimal() +
  coord_flip() + 
  facet_wrap(~high_experience)

# Student analytics preferences of teachers

d_1 <- d %>%
  filter(high_experience==1) %>%
  item_analysis(refs, s = "rank the kind of information about your students you would be more interested in accessing when reflecting ", n_choices=3, choice_split_take=4) %>%
  select(item, chosen_prop_ci, nn, nchosen) %>%
  separate(chosen_prop_ci, into=c('p', 'cilower', 'ciupper'), sep=' ') %>%
  mutate(p=as.numeric(p), cilower=as.numeric(cilower), ciupper=as.numeric(ciupper)) %>%
  mutate(high_experience='Expert')

d_2 <- d %>%
  filter(high_experience==0) %>%
  item_analysis(refs, s = "rank the kind of information about your students you would be more interested in accessing when reflecting ", n_choices=3, choice_split_take=4) %>% 
  select(item, chosen_prop_ci, nn, nchosen) %>%
  separate(chosen_prop_ci, into=c('p', 'cilower', 'ciupper'), sep=' ') %>%
  mutate(p=as.numeric(p), cilower=as.numeric(cilower), ciupper=as.numeric(ciupper)) %>%
  mutate(high_experience='Novice')

d_plot <- rbind(d_1, d_2) %>%
  arrange(item)

d_plot

# improved_learning_after_i_help_a_student
# misconceptions_in_learning
# student_learning_and_progress
# students_common_errors
# students_disengagement
# students_emotions
# students_feedback
d_plot %>%
  select(item, high_experience, nn, nchosen, p)
binom.test(24, 60, p = 11/42) # improved_learning_after_i_help_a_student, p = .019
binom.test(20, 60, p = 20/42) # students_disengagement,  p = .028
binom.test(11, 60, p = 13/42) # students_emotions,  p = .036
binom.test(38, 60, p = 22/42) # student_learning_and_progress,  p = .094
binom.test(22, 60, p = 20/42) # students_feedback,  p = .094
binom.test(16, 60, p = 14/42) # misconceptions_in_learning, p=.337
binom.test(19, 60, p = 10/42) # students_common_errors,  p = 172

d_plot %>% 
  mutate(
    item = ifelse(
      item == 'information_from_learning_software_about_students_mastery_of_new_skills_and_knowledge',
      'learning_software_mastery',
      item
    )
  ) %>%
  mutate(
    item = ifelse(
      item == 'information_from_learning_software_about_students_progress',
      'learning_software_progress',
      item
    )
  ) %>%
  ggplot(aes(x = reorder(item, p), y = p)) +
  stat_summary(geom = "bar", fun = mean, position = "dodge") +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = cilower, ymax = ciupper), position = "dodge") + 
  geom_errorbar(stat='summary', width=.2) + 
  labs(title = "Student Analytics Preferences",
       x = "Analytics",
       y = "% Top 3 Rank",
       caption = "") +
  theme_minimal() +
  coord_flip() + 
  facet_wrap(~high_experience)



n <- 100 # sample size
pval <- replicate(10000, { # replications of experiment
  x <- rbinom(1, size = n, # data-generating model with
              prob = 0.5 + 0.15) # minimum relevant effect
  binom.test(x, n = n, p = 0.5)$p.value # p-value of test against H0
})
mean(pval < 0.05) # simulated power at alpha = 0.05
