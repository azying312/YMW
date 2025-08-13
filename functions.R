library(dplyr)
library(tidyr)
library(labelled)
library(purrr)

six_likert_grade <- function(data, var_name, x_name) {
  var_sym <- sym(var_name)
  
  # Extract value labels
  labels <- attr(data[[var_name]], "labels")
  if (is.null(labels)) {
    labels <- setNames(1:6, paste("Option", 1:6))
  }
  
  # Convert to factor with correct labels
  data <- data %>%
    filter(!is.na(!!var_sym), !!var_sym != 0) %>%
    mutate(
      response_label = factor(
        !!var_sym,
        levels = labels,
        labels = names(labels),
        ordered = TRUE
      )
    )
  
  # Count responses by grade and label
  data %>%
    count(grade, response_label) %>%
    ggplot(aes(x = response_label, y = n, fill = as.factor(grade))) +
    geom_col(position = "dodge") +
    labs(
      x = x_name,
      y = "Frequency",
      fill = "Grade",
      title = ""
    ) +
    theme_minimal(base_size = 20) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
}

five_likert_grade <- function(data, var_name, x_name) {
  var_sym <- sym(var_name)
  
  # Extract value labels
  labels <- attr(data[[var_name]], "labels")
  if (is.null(labels)) {
    labels <- setNames(1:6, paste("Option", 1:5))
  }
  
  # Convert to factor with correct labels
  data <- data %>%
    filter(!is.na(!!var_sym), !!var_sym != 0) %>%
    mutate(
      response_label = factor(
        !!var_sym,
        levels = labels,
        labels = names(labels),
        ordered = TRUE
      )
    )
  
  # Count responses by grade and label
  data %>%
    count(grade, response_label) %>%
    ggplot(aes(x = response_label, y = n, fill = as.factor(grade))) +
    geom_col(position = "dodge") +
    labs(
      x = x_name,
      y = "Frequency",
      fill = "Grade",
      title = ""
    ) +
    theme_minimal(base_size = 20) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
}

#### Create Crosstabs for Qs

bin_fcn <- function(df, question_id) {
  
  ans <- df %>%
    filter(!is.na(.data[[question_id]])) %>%
    count(grade, response = .data[[question_id]]) %>%
    complete(grade, response = 0:1, fill = list(n = 0)) %>%
    arrange(grade, response)
  
  ans
}

# multi_response_fcn <- function(df, question_id) {
#   
#   selected <- df %>%
#     select(matches(paste0("^", question_id, "_\\d+$"))) %>%
#     mutate(across(everything(), ~ zap_labels(.)))
#   
#   selected$grade <- df$grade
#   
#   ans <- selected %>%
#     pivot_longer(
#       cols = -grade,
#       names_to = "option",
#       values_to = "selected"
#     ) %>%
#     filter(selected == 1) %>%
#     mutate(option_num = as.integer(gsub(paste0(question_id, "_"), "", option))) %>%
#     count(grade, option_num)
#   
#   ans
# }

multi_response_fcn <- function(df, question_id) {
  selected <- df %>%
    select(all_of("grade"), matches(paste0("^", question_id, "_\\d+$"))) %>%
    mutate(across(-grade, ~ zap_labels(.)))
  
  ans_long <- selected %>%
    pivot_longer(
      cols = -grade,
      names_to = "option",
      values_to = "selected"
    ) %>%
    mutate(option_num = as.integer(gsub(paste0(question_id, "_"), "", option))) %>%
    filter(!is.na(selected))
  
  counts <- ans_long %>%
    filter(selected == 1) %>%
    count(grade, option_num, name = "count")
  
  totals <- ans_long %>%
    group_by(grade) %>%
    summarise(total_responses = n(), .groups = "drop")
  
  # complete all grade x option_num combos, fill missing counts with 0
  all_grades <- unique(df$grade)
  all_options <- unique(ans_long$option_num)
  
  counts_complete <- counts %>%
    complete(grade = all_grades, option_num = all_options, fill = list(count = 0))
  
  ans <- counts_complete %>%
    left_join(totals, by = "grade") %>%
    mutate(proportion = round(count / total_responses * 100, 2)) %>%
    arrange(grade, option_num)
  
  return(ans)
}

# print_crosstabs_by_grade <- function(data, var_prefix) {
#   vars <- data %>% select(starts_with(var_prefix)) %>% names()
#   
#   crosstabs <- map(vars, function(var) {
#     data %>%
#       filter(!is.na(grade)) %>%
#       count(grade, value = .data[[var]], name = "count") %>%
#       complete(grade, value, fill = list(count = 0)) %>%
#       arrange(grade, value)
#   })
#   
#   names(crosstabs) <- vars
#   
#   for (var in vars) {
#     cat("\n--- Crosstab for", var, "---\n")
#     print(crosstabs[[var]], n = 100)
#   }
#   
#   invisible(crosstabs)
# }

print_crosstabs_by_grade <- function(data, var_prefix) {
  vars <- data %>% select(starts_with(var_prefix)) %>% names()
  
  crosstabs <- map(vars, function(var) {
    data %>%
      filter(!is.na(grade), !is.na(.data[[var]])) %>%
      count(grade, value = .data[[var]], name = "count") %>%
      complete(grade, value, fill = list(count = 0)) %>%
      group_by(grade) %>%
      mutate(
        total = sum(count),
        proportion = round(count / total * 100, 2)
      ) %>%
      ungroup() %>%
      arrange(grade, value)
  })
  
  names(crosstabs) <- vars
  
  for (var in vars) {
    cat("\n--- Crosstab for", var, "---\n")
    print(crosstabs[[var]], n = 100)
  }
  
  invisible(crosstabs)
}

## Cross tabs

get_likert_grade_crosstab <- function(data, var) {
  var <- rlang::ensym(var)
  
  data %>%
    filter(!is.na(grade), !!var != 0) %>%
    count(grade, !!var) %>%
    pivot_wider(
      names_from = !!var,
      values_from = n,
      values_fill = 0
    ) %>%
    mutate(total = rowSums(select(., -grade))) %>%
    mutate(across(
      -c(grade, total),
      ~ round(.x / total * 100, 2)
    )) %>%
    select(-total)
}