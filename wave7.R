library(tidyverse)
library(haven)
library(ggplot2)
library(viridis)
library(forcats)
library(rlang)

source("~/YMW/functions.R")

wave7.data <- read_sav("/Volumes/T7/YMW Data/Wave7_downloaded070125_MS only.sav")
# wave7.data <- read.csv("/Volumes/T7/YMW Data/Wave7_downloaded070125_MS only.csv")
dim(wave7.data)

# Filter for non-zero surveys
summary(wave7.data$Progress)
wave7.data.complete <- wave7.data %>% 
  filter(Progress != 0)
dim(wave7.data.complete)

# Recode variable names
wave7.data.recode <- wave7.data.complete %>% 
  dplyr::select(-c(StartDate, EndDate, Status, IPAddress, Duration__in_seconds_, Finished, RecordedDate,
            ResponseId, RecipientLastName, RecipientFirstName, RecipientEmail, ExternalReference,
            LocationLatitude, LocationLongitude, DistributionChannel, UserLanguage, assent, assent2,
            semail, Q116, Q116, pname, Q125, Q139, Q159_1, Q173, dob_1, dob_2, yob, Q157, Q157_3_TEXT, 
            schgra,Q159_2,Q159_3,Q159_4,Q174_1,Q174_2,Q174_3,Q174_4,Q174_5,Q174_6,Q174_7,Q174_8,
            Q174_9,sch_1,sch_2,sch_3,Q175,Q122_1,Q122_2,Q122_3,Q122_4,Q122_5,Q160,Q161,Q163,Q162,
            Q164,Q131_1,Q131_2,Q131_3,Q131_4,Q131_5,Q131_6,Q131_7,Q131_8,Q131_9,Q131_10,Q131_11,Q131_12,
            Q179_1,Q179_4,Q179_5,Q179_6,Q179_7,Q179_8,Q179_11,Q179_9,Q179_10,Q179_10_TEXT,
            hstruct_1,hstruct_2,hstruct_3,hstruct_4,hstruct_5,hstruct_6,hstruct_7,hstruct_8,hstruct_9,hstruct_10,hstruct_10_TEXT,
            Q155,Q155_6_TEXT,Q156,Q156_6_TEXT,Q181_1,Q181_2,Q181_3,Q181_6,hisp,Asian_ethnicity_1,
            Asian_ethnicity_2,Asian_ethnicity_3,Asian_ethnicity_4,Asian_ethnicity_5,Asian_ethnicity_7,
            Asian_ethnicity_6,Asian_ethnicity_6_TEXT,Q165_1,Q165_2,Q165_3,Q165_4,Q165_5,Q165_6,Q165_6_TEXT,
            homlang,homlang_6_TEXT,momed,relpar_1,relpar_2,relpar_3,relpar_4,relpar_5,relpar_6,relpar_7,
            pet,pettyp_1,pettyp_2,pettyp_3,pettyp_5,pettyp_7,pettyp_8,pettyp_9,pettyp_10,pettyp_4,pettyp_11,pettyp_6,pettyp_6_TEXT,
            Q166,Q166_6_TEXT,Q124,pettim,petscreen,Q167,petqs_1,petqs_2,petqs_3,petqs_4,petqs_5,petqs_6,petqs_7,petqs_8,petqs_9,petqs_10,
            Q182,Q135,Q136
            
  )) %>% 
  rename(
    name = Q3,
    multitask_hw = Q154,
    multitask_device = Q157.0
  ) %>% 
  mutate(grade = as.numeric(grade)) %>% 
  filter(grade %in% c(6,7,8))

dim(wave7.data.recode)
table(wave7.data.recode$grade)

table(wave7.data.recode$grade, wave7.data.recode$gender)
# wave7.data.recode$race_3
wave7.data.recode$race_1 # White
wave7.data.recode$race_2 # Black
wave7.data.recode$race_4 # Asian
wave7.data.recode$race_5 # Native American
wave7.data.recode$race_6 # Other
wave7.data.recode$race_7 # Latin American
wave7.data.recode$race_8 # Middle Eastern
wave7.data.recode$race_9 # Click to Write
# wave7.data.recode$race_6_TEXT

wave7.data.recode <- wave7.data.recode %>%
  mutate(
    race_count = rowSums(select(., race_1:race_8) == 1, na.rm = TRUE),
    race = case_when(
      race_count > 1 ~ "Mixed",
      race_1 == 1 ~ "White",
      race_2 == 1 ~ "Black",
      race_4 == 1 ~ "Asian",
      race_5 == 1 ~ "Native American",
      race_6 == 1 ~ "Other",
      race_7 == 1 ~ "Latin American",
      race_8 == 1 ~ "Middle Eastern",
      race_9 == 1 ~ "Click to Write",
      TRUE ~ NA_character_
    )
  )

table(wave7.data.recode$race)
table(wave7.data.recode$race_count)

### Tables

# grade
table(wave7.data.recode$grade)

## age you got smart phone
table(wave7.data.recode$agesmart)
wave7.data.recode %>% 
  filter(agesmart != 0) %>% 
  # group_by(grade) %>% 
  summarise(mean(agesmart, na.rm=TRUE),
            sd(agesmart, na.rm=TRUE))

# anova
summary(aov(agesmart ~ grade, data = wave7.data.recode))

wave7.data.recode %>%
  count(grade, agesmart) %>%
  # filter(!is.na(grade)) %>% 
  # filter(!is.na(agesmart)) %>% 
  ggplot(aes(x = factor(agesmart), y = n, fill = as.factor(grade))) +
  geom_col(position = "dodge") +
  labs(
    x = "Age Got First Smartphone",
    y = "Frequency",
    title = " ",
    fill = "Grade"
  ) +
  scale_x_discrete(labels = c("0" = "None", "8" = "8", "9" = "9", "10" = "10", "11" = "11", "12" = "12", "13" = "13", "14" = "14+")) +
  theme_minimal(base_size = 14)

source("functions.R")
## check_*
table(wave7.data.recode$check_1)
get_likert_grade_crosstab(wave7.data.recode, check_1)

six_likert_grade(wave7.data.recode, "check_1", "")

table(wave7.data.recode$check_2)
get_likert_grade_crosstab(wave7.data.recode, check_2)
six_likert_grade(wave7.data.recode, "check_2", "")

table(wave7.data.recode$check_3)
get_likert_grade_crosstab(wave7.data.recode, check_3)
six_likert_grade(wave7.data.recode, "check_3", "")

# multitask_hw
table(wave7.data.recode$multitask_hw)
get_likert_grade_crosstab(wave7.data.recode, multitask_hw)
five_likert_grade(wave7.data.recode, "multitask_hw", "")

# multitask_device
table(wave7.data.recode$multitask_device)
get_likert_grade_crosstab(wave7.data.recode, multitask_device)
five_likert_grade(wave7.data.recode, "multitask_device", "")

# socadvice
colSums(wave7.data.recode %>% dplyr::select(starts_with("Q185_")) == 1, na.rm = TRUE)
print(multi_response_fcn(wave7.data.recode, "Q185_"), n=100)

# socadvice2
colSums(wave7.data.recode %>% dplyr::select(starts_with("Q167_")) == 1, na.rm = TRUE)
print(multi_response_fcn(wave7.data.recode, "Q167_"), n=100)

# phuse
lapply(wave7.data.recode %>% dplyr::select(starts_with("phuse")), table, useNA = "ifany")
print_crosstabs_by_grade(wave7.data.recode, "phuse")
get_likert_grade_crosstab(wave7.data.recode, multitask_hw)

# parmon
lapply(wave7.data.recode %>% dplyr::select(starts_with("parmon_")), table, useNA = "ifany")
table(wave7.data.recode$parmon3)

get_likert_grade_crosstab(wave7.data.recode, parmon3)

# anova
summary(aov(parmon3 ~ grade, data = wave7.data.recode))

print_crosstabs_by_grade(wave7.data.recode, "parmon_")

## smwuse
# source("functions.R")
colSums(wave7.data.recode %>% dplyr::select(starts_with("smwuse")) == 1, na.rm = TRUE)
result_df <- multi_response_fcn(wave7.data.recode, "smwuse")
result_df$proportion

# Q120
table(wave7.data.recode$Q120)
wave7.data.recode %>% 
  filter(Q120 != 0) %>% 
  group_by(grade) %>% 
  summarise(mean(Q120, na.rm=TRUE))
bin_fcn(wave7.data.recode, "Q120")

# Q121
colSums(wave7.data.recode %>% dplyr::select(starts_with("Q121_")) == 1, na.rm = TRUE)
print(multi_response_fcn(wave7.data.recode, "Q121"), n=100)

# smjoin
lapply(wave7.data.recode %>% filter() %>% dplyr::select(starts_with("smjoin")), table)
print_crosstabs_by_grade(wave7.data.recode, "smjoin_")

wave7.data.recode$smjoin_1

# smfav
table(wave7.data.recode$smfav)
print(wave7.data.recode %>%
        filter(grade %in% c(6,7,8)) %>% 
        filter(smfav != 0) %>%
        count(grade, smfav) %>%   
        complete(grade, smfav, fill = list(n = 0)) %>% 
        arrange(grade, smfav), n=100)
wave7.data.recode$smfav_12_TEXT
# 1 NONE - I don't use these at all
#     11                         Tik Tok
#      4                        Snapchat
#      2                       Instagram
#     17        Click to write Choice 12
#      3                        Facebook
#      6                         Twitter
#      7                         Discord
#     14        Click to write Choice 10
#     15        Click to write Choice 11
#     16        Click to write Choice 12
#     12                           Other
# smsida
table(wave7.data.recode$smsida)
bin_fcn(wave7.data.recode, "smsida")

summary(aov(smsida~grade, data = wave7.data.recode))

# onnetda
table(wave7.data.recode$onnetda)
bin_fcn(wave7.data.recode, "onnetda")

summary(aov(onnetda~grade, data = wave7.data.recode))

# Q187
colSums(wave7.data.recode %>% dplyr::select(starts_with("Q187")) == 1, na.rm = TRUE)
ans_df <- multi_response_fcn(wave7.data.recode, "Q187")
ans_df$proportion
print(multi_response_fcn(wave7.data.recode, "Q187"), n=100)

# Q117
table(wave7.data.recode$Q117)
wave7.data.recode %>% 
  filter(Q117 != 0) %>% 
  group_by(grade) %>% 
  count(grade, Q117) %>% 
  pivot_wider(names_from = Q117, values_from = n, values_fill = 0)

# snapage
table(wave7.data.recode$snapage)
wave7.data.recode %>% 
  filter(snapage != 0) %>% 
  group_by(grade) %>% 
  count(grade, snapage) %>% 
  pivot_wider(names_from = snapage, values_from = n, values_fill = 0)

# instage
table(wave7.data.recode$instage)

# instpag
table(wave7.data.recode$instpag)
table(wave7.data.recode$grade, wave7.data.recode$instpag)
summary(aov(instpag~grade, data = wave7.data.recode))

# Q128
colSums(wave7.data.recode %>% dplyr::select(starts_with("Q128")) == 1, na.rm = TRUE)
print(multi_response_fcn(wave7.data.recode, "Q128"), n=50)
# wave7.data.recode$Q128_1

# smfpri
table(wave7.data.recode$smfpri)
wave7.data.recode %>% 
  filter(smfpri != 0) %>% 
  group_by(grade) %>% 
  count(grade, smfpri) %>% 
  pivot_wider(names_from = smfpri, values_from = n, values_fill = 0)

# Q148
table(wave7.data.recode$Q148)
wave7.data.recode %>% 
  filter(Q148 != 0) %>% 
  group_by(grade) %>% 
  count(grade, Q148) %>% 
  pivot_wider(names_from = Q148, values_from = n, values_fill = 0)

# wave7.data.recode$Q148

# Q149
colSums(wave7.data.recode %>% dplyr::select(starts_with("Q149")) == 1, na.rm = TRUE)
print(multi_response_fcn(wave7.data.recode, "Q149"), n=50)

# Q150
table(wave7.data.recode$Q150)
summary(aov(Q150~grade, data = wave7.data.recode))
table(wave7.data.recode$grade, wave7.data.recode$Q150)

# Q190
lapply(wave7.data.recode %>% filter() %>% dplyr::select(starts_with("Q190")), table)
print_crosstabs_by_grade(wave7.data.recode, "Q190")

# smffri
table(wave7.data.recode$smffri)
table(wave7.data.recode$grade, wave7.data.recode$smffri)

# meduse
lapply(wave7.data.recode %>% filter() %>% dplyr::select(starts_with("meduse")), table)

table(wave7.data.recode$grade, wave7.data.recode$meduse_1)
table(wave7.data.recode$grade, wave7.data.recode$meduse_8)
table(wave7.data.recode$grade, wave7.data.recode$meduse_6)
table(wave7.data.recode$grade, wave7.data.recode$meduse_7)
table(wave7.data.recode$grade, wave7.data.recode$meduse_16)
table(wave7.data.recode$grade, wave7.data.recode$meduse_4)
table(wave7.data.recode$grade, wave7.data.recode$meduse_9)
table(wave7.data.recode$grade, wave7.data.recode$meduse_10)
table(wave7.data.recode$grade, wave7.data.recode$meduse_5)

summary(aov(meduse_1~grade, data = wave7.data.recode))
# summary(aov(meduse_2~grade, data = wave7.data.recode))
# summary(aov(meduse_3~grade, data = wave7.data.recode))
summary(aov(meduse_4~grade, data = wave7.data.recode)) 
summary(aov(meduse_5~grade, data = wave7.data.recode))
summary(aov(meduse_6~grade, data = wave7.data.recode))
summary(aov(meduse_7~grade, data = wave7.data.recode))
summary(aov(meduse_8~grade, data = wave7.data.recode))
summary(aov(meduse_9~grade, data = wave7.data.recode))
summary(aov(meduse_10~grade, data = wave7.data.recode))
summary(aov(meduse_16~grade, data = wave7.data.recode))

# smuse
lapply(wave7.data.recode %>% filter() %>% dplyr::select(starts_with("smuse")), table)

# smfbeh
# lapply(wave7.data.recode %>% filter() %>% dplyr::select(starts_with("smfbeh_")), table)
colSums(wave7.data.recode %>% dplyr::select(starts_with("smfbeh_")) == 1, na.rm = TRUE)

summary(aov(smfbeh_4~grade, data = wave7.data.recode))
summary(aov(smfbeh_5~grade, data = wave7.data.recode))
summary(aov(smfbeh_6~grade, data = wave7.data.recode))


# Q125
colSums(wave7.data.recode %>% dplyr::select(starts_with("Q125")) == 1, na.rm = TRUE)

# smfbeh2
lapply(wave7.data.recode %>% filter() %>% dplyr::select(starts_with("smfbeh2")), table)
table(wave7.data.recode$grade, wave7.data.recode$smfbeh2_2)
table(wave7.data.recode$grade, wave7.data.recode$smfbeh2_3)
table(wave7.data.recode$grade, wave7.data.recode$smfbeh2_4)
table(wave7.data.recode$grade, wave7.data.recode$smfbeh2_5)
table(wave7.data.recode$grade, wave7.data.recode$smfbeh2_6)
table(wave7.data.recode$grade, wave7.data.recode$smfbeh2_7)

summary(aov(smfbeh2_2~grade, data=wave7.data.recode))
summary(aov(smfbeh2_3~grade, data=wave7.data.recode))
summary(aov(smfbeh2_4~grade, data=wave7.data.recode))
summary(aov(smfbeh2_5~grade, data=wave7.data.recode))
summary(aov(smfbeh2_6~grade, data=wave7.data.recode))
summary(aov(smfbeh2_7~grade, data=wave7.data.recode))
# smfbeh2_2
# wave7.data.recode$smfbeh2_2

# smfri_1
# lapply(wave7.data.recode %>% filter() %>% dplyr::select(starts_with("smfri_1")), table)
colSums(wave7.data.recode %>% dplyr::select(starts_with("smfri_1")) == 1, na.rm = TRUE)

summary(aov(smfri_1_1 ~ grade, data = wave7.data.recode))
summary(aov(smfri_1_2 ~ grade, data = wave7.data.recode))
summary(aov(smfri_1_3 ~ grade, data = wave7.data.recode))
summary(aov(smfri_1_4 ~ grade, data = wave7.data.recode))
summary(aov(smfri_1_5 ~ grade, data = wave7.data.recode))
summary(aov(smfri_1_6 ~ grade, data = wave7.data.recode))
summary(aov(smfri_1_7 ~ grade, data = wave7.data.recode))


# smfri_2
lapply(wave7.data.recode %>% filter() %>% dplyr::select(starts_with("smfri_2")), table)

# Q158_
colSums(wave7.data.recode %>% dplyr::select(starts_with("Q158_")) == 1, na.rm = TRUE)

summary(aov(Q158_1 ~ grade, data = wave7.data.recode))
summary(aov(Q158_2 ~ grade, data = wave7.data.recode))
summary(aov(Q158_3 ~ grade, data = wave7.data.recode))
summary(aov(Q158_4 ~ grade, data = wave7.data.recode))
summary(aov(Q158_5 ~ grade, data = wave7.data.recode))
summary(aov(Q158_6 ~ grade, data = wave7.data.recode))

# smfsup_
colSums(wave7.data.recode %>% dplyr::select(starts_with("smfsup_")) == 1, na.rm = TRUE)
summary(aov(smfsup_1 ~ grade, data = wave7.data.recode))
summary(aov(smfsup_2 ~ grade, data = wave7.data.recode))
summary(aov(smfsup_3 ~ grade, data = wave7.data.recode))
summary(aov(smfsup_4 ~ grade, data = wave7.data.recode))
summary(aov(smfsup_5 ~ grade, data = wave7.data.recode))
summary(aov(smfsup_11 ~ grade, data = wave7.data.recode))

# Q152_
colSums(wave7.data.recode %>% dplyr::select(starts_with("Q152_")) == 1, na.rm = TRUE)
print(multi_response_fcn(wave7.data.recode, "Q152"), n=100)

Q152_topics <- wave7.data.recode %>%
  select(matches("^Q152_[0-9_]+$"))
long_topics <- Q152_topics %>%
  pivot_longer(everything(), names_to = "topic_var", values_to = "topic_value") %>%
  filter(!is.na(topic_value))

top_topics <- long_topics %>%
  count(topic_var, sort = TRUE)

table(wave7.data.recode$grade, wave7.data.recode$Q152_15)
table(wave7.data.recode$grade, wave7.data.recode$Q152_6)
table(wave7.data.recode$grade, wave7.data.recode$Q152_13)

summary(aov(Q152_1 ~ grade, data = wave7.data.recode))
summary(aov(Q152_2 ~ grade, data = wave7.data.recode))
summary(aov(Q152_3 ~ grade, data = wave7.data.recode))
# summary(aov(Q152_4 ~ grade, data = wave7.data.recode))
summary(aov(Q152_5 ~ grade, data = wave7.data.recode))
summary(aov(Q152_6 ~ grade, data = wave7.data.recode))
summary(aov(Q152_7 ~ grade, data = wave7.data.recode))
summary(aov(Q152_8 ~ grade, data = wave7.data.recode)) ##
summary(aov(Q152_13 ~ grade, data = wave7.data.recode))
summary(aov(Q152_15 ~ grade, data = wave7.data.recode))

# smuse_
lapply(wave7.data.recode %>% filter() %>% dplyr::select(starts_with("smuse_")), table)
summary(aov(smuse_1~grade, data = wave7.data.recode))


# app_
lapply(wave7.data.recode %>% filter() %>% dplyr::select(starts_with("app_")), table)

table(wave7.data.recode$grade, wave7.data.recode$app_1)
table(wave7.data.recode$grade, wave7.data.recode$app_2)
table(wave7.data.recode$grade, wave7.data.recode$app_3)
table(wave7.data.recode$grade, wave7.data.recode$app_4)
table(wave7.data.recode$grade, wave7.data.recode$app_5)
table(wave7.data.recode$grade, wave7.data.recode$app_6)
table(wave7.data.recode$grade, wave7.data.recode$app_7)
table(wave7.data.recode$grade, wave7.data.recode$app_8)
table(wave7.data.recode$grade, wave7.data.recode$app_10)

summary(aov(app_1~grade, data = wave7.data.recode))
summary(aov(app_2~grade, data = wave7.data.recode))
summary(aov(app_3~grade, data = wave7.data.recode))
summary(aov(app_4~grade, data = wave7.data.recode))
summary(aov(app_5~grade, data = wave7.data.recode))
summary(aov(app_6~grade, data = wave7.data.recode))
summary(aov(app_7~grade, data = wave7.data.recode))
summary(aov(app_8~grade, data = wave7.data.recode))
# summary(aov(app_9~grade, data = wave7.data.recode))
summary(aov(app_10~grade, data = wave7.data.recode))

# Q192_
lapply(wave7.data.recode %>% filter() %>% dplyr::select(starts_with("Q192_")), table)
print_crosstabs_by_grade(wave7.data.recode, "Q192")
table(wave7.data.recode$grade, wave7.data.recode$Q192_1)
table(wave7.data.recode$grade, wave7.data.recode$Q192_2)
table(wave7.data.recode$grade, wave7.data.recode$Q192_3)
table(wave7.data.recode$grade, wave7.data.recode$Q192_4)
table(wave7.data.recode$grade, wave7.data.recode$Q192_5)
table(wave7.data.recode$grade, wave7.data.recode$Q192_6)
table(wave7.data.recode$grade, wave7.data.recode$Q192_7)
table(wave7.data.recode$grade, wave7.data.recode$Q192_8)
table(wave7.data.recode$grade, wave7.data.recode$Q192_9)
table(wave7.data.recode$grade, wave7.data.recode$Q192_10)
table(wave7.data.recode$grade, wave7.data.recode$Q192_11)

summary(aov(Q192_1~grade, data = wave7.data.recode))
summary(aov(Q192_2~grade, data = wave7.data.recode))
summary(aov(Q192_3~grade, data = wave7.data.recode))
summary(aov(Q192_4~grade, data = wave7.data.recode))
summary(aov(Q192_5~grade, data = wave7.data.recode))
summary(aov(Q192_6~grade, data = wave7.data.recode))
summary(aov(Q192_7~grade, data = wave7.data.recode))
summary(aov(Q192_8~grade, data = wave7.data.recode))
summary(aov(Q192_9~grade, data = wave7.data.recode))
summary(aov(Q192_10~grade, data = wave7.data.recode))
summary(aov(Q192_11~grade, data = wave7.data.recode))

# Q193_
lapply(wave7.data.recode %>% filter() %>% dplyr::select(starts_with("Q193_")), table)
print_crosstabs_by_grade(wave7.data.recode, "Q193_")

summary(aov(Q193_1~grade, data = wave7.data.recode))

# Q195
lapply(wave7.data.recode %>% filter() %>% dplyr::select(starts_with("Q195")), table)
print_crosstabs_by_grade(wave7.data.recode, "Q195")

summary(aov(Q195~grade, data = wave7.data.recode))
table(wave7.data.recode$grade, wave7.data.recode$Q195)

# Q194
# lapply(wave7.data.recode %>% filter() %>% dplyr::select(starts_with("Q194")), table)
colSums(wave7.data.recode %>% dplyr::select(starts_with("Q194")) == 1, na.rm = TRUE)

# Q194
lapply(wave7.data.recode %>% filter() %>% dplyr::select(starts_with("stress")), table)
# colSums(wave7.data.recode %>% dplyr::select(starts_with("stress")) == 1, na.rm = TRUE)

summary(aov(stress_1~grade, data=wave7.data.recode))
summary(aov(stress_2~grade, data=wave7.data.recode))
summary(aov(stress_3~grade, data=wave7.data.recode))
summary(aov(stress_4~grade, data=wave7.data.recode))
summary(aov(stress_5~grade, data=wave7.data.recode))
summary(aov(stress_6~grade, data=wave7.data.recode))
summary(aov(stress_7~grade, data=wave7.data.recode))
summary(aov(stress_8~grade, data=wave7.data.recode))
# summary(aov(stress_9~grade, data=wave7.data.recode))
summary(aov(stress_10~grade, data=wave7.data.recode))
summary(aov(stress_11~grade, data=wave7.data.recode))
summary(aov(stress_12~grade, data=wave7.data.recode))
summary(aov(stress_13~grade, data=wave7.data.recode)) ####
summary(aov(stress_14~grade, data=wave7.data.recode))
summary(aov(stress_15~grade, data=wave7.data.recode))
summary(aov(stress_16~grade, data=wave7.data.recode))
summary(aov(stress_17~grade, data=wave7.data.recode))

table(wave7.data.recode$grade, wave7.data.recode$stress_13)

# Q134
lapply(wave7.data.recode %>% filter() %>% dplyr::select(starts_with("Q134")), table)

table(wave7.data.recode$grade, wave7.data.recode$Q134_1)
table(wave7.data.recode$grade, wave7.data.recode$Q134_2)
table(wave7.data.recode$grade, wave7.data.recode$Q134_3)
table(wave7.data.recode$grade, wave7.data.recode$Q134_4)

summary(aov(Q134_1~grade, data=wave7.data.recode))


# Q151
lapply(wave7.data.recode %>% filter() %>% dplyr::select(starts_with("Q151")), table)
# colSums(wave7.data.recode %>% dplyr::select(starts_with("Q151")) == 1, na.rm = TRUE)
print_crosstabs_by_grade(wave7.data.recode, "Q151")
table(wave7.data.recode$grade, wave7.data.recode$Q151)

# Q196
lapply(wave7.data.recode %>% filter() %>% dplyr::select(starts_with("Q196")), table)

summary(aov(Q196_1~grade, data=wave7.data.recode))
summary(aov(Q196_2~grade, data=wave7.data.recode))
summary(aov(Q196_3~grade, data=wave7.data.recode))
summary(aov(Q196_4~grade, data=wave7.data.recode))


# Q197
lapply(wave7.data.recode %>% filter() %>% dplyr::select(starts_with("Q197")), table)

# dep
lapply(wave7.data.recode %>% filter() %>% dplyr::select(starts_with("dep")), table)
summary(aov(dep_1~grade, data=wave7.data.recode))
summary(aov(dep_2~grade, data=wave7.data.recode))
summary(aov(dep_3~grade, data=wave7.data.recode))
summary(aov(dep_4~grade, data=wave7.data.recode))
summary(aov(dep_5~grade, data=wave7.data.recode))
summary(aov(dep_6~grade, data=wave7.data.recode))
summary(aov(dep_7~grade, data=wave7.data.recode))
summary(aov(dep_8~grade, data=wave7.data.recode))
summary(aov(dep_9~grade, data=wave7.data.recode))
summary(aov(dep_10~grade, data=wave7.data.recode))

# socanx
lapply(wave7.data.recode %>% filter() %>% dplyr::select(starts_with("socanx")), table)

# Q113
lapply(wave7.data.recode %>% filter() %>% dplyr::select(starts_with("Q113")), table)

# Q198
lapply(wave7.data.recode %>% filter() %>% dplyr::select(starts_with("Q198")), table)

# Q199
lapply(wave7.data.recode %>% filter() %>% dplyr::select(starts_with("Q199")), table)

# probint
lapply(wave7.data.recode %>% filter() %>% dplyr::select(starts_with("probint")), table)

# sexor
lapply(wave7.data.recode %>% filter() %>% dplyr::select(starts_with("sexor")), table)

# Q156
table(wave7.data.recode$Q156.0)

# Q201
colSums(wave7.data.recode %>% dplyr::select(starts_with("Q201")) == 1, na.rm = TRUE)

# Q122
colSums(wave7.data.recode %>% dplyr::select(starts_with("Q122")) == 1, na.rm = TRUE)

# smmean_
lapply(wave7.data.recode %>% filter() %>% dplyr::select(starts_with("smmean_")), table)

summary(aov(smmean_1~grade, data = wave7.data.recode))
summary(aov(smmean_2~grade, data = wave7.data.recode))
summary(aov(smmean_3~grade, data = wave7.data.recode))
summary(aov(smmean_4~grade, data = wave7.data.recode))
summary(aov(smmean_5~grade, data = wave7.data.recode))
# summary(aov(smmean_6~grade, data = wave7.data.recode))
summary(aov(smmean_7~grade, data = wave7.data.recode))
summary(aov(smmean_8~grade, data = wave7.data.recode))
summary(aov(smmean_9~grade, data = wave7.data.recode))

# smmeanab
colSums(wave7.data.recode %>% dplyr::select(starts_with("smmeanab")) == 1, na.rm = TRUE)

# Q168
lapply(wave7.data.recode %>% filter() %>% dplyr::select(starts_with("Q168_")), table)
summary(aov(Q168_1~grade, data = wave7.data.recode))
summary(aov(Q168_2~grade, data = wave7.data.recode))
summary(aov(Q168_3~grade, data = wave7.data.recode))
summary(aov(Q168_4~grade, data = wave7.data.recode))


# smbeho_
lapply(wave7.data.recode %>% filter() %>% dplyr::select(starts_with("smbeho_")), table)

summary(aov(smbeho_2~grade, data = wave7.data.recode))
summary(aov(smbeho_3~grade, data = wave7.data.recode))
summary(aov(smbeho_4~grade, data = wave7.data.recode))

# smseppl_
lapply(wave7.data.recode %>% filter() %>% dplyr::select(starts_with("smseppl_")), table)

summary(aov(smseppl_1~grade, data = wave7.data.recode)) #
summary(aov(smseppl_2~grade, data = wave7.data.recode))
summary(aov(smseppl_3~grade, data = wave7.data.recode))
summary(aov(smseppl_4~grade, data = wave7.data.recode))
summary(aov(smseppl_5~grade, data = wave7.data.recode))
summary(aov(smseppl_6~grade, data = wave7.data.recode)) # 
summary(aov(smseppl_7~grade, data = wave7.data.recode))
summary(aov(smseppl_8~grade, data = wave7.data.recode))
summary(aov(smseppl_9~grade, data = wave7.data.recode))
summary(aov(smseppl_10~grade, data = wave7.data.recode))
summary(aov(smseppl_11~grade, data = wave7.data.recode))


# Q166
table(wave7.data.recode$Q166.0)

# sleep
table(wave7.data.recode$sleep)
# bedtim
table(wave7.data.recode$bedtim)
# Q126
table(wave7.data.recode$Q126)
# sleepwkd
table(wave7.data.recode$sleepwkd)

# sleho
lapply(wave7.data.recode %>% filter() %>% dplyr::select(starts_with("sleho_")), table)

# Q169
table(wave7.data.recode$Q169)
# sleepscr
table(wave7.data.recode$sleepscr)
# Q170
table(wave7.data.recode$Q170)
# Q171
table(wave7.data.recode$Q171)
# phyact
table(wave7.data.recode$phyact)
# Q160.0
table(wave7.data.recode$Q160.0)

# weight2
table(wave7.data.recode$weight2)

# bodimg
lapply(wave7.data.recode %>% filter() %>% dplyr::select(starts_with("bodimg")), table)

# Q200
lapply(wave7.data.recode %>% filter() %>% dplyr::select(starts_with("Q200_")), table)

# date
table(wave7.data.recode$date)

# dateage
table(wave7.data.recode$dateage)

# gameplay_
lapply(wave7.data.recode %>% filter() %>% dplyr::select(starts_with("gameplay_")), table)

# timtv
table(wave7.data.recode$timtv)
# Q202
table(wave7.data.recode$Q202)

# vidtyp_
lapply(wave7.data.recode %>% filter() %>% dplyr::select(starts_with("vidtyp_")), table)

# ptamu2
ptamu2_topics <- wave7.data.recode %>%
  select(matches("^ptamu2_[0-9_]+$"))
long_topics <- ptamu2_topics %>%
  pivot_longer(everything(), names_to = "topic_var", values_to = "topic_value") %>%
  filter(!is.na(topic_value))

top_topics <- long_topics %>%
  count(topic_var, sort = TRUE)

# ptamu2_topics$ptamu2_1_0
table(wave7.data.recode$grade, wave7.data.recode$ptamu2_1_0)
table(wave7.data.recode$grade, wave7.data.recode$ptamu2_1_1)

table(wave7.data.recode$grade, wave7.data.recode$ptamu2_7_0)
table(wave7.data.recode$grade, wave7.data.recode$ptamu2_7_1)

table(wave7.data.recode$grade, wave7.data.recode$ptamu2_32_0)
table(wave7.data.recode$grade, wave7.data.recode$ptamu2_32_1)

# Q132
wave7.data.recode$Q132
table(wave7.data.recode$grade, wave7.data.recode$Q132)
summary(aov(Q132~grade, data = wave7.data.recode))

# Q133
wave7.data.recode$Q133
table(wave7.data.recode$grade, wave7.data.recode$Q133)










