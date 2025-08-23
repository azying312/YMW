library(dplyr)
library(ggplot2)
library(tidyr)
library(ltm)

pre.data <- read.csv("/Volumes/T7/YMW Data/Summer Workshop/2025/Summer Virtual Workshop Pre-Survey 2025_August 23, 2025_14.34.csv", header=TRUE)
post.data <- read.csv("/Volumes/T7/YMW Data/Summer Workshop/2025/Summer Virtual Workshop Post-Survey 2025_August 23, 2025_14.16 2.csv", header=TRUE)
application.data <- read.csv("/Volumes/T7/YMW Data/Summer Workshop/2025/Summer 2025 Virtual Workshop Application (Responses) - Sheet2.csv")

### Survey Qs
# pre_data_questions <- pre.data[c(1,2),]
# post_data_questions <- post.data[c(1,2),]
# 
# pre.data.full <- pre.data[-c(1,2),]
# post.data.full <- post.data[-c(1,2),]

### Match participants

pre.data$sname <- tolower(gsub("[[:punct:][:space:]]+", "", pre.data$sname))
pre.data.full <- pre.data %>% 
  mutate(sname = ifelse(sname == "thaliahey", "thaliahenry", sname)) %>% 
  arrange(sname)
post.data$sname <- tolower(gsub("[[:punct:][:space:]]+", "",  post.data$sname))
post.data.full <- post.data %>% 
  mutate(sname = ifelse(sname == "cecilia", "ceciliaan", sname),
         sname = ifelse(sname == "jiahuiginnyzhang", "ginnyzhang", sname)) %>% 
  arrange(sname)
application.data$Student.Name <- tolower(gsub("[[:punct:][:space:]]+", "",  application.data$Student.Name))
application.data.participant <- application.data %>%
  rename(sname = Student.Name) %>%
  dplyr::select(sname, Participant) %>% 
  # change names to match the surveys
  dplyr::mutate(sname = ifelse(sname == "emory", "emorywhitfield", sname),
         sname = ifelse(sname == "audreylilavatisinghhyman", "audreyhyman", sname),
         sname = ifelse(sname == "arwen", "arwensun", sname),
         sname = ifelse(sname == "elifnaztiryaki", "eliftiryaki", sname)) %>% 
  arrange(sname)

# Get actual participants
pre.data.full <- pre.data.full %>% 
  left_join(application.data.participant)

post.data.full <- post.data.full %>% 
  left_join(application.data.participant)

table(pre.data.full$Participant)
table(post.data.full$Participant)

na.participants <- pre.data.full %>% filter(is.na(Participant))
na.participants <- post.data.full %>% filter(is.na(Participant))
na.participants$sname

false.participants <- pre.data.full %>% filter(Participant==FALSE)
false.participants <- post.data.full %>% filter(Participant==FALSE)
false.participants$sname

prenames_to_false <- c("vinedickson", "arielmorgan", "jennyscot", "jeffersonsmart")
pre.data.full <- pre.data.full %>%
  mutate(Participant = if_else(sname %in% prenames_to_false, FALSE, Participant))
table(pre.data.full$Participant)

postnames_to_false <- c("catherinewang", "")
post.data.full <- post.data.full %>%
  mutate(Participant = if_else(sname %in% postnames_to_false, FALSE, Participant))
table(post.data.full$Participant)

### Filter duplicate surveys

pre.data.true <- pre.data.full %>% 
  filter(Participant == TRUE)
pre.data.filtered <- pre.data.true[-c(13, 15, 26, 28),]
pre.data.qs <- pre.data.full %>% 
  filter(is.na(Participant))
pre.data.merged <- bind_rows(pre.data.filtered, pre.data.qs)

post.data.true <- post.data.full %>% 
  filter(Participant == TRUE)
post.data.filtered <- post.data.true
post.data.qs <- post.data.full %>% 
  filter(is.na(Participant))
post.data.merged <- bind_rows(post.data.filtered, post.data.qs)

### Clean survey data

# pre data
pre.data.cleaned <- pre.data.merged %>% 
  mutate(sname=ifelse(sname=="", NA, sname)) %>% 
  mutate(Science.ID_1=ifelse(Science.ID_1=="", NA, Science.ID_1)) %>% 
  mutate(Science.ID_2=ifelse(Science.ID_2=="", NA, Science.ID_2)) %>% 
  mutate(Science.ID_3=ifelse(Science.ID_3=="", NA, Science.ID_3)) %>% 
  mutate(Online.identities_1=ifelse(Online.identities_1=="", NA, Online.identities_1)) %>% 
  mutate(Online.identities_2=ifelse(Online.identities_2=="", NA, Online.identities_2)) %>% 
  mutate(Online.identities_3=ifelse(Online.identities_3=="", NA, Online.identities_3)) %>% 
  mutate(Online.identities_4=ifelse(Online.identities_4=="", NA, Online.identities_4)) %>% 
  mutate(Online.identities_5=ifelse(Online.identities_5=="", NA, Online.identities_5)) %>% 
  mutate(Online.identities_6=ifelse(Online.identities_6=="", NA, Online.identities_6)) %>% 
  mutate(Online.identities_7=ifelse(Online.identities_7=="", NA, Online.identities_7)) %>% 
  mutate(Online.identities_8=ifelse(Online.identities_8=="", NA, Online.identities_8)) %>% 
  mutate(Online.identities_9=ifelse(Online.identities_9=="", NA, Online.identities_9)) %>% 
  mutate(Online.identities_10=ifelse(Online.identities_10=="", NA, Online.identities_10)) %>% 
  mutate(Positive.online.comm_15=ifelse(Positive.online.comm_15=="", NA, Positive.online.comm_15)) %>% 
  mutate(Positive.online.comm_17=ifelse(Positive.online.comm_17=="", NA, Positive.online.comm_17)) %>% 
  mutate(Positive.online.comm_18=ifelse(Positive.online.comm_18=="", NA, Positive.online.comm_18)) %>% 
  mutate(Positive.online.comm_19=ifelse(Positive.online.comm_19=="", NA, Positive.online.comm_19)) %>% 
  mutate(Positive.online.comm_20=ifelse(Positive.online.comm_20=="", NA, Positive.online.comm_20)) %>% 
  mutate(Positive.online.comm_21=ifelse(Positive.online.comm_21=="", NA, Positive.online.comm_21)) %>% 
  mutate(Q79_14=ifelse(Q79_14=="", NA, Q79_14)) %>% 
  mutate(Q79_13=ifelse(Q79_13=="", NA, Q79_13)) %>% 
  mutate(Computing.attitudes_1=ifelse(Computing.attitudes_1=="", NA, Computing.attitudes_1)) %>% 
  mutate(Computing.attitudes_2=ifelse(Computing.attitudes_2=="", NA, Computing.attitudes_2)) %>% 
  mutate(Computing.attitudes_3=ifelse(Computing.attitudes_3=="", NA, Computing.attitudes_3)) %>% 
  mutate(Computing.attitudes_4=ifelse(Computing.attitudes_4=="", NA, Computing.attitudes_4)) %>% 
  mutate(Computing.attitudes_5=ifelse(Computing.attitudes_5=="", NA, Computing.attitudes_5)) %>% 
  mutate(Science.career.motiv_1=ifelse(Science.career.motiv_1=="", NA, Science.career.motiv_1)) %>% 
  mutate(Science.career.motiv_2=ifelse(Science.career.motiv_2=="", NA, Science.career.motiv_2)) %>% 
  mutate(Tech.careers_1=ifelse(Tech.careers_1=="", NA, Tech.careers_1)) %>% 
  mutate(Tech.careers_2=ifelse(Tech.careers_2=="", NA, Tech.careers_2)) %>% 
  mutate(Tech.careers_3=ifelse(Tech.careers_3=="", NA, Tech.careers_3)) %>% 
  mutate(Tech.careers_4=ifelse(Tech.careers_4=="", NA, Tech.careers_4)) %>% 
  mutate(Tech.careers_5=ifelse(Tech.careers_5=="", NA, Tech.careers_5)) %>% 
  mutate(app_1=ifelse(app_1=="", NA, app_1)) %>% 
  mutate(app_2=ifelse(app_2=="", NA, app_2)) %>% 
  mutate(app_3=ifelse(app_3=="", NA, app_3)) %>% 
  mutate(app_4=ifelse(app_4=="", NA, app_4)) %>% 
  mutate(app_5=ifelse(app_5=="", NA, app_5)) %>% 
  mutate(app_6=ifelse(app_6=="", NA, app_6)) %>% 
  mutate(app_7=ifelse(app_7=="", NA, app_7)) %>% 
  mutate(app_8=ifelse(app_8=="", NA, app_8)) %>% 
  mutate(app_9=ifelse(app_9=="", NA, app_9)) %>% 
  mutate(app_10=ifelse(app_10=="", NA, app_10)) %>% 
  mutate(app_11=ifelse(app_11=="", NA, app_11)) %>% 
  mutate(app_12=ifelse(app_12=="", NA, app_12)) %>% 
  mutate(STEM.innovation_1=ifelse(STEM.innovation_1=="", NA, STEM.innovation_1)) %>% 
  mutate(STEM.innovation_2=ifelse(STEM.innovation_2=="", NA, STEM.innovation_2)) %>% 
  mutate(STEM.innovation_3=ifelse(STEM.innovation_3=="", NA, STEM.innovation_3)) %>% 
  mutate(STEM.innovation_4=ifelse(STEM.innovation_4=="", NA, STEM.innovation_4)) %>% 
  mutate(STEM.innovation_5=ifelse(STEM.innovation_5=="", NA, STEM.innovation_5)) %>% 
  mutate(STEM.innovation_6=ifelse(STEM.innovation_6=="", NA, STEM.innovation_6)) %>% 
  mutate(STEM.innovation_7=ifelse(STEM.innovation_7=="", NA, STEM.innovation_7)) %>% 
  mutate(STEM.innovation_8=ifelse(STEM.innovation_8=="", NA, STEM.innovation_8)) %>% 
  mutate(STEM.innovation_9=ifelse(STEM.innovation_9=="", NA, STEM.innovation_9)) %>% 
  mutate(STEM.innovation_10=ifelse(STEM.innovation_10=="", NA, STEM.innovation_10)) %>% 
  mutate(AI_1=ifelse(AI_1=="", NA, AI_1)) %>% 
  mutate(AI_2=ifelse(AI_2=="", NA, AI_2)) %>% 
  mutate(AI_3=ifelse(AI_3=="", NA, AI_3)) %>% 
  mutate(AI_4=ifelse(AI_4=="", NA, AI_4)) %>% 
  mutate(smfbeh2_2=ifelse(smfbeh2_2=="", NA, smfbeh2_2)) %>% 
  mutate(smfbeh2_22=ifelse(smfbeh2_22=="", NA, smfbeh2_22)) %>% 
  mutate(smfbeh2_3=ifelse(smfbeh2_3=="", NA, smfbeh2_3)) %>% 
  mutate(smfbeh2_5=ifelse(smfbeh2_5=="", NA, smfbeh2_5)) %>% 
  mutate(meduse_2=ifelse(meduse_2=="", NA, meduse_2)) %>% 
  mutate(meduse_3=ifelse(meduse_3=="", NA, meduse_3)) %>% 
  mutate(meduse_5=ifelse(meduse_5=="", NA, meduse_5)) %>% 
  mutate(Self.esteem_1=ifelse(Self.esteem_1=="", NA, Self.esteem_1)) %>% 
  mutate(Self.esteem_2=ifelse(Self.esteem_2=="", NA, Self.esteem_2)) %>% 
  mutate(Self.esteem_3=ifelse(Self.esteem_3=="", NA, Self.esteem_3)) %>% 
  mutate(Self.esteem_4=ifelse(Self.esteem_4=="", NA, Self.esteem_4)) %>% 
  mutate(Self.esteem_5=ifelse(Self.esteem_5=="", NA, Self.esteem_5)) %>% 
  mutate(Self.esteem_6=ifelse(Self.esteem_6=="", NA, Self.esteem_6)) %>% 
  mutate(Self.esteem_7=ifelse(Self.esteem_7=="", NA, Self.esteem_7)) %>% 
  mutate(Self.esteem_8=ifelse(Self.esteem_8=="", NA, Self.esteem_8)) %>% 
  mutate(Self.esteem_9=ifelse(Self.esteem_9=="", NA, Self.esteem_9)) %>% 
  mutate(Self.esteem_10=ifelse(Self.esteem_10=="", NA, Self.esteem_10)) %>% 
  mutate(Agency_1=ifelse(Agency_1=="", NA, Agency_1)) %>% 
  mutate(Agency_2=ifelse(Agency_2=="", NA, Agency_2)) %>% 
  mutate(Agency_3=ifelse(Agency_3=="", NA, Agency_3)) %>% 
  mutate(Agency_4=ifelse(Agency_4=="", NA, Agency_4)) %>% 
  mutate(Agency_5=ifelse(Agency_5=="", NA, Agency_5)) %>% 
  mutate(Agency_6=ifelse(Agency_6=="", NA, Agency_6)) %>% 
  mutate(Agency_7=ifelse(Agency_7=="", NA, Agency_7)) %>% 
  mutate(Agency_8=ifelse(Agency_8=="", NA, Agency_8)) %>% 
  mutate(Agency_9=ifelse(Agency_9=="", NA, Agency_9)) %>% 
  dplyr::select(c(sname, Science.ID_1,Science.ID_2,Science.ID_3,Online.identities_1,
                  Online.identities_2,Online.identities_3,Online.identities_4,Online.identities_5,Online.identities_6,Online.identities_7,Online.identities_8,Online.identities_9,
                  Online.identities_10,Positive.online.comm_15,Positive.online.comm_17,Positive.online.comm_18,Positive.online.comm_19,Positive.online.comm_20,Positive.online.comm_21,Q79_14,Q79_13,
                  Computing.attitudes_1,Computing.attitudes_2,Computing.attitudes_3,
                  Computing.attitudes_4,Computing.attitudes_5,Science.career.motiv_1,
                  Science.career.motiv_2, Tech.careers_1,Tech.careers_2,Tech.careers_3,Tech.careers_4,Tech.careers_5,app_1,app_2,app_3,app_4,app_5,app_6,app_7,app_8,app_9,app_10, app_11,app_12,
                  STEM.innovation_1,STEM.innovation_2,STEM.innovation_3,STEM.innovation_4,STEM.innovation_5,STEM.innovation_6,STEM.innovation_7,STEM.innovation_8,STEM.innovation_9,STEM.innovation_10,
                  AI_1,AI_2,AI_3,AI_4,smfbeh2_2,smfbeh2_22,smfbeh2_3,smfbeh2_5,meduse_2,
                  meduse_3,meduse_5,Self.esteem_1,Self.esteem_2,Self.esteem_3,Self.esteem_4,Self.esteem_5,Self.esteem_6,Self.esteem_7,Self.esteem_8,Self.esteem_9,Self.esteem_10,
                  Agency_1,Agency_2,Agency_3,Agency_4,Agency_5,Agency_6,Agency_7,Agency_8,Agency_9,
                  Participant)) %>%
  rename("Positive.online.comm_1"="Positive.online.comm_15") %>%
  rename("Positive.online.comm_2"="meduse_2") %>%
  rename("Positive.online.comm_3"="meduse_3") %>%
  rename("Positive.online.comm_4"="Q79_14") %>%
  rename("Positive.online.comm_5"="Q79_13") %>%
  rename("Positive.online.comm_6"="Positive.online.comm_17") %>%
  rename("Positive.online.comm_7"="Positive.online.comm_18") %>%
  rename("Positive.online.comm_8"="Positive.online.comm_19") %>%
  rename("Positive.online.comm_9"="Positive.online.comm_20") %>%
  rename("Positive.online.comm_10"="Positive.online.comm_21") %>%
  rename("Positive.online.comm_11"="meduse_5") %>%
  filter(rowSums(is.na(.)) != ncol(.))

# post data
post.data.cleaned <- post.data.merged %>% 
  mutate(sname=ifelse(sname=="", NA, sname)) %>% 
  mutate(Science.ID_1=ifelse(Science.ID_1=="", NA, Science.ID_1)) %>% 
  mutate(Science.ID_2=ifelse(Science.ID_2=="", NA, Science.ID_2)) %>% 
  mutate(Science.ID_3=ifelse(Science.ID_3=="", NA, Science.ID_3)) %>% 
  mutate(Online.identities_1=ifelse(Online.identities_1=="", NA, Online.identities_1)) %>% 
  mutate(Online.identities_2=ifelse(Online.identities_2=="", NA, Online.identities_2)) %>% 
  mutate(Online.identities_3=ifelse(Online.identities_3=="", NA, Online.identities_3)) %>% 
  mutate(Online.identities_4=ifelse(Online.identities_4=="", NA, Online.identities_4)) %>% 
  mutate(Online.identities_5=ifelse(Online.identities_5=="", NA, Online.identities_5)) %>% 
  mutate(Online.identities_6=ifelse(Online.identities_6=="", NA, Online.identities_6)) %>% 
  mutate(Online.identities_7=ifelse(Online.identities_7=="", NA, Online.identities_7)) %>% 
  mutate(Online.identities_8=ifelse(Online.identities_8=="", NA, Online.identities_8)) %>% 
  mutate(Online.identities_9=ifelse(Online.identities_9=="", NA, Online.identities_9)) %>% 
  mutate(Online.identities_10=ifelse(Online.identities_10=="", NA, Online.identities_10)) %>% 
  mutate(Positive.online.comm_1=ifelse(Positive.online.comm_1=="", NA, Positive.online.comm_1)) %>% 
  mutate(Positive.online.comm_2=ifelse(Positive.online.comm_2=="", NA, Positive.online.comm_2)) %>% 
  mutate(Positive.online.comm_3=ifelse(Positive.online.comm_3=="", NA, Positive.online.comm_3)) %>% 
  mutate(Positive.online.comm_4=ifelse(Positive.online.comm_4=="", NA, Positive.online.comm_4)) %>% 
  mutate(Positive.online.comm_5=ifelse(Positive.online.comm_5=="", NA, Positive.online.comm_5)) %>% 
  mutate(Positive.online.comm_6=ifelse(Positive.online.comm_6=="", NA, Positive.online.comm_6)) %>%
  mutate(Positive.online.comm_7=ifelse(Positive.online.comm_7=="", NA, Positive.online.comm_7)) %>% 
  mutate(Positive.online.comm_8=ifelse(Positive.online.comm_8=="", NA, Positive.online.comm_8)) %>% 
  mutate(Positive.online.comm_9=ifelse(Positive.online.comm_9=="", NA, Positive.online.comm_9)) %>% 
  mutate(Positive.online.comm_10=ifelse(Positive.online.comm_10=="", NA, Positive.online.comm_10)) %>% 
  mutate(Positive.online.comm_11=ifelse(Positive.online.comm_11=="", NA, Positive.online.comm_11)) %>% 
  mutate(Computing.attitudes_1=ifelse(Computing.attitudes_1=="", NA, Computing.attitudes_1)) %>% 
  mutate(Computing.attitudes_2=ifelse(Computing.attitudes_2=="", NA, Computing.attitudes_2)) %>% 
  mutate(Computing.attitudes_3=ifelse(Computing.attitudes_3=="", NA, Computing.attitudes_3)) %>% 
  mutate(Computing.attitudes_4=ifelse(Computing.attitudes_4=="", NA, Computing.attitudes_4)) %>% 
  mutate(Computing.attitudes_5=ifelse(Computing.attitudes_5=="", NA, Computing.attitudes_5)) %>% 
  # mutate(Supportive.STEM.envi_1=ifelse(Supportive.STEM.envi_1=="", NA, Supportive.STEM.envi_1)) %>%
  # mutate(Supportive.STEM.envi_2=ifelse(Supportive.STEM.envi_2=="", NA, Supportive.STEM.envi_2)) %>%
  mutate(Tech.careers_1=ifelse(Tech.careers_1=="", NA, Tech.careers_1)) %>% 
  mutate(Tech.careers_2=ifelse(Tech.careers_2=="", NA, Tech.careers_2)) %>% 
  mutate(Tech.careers_3=ifelse(Tech.careers_3=="", NA, Tech.careers_3)) %>% 
  mutate(Tech.careers_4=ifelse(Tech.careers_4=="", NA, Tech.careers_4)) %>% 
  mutate(Tech.careers_5=ifelse(Tech.careers_5=="", NA, Tech.careers_5)) %>% 
  mutate(app_1=ifelse(app_1=="", NA, app_1)) %>% 
  mutate(app_2=ifelse(app_2=="", NA, app_2)) %>% 
  mutate(app_3=ifelse(app_3=="", NA, app_3)) %>% 
  mutate(app_4=ifelse(app_4=="", NA, app_4)) %>% 
  mutate(app_5=ifelse(app_5=="", NA, app_5)) %>% 
  mutate(app_6=ifelse(app_6=="", NA, app_6)) %>% 
  mutate(app_7=ifelse(app_7=="", NA, app_7)) %>% 
  mutate(app_8=ifelse(app_8=="", NA, app_8)) %>% 
  # mutate(app_9=ifelse(app_9=="", NA, app_9)) %>% 
  mutate(app_10=ifelse(app_10=="", NA, app_10)) %>% 
  mutate(app_11=ifelse(app_11=="", NA, app_11)) %>% 
  mutate(app_12=ifelse(app_12=="", NA, app_12)) %>% 
  mutate(app_13=ifelse(app_13=="", NA, app_13)) %>%
  mutate(STEM.innovation_1=ifelse(STEM.innovation_1=="", NA, STEM.innovation_1)) %>% 
  mutate(STEM.innovation_2=ifelse(STEM.innovation_2=="", NA, STEM.innovation_2)) %>% 
  mutate(STEM.innovation_3=ifelse(STEM.innovation_3=="", NA, STEM.innovation_3)) %>% 
  mutate(STEM.innovation_4=ifelse(STEM.innovation_4=="", NA, STEM.innovation_4)) %>% 
  mutate(STEM.innovation_5=ifelse(STEM.innovation_5=="", NA, STEM.innovation_5)) %>% 
  mutate(STEM.innovation_6=ifelse(STEM.innovation_6=="", NA, STEM.innovation_6)) %>% 
  mutate(STEM.innovation_7=ifelse(STEM.innovation_7=="", NA, STEM.innovation_7)) %>% 
  mutate(STEM.innovation_8=ifelse(STEM.innovation_8=="", NA, STEM.innovation_8)) %>% 
  mutate(STEM.innovation_9=ifelse(STEM.innovation_9=="", NA, STEM.innovation_9)) %>% 
  mutate(STEM.innovation_10=ifelse(STEM.innovation_10=="", NA, STEM.innovation_10)) %>% 
  mutate(AI_1=ifelse(Q170_1=="", NA, Q170_1)) %>%
  mutate(AI_2=ifelse(Q170_2=="", NA, Q170_2)) %>%
  mutate(AI_3=ifelse(Q170_3=="", NA, Q170_3)) %>%
  mutate(AI_4=ifelse(Q170_4=="", NA, Q170_4)) %>%
  mutate(smfbeh2_2=ifelse(smfbeh2_2=="", NA, smfbeh2_2)) %>% 
  mutate(smfbeh2_4=ifelse(smfbeh2_4=="", NA, smfbeh2_4)) %>% 
  mutate(smfbeh2_3=ifelse(smfbeh2_3=="", NA, smfbeh2_3)) %>% 
  mutate(smfbeh2_5=ifelse(smfbeh2_5=="", NA, smfbeh2_5)) %>% 
  # mutate(meduse_2=ifelse(meduse_2=="", NA, meduse_2)) %>% 
  # mutate(meduse_3=ifelse(meduse_3=="", NA, meduse_3)) %>% 
  # mutate(meduse_5=ifelse(meduse_5=="", NA, meduse_5)) %>% 
  mutate(Self.esteem_1=ifelse(Self.esteem_1=="", NA, Self.esteem_1)) %>% 
  mutate(Self.esteem_2=ifelse(Self.esteem_2=="", NA, Self.esteem_2)) %>% 
  mutate(Self.esteem_3=ifelse(Self.esteem_3=="", NA, Self.esteem_3)) %>% 
  mutate(Self.esteem_4=ifelse(Self.esteem_4=="", NA, Self.esteem_4)) %>% 
  mutate(Self.esteem_5=ifelse(Self.esteem_5=="", NA, Self.esteem_5)) %>% 
  mutate(Self.esteem_6=ifelse(Self.esteem_6=="", NA, Self.esteem_6)) %>% 
  mutate(Self.esteem_7=ifelse(Self.esteem_7=="", NA, Self.esteem_7)) %>% 
  mutate(Self.esteem_8=ifelse(Self.esteem_8=="", NA, Self.esteem_8)) %>% 
  mutate(Self.esteem_9=ifelse(Self.esteem_9=="", NA, Self.esteem_9)) %>% 
  mutate(Self.esteem_10=ifelse(Self.esteem_10=="", NA, Self.esteem_10)) %>% 
  mutate(Agency_1=ifelse(Agency_1=="", NA, Agency_1)) %>% 
  mutate(Agency_2=ifelse(Agency_2=="", NA, Agency_2)) %>% 
  mutate(Agency_3=ifelse(Agency_3=="", NA, Agency_3)) %>% 
  mutate(Agency_4=ifelse(Agency_4=="", NA, Agency_4)) %>% 
  mutate(Agency_5=ifelse(Agency_5=="", NA, Agency_5)) %>% 
  mutate(Agency_6=ifelse(Agency_6=="", NA, Agency_6)) %>% 
  mutate(Agency_7=ifelse(Agency_7=="", NA, Agency_7)) %>% 
  mutate(Agency_8=ifelse(Agency_8=="", NA, Agency_8)) %>% 
  mutate(Agency_9=ifelse(Agency_9=="", NA, Agency_9)) %>% 
  dplyr::select(c(sname, Science.ID_1,Science.ID_2,Science.ID_3,Online.identities_1,
                  Online.identities_2,Online.identities_3,Online.identities_4,Online.identities_5,Online.identities_6,Online.identities_7,Online.identities_8,Online.identities_9,
                  Online.identities_10,
                  Positive.online.comm_1,
                  Positive.online.comm_2,
                  Positive.online.comm_3,
                  Positive.online.comm_4,
                  Positive.online.comm_5,
                  Positive.online.comm_6,
                  Positive.online.comm_7,
                  Positive.online.comm_8,
                  Positive.online.comm_9,
                  Positive.online.comm_10,
                  Positive.online.comm_11,
                  Computing.attitudes_1,Computing.attitudes_2,Computing.attitudes_3,
                  Computing.attitudes_4,Computing.attitudes_5,
                  # Supportive.STEM.envi_1, Supportive.STEM.envi_2,
                  Tech.careers_1,Tech.careers_2,Tech.careers_3,Tech.careers_4,Tech.careers_5,
                  app_1,app_2,app_3,app_4,app_5,app_6,app_7,app_8,app_10,app_11,app_12,app_13,
                  STEM.innovation_1,STEM.innovation_2,STEM.innovation_3,STEM.innovation_4,STEM.innovation_5,STEM.innovation_6,STEM.innovation_7,STEM.innovation_8,STEM.innovation_9,STEM.innovation_10,
                  AI_1,AI_2,AI_3,AI_4,
                  smfbeh2_2,smfbeh2_4,smfbeh2_3,smfbeh2_5,
                  Self.esteem_1,Self.esteem_2,Self.esteem_3,Self.esteem_4,Self.esteem_5,Self.esteem_6,Self.esteem_7,Self.esteem_8,Self.esteem_9,Self.esteem_10,
                  Agency_1,Agency_2,Agency_3,Agency_4,Agency_5,Agency_6,Agency_7,Agency_8,Agency_9,
                  Participant)) %>% 
  # rename("Science.career.motiv_1"="Supportive.STEM.envi_1") %>% 
  # rename("Science.career.motiv_2"="Supportive.STEM.envi_2") %>% 
  rename("app_9"="app_11") %>% 
  rename("app_10"="app_10") %>% 
  rename("app_11"="app_12") %>% 
  rename("app_12"="app_13") %>% 
  # rename("AI_1"="Q170_1") %>% 
  # rename("AI_2"="Q170_2") %>% 
  # rename("AI_3"="Q170_3") %>% 
  # rename("AI_4"="Q170_4") %>% 
  rename("smfbeh2_2"="smfbeh2_2") %>% 
  rename("smfbeh2_22"="smfbeh2_3") %>% 
  rename("smfbeh2_3"="smfbeh2_4") %>% 
  rename("smfbeh2_5"="smfbeh2_5") %>% 
  filter(rowSums(is.na(.)) != ncol(.)) %>% 
  rename_with(~ paste0("post_", .)) %>% 
  rename("sname"="post_sname")

## Merge pre and post data
pre_post_data <- pre.data.cleaned %>%
  left_join(post.data.cleaned, by="sname")

## Survey Qs

pre_post_data_questions <- pre_post_data %>% filter(is.na(Participant))
pre_post_data_filtered <- pre_post_data %>% filter(!is.na(Participant))
dim(pre_post_data_filtered)

### RECODE SCALES
important_scale <- c("Extremely important", "Very important", "Important", "Somewhat important", "Slightly important", "Not important")
agree_scale <- c("Strongly agree", "Agree", "Undecided", "Disagree", "Strongly disagree")
agree_scale2 <- c("Strongly agree", "Agree", "Agree a little", "Disagree a little", "Disagree", "Strongly disagree")
agree_scale3 <- c("Strongly agree", "Agree", "Neither agree nor disagree", "Disagree", "Strongly Disagree")
agree_scale4 <- c("Strongly agree", "Agree", "Neutral", "Disagree", "Strongly Disagree")
interest_scale <- c("Very interested", "Somewhat interested", "Not interested")
feel_scale <- c("Never", "Rarely", "Sometimes", "Often", "Always")

recoded_pre_post_data <- pre_post_data_filtered %>% 
  mutate(across(where(~ any(. %in% important_scale)), ~ case_when(
    . == "Not important" ~ 1,
    . == "Slightly important" ~ 2,
    . == "Somewhat important" ~ 3,
    . == "Important" ~ 4,
    . == "Very important" ~ 5,
    . == "Extremely important" ~ 6, 
    TRUE ~ NA_real_
  ))) %>% 
  mutate(across(where(~ any(. %in% agree_scale)), ~ case_when(
    . == "Strongly disagree" ~ 1,
    . == "Disagree" ~ 2,
    . == "Undecided" ~ 3,
    . == "Agree" ~ 4,
    . == "Strongly agree" ~ 5,
    TRUE ~ NA_real_
  ))) %>% 
  mutate(across(where(~ any(. %in% interest_scale)), ~ case_when(
    . == "Not interested" ~ 1,
    . == "Somewhat interested" ~ 2,
    . == "Very interested" ~ 3,
    TRUE ~ NA_real_
  ))) %>% 
  mutate(across(where(~ any(. %in% agree_scale2)), ~ case_when(
    . == "Strongly disagree" ~ 1,
    . == "Disagree" ~ 2,
    . == "Disagree a little" ~ 3,
    . == "Agree a little" ~ 4,
    . == "Agree" ~ 5,
    . == "Strongly agree" ~ 6, 
    TRUE ~ NA_real_
  ))) %>% 
  mutate(across(where(~ any(. %in% agree_scale3)), ~ case_when(
    . == "Strongly Disagree" ~ 1,
    . == "Disagree" ~ 2,
    . == "Neither agree nor disagree" ~ 3,
    . == "Agree" ~ 4,
    . == "Strongly agree" ~ 5,
    TRUE ~ NA_real_
  ))) %>% 
  mutate(across(where(~ any(. %in% agree_scale4)), ~ case_when(
    . == "Strongly Disagree" ~ 1,
    . == "Disagree" ~ 2,
    . == "Neutral" ~ 3,
    . == "Agree" ~ 4,
    . == "Strongly agree" ~ 5,
    TRUE ~ NA_real_
  ))) %>% 
  mutate(across(where(~ any(. %in% feel_scale)), ~ case_when(
    . == "Never" ~ 1,
    . == "Rarely" ~ 2,
    . == "Sometimes" ~ 3,
    . == "Often" ~ 4,
    . == "Always" ~ 5,
    TRUE ~ NA_real_
  ))) 

summary(recoded_pre_post_data)


### Paired t-test

## Science.ID_1
# Check question match
pre_post_data_questions$Science.ID_1
pre_post_data_questions$post_Science.ID_1
# pre data
sum(!is.na(recoded_pre_post_data$Science.ID_1))
round(mean(recoded_pre_post_data$Science.ID_1, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$Science.ID_1, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_Science.ID_1))
round(mean(recoded_pre_post_data$post_Science.ID_1, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_Science.ID_1, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$Science.ID_1, recoded_pre_post_data$post_Science.ID_1, paired=TRUE)
