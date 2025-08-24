library(dplyr)
library(ggplot2)
library(tidyr)
library(ltm)

pre.data <- read.csv("/Volumes/T7/YMW Data/Summer Workshop/2025/Summer Virtual Workshop Pre-Survey 2025_August 23, 2025_14.34.csv", header=TRUE)
post.data <- read.csv("/Volumes/T7/YMW Data/Summer Workshop/2025/Summer Virtual Workshop Post-Survey 2025_August 23, 2025_14.16 2.csv", header=TRUE)
application.data <- read.csv("/Volumes/T7/YMW Data/Summer Workshop/2025/Summer 2025 Virtual Workshop Application (Responses) - Sheet2.csv")

### Survey Qs
pre_data_questions <- pre.data[c(1,2),]
post_data_questions <- post.data[c(1,2),]
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
  mutate(Science.ID_1=as.numeric(ifelse(Science.ID_1=="", NA, Science.ID_1))) %>% 
  mutate(Science.ID_2=as.numeric(ifelse(Science.ID_2=="", NA, Science.ID_2))) %>% 
  mutate(Science.ID_3=as.numeric(ifelse(Science.ID_3=="", NA, Science.ID_3))) %>% 
  mutate(Online.identities_1=as.numeric(ifelse(Online.identities_1=="", NA, Online.identities_1))) %>% 
  mutate(Online.identities_2=as.numeric(ifelse(Online.identities_2=="", NA, Online.identities_2))) %>% 
  mutate(Online.identities_3=as.numeric(ifelse(Online.identities_3=="", NA, Online.identities_3))) %>% 
  mutate(Online.identities_4=as.numeric(ifelse(Online.identities_4=="", NA, Online.identities_4))) %>% 
  mutate(Online.identities_5=as.numeric(ifelse(Online.identities_5=="", NA, Online.identities_5))) %>% 
  mutate(Online.identities_6=as.numeric(ifelse(Online.identities_6=="", NA, Online.identities_6))) %>% 
  mutate(Online.identities_7=as.numeric(ifelse(Online.identities_7=="", NA, Online.identities_7))) %>% 
  mutate(Online.identities_8=as.numeric(ifelse(Online.identities_8=="", NA, Online.identities_8))) %>% 
  mutate(Online.identities_9=as.numeric(ifelse(Online.identities_9=="", NA, Online.identities_9))) %>% 
  mutate(Online.identities_10=as.numeric(ifelse(Online.identities_10=="", NA, Online.identities_10))) %>% 
  mutate(Positive.online.comm_1=as.numeric(ifelse(Positive.online.comm_1=="", NA, Positive.online.comm_1))) %>% 
  mutate(Positive.online.comm_2=as.numeric(ifelse(Positive.online.comm_2=="", NA, Positive.online.comm_2))) %>% 
  mutate(Positive.online.comm_3=as.numeric(ifelse(Positive.online.comm_3=="", NA, Positive.online.comm_3))) %>% 
  mutate(Positive.online.comm_4=as.numeric(ifelse(Positive.online.comm_4=="", NA, Positive.online.comm_4))) %>% 
  mutate(Positive.online.comm_5=as.numeric(ifelse(Positive.online.comm_5=="", NA, Positive.online.comm_5))) %>% 
  mutate(Positive.online.comm_6=as.numeric(ifelse(Positive.online.comm_6=="", NA, Positive.online.comm_6))) %>%
  mutate(Positive.online.comm_7=as.numeric(ifelse(Positive.online.comm_7=="", NA, Positive.online.comm_7))) %>% 
  mutate(Positive.online.comm_8=as.numeric(ifelse(Positive.online.comm_8=="", NA, Positive.online.comm_8))) %>% 
  mutate(Positive.online.comm_9=as.numeric(ifelse(Positive.online.comm_9=="", NA, Positive.online.comm_9))) %>% 
  mutate(Positive.online.comm_10=as.numeric(ifelse(Positive.online.comm_10=="", NA, Positive.online.comm_10))) %>% 
  mutate(Positive.online.comm_11=as.numeric(ifelse(Positive.online.comm_11=="", NA, Positive.online.comm_11))) %>% 
  mutate(Computing.attitudes_1=as.numeric(ifelse(Computing.attitudes_1=="", NA, Computing.attitudes_1))) %>% 
  mutate(Computing.attitudes_2=as.numeric(ifelse(Computing.attitudes_2=="", NA, Computing.attitudes_2))) %>% 
  mutate(Computing.attitudes_3=as.numeric(ifelse(Computing.attitudes_3=="", NA, Computing.attitudes_3))) %>% 
  mutate(Computing.attitudes_4=as.numeric(ifelse(Computing.attitudes_4=="", NA, Computing.attitudes_4))) %>% 
  mutate(Computing.attitudes_5=as.numeric(ifelse(Computing.attitudes_5=="", NA, Computing.attitudes_5))) %>% 
  # mutate(Supportive.STEM.envi_1=ifelse(Supportive.STEM.envi_1=="", NA, Supportive.STEM.envi_1)) %>%
  # mutate(Supportive.STEM.envi_2=ifelse(Supportive.STEM.envi_2=="", NA, Supportive.STEM.envi_2)) %>%
  mutate(Tech.careers_1=as.numeric(ifelse(Tech.careers_1=="", NA, Tech.careers_1))) %>% 
  mutate(Tech.careers_2=as.numeric(ifelse(Tech.careers_2=="", NA, Tech.careers_2))) %>% 
  mutate(Tech.careers_3=as.numeric(ifelse(Tech.careers_3=="", NA, Tech.careers_3))) %>% 
  mutate(Tech.careers_4=as.numeric(ifelse(Tech.careers_4=="", NA, Tech.careers_4))) %>% 
  mutate(Tech.careers_5=as.numeric(ifelse(Tech.careers_5=="", NA, Tech.careers_5))) %>% 
  mutate(app_1=as.numeric(ifelse(app_1=="", NA, app_1))) %>% 
  mutate(app_2=as.numeric(ifelse(app_2=="", NA, app_2))) %>% 
  mutate(app_3=as.numeric(ifelse(app_3=="", NA, app_3))) %>% 
  mutate(app_4=as.numeric(ifelse(app_4=="", NA, app_4))) %>% 
  mutate(app_5=as.numeric(ifelse(app_5=="", NA, app_5))) %>% 
  mutate(app_6=as.numeric(ifelse(app_6=="", NA, app_6))) %>% 
  mutate(app_7=as.numeric(ifelse(app_7=="", NA, app_7))) %>% 
  mutate(app_8=as.numeric(ifelse(app_8=="", NA, app_8))) %>% 
  # mutate(app_9=ifelse(app_9=="", NA, app_9)) %>% 
  mutate(app_10=as.numeric(ifelse(app_10=="", NA, app_10))) %>% 
  mutate(app_11=as.numeric(ifelse(app_11=="", NA, app_11))) %>% 
  mutate(app_12=as.numeric(ifelse(app_12=="", NA, app_12))) %>% 
  mutate(app_13=as.numeric(ifelse(app_13=="", NA, app_13))) %>%
  mutate(STEM.innovation_1=as.numeric(ifelse(STEM.innovation_1=="", NA, STEM.innovation_1))) %>% 
  mutate(STEM.innovation_2=as.numeric(ifelse(STEM.innovation_2=="", NA, STEM.innovation_2))) %>% 
  mutate(STEM.innovation_3=as.numeric(ifelse(STEM.innovation_3=="", NA, STEM.innovation_3))) %>% 
  mutate(STEM.innovation_4=as.numeric(ifelse(STEM.innovation_4=="", NA, STEM.innovation_4))) %>% 
  mutate(STEM.innovation_5=as.numeric(ifelse(STEM.innovation_5=="", NA, STEM.innovation_5))) %>% 
  mutate(STEM.innovation_6=as.numeric(ifelse(STEM.innovation_6=="", NA, STEM.innovation_6))) %>% 
  mutate(STEM.innovation_7=as.numeric(ifelse(STEM.innovation_7=="", NA, STEM.innovation_7))) %>% 
  mutate(STEM.innovation_8=as.numeric(ifelse(STEM.innovation_8=="", NA, STEM.innovation_8))) %>% 
  mutate(STEM.innovation_9=as.numeric(ifelse(STEM.innovation_9=="", NA, STEM.innovation_9))) %>% 
  mutate(STEM.innovation_10=as.numeric(ifelse(STEM.innovation_10=="", NA, STEM.innovation_10))) %>% 
  mutate(AI_1=as.numeric(ifelse(Q170_1=="", NA, Q170_1))) %>%
  mutate(AI_2=as.numeric(ifelse(Q170_2=="", NA, Q170_2))) %>%
  mutate(AI_3=as.numeric(ifelse(Q170_3=="", NA, Q170_3))) %>%
  mutate(AI_4=as.numeric(ifelse(Q170_4=="", NA, Q170_4))) %>%
  mutate(smfbeh2_2=as.numeric(ifelse(smfbeh2_2=="", NA, smfbeh2_2))) %>% 
  mutate(smfbeh2_4=as.numeric(ifelse(smfbeh2_4=="", NA, smfbeh2_4))) %>% 
  mutate(smfbeh2_3=as.numeric(ifelse(smfbeh2_3=="", NA, smfbeh2_3))) %>% 
  mutate(smfbeh2_5=as.numeric(ifelse(smfbeh2_5=="", NA, smfbeh2_5))) %>% 
  # mutate(meduse_2=ifelse(meduse_2=="", NA, meduse_2)) %>% 
  # mutate(meduse_3=ifelse(meduse_3=="", NA, meduse_3)) %>% 
  # mutate(meduse_5=ifelse(meduse_5=="", NA, meduse_5)) %>% 
  mutate(Self.esteem_1=as.numeric(ifelse(Self.esteem_1=="", NA, Self.esteem_1))) %>% 
  mutate(Self.esteem_2=as.numeric(ifelse(Self.esteem_2=="", NA, Self.esteem_2))) %>% 
  mutate(Self.esteem_3=as.numeric(ifelse(Self.esteem_3=="", NA, Self.esteem_3))) %>% 
  mutate(Self.esteem_4=as.numeric(ifelse(Self.esteem_4=="", NA, Self.esteem_4))) %>% 
  mutate(Self.esteem_5=as.numeric(ifelse(Self.esteem_5=="", NA, Self.esteem_5))) %>% 
  mutate(Self.esteem_6=as.numeric(ifelse(Self.esteem_6=="", NA, Self.esteem_6))) %>% 
  mutate(Self.esteem_7=as.numeric(ifelse(Self.esteem_7=="", NA, Self.esteem_7))) %>% 
  mutate(Self.esteem_8=as.numeric(ifelse(Self.esteem_8=="", NA, Self.esteem_8))) %>% 
  mutate(Self.esteem_9=as.numeric(ifelse(Self.esteem_9=="", NA, Self.esteem_9))) %>% 
  mutate(Self.esteem_10=as.numeric(ifelse(Self.esteem_10=="", NA, Self.esteem_10))) %>% 
  mutate(Agency_1=as.numeric(ifelse(Agency_1=="", NA, Agency_1))) %>% 
  mutate(Agency_2=as.numeric(ifelse(Agency_2=="", NA, Agency_2))) %>% 
  mutate(Agency_3=as.numeric(ifelse(Agency_3=="", NA, Agency_3))) %>% 
  mutate(Agency_4=as.numeric(ifelse(Agency_4=="", NA, Agency_4))) %>% 
  mutate(Agency_5=as.numeric(ifelse(Agency_5=="", NA, Agency_5))) %>% 
  mutate(Agency_6=as.numeric(ifelse(Agency_6=="", NA, Agency_6))) %>% 
  mutate(Agency_7=as.numeric(ifelse(Agency_7=="", NA, Agency_7))) %>% 
  mutate(Agency_8=as.numeric(ifelse(Agency_8=="", NA, Agency_8))) %>% 
  mutate(Agency_9=as.numeric(ifelse(Agency_9=="", NA, Agency_9))) %>% 
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

post_data_questions_select <- post_data_questions %>% 
  rename("AI_1"="Q170_1") %>%
  rename("AI_2"="Q170_2") %>%
  rename("AI_3"="Q170_3") %>%
  rename("AI_4"="Q170_4") %>%
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
                  Agency_1,Agency_2,Agency_3,Agency_4,Agency_5,Agency_6,Agency_7,Agency_8,Agency_9)) %>% 
  rename_with(~ paste0("post_", .)) %>% 
  dplyr::select(starts_with("post_")) %>% 
  rename("sname"="post_sname")


## Merge pre and post data
pre_post_data <- pre.data.cleaned %>%
  left_join(post.data.cleaned, by="sname")

## Survey Qs
pre_questions <- pre_post_data %>% filter(is.na(Participant)) %>%
  dplyr::select(-starts_with("post_"))

# merge Qs
pre_post_data_questions <- pre_questions %>%
  left_join(post_data_questions_select)

##################################################################################################

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


## Science.ID_2
# Check question match
pre_post_data_questions$Science.ID_2
pre_post_data_questions$post_Science.ID_2
# pre data
sum(!is.na(recoded_pre_post_data$Science.ID_2))
round(mean(recoded_pre_post_data$Science.ID_2, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$Science.ID_2, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_Science.ID_2))
round(mean(recoded_pre_post_data$post_Science.ID_2, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_Science.ID_2, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$Science.ID_2, recoded_pre_post_data$post_Science.ID_2, paired=TRUE)


## Science.ID_3
# Check question match
pre_post_data_questions$Science.ID_3
pre_post_data_questions$post_Science.ID_3
# pre data
sum(!is.na(recoded_pre_post_data$Science.ID_3))
round(mean(recoded_pre_post_data$Science.ID_3, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$Science.ID_3, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_Science.ID_3))
round(mean(recoded_pre_post_data$post_Science.ID_3, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_Science.ID_3, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$Science.ID_3, recoded_pre_post_data$post_Science.ID_3, paired=TRUE)


## Online.identities_1
# Check question match
pre_post_data_questions$Online.identities_1
pre_post_data_questions$post_Online.identities_1
# pre data
sum(!is.na(recoded_pre_post_data$Online.identities_1))
round(mean(recoded_pre_post_data$Online.identities_1, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$Online.identities_1, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_Online.identities_1))
round(mean(recoded_pre_post_data$post_Online.identities_1, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_Online.identities_1, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$Online.identities_1, recoded_pre_post_data$post_Online.identities_1, paired=TRUE)

## Online.identities_2
# Check question match
pre_post_data_questions$Online.identities_2
pre_post_data_questions$post_Online.identities_2
# pre data
sum(!is.na(recoded_pre_post_data$Online.identities_2))
round(mean(recoded_pre_post_data$Online.identities_2, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$Online.identities_2, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_Online.identities_2))
round(mean(recoded_pre_post_data$post_Online.identities_2, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_Online.identities_2, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$Online.identities_2, recoded_pre_post_data$post_Online.identities_2, paired=TRUE)

## Online.identities_3
# Check question match
pre_post_data_questions$Online.identities_3
pre_post_data_questions$post_Online.identities_3
# pre data
sum(!is.na(recoded_pre_post_data$Online.identities_3))
round(mean(recoded_pre_post_data$Online.identities_3, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$Online.identities_3, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_Online.identities_3))
round(mean(recoded_pre_post_data$post_Online.identities_3, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_Online.identities_3, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$Online.identities_3, recoded_pre_post_data$post_Online.identities_3, paired=TRUE)

## Online.identities_4
# Check question match
pre_post_data_questions$Online.identities_4
pre_post_data_questions$post_Online.identities_4
# pre data
sum(!is.na(recoded_pre_post_data$Online.identities_4))
round(mean(recoded_pre_post_data$Online.identities_4, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$Online.identities_4, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_Online.identities_4))
round(mean(recoded_pre_post_data$post_Online.identities_4, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_Online.identities_4, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$Online.identities_4, recoded_pre_post_data$post_Online.identities_4, paired=TRUE)

## Online.identities_5
# Check question match
pre_post_data_questions$Online.identities_5
pre_post_data_questions$post_Online.identities_5
# pre data
sum(!is.na(recoded_pre_post_data$Online.identities_5))
round(mean(recoded_pre_post_data$Online.identities_5, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$Online.identities_5, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_Online.identities_5))
round(mean(recoded_pre_post_data$post_Online.identities_5, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_Online.identities_5, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$Online.identities_5, recoded_pre_post_data$post_Online.identities_5, paired=TRUE)

## Online.identities_6
# Check question match
pre_post_data_questions$Online.identities_6
pre_post_data_questions$post_Online.identities_6
# pre data
sum(!is.na(recoded_pre_post_data$Online.identities_6))
round(mean(recoded_pre_post_data$Online.identities_6, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$Online.identities_6, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_Online.identities_6))
round(mean(recoded_pre_post_data$post_Online.identities_6, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_Online.identities_6, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$Online.identities_6, recoded_pre_post_data$post_Online.identities_6, paired=TRUE)

## Online.identities_7
# Check question match
pre_post_data_questions$Online.identities_7
pre_post_data_questions$post_Online.identities_7
# pre data
sum(!is.na(recoded_pre_post_data$Online.identities_7))
round(mean(recoded_pre_post_data$Online.identities_7, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$Online.identities_7, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_Online.identities_7))
round(mean(recoded_pre_post_data$post_Online.identities_7, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_Online.identities_7, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$Online.identities_7, recoded_pre_post_data$post_Online.identities_7, paired=TRUE)

## Online.identities_8
# Check question match
pre_post_data_questions$Online.identities_8
pre_post_data_questions$post_Online.identities_8
# pre data
sum(!is.na(recoded_pre_post_data$Online.identities_8))
round(mean(recoded_pre_post_data$Online.identities_8, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$Online.identities_8, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_Online.identities_8))
round(mean(recoded_pre_post_data$post_Online.identities_8, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_Online.identities_8, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$Online.identities_8, recoded_pre_post_data$post_Online.identities_8, paired=TRUE)

## Online.identities_9
# Check question match
pre_post_data_questions$Online.identities_9
pre_post_data_questions$post_Online.identities_9
# pre data
sum(!is.na(recoded_pre_post_data$Online.identities_9))
round(mean(recoded_pre_post_data$Online.identities_9, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$Online.identities_9, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_Online.identities_9))
round(mean(recoded_pre_post_data$post_Online.identities_9, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_Online.identities_9, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$Online.identities_9, recoded_pre_post_data$post_Online.identities_9, paired=TRUE)

## Online.identities_10
# Check question match
pre_post_data_questions$Online.identities_10
pre_post_data_questions$post_Online.identities_10
# pre data
sum(!is.na(recoded_pre_post_data$Online.identities_10))
round(mean(recoded_pre_post_data$Online.identities_10, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$Online.identities_10, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_Online.identities_10))
round(mean(recoded_pre_post_data$post_Online.identities_10, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_Online.identities_10, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$Online.identities_10, recoded_pre_post_data$post_Online.identities_10, paired=TRUE)

## Positive.online.comm_1
# Check question match
pre_post_data_questions$Positive.online.comm_1
pre_post_data_questions$post_Positive.online.comm_1
# pre data
sum(!is.na(recoded_pre_post_data$Positive.online.comm_1))
round(mean(recoded_pre_post_data$Positive.online.comm_1, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$Positive.online.comm_1, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_Positive.online.comm_1))
round(mean(recoded_pre_post_data$post_Positive.online.comm_1, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_Positive.online.comm_1, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$Positive.online.comm_1, recoded_pre_post_data$post_Positive.online.comm_1, paired=TRUE)

## Positive.online.comm_2
# Check question match
pre_post_data_questions$Positive.online.comm_2
pre_post_data_questions$post_Positive.online.comm_2
# pre data
sum(!is.na(recoded_pre_post_data$Positive.online.comm_2))
round(mean(recoded_pre_post_data$Positive.online.comm_2, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$Positive.online.comm_2, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_Positive.online.comm_2))
round(mean(recoded_pre_post_data$post_Positive.online.comm_2, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_Positive.online.comm_2, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$Positive.online.comm_2, recoded_pre_post_data$post_Positive.online.comm_2, paired=TRUE)


## Positive.online.comm_3
# Check question match
pre_post_data_questions$Positive.online.comm_3
pre_post_data_questions$post_Positive.online.comm_3
# pre data
sum(!is.na(recoded_pre_post_data$Positive.online.comm_3))
round(mean(recoded_pre_post_data$Positive.online.comm_3, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$Positive.online.comm_3, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_Positive.online.comm_3))
round(mean(recoded_pre_post_data$post_Positive.online.comm_3, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_Positive.online.comm_3, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$Positive.online.comm_3, recoded_pre_post_data$post_Positive.online.comm_3, paired=TRUE)

## Positive.online.comm_4
# Check question match
pre_post_data_questions$Positive.online.comm_4
pre_post_data_questions$post_Positive.online.comm_4
# pre data
sum(!is.na(recoded_pre_post_data$Positive.online.comm_4))
round(mean(recoded_pre_post_data$Positive.online.comm_4, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$Positive.online.comm_4, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_Positive.online.comm_4))
round(mean(recoded_pre_post_data$post_Positive.online.comm_4, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_Positive.online.comm_4, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$Positive.online.comm_4, recoded_pre_post_data$post_Positive.online.comm_4, paired=TRUE)

## Positive.online.comm_5
# Check question match
pre_post_data_questions$Positive.online.comm_5
pre_post_data_questions$post_Positive.online.comm_5
# pre data
sum(!is.na(recoded_pre_post_data$Positive.online.comm_5))
round(mean(recoded_pre_post_data$Positive.online.comm_5, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$Positive.online.comm_5, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_Positive.online.comm_5))
round(mean(recoded_pre_post_data$post_Positive.online.comm_5, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_Positive.online.comm_5, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$Positive.online.comm_5, recoded_pre_post_data$post_Positive.online.comm_5, paired=TRUE)

## Positive.online.comm_6
# Check question match
pre_post_data_questions$Positive.online.comm_6
pre_post_data_questions$post_Positive.online.comm_6
# pre data
sum(!is.na(recoded_pre_post_data$Positive.online.comm_6))
round(mean(recoded_pre_post_data$Positive.online.comm_6, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$Positive.online.comm_6, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_Positive.online.comm_6))
round(mean(recoded_pre_post_data$post_Positive.online.comm_6, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_Positive.online.comm_6, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$Positive.online.comm_6, recoded_pre_post_data$post_Positive.online.comm_6, paired=TRUE)

## Positive.online.comm_7
# Check question match
pre_post_data_questions$Positive.online.comm_7
pre_post_data_questions$post_Positive.online.comm_7
# pre data
sum(!is.na(recoded_pre_post_data$Positive.online.comm_7))
round(mean(recoded_pre_post_data$Positive.online.comm_7, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$Positive.online.comm_7, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_Positive.online.comm_7))
round(mean(recoded_pre_post_data$post_Positive.online.comm_7, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_Positive.online.comm_7, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$Positive.online.comm_7, recoded_pre_post_data$post_Positive.online.comm_7, paired=TRUE)

## Positive.online.comm_8
# Check question match
pre_post_data_questions$Positive.online.comm_8
pre_post_data_questions$post_Positive.online.comm_8
# pre data
sum(!is.na(recoded_pre_post_data$Positive.online.comm_8))
round(mean(recoded_pre_post_data$Positive.online.comm_8, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$Positive.online.comm_8, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_Positive.online.comm_8))
round(mean(recoded_pre_post_data$post_Positive.online.comm_8, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_Positive.online.comm_8, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$Positive.online.comm_8, recoded_pre_post_data$post_Positive.online.comm_8, paired=TRUE)

## Positive.online.comm_9
# Check question match
pre_post_data_questions$Positive.online.comm_9
pre_post_data_questions$post_Positive.online.comm_9
# pre data
sum(!is.na(recoded_pre_post_data$Positive.online.comm_9))
round(mean(recoded_pre_post_data$Positive.online.comm_9, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$Positive.online.comm_9, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_Positive.online.comm_9))
round(mean(recoded_pre_post_data$post_Positive.online.comm_9, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_Positive.online.comm_9, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$Positive.online.comm_9, recoded_pre_post_data$post_Positive.online.comm_9, paired=TRUE)

## Positive.online.comm_10
# Check question match
pre_post_data_questions$Positive.online.comm_10
pre_post_data_questions$post_Positive.online.comm_10
# pre data
sum(!is.na(recoded_pre_post_data$Positive.online.comm_10))
round(mean(recoded_pre_post_data$Positive.online.comm_10, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$Positive.online.comm_10, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_Positive.online.comm_10))
round(mean(recoded_pre_post_data$post_Positive.online.comm_10, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_Positive.online.comm_10, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$Positive.online.comm_10, recoded_pre_post_data$post_Positive.online.comm_10, paired=TRUE)

## Positive.online.comm_11
# Check question match
pre_post_data_questions$Positive.online.comm_11
pre_post_data_questions$post_Positive.online.comm_11
# pre data
sum(!is.na(recoded_pre_post_data$Positive.online.comm_11))
round(mean(recoded_pre_post_data$Positive.online.comm_11, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$Positive.online.comm_11, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_Positive.online.comm_11))
round(mean(recoded_pre_post_data$post_Positive.online.comm_11, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_Positive.online.comm_11, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$Positive.online.comm_11, recoded_pre_post_data$post_Positive.online.comm_11, paired=TRUE)

## Computing.attitudes_1
# Check question match
pre_post_data_questions$Computing.attitudes_1
pre_post_data_questions$post_Computing.attitudes_1
# pre data
sum(!is.na(recoded_pre_post_data$Computing.attitudes_1))
round(mean(recoded_pre_post_data$Computing.attitudes_1, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$Computing.attitudes_1, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_Computing.attitudes_1))
round(mean(recoded_pre_post_data$post_Computing.attitudes_1, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_Computing.attitudes_1, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$Computing.attitudes_1, recoded_pre_post_data$post_Computing.attitudes_1, paired=TRUE)

## Computing.attitudes_2
# Check question match
pre_post_data_questions$Computing.attitudes_2
pre_post_data_questions$post_Computing.attitudes_2
# pre data
sum(!is.na(recoded_pre_post_data$Computing.attitudes_2))
round(mean(recoded_pre_post_data$Computing.attitudes_2, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$Computing.attitudes_2, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_Computing.attitudes_2))
round(mean(recoded_pre_post_data$post_Computing.attitudes_2, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_Computing.attitudes_2, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$Computing.attitudes_2, recoded_pre_post_data$post_Computing.attitudes_2, paired=TRUE)

## Computing.attitudes_3
# Check question match
pre_post_data_questions$Computing.attitudes_3
pre_post_data_questions$post_Computing.attitudes_3
# pre data
sum(!is.na(recoded_pre_post_data$Computing.attitudes_3))
round(mean(recoded_pre_post_data$Computing.attitudes_3, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$Computing.attitudes_3, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_Computing.attitudes_3))
round(mean(recoded_pre_post_data$post_Computing.attitudes_3, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_Computing.attitudes_3, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$Computing.attitudes_3, recoded_pre_post_data$post_Computing.attitudes_3, paired=TRUE)

## Computing.attitudes_4
# Check question match
pre_post_data_questions$Computing.attitudes_4
pre_post_data_questions$post_Computing.attitudes_4
# pre data
sum(!is.na(recoded_pre_post_data$Computing.attitudes_4))
round(mean(recoded_pre_post_data$Computing.attitudes_4, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$Computing.attitudes_4, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_Computing.attitudes_4))
round(mean(recoded_pre_post_data$post_Computing.attitudes_4, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_Computing.attitudes_4, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$Computing.attitudes_4, recoded_pre_post_data$post_Computing.attitudes_4, paired=TRUE)

## Computing.attitudes_5
# Check question match
pre_post_data_questions$Computing.attitudes_5
pre_post_data_questions$post_Computing.attitudes_5
# pre data
sum(!is.na(recoded_pre_post_data$Computing.attitudes_5))
round(mean(recoded_pre_post_data$Computing.attitudes_5, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$Computing.attitudes_5, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_Computing.attitudes_5))
round(mean(recoded_pre_post_data$post_Computing.attitudes_5, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_Computing.attitudes_5, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$Computing.attitudes_5, recoded_pre_post_data$post_Computing.attitudes_5, paired=TRUE)

## Science.career.motiv_1
# Check question match
pre_post_data_questions$Science.career.motiv_1
pre_post_data_questions$post_Science.career.motiv_1
# pre data
sum(!is.na(recoded_pre_post_data$Science.career.motiv_1))
round(mean(recoded_pre_post_data$Science.career.motiv_1, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$Science.career.motiv_1, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_Science.career.motiv_1))
round(mean(recoded_pre_post_data$post_Science.career.motiv_1, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_Science.career.motiv_1, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$Science.career.motiv_1, recoded_pre_post_data$post_Science.career.motiv_1, paired=TRUE)

## Science.career.motiv_2
# Check question match
pre_post_data_questions$Science.career.motiv_2
pre_post_data_questions$post_Science.career.motiv_2
# pre data
sum(!is.na(recoded_pre_post_data$Science.career.motiv_2))
round(mean(recoded_pre_post_data$Science.career.motiv_2, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$Science.career.motiv_2, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_Science.career.motiv_2))
round(mean(recoded_pre_post_data$post_Science.career.motiv_2, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_Science.career.motiv_2, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$Science.career.motiv_2, recoded_pre_post_data$post_Science.career.motiv_2, paired=TRUE)

## Tech.careers_1
# Check question match
pre_post_data_questions$Tech.careers_1
pre_post_data_questions$post_Tech.careers_1
# pre data
sum(!is.na(recoded_pre_post_data$Tech.careers_1))
round(mean(recoded_pre_post_data$Tech.careers_1, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$Tech.careers_1, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_Tech.careers_1))
round(mean(recoded_pre_post_data$post_Tech.careers_1, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_Tech.careers_1, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$Tech.careers_1, recoded_pre_post_data$post_Tech.careers_1, paired=TRUE)

## Tech.careers_2
# Check question match
pre_post_data_questions$Tech.careers_2
pre_post_data_questions$post_Tech.careers_2
# pre data
sum(!is.na(recoded_pre_post_data$Tech.careers_2))
round(mean(recoded_pre_post_data$Tech.careers_2, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$Tech.careers_2, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_Tech.careers_2))
round(mean(recoded_pre_post_data$post_Tech.careers_2, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_Tech.careers_2, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$Tech.careers_2, recoded_pre_post_data$post_Tech.careers_2, paired=TRUE)

## Tech.careers_3
# Check question match
pre_post_data_questions$Tech.careers_3
pre_post_data_questions$post_Tech.careers_3
# pre data
sum(!is.na(recoded_pre_post_data$Tech.careers_3))
round(mean(recoded_pre_post_data$Tech.careers_3, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$Tech.careers_3, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_Tech.careers_3))
round(mean(recoded_pre_post_data$post_Tech.careers_3, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_Tech.careers_3, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$Tech.careers_3, recoded_pre_post_data$post_Tech.careers_3, paired=TRUE)

## Tech.careers_4
# Check question match
pre_post_data_questions$Tech.careers_4
pre_post_data_questions$post_Tech.careers_4
# pre data
sum(!is.na(recoded_pre_post_data$Tech.careers_4))
round(mean(recoded_pre_post_data$Tech.careers_4, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$Tech.careers_4, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_Tech.careers_4))
round(mean(recoded_pre_post_data$post_Tech.careers_4, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_Tech.careers_4, na.rm=TRUE), 3)
# paired t-test
# t.test(recoded_pre_post_data$Tech.careers_4, recoded_pre_post_data$post_Tech.careers_4, paired=TRUE)

## Tech.careers_5
# Check question match
pre_post_data_questions$Tech.careers_5
pre_post_data_questions$post_Tech.careers_5
# pre data
sum(!is.na(recoded_pre_post_data$Tech.careers_5))
round(mean(recoded_pre_post_data$Tech.careers_5, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$Tech.careers_5, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_Tech.careers_5))
round(mean(recoded_pre_post_data$post_Tech.careers_5, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_Tech.careers_5, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$Tech.careers_5, recoded_pre_post_data$post_Tech.careers_5, paired=TRUE)

## app_1
# Check question match
pre_post_data_questions$app_1
pre_post_data_questions$post_app_1
# pre data
sum(!is.na(recoded_pre_post_data$app_1))
round(mean(recoded_pre_post_data$app_1, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$app_1, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_app_1))
round(mean(recoded_pre_post_data$post_app_1, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_app_1, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$app_1, recoded_pre_post_data$post_app_1, paired=TRUE)

## app_2
# Check question match
pre_post_data_questions$app_2
pre_post_data_questions$post_app_2
# pre data
sum(!is.na(recoded_pre_post_data$app_2))
round(mean(recoded_pre_post_data$app_2, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$app_2, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_app_2))
round(mean(recoded_pre_post_data$post_app_2, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_app_2, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$app_2, recoded_pre_post_data$post_app_2, paired=TRUE)

## app_3
# Check question match
pre_post_data_questions$app_3
pre_post_data_questions$post_app_3
# pre data
sum(!is.na(recoded_pre_post_data$app_3))
round(mean(recoded_pre_post_data$app_3, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$app_3, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_app_3))
round(mean(recoded_pre_post_data$post_app_3, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_app_3, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$app_3, recoded_pre_post_data$post_app_3, paired=TRUE)

## app_4
# Check question match
pre_post_data_questions$app_4
pre_post_data_questions$post_app_4
# pre data
sum(!is.na(recoded_pre_post_data$app_4))
round(mean(recoded_pre_post_data$app_4, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$app_4, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_app_4))
round(mean(recoded_pre_post_data$post_app_4, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_app_4, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$app_4, recoded_pre_post_data$post_app_4, paired=TRUE)

## app_5
# Check question match
pre_post_data_questions$app_5
pre_post_data_questions$post_app_5
# pre data
sum(!is.na(recoded_pre_post_data$app_5))
round(mean(recoded_pre_post_data$app_5, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$app_5, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_app_5))
round(mean(recoded_pre_post_data$post_app_5, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_app_5, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$app_5, recoded_pre_post_data$post_app_5, paired=TRUE)

## app_6
# Check question match
pre_post_data_questions$app_6
pre_post_data_questions$post_app_6
# pre data
sum(!is.na(recoded_pre_post_data$app_6))
round(mean(recoded_pre_post_data$app_6, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$app_6, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_app_6))
round(mean(recoded_pre_post_data$post_app_6, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_app_6, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$app_6, recoded_pre_post_data$post_app_6, paired=TRUE)

## app_7
# Check question match
pre_post_data_questions$app_7
pre_post_data_questions$post_app_7
# pre data
sum(!is.na(recoded_pre_post_data$app_7))
round(mean(recoded_pre_post_data$app_7, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$app_7, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_app_7))
round(mean(recoded_pre_post_data$post_app_7, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_app_7, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$app_7, recoded_pre_post_data$post_app_7, paired=TRUE)

## app_8
# Check question match
pre_post_data_questions$app_8
pre_post_data_questions$post_app_8
# pre data
sum(!is.na(recoded_pre_post_data$app_8))
round(mean(recoded_pre_post_data$app_8, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$app_8, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_app_8))
round(mean(recoded_pre_post_data$post_app_8, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_app_8, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$app_8, recoded_pre_post_data$post_app_8, paired=TRUE)

## app_9
# Check question match
pre_post_data_questions$app_9
pre_post_data_questions$post_app_9
# pre data
sum(!is.na(recoded_pre_post_data$app_9))
round(mean(recoded_pre_post_data$app_9, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$app_9, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_app_9))
round(mean(recoded_pre_post_data$post_app_9, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_app_9, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$app_9, recoded_pre_post_data$post_app_9, paired=TRUE)

## app_10
# Check question match
pre_post_data_questions$app_10
pre_post_data_questions$post_app_10
# pre data
sum(!is.na(recoded_pre_post_data$app_10))
round(mean(recoded_pre_post_data$app_10, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$app_10, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_app_10))
round(mean(recoded_pre_post_data$post_app_10, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_app_10, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$app_10, recoded_pre_post_data$post_app_10, paired=TRUE)

## app_11
# Check question match
pre_post_data_questions$app_11
pre_post_data_questions$post_app_11
# pre data
sum(!is.na(recoded_pre_post_data$app_11))
round(mean(recoded_pre_post_data$app_11, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$app_11, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_app_11))
round(mean(recoded_pre_post_data$post_app_11, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_app_11, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$app_11, recoded_pre_post_data$post_app_11, paired=TRUE)

## app_12
# Check question match
pre_post_data_questions$app_12
pre_post_data_questions$post_app_12
# pre data
sum(!is.na(recoded_pre_post_data$app_12))
round(mean(recoded_pre_post_data$app_12, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$app_12, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_app_12))
round(mean(recoded_pre_post_data$post_app_12, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_app_12, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$app_12, recoded_pre_post_data$post_app_12, paired=TRUE)

## STEM.innovation_1
# Check question match
pre_post_data_questions$STEM.innovation_1
pre_post_data_questions$post_STEM.innovation_1
# pre data
sum(!is.na(recoded_pre_post_data$STEM.innovation_1))
round(mean(recoded_pre_post_data$STEM.innovation_1, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$STEM.innovation_1, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_STEM.innovation_1))
round(mean(recoded_pre_post_data$post_STEM.innovation_1, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_STEM.innovation_1, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$STEM.innovation_1, recoded_pre_post_data$post_STEM.innovation_1, paired=TRUE)

## STEM.innovation_2
# Check question match
pre_post_data_questions$STEM.innovation_2
pre_post_data_questions$post_STEM.innovation_2
# pre data
sum(!is.na(recoded_pre_post_data$STEM.innovation_2))
round(mean(recoded_pre_post_data$STEM.innovation_2, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$STEM.innovation_2, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_STEM.innovation_2))
round(mean(recoded_pre_post_data$post_STEM.innovation_2, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_STEM.innovation_2, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$STEM.innovation_2, recoded_pre_post_data$post_STEM.innovation_2, paired=TRUE)

## STEM.innovation_3
# Check question match
pre_post_data_questions$STEM.innovation_3
pre_post_data_questions$post_STEM.innovation_3
# pre data
sum(!is.na(recoded_pre_post_data$STEM.innovation_3))
round(mean(recoded_pre_post_data$STEM.innovation_3, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$STEM.innovation_3, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_STEM.innovation_3))
round(mean(recoded_pre_post_data$post_STEM.innovation_3, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_STEM.innovation_3, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$STEM.innovation_3, recoded_pre_post_data$post_STEM.innovation_3, paired=TRUE)

## STEM.innovation_4
# Check question match
pre_post_data_questions$STEM.innovation_4
pre_post_data_questions$post_STEM.innovation_4
# pre data
sum(!is.na(recoded_pre_post_data$STEM.innovation_4))
round(mean(recoded_pre_post_data$STEM.innovation_4, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$STEM.innovation_4, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_STEM.innovation_4))
round(mean(recoded_pre_post_data$post_STEM.innovation_4, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_STEM.innovation_4, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$STEM.innovation_4, recoded_pre_post_data$post_STEM.innovation_4, paired=TRUE)

## STEM.innovation_5
# Check question match
pre_post_data_questions$STEM.innovation_5
pre_post_data_questions$post_STEM.innovation_5
# pre data
sum(!is.na(recoded_pre_post_data$STEM.innovation_5))
round(mean(recoded_pre_post_data$STEM.innovation_5, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$STEM.innovation_5, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_STEM.innovation_5))
round(mean(recoded_pre_post_data$post_STEM.innovation_5, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_STEM.innovation_5, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$STEM.innovation_5, recoded_pre_post_data$post_STEM.innovation_5, paired=TRUE)

## STEM.innovation_6
# Check question match
pre_post_data_questions$STEM.innovation_6
pre_post_data_questions$post_STEM.innovation_6
# pre data
sum(!is.na(recoded_pre_post_data$STEM.innovation_6))
round(mean(recoded_pre_post_data$STEM.innovation_6, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$STEM.innovation_6, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_STEM.innovation_6))
round(mean(recoded_pre_post_data$post_STEM.innovation_6, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_STEM.innovation_6, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$STEM.innovation_6, recoded_pre_post_data$post_STEM.innovation_6, paired=TRUE)

## STEM.innovation_7
# Check question match
pre_post_data_questions$STEM.innovation_7
pre_post_data_questions$post_STEM.innovation_7
# pre data
sum(!is.na(recoded_pre_post_data$STEM.innovation_7))
round(mean(recoded_pre_post_data$STEM.innovation_7, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$STEM.innovation_7, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_STEM.innovation_7))
round(mean(recoded_pre_post_data$post_STEM.innovation_7, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_STEM.innovation_7, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$STEM.innovation_7, recoded_pre_post_data$post_STEM.innovation_7, paired=TRUE)

## STEM.innovation_8
# Check question match
pre_post_data_questions$STEM.innovation_8
pre_post_data_questions$post_STEM.innovation_8
# pre data
sum(!is.na(recoded_pre_post_data$STEM.innovation_8))
round(mean(recoded_pre_post_data$STEM.innovation_8, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$STEM.innovation_8, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_STEM.innovation_8))
round(mean(recoded_pre_post_data$post_STEM.innovation_8, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_STEM.innovation_8, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$STEM.innovation_8, recoded_pre_post_data$post_STEM.innovation_8, paired=TRUE)

## STEM.innovation_9
# Check question match
pre_post_data_questions$STEM.innovation_9
pre_post_data_questions$post_STEM.innovation_9
# pre data
sum(!is.na(recoded_pre_post_data$STEM.innovation_9))
round(mean(recoded_pre_post_data$STEM.innovation_9, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$STEM.innovation_9, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_STEM.innovation_9))
round(mean(recoded_pre_post_data$post_STEM.innovation_9, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_STEM.innovation_9, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$STEM.innovation_9, recoded_pre_post_data$post_STEM.innovation_9, paired=TRUE)

## STEM.innovation_10
# Check question match
pre_post_data_questions$STEM.innovation_10
pre_post_data_questions$post_STEM.innovation_10
# pre data
sum(!is.na(recoded_pre_post_data$STEM.innovation_10))
round(mean(recoded_pre_post_data$STEM.innovation_10, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$STEM.innovation_10, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_STEM.innovation_10))
round(mean(recoded_pre_post_data$post_STEM.innovation_10, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_STEM.innovation_10, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$STEM.innovation_10, recoded_pre_post_data$post_STEM.innovation_10, paired=TRUE)

## AI_1
# Check question match
pre_post_data_questions$AI_1
pre_post_data_questions$post_AI_1
# pre data
sum(!is.na(recoded_pre_post_data$AI_1))
round(mean(recoded_pre_post_data$AI_1, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$AI_1, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_AI_1))
round(mean(recoded_pre_post_data$post_AI_1, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_AI_1, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$AI_1, recoded_pre_post_data$post_AI_1, paired=TRUE)

## AI_2
# Check question match
pre_post_data_questions$AI_2
pre_post_data_questions$post_AI_2
# pre data
sum(!is.na(recoded_pre_post_data$AI_2))
round(mean(recoded_pre_post_data$AI_2, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$AI_2, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_AI_2))
round(mean(recoded_pre_post_data$post_AI_2, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_AI_2, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$AI_2, recoded_pre_post_data$post_AI_2, paired=TRUE)

## AI_3
# Check question match
pre_post_data_questions$AI_3
pre_post_data_questions$post_AI_3
# pre data
sum(!is.na(recoded_pre_post_data$AI_3))
round(mean(recoded_pre_post_data$AI_3, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$AI_3, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_AI_3))
round(mean(recoded_pre_post_data$post_AI_3, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_AI_3, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$AI_3, recoded_pre_post_data$post_AI_3, paired=TRUE)

## AI_4
# Check question match
pre_post_data_questions$AI_4
pre_post_data_questions$post_AI_4
# pre data
sum(!is.na(recoded_pre_post_data$AI_4))
round(mean(recoded_pre_post_data$AI_4, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$AI_4, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_AI_4))
round(mean(recoded_pre_post_data$post_AI_4, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_AI_4, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$AI_4, recoded_pre_post_data$post_AI_4, paired=TRUE)

## smfbeh2_2
# Check question match
pre_post_data_questions$smfbeh2_2
pre_post_data_questions$post_smfbeh2_2
# pre data
sum(!is.na(recoded_pre_post_data$smfbeh2_2))
round(mean(recoded_pre_post_data$smfbeh2_2, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$smfbeh2_2, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_smfbeh2_2))
round(mean(recoded_pre_post_data$post_smfbeh2_2, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_smfbeh2_2, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$smfbeh2_2, recoded_pre_post_data$post_smfbeh2_2, paired=TRUE)

## smfbeh2_22
# Check question match
pre_post_data_questions$smfbeh2_22
pre_post_data_questions$post_smfbeh2_22
# pre data
sum(!is.na(recoded_pre_post_data$smfbeh2_22))
round(mean(recoded_pre_post_data$smfbeh2_22, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$smfbeh2_22, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_smfbeh2_22))
round(mean(recoded_pre_post_data$post_smfbeh2_22, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_smfbeh2_22, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$smfbeh2_22, recoded_pre_post_data$post_smfbeh2_22, paired=TRUE)

## smfbeh2_3
# Check question match
pre_post_data_questions$smfbeh2_3
pre_post_data_questions$post_smfbeh2_3
# pre data
sum(!is.na(recoded_pre_post_data$smfbeh2_3))
round(mean(recoded_pre_post_data$smfbeh2_3, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$smfbeh2_3, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_smfbeh2_3))
round(mean(recoded_pre_post_data$post_smfbeh2_3, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_smfbeh2_3, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$smfbeh2_3, recoded_pre_post_data$post_smfbeh2_3, paired=TRUE)

## smfbeh2_3
# Check question match
pre_post_data_questions$smfbeh2_3
pre_post_data_questions$post_smfbeh2_3
# pre data
sum(!is.na(recoded_pre_post_data$smfbeh2_3))
round(mean(recoded_pre_post_data$smfbeh2_3, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$smfbeh2_3, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_smfbeh2_3))
round(mean(recoded_pre_post_data$post_smfbeh2_3, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_smfbeh2_3, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$smfbeh2_3, recoded_pre_post_data$post_smfbeh2_3, paired=TRUE)

## smfbeh2_4
# Check question match
pre_post_data_questions$smfbeh2_5
pre_post_data_questions$post_smfbeh2_5
# pre data
sum(!is.na(recoded_pre_post_data$smfbeh2_5))
round(mean(recoded_pre_post_data$smfbeh2_5, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$smfbeh2_5, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_smfbeh2_5))
round(mean(recoded_pre_post_data$post_smfbeh2_5, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_smfbeh2_5, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$smfbeh2_5, recoded_pre_post_data$post_smfbeh2_5, paired=TRUE)

## Self.esteem_1
# Check question match
pre_post_data_questions$Self.esteem_1
pre_post_data_questions$post_Self.esteem_1
# pre data
sum(!is.na(recoded_pre_post_data$Self.esteem_1))
round(mean(recoded_pre_post_data$Self.esteem_1, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$Self.esteem_1, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_Self.esteem_1))
round(mean(recoded_pre_post_data$post_Self.esteem_1, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_Self.esteem_1, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$Self.esteem_1, recoded_pre_post_data$post_Self.esteem_1, paired=TRUE)

## Self.esteem_2
# Check question match
pre_post_data_questions$Self.esteem_2
pre_post_data_questions$post_Self.esteem_2
# pre data
sum(!is.na(recoded_pre_post_data$Self.esteem_2))
round(mean(recoded_pre_post_data$Self.esteem_2, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$Self.esteem_2, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_Self.esteem_2))
round(mean(recoded_pre_post_data$post_Self.esteem_2, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_Self.esteem_2, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$Self.esteem_2, recoded_pre_post_data$post_Self.esteem_2, paired=TRUE)

## Self.esteem_3
# Check question match
pre_post_data_questions$Self.esteem_3
pre_post_data_questions$post_Self.esteem_3
# pre data
sum(!is.na(recoded_pre_post_data$Self.esteem_3))
round(mean(recoded_pre_post_data$Self.esteem_3, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$Self.esteem_3, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_Self.esteem_3))
round(mean(recoded_pre_post_data$post_Self.esteem_3, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_Self.esteem_3, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$Self.esteem_3, recoded_pre_post_data$post_Self.esteem_3, paired=TRUE)

## Self.esteem_4
# Check question match
pre_post_data_questions$Self.esteem_4
pre_post_data_questions$post_Self.esteem_4
# pre data
sum(!is.na(recoded_pre_post_data$Self.esteem_4))
round(mean(recoded_pre_post_data$Self.esteem_4, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$Self.esteem_4, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_Self.esteem_4))
round(mean(recoded_pre_post_data$post_Self.esteem_4, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_Self.esteem_4, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$Self.esteem_4, recoded_pre_post_data$post_Self.esteem_4, paired=TRUE)

## Self.esteem_5
# Check question match
pre_post_data_questions$Self.esteem_5
pre_post_data_questions$post_Self.esteem_5
# pre data
sum(!is.na(recoded_pre_post_data$Self.esteem_5))
round(mean(recoded_pre_post_data$Self.esteem_5, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$Self.esteem_5, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_Self.esteem_5))
round(mean(recoded_pre_post_data$post_Self.esteem_5, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_Self.esteem_5, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$Self.esteem_5, recoded_pre_post_data$post_Self.esteem_5, paired=TRUE)

## Self.esteem_6
# Check question match
pre_post_data_questions$Self.esteem_6
pre_post_data_questions$post_Self.esteem_6
# pre data
sum(!is.na(recoded_pre_post_data$Self.esteem_6))
round(mean(recoded_pre_post_data$Self.esteem_6, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$Self.esteem_6, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_Self.esteem_6))
round(mean(recoded_pre_post_data$post_Self.esteem_6, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_Self.esteem_6, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$Self.esteem_6, recoded_pre_post_data$post_Self.esteem_6, paired=TRUE)

## Self.esteem_7
# Check question match
pre_post_data_questions$Self.esteem_7
pre_post_data_questions$post_Self.esteem_7
# pre data
sum(!is.na(recoded_pre_post_data$Self.esteem_7))
round(mean(recoded_pre_post_data$Self.esteem_7, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$Self.esteem_7, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_Self.esteem_7))
round(mean(recoded_pre_post_data$post_Self.esteem_7, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_Self.esteem_7, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$Self.esteem_7, recoded_pre_post_data$post_Self.esteem_7, paired=TRUE)

## Self.esteem_8
# Check question match
pre_post_data_questions$Self.esteem_8
pre_post_data_questions$post_Self.esteem_8
# pre data
sum(!is.na(recoded_pre_post_data$Self.esteem_8))
round(mean(recoded_pre_post_data$Self.esteem_8, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$Self.esteem_8, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_Self.esteem_8))
round(mean(recoded_pre_post_data$post_Self.esteem_8, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_Self.esteem_8, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$Self.esteem_8, recoded_pre_post_data$post_Self.esteem_8, paired=TRUE)

## Self.esteem_9
# Check question match
pre_post_data_questions$Self.esteem_9
pre_post_data_questions$post_Self.esteem_9
# pre data
sum(!is.na(recoded_pre_post_data$Self.esteem_9))
round(mean(recoded_pre_post_data$Self.esteem_9, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$Self.esteem_9, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_Self.esteem_9))
round(mean(recoded_pre_post_data$post_Self.esteem_9, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_Self.esteem_9, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$Self.esteem_9, recoded_pre_post_data$post_Self.esteem_9, paired=TRUE)

## Self.esteem_10
# Check question match
pre_post_data_questions$Self.esteem_10
pre_post_data_questions$post_Self.esteem_10
# pre data
sum(!is.na(recoded_pre_post_data$Self.esteem_10))
round(mean(recoded_pre_post_data$Self.esteem_10, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$Self.esteem_10, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_Self.esteem_10))
round(mean(recoded_pre_post_data$post_Self.esteem_10, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_Self.esteem_10, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$Self.esteem_10, recoded_pre_post_data$post_Self.esteem_10, paired=TRUE)

## Agency_1
# Check question match
pre_post_data_questions$Agency_1
pre_post_data_questions$post_Agency_1
# pre data
sum(!is.na(recoded_pre_post_data$Agency_1))
round(mean(recoded_pre_post_data$Agency_1, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$Agency_1, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_Agency_1))
round(mean(recoded_pre_post_data$post_Agency_1, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_Agency_1, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$Agency_1, recoded_pre_post_data$post_Agency_1, paired=TRUE)

## Agency_2
# Check question match
pre_post_data_questions$Agency_2
pre_post_data_questions$post_Agency_2
# pre data
sum(!is.na(recoded_pre_post_data$Agency_2))
round(mean(recoded_pre_post_data$Agency_2, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$Agency_2, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_Agency_2))
round(mean(recoded_pre_post_data$post_Agency_2, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_Agency_2, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$Agency_2, recoded_pre_post_data$post_Agency_2, paired=TRUE)

## Agency_3
# Check question match
pre_post_data_questions$Agency_3
pre_post_data_questions$post_Agency_3
# pre data
sum(!is.na(recoded_pre_post_data$Agency_3))
round(mean(recoded_pre_post_data$Agency_3, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$Agency_3, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_Agency_3))
round(mean(recoded_pre_post_data$post_Agency_3, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_Agency_3, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$Agency_3, recoded_pre_post_data$post_Agency_3, paired=TRUE)

## Agency_4
# Check question match
pre_post_data_questions$Agency_4
pre_post_data_questions$post_Agency_4
# pre data
sum(!is.na(recoded_pre_post_data$Agency_4))
round(mean(recoded_pre_post_data$Agency_4, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$Agency_4, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_Agency_4))
round(mean(recoded_pre_post_data$post_Agency_4, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_Agency_4, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$Agency_4, recoded_pre_post_data$post_Agency_4, paired=TRUE)

## Agency_5
# Check question match
pre_post_data_questions$Agency_5
pre_post_data_questions$post_Agency_5
# pre data
sum(!is.na(recoded_pre_post_data$Agency_5))
round(mean(recoded_pre_post_data$Agency_5, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$Agency_5, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_Agency_5))
round(mean(recoded_pre_post_data$post_Agency_5, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_Agency_5, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$Agency_5, recoded_pre_post_data$post_Agency_5, paired=TRUE)

## Agency_6
# Check question match
pre_post_data_questions$Agency_6
pre_post_data_questions$post_Agency_6
# pre data
sum(!is.na(recoded_pre_post_data$Agency_6))
round(mean(recoded_pre_post_data$Agency_6, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$Agency_6, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_Agency_6))
round(mean(recoded_pre_post_data$post_Agency_6, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_Agency_6, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$Agency_6, recoded_pre_post_data$post_Agency_6, paired=TRUE)

## Agency_7
# Check question match
pre_post_data_questions$Agency_7
pre_post_data_questions$post_Agency_7
# pre data
sum(!is.na(recoded_pre_post_data$Agency_7))
round(mean(recoded_pre_post_data$Agency_7, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$Agency_7, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_Agency_7))
round(mean(recoded_pre_post_data$post_Agency_7, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_Agency_7, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$Agency_7, recoded_pre_post_data$post_Agency_7, paired=TRUE)

## Agency_8
# Check question match
pre_post_data_questions$Agency_8
pre_post_data_questions$post_Agency_8
# pre data
sum(!is.na(recoded_pre_post_data$Agency_8))
round(mean(recoded_pre_post_data$Agency_8, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$Agency_8, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_Agency_8))
round(mean(recoded_pre_post_data$post_Agency_8, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_Agency_8, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$Agency_8, recoded_pre_post_data$post_Agency_8, paired=TRUE)

## Agency_9
# Check question match
pre_post_data_questions$Agency_9
pre_post_data_questions$post_Agency_9
# pre data
sum(!is.na(recoded_pre_post_data$Agency_9))
round(mean(recoded_pre_post_data$Agency_9, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$Agency_9, na.rm=TRUE), 3)
# post data
sum(!is.na(recoded_pre_post_data$post_Agency_9))
round(mean(recoded_pre_post_data$post_Agency_9, na.rm=TRUE), 3)
round(sd(recoded_pre_post_data$post_Agency_9, na.rm=TRUE), 3)
# paired t-test
t.test(recoded_pre_post_data$Agency_9, recoded_pre_post_data$post_Agency_9, paired=TRUE)
