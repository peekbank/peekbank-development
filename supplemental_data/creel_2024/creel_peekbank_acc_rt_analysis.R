# run after running Step2a in Creel code

library(tidyverse)
source(here("supplemental_data","creel_2024","rt_helper.R")) #loading local helper, this is just to adjust the sampling rate for rt computation to match creel data
library(cowplot)
library(ggpubr)
library(here)
#data <- read_csv(here("supplemental_data","creel_2024","all_data.csv"))
##dat is all data

target_window_min <- 200
target_window_max <- 4000
#compute RT
binarize_dat <- dat %>%
  mutate(prop = Targ/(Targ+Comp)) %>%
  mutate(
    target_look_binary = case_when(
      prop>.5 ~ 1,
      prop<=.5 ~ 0,
    )
  ) %>%
  mutate(
    aoi = case_when(
      target_look_binary == 1 ~ "target",
      target_look_binary == 0 ~ "distractor",
      TRUE ~ NA
    )
  ) 

# #reformat data
rt_data <- binarize_dat %>%
  filter(any(WOT == 50), # must have data at starting point
         WOT >= 50) %>% # only pass data after 0
  select(Subject,Age,AgeGroups,Tloc,Trialcount,Response,aoi,) %>%
  group_by(Subject,Age,AgeGroups,Tloc,Trialcount,Response) %>%
  summarise(lengths = rle(aoi)$lengths, 
            values = rle(aoi)$values) #run-length-encoded format expected

# compute RTs
rts <- rt_data %>%
  group_by(Subject,Age,AgeGroups,Tloc,Trialcount,Response) %>%
  nest() %>%
  mutate(data = lapply(data, get_rt)) %>% 
  unnest(cols = c(data)) 

#summarize
summarized_trial_data <- binarize_dat %>%
  filter(WOT>=target_window_min) %>%
  filter(WOT<=target_window_max) %>%
  group_by(Subject,Age,AgeGroups,Tloc,Trialcount) %>%
  summarize(
    pointing_acc = mean(Response=="target", na.rm=TRUE),
    looking_acc_prop = mean(prop,na.rm=TRUE),
    looking_acc_bin = mean(target_look_binary,na.rm=TRUE)
  )

# ggplot(summarized_trial_data,aes(pointing_acc,looking_acc_prop))+
#   geom_jitter(alpha=0.01)+
#   geom_smooth(method="lm")
  
summarized_trial_data <- summarized_trial_data %>%
  left_join(rts,by=c("Subject","Age","AgeGroups","Tloc","Trialcount"))

summarized_trial_data_rt <- summarized_trial_data %>%
  filter(!is.na(rt)) %>%
  filter(shift_type == "D-T") %>%
  #remove RTs that are too fast
  filter(rt>=367)

subj_rt_acc <- summarized_trial_data_rt %>%
  group_by(Subject,Age,AgeGroups) %>%
  summarize(
    mean_rt = mean(rt,na.rm=TRUE),
    mean_rt_correct = mean(rt[pointing_acc==1],na.rm=TRUE),
    mean_pointing_acc = mean(pointing_acc,na.rm=TRUE),
    mean_looking_acc_prop = mean(looking_acc_prop,na.rm=TRUE),
    mean_looking_acc_bin = mean(looking_acc_bin,na.rm=TRUE),
    n_trials = n(),
    n_correct_trials=sum(pointing_acc==1,na.rm=TRUE)
  )
