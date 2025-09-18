library(cowplot)

#summarize by subject
d_timecourse_by_dataset_age_subj <- d_aoi |>
  filter(t_norm>-1000) |> 
  #create age bins cutoffs
  # mutate(
  #   age_bin = cut(age, breaks = c(0,12,15,18,21,24,30,36,60), 
  #                 labels = c("<12", "12-15","15-18", "18-21", "21-24", "24-30","30-36", ">36"))
  # ) |> 
  mutate(
    age_bin = cut(age, breaks = c(0,12,15,18,21,24,27,30,36,60), 
                  labels = c("<12", "12-15","15-18", "18-21", "21-24", "24-27","27-30","30-36", ">36"))
  ) |>
  #downsample t_norm to 50ms bins
  #mutate(t_norm_downsampled = round(t_norm/50)*50) |>
  group_by(dataset_name,dataset_id,subject_id,age_bin,t_norm) |> 
  #calculate proportion time spent looking at target
  summarize(
    n=n(),
    accuracy = mean(correct, na.rm=TRUE))

#by dataset and age bin
d_timecourse_by_dataset_age_overall <- d_timecourse_by_dataset_age_subj |>
  filter(n>=5) |> 
  group_by(dataset_name,dataset_id,age_bin,t_norm) |>
  summarize(
    n_subj=n(),
    proportion_target_looking = mean(accuracy,na.rm=TRUE)
  ) |> 
  ungroup() |> 
  #create joined dataset_name and age_bin column
  tidyr::unite(dataset_age_bin,dataset_name, age_bin,remove=F)
#across dataset
d_timecourse_across_dataset_by_age_overall <- d_timecourse_by_dataset_age_overall |>
  #filter(n_subj>10) |> 
  group_by(age_bin,t_norm) |>
  summarize(
    N = n(),
    accuracy = mean(proportion_target_looking,na.rm=TRUE),
    #standard error of the mean
    sem = sd(proportion_target_looking,na.rm=TRUE)/sqrt(N)
  ) 

#by age bin, not representing dataset structure
d_timecourse_by_age_overall <- d_timecourse_by_dataset_age_subj |>
  filter(n>=5) |> 
  group_by(age_bin,t_norm) |>
  summarize(
    n_subj=n(),
    proportion_target_looking = mean(accuracy,na.rm=TRUE),
    sem = sd(accuracy,na.rm=TRUE)/sqrt(n_subj)
  ) |> 
  rename(accuracy=proportion_target_looking)

#overall plot
d_timecourse_by_age_overall |> 
  filter(age_bin!="<12") |> 
  filter(t_norm>-500) |> 
  ggplot(aes(x=t_norm, y=accuracy, group=age_bin, color=age_bin)) +
  geom_rect(xmin=2000,xmax=4000,ymin=0,ymax=1.2,fill="#E5E4E2",color="#E5E4E2",alpha=0.05)+
  geom_rect(xmin=200,xmax=2000,ymin=0,ymax=1.2,fill="#F2F2F2",color="#F2F2F2",alpha=0.05)+
  geom_smooth(stat="identity", aes(y=accuracy, ymin=accuracy-sem, ymax=accuracy+sem,fill=age_bin))+
  #geom_smooth(stat="smooth")+
  geom_hline(yintercept=0.5, linetype="dashed")+
  geom_vline(xintercept=0, linetype="solid")+
  #geom_vline(xintercept=200,linetype="dotted")+
  #geom_vline(xintercept=2000,linetype="dotted",color="red",aes=0.1)+
  #geom_vline(xintercept=4000,linetype="dotted")+
  #add text
  annotate("text", x=2, y=0.85, label="Label Onset", hjust=0, vjust=0)+
  annotate("text", x=350, y=0.99, label="SHORT WINDOW",hjust=0, vjust=0)+
  annotate("text", x=1500, y=0.93, label="LONG WINDOW",hjust=0, vjust=0)+
  geom_segment(aes(x = 1000, y = 0.98, xend = 200, yend = 0.98),
               arrow = arrow(type = "closed", angle = 30, length = unit(0.1, "inches")), color="black")+
  geom_segment(aes(x = 1000, y = 0.98, xend = 2000, yend = 0.98),
               arrow = arrow(type = "closed", angle = 30, length = unit(0.1, "inches")), color="black")+
  geom_segment(aes(x = 2000, y = 0.92, xend = 200, yend = 0.92),
               arrow = arrow(type = "closed", angle = 30, length = unit(0.1, "inches")), color="black")+
  geom_segment(aes(x = 2000, y = 0.92, xend = 4000, yend = 0.92),
               arrow = arrow(type = "closed", angle = 30, length = unit(0.1, "inches")), color="black")+
  #geom_line(data=filter(d_timecourse_by_dataset_age_overall,age_bin!="<12"),aes(y=proportion_target_looking,group=dataset_age_bin),alpha=0.5)+
  scale_color_viridis_d(name="Age Bin")+
  scale_fill_viridis_d(name="Age Bin")+
  #facet_wrap(~age_bin)+
  scale_y_continuous(breaks=seq(0.4,1,0.1),limits=c(0.4,1))+
  xlab("Time from trial onset (ms)")+
  ylab("Proportion Target Looking")+
  theme_cowplot()
#change legend position to be inside figure
#theme(legend.position=c(0.67,0.82))
  
#overall plot taking dataset structure into account
d_timecourse_across_dataset_by_age_overall |> 
  filter(age_bin!="<12") |> 
  filter(t_norm>-500) |> 
  ggplot(aes(x=t_norm, y=accuracy, group=age_bin, color=age_bin)) +
  geom_rect(xmin=2000,xmax=4000,ymin=0,ymax=1.2,fill="#D3D3D3",color="#D3D3D3",alpha=0.05)+
  geom_rect(xmin=200,xmax=2000,ymin=0,ymax=1.2,fill="#E5E4E2",color="#E5E4E2",alpha=0.05)+
  geom_smooth(stat="identity", aes(y=accuracy, ymin=accuracy-sem, ymax=accuracy+sem,fill=age_bin))+
  #geom_smooth(stat="smooth")+
  geom_hline(yintercept=0.5, linetype="dashed")+
  geom_vline(xintercept=0, linetype="solid")+
  #geom_vline(xintercept=200,linetype="dotted")+
  #geom_vline(xintercept=2000,linetype="dotted",color="red",aes=0.1)+
  #geom_vline(xintercept=4000,linetype="dotted")+
  #add text
  annotate("text", x=2, y=0.85, label="Label Onset", hjust=0, vjust=0)+
  annotate("text", x=300, y=0.95, label="SHORT WINDOW",hjust=0, vjust=0)+
  annotate("text", x=2100, y=0.95, label="LONG WINDOW",hjust=0, vjust=0)+
  #geom_line(data=filter(d_timecourse_by_dataset_age_overall,age_bin!="<12"),aes(y=proportion_target_looking,group=dataset_age_bin),alpha=0.5)+
  scale_color_viridis_d(name="Age Bin")+
  scale_fill_viridis_d(name="Age Bin")+
  #facet_wrap(~age_bin)+
  scale_y_continuous(breaks=seq(0.4,1,0.1),limits=c(0.4,1))+
  xlab("Time from trial onset (ms)")+
  ylab("Proportion Target Looking")+
  theme_cowplot()
  #change legend position to be inside figure
  #theme(legend.position=c(0.67,0.82))
