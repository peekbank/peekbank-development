suppressPackageStartupMessages(library(here))
suppressPackageStartupMessages(library(psych))
suppressPackageStartupMessages(library(lmerTest))
suppressPackageStartupMessages(library(lavaan))
suppressPackageStartupMessages(library(tidySEM))
suppressPackageStartupMessages(library(xtable))
suppressPackageStartupMessages(library(papaja))
suppressPackageStartupMessages(library(viridis))
#suppressPackageStartupMessages(library(nlme))
suppressPackageStartupMessages(library(cowplot))

source(here("helper","common.R"))

source(here("helper/modified_sem_plot.R"))

#load d_aoi for timecourse plot
d_aoi <- readRDS(here("cached_intermediates","0_d_aoi.Rds"))
d_trial <- readRDS(here("cached_intermediates","1_d_trial.Rds")) |>
  mutate(log_age = log(age))
d_sub <- readRDS(here("cached_intermediates","1_d_sub.Rds")) |>
  group_by(subject_id) |>
  arrange(age) |>
  mutate(admin_num = 1:n(), 
         time_since_t0 = age - age[1],
         delta_t = c(0, diff(age)))

d_trial$age_s <- scale(d_trial$age)[,1]
d_trial$log_age_s <- scale(d_trial$log_age)[,1]   
d_sub$age_s <- scale(d_sub$age)[,1]
d_sub$log_age_s <- scale(d_sub$log_age)[,1]   

AGEVAR_CUTOFF <- 6

datasets_with_age_variation <- d_trial |>
  group_by(dataset_name) |>
  summarise(age_range = max(age) - min(age)) |>
  filter(age_range >= AGEVAR_CUTOFF)

d_sub_agevar <- d_sub |>
  filter(dataset_name %in% datasets_with_age_variation$dataset_name)

longitudinal <- d_sub |>
  group_by(dataset_name, subject_id) |>
  count() |>
  filter(n > 1)

d_sub_firstadmin <- d_sub |>
  group_by(dataset_name, subject_id) |>
  filter(admin_num == 1) |>
  ungroup() |>
  select(-admin_num, -time_since_t0, -delta_t)

knitr::opts_chunk$set(echo=FALSE, message=FALSE, warning=FALSE, cache=TRUE)

#set to TRUE the first time running the document to ensure all cached elements are created (FALSE thereafter)
FIRST_TIME = FALSE

dataset_characteristics <- d_sub |>
  group_by(dataset_name) |>
  summarise(`N subjects` = length(unique(subject_id)),
            `N admins` = n(), 
            `Mean Age` = mean(age), 
            `Min Age` = min(age), 
            `Max Age` = max(age), 
            `Avg Trials` = mean(n_trials[!is.na(long_window_accuracy)]),
            `Avg RT Trials` = mean(n_trials_rt[!is.na(rt)]),
            `CDIs` = ifelse(!is.na(any(prod)), "x", ""), 
            `longitudinal` = ifelse(any(duplicated(subject_id)), "x", "")) |>
  arrange(desc(`N subjects`)) 

dataset_characteristics <- dataset_characteristics |>
  bind_rows(tibble(dataset_name = "Total", 
                       `N subjects` = sum(dataset_characteristics$`N subjects`),
                       `N admins` = sum(dataset_characteristics$`N admins`), 
                       `Mean Age` = mean(dataset_characteristics$`Mean Age`),
                       `Min Age` = min(dataset_characteristics$`Min Age`), 
                       `Max Age` = max(dataset_characteristics$`Max Age`), 
                       `Avg Trials` = mean(dataset_characteristics$`Avg Trials`),
                       `Avg RT Trials` = mean(dataset_characteristics$`Avg RT Trials`),
                       `CDIs` = as.character(sum(dataset_characteristics$CDIs != "")),
                       `longitudinal` = as.character(sum(dataset_characteristics$longitudinal != ""))))

# xtable(dataset_characteristics,
#        caption = "Characteristics of included datasets from Peekbank. `Admins` denotes separate experimental sessions.")


#summarize by subject
d_timecourse_by_dataset_age_subj <- d_aoi |>
  filter(t_norm>-2000) |> 
  #create age bins cutoffs
  mutate(
    age_bin = cut(age, breaks = c(0,12,15,18,21,24,27,30,36,60), 
                  labels = c("<12", "12-15","15-18", "18-21", "21-24", "24-27","27-30","30-36", ">36"))
  ) |>
  #downsample t_norm to 50ms binsv
  #mutate(t_norm_downsampled = round(t_norm/50)*50) |>
  group_by(dataset_name,dataset_id,subject_id,age_bin,t_norm) |> 
  #calculate proportion time spent looking at target
  summarize(
    n=n(),
    accuracy = mean(correct, na.rm=TRUE))

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
  filter(t_norm>-1000) |> 
  ggplot(aes(x=t_norm, y=accuracy, group=age_bin, color=age_bin)) +
  geom_rect(xmin=2000,xmax=4000,ymin=0,ymax=1.2,fill="#DFDFDF",color=NA,alpha=0.05)+
  geom_rect(xmin=200,xmax=2000,ymin=0,ymax=1.2,fill="#F2F2F2",color=NA,alpha=0.05)+
  geom_smooth(stat="identity", aes(y=accuracy, ymin=accuracy-sem, ymax=accuracy+sem,fill=age_bin))+
  #geom_smooth(stat="smooth")+
  geom_hline(yintercept=0.5, linetype="dashed")+
  geom_vline(xintercept=0, linetype="solid")+
  #add text
  annotate("text", x=50, y=0.85, label="Label Onset", hjust=0, vjust=0,size=3)+
  annotate("text", x=400, y=0.99, label="SHORT WINDOW",hjust=0, vjust=0,size=2.5)+
  annotate("text", x=1500, y=0.93, label="LONG WINDOW",hjust=0, vjust=0,size=2.5)+
  geom_segment(aes(x = 1000, y = 0.98, xend = 200, yend = 0.98),
               arrow = arrow(type = "closed", angle = 20, length = unit(0.05, "inches")), color="black")+
  geom_segment(aes(x = 1000, y = 0.98, xend = 2000, yend = 0.98),
               arrow = arrow(type = "closed", angle = 20, length = unit(0.05, "inches")), color="black")+
  geom_segment(aes(x = 2000, y = 0.92, xend = 200, yend = 0.92),
               arrow = arrow(type = "closed", angle = 20, length = unit(0.05, "inches")), color="black")+
  geom_segment(aes(x = 2000, y = 0.92, xend = 4000, yend = 0.92),
               arrow = arrow(type = "closed", angle = 20, length = unit(0.05, "inches")), color="black")+
  scale_color_viridis_d(name="Age Bin\n(mos)", guide = guide_legend(reverse = TRUE))+
  scale_fill_viridis_d(name="Age Bin\n(mos)", guide = guide_legend(reverse = TRUE))+
  #facet_wrap(~age_bin)+
  scale_y_continuous(breaks=seq(0.4,1,0.1),limits=c(0.4,1))+
  scale_x_continuous(breaks=seq(-1000,4000,1000),limits=c(-1050,4000))+
  xlab("Time from trial onset (ms)")+
  ylab("Proportion Target Looking")+
  theme_cowplot()+
  theme(legend.key.size = unit(0.3, 'cm'), #change legend key size
        legend.key.height = unit(0.3, 'cm'), #change legend key height
        legend.key.width = unit(0.3, 'cm'), #change legend key width
        legend.text = element_text(size=9),
        legend.title = element_text(size=9),
        legend.position=c(0.03,0.6),
        axis.title = element_text(size=12),
        axis.text = element_text(size=10))

d_trial$age_s <- scale(d_trial$age)[,1]
d_trial$log_age_s <- scale(d_trial$log_age)[,1]   

acc_mods_lmer <- list(lwa_lin = lmer(long_window_accuracy ~ age_s  +
                                     + (age_s | dataset_name) + (1 | subject_id), data = d_trial),
                      lwa_log = lmer(long_window_accuracy ~ log_age_s +
                                     + (log_age_s | dataset_name) + (1 | subject_id), data = d_trial),
                      swa_lin = lmer(short_window_accuracy ~ age_s  +
                                     + (age_s | dataset_name) + (1 | subject_id), data = d_trial),
                      swa_log = lmer(short_window_accuracy ~ log_age_s +
                                     + (log_age_s | dataset_name) + (1 | subject_id), data = d_trial))

acc_mods_lmer_summary <- map_df(acc_mods_lmer, ~broom.mixed::glance(.x)) |>
  mutate(model = names(acc_mods_lmer), 
         r2 = map_dbl(acc_mods_lmer, ~performance::r2_nakagawa(.x)$R2_conditional))

rt_mods_lmer <- list(log_lin = lmer(log_rt ~ age_s + 
                                      (age_s | dataset_name) + (1 | subject_id), data = d_trial),
                     log_log = lmer(log_rt ~ log_age_s + 
                                      (log_age_s | dataset_name) + (1 | subject_id), data = d_trial),
                     lin_lin = lmer(rt ~ age_s + 
                                      (age_s | dataset_name) + (1 | subject_id), data = d_trial),
                     lin_log = lmer(rt ~ log_age_s +  
                                      (log_age_s | dataset_name) + (1 | subject_id), data = d_trial))

rt_mods_lmer_summary <- map_df(rt_mods_lmer, ~broom.mixed::glance(.x)) |>
  mutate(model = names(rt_mods_lmer), 
         r2 = map_dbl(rt_mods_lmer, ~performance::r2_nakagawa(.x)$R2_conditional))


#library(RColorBrewer)

mycolors = c("#1B9E77", "#D95F02", "#E7298A", "#66A61E", "#1B9E77", "#D95F02", "#E7298A", "#66A61E", "#1B9E77", "#D95F02", "#E7298A", "#66A61E","#1B9E77", "#D95F02")
# 4 colors from brewer scale "Dark2" that I thought had decent contrast against the grey/black background dots


a <- ggplot(d_sub,
            aes(x = age, y = long_window_accuracy)) + 
  geom_point(alpha = .02) + 
  geom_line(aes(group = subject_id), alpha = .05) + 
  geom_smooth(data = d_sub_agevar,
              aes(group = dataset_name, col = dataset_name), se=FALSE,
              alpha = 1, method = "lm", size = .25, lty = 1) + 
  geom_smooth(method = "lm") +
  geom_hline(yintercept = .5, lty = 3) + 
  scale_x_log10(breaks = c(12,24,36,48,60)) +
  scale_color_manual(values=mycolors, guide = FALSE) + 
  ylab("Proportion Target Looking") + 
  xlab("Age (months)")

b <- ggplot(d_sub,
            aes(x = age, y = rt)) + 
  geom_point(alpha = .02) + 
  geom_line(aes(group = subject_id), alpha = .05) + 
  geom_smooth(data = d_sub_agevar,
              aes(group = dataset_name, col = dataset_name), se=FALSE,
              alpha = .1, method = "lm", size = .25, lty = 1) + 
  geom_smooth(method = "lm") +
  scale_x_log10(breaks = c(12,24,36,48,60)) +
  scale_y_log10() +
  scale_color_manual(values=mycolors, guide = FALSE) + 
  theme(legend.position = "bottom") + 
  ylab("Reaction Time (log)") + 
  xlab("Age (months)")

cowplot::plot_grid(a,b)

acc_var_mod <- lmer(long_window_acc_var ~ log_age_s + (log_age_s | dataset_name) + 
                 (1 | subject_id), 
               data = d_sub)


rt_var_mod <- lmer(log_rt_var ~ log_age_s + (log_age_s | dataset_name) + 
                 (1 | subject_id), 
               data = d_sub)

  mycolors = c("#1B9E77", "#D95F02", "#E7298A", "#66A61E", "#1B9E77", "#D95F02", "#E7298A", "#66A61E", "#1B9E77", "#D95F02", "#E7298A", "#66A61E","#1B9E77", "#D95F02")

c <- ggplot(d_sub,
       aes(x = age, y = long_window_acc_var)) + 
  geom_point(alpha = .02) + 
  geom_line(aes(group = subject_id), alpha = .05) + 
  geom_smooth(data = d_sub_agevar,
              aes(group = dataset_name, col = dataset_name), se=FALSE,
              alpha = .1, method = "lm", size = .25, lty = 1) + 
    geom_smooth(method = "lm") +
  scale_x_log10(breaks = c(12,24,36,48,60)) +
  scale_color_manual(values=mycolors, guide = FALSE) + 
  ylab("SD of Prop Target Looking") + 
  xlab("Age (months)")

d <- ggplot(d_sub,
       aes(x = age, y = log_rt_var)) + 
  geom_point(alpha = .02) + 
  geom_line(aes(group = subject_id), alpha = .05) + 
  geom_smooth(data = d_sub_agevar,
              aes(group = dataset_name, col = dataset_name), se=FALSE,
              alpha = .1, method = "lm", size = .25, lty = 1) + 
    geom_smooth(method = "lm") +
  scale_x_log10(breaks = c(12,24,36,48,60)) +
  scale_color_manual(values=mycolors, guide = FALSE) + 
  theme(legend.position = "bottom") + 
  ylab("SD of log RT") + 
  xlab("Age (months)")

cowplot::plot_grid(c,d)

d_sub_mat <- d_sub |>
  ungroup() |>
  select(dataset_name, log_rt, log_rt_var, long_window_accuracy, long_window_acc_var, prod, comp, age, log_age) 


d_sub_mat_s <- d_sub_mat |>
  ungroup() |>
  mutate(across(all_of(c("log_rt", "log_rt_var", "long_window_accuracy", 
                         "long_window_acc_var", "prod", "comp")), 
                       ~ age_scale(.x, age))) 

parallel_ana <- capture.output(fa.parallel(select(d_sub_mat, -dataset_name, -age, -log_age), fa = "fa", 
            use = "pairwise.complete.obs", plot=FALSE))

fa3_model <- "vocab =~ prod + comp
              accuracy =~ long_window_accuracy + long_window_acc_var
              speed =~ log_rt + log_rt_var"

fit3 <- cfa(fa3_model, d_sub_mat_s, std.lv=TRUE, missing='fiml')

fit_stats <- suppressMessages(summary(fit3, fit.measures=TRUE, standardize=TRUE))


d_sub_mat_s_renamed <- d_sub_mat_s |>
  rename(acc = long_window_accuracy, 
         acc_sd = long_window_acc_var, 
         log_rt_sd = log_rt_var)

fa3_age_model <- "
# measurement model
vocab =~ prod + comp
accuracy =~ acc + acc_sd
speed =~ log_rt + log_rt_sd

# regressions
vocab ~ log_age
accuracy ~ log_age
speed ~ log_age
"

fit3_age <- cfa(fa3_age_model, d_sub_mat_s_renamed, std.lv=TRUE, missing='fiml')

fit3_age_summary <- suppressMessages(summary(fit3_age, fit.measures=TRUE, 
                                             standardize=TRUE))

layout_age = matrix(nrow=5, ncol = 6,
                data = c(NA,NA,"log_age",NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, 
                         "speed",NA,"accuracy",NA,"vocab",NA,
                         NA, NA, NA, NA, NA, NA, 
                         "log_rt","log_rt_sd", "acc",
                         "acc_sd", "prod","comp"), 
                byrow = TRUE)


node_text <- tibble(name=c("acc", "acc_sd", "accuracy", "comp", "log_age", "log_rt", "log_rt_sd", "prod", "speed", "vocab"), pretty=c("Target Looking", "Target Looking (SD)", "Accuracy", "Comprehension", "Age (log months)", "log RT", "log RT (SD)", "Production", "Speed", "Vocabulary"))

nodes <- get_nodes(fit3_age) |> tibble() |> left_join(node_text) |> mutate(label=ifelse(shape=="oval",pretty, str_c(pretty, "\n", est_sig)))


edges <- get_edges(fit3_age) |> filter(label!="1.00") |> mutate(label_location=.5) |> mutate(label_hjust=.5, label_vjust=.5, label_lineheight=1.5)

edges[9, "label_hjust"]=1
edges[7, "label_hjust"]=1

edges[18, "label_hjust"]=0
edges[16, "label_hjust"]=0

edges[17,"curvature"]=50
edges[17,"label_location"]=.6
edges[, "label_fill"]="white"

source(here("helper/modified_sem_plot.R"))

#View(edges)
prepare_graph(edges=edges, nodes=nodes, text_size = 2.5, layout = t(layout_age),
          rect_width=4.2,
          rect_height=3,
          ellipses_width=3,
          ellipses_height=3,
          variance_height=2,
          variance_width=1.5,
          arrow_angle=15,
          arrow_length=.1,
          var_arrow_angle=15,
          var_arrow_length=.1,
          spacing_y=4,
          spacing_x=4,
          fix_coord=FALSE) |> plot_happy_arrows()

d_growth <- d_sub |>
  filter(!is.na(prod), subject_id %in% longitudinal$subject_id) |>
  group_by(subject_id) |>
  arrange(age) |>
  mutate(rt_t0 = log_rt[1]) |>
  ungroup() |>
  mutate(rt_t0_group = ifelse(rt_t0 < median(rt_t0, na.rm=TRUE), "low", "high"))

d_growth$age_15 <- d_growth$age - 15

# note that it's singular to add an age by dataset random effect
growth_mod <- lmer(prod ~ poly(age_15,2) *rt_t0 + (age  | subject_id) + (1 | dataset_name), 
     data = d_growth, 
     control = lmerControl(optimizer = "bobyqa"))

# summary(growth_mod)

d_sub_prod <- filter(d_sub, !is.na(prod), !is.na(log_rt)) |>
  ungroup() |>
  mutate(log_rt_resid = resid(lm(log_rt ~ log_age))) |>
  group_by(subject_id) |>
  mutate(log_rt_0 = log_rt[1],
         log_rt_0_resid = log_rt_resid[1],
         acc_0 = long_window_accuracy[1]) |> 
  filter(!is.na(log_rt_0)) 

d_sub_prod$age_c <- d_sub_prod$age - mean(d_sub_prod$age, na.rm = TRUE)
d_sub_prod$log_rt_0_c <- d_sub_prod$log_rt_0 - mean(d_sub_prod$log_rt_0, na.rm = TRUE)





fitted_vals <- readRDS("brms1_fitted.rds")


new_data <- expand.grid(
  age_c = seq(min(d_sub_prod$age_c), max(d_sub_prod$age_c), length.out = 100),
  log_rt_0_c = c(-1, 0, 1)  # e.g., low, mean, high values
)

new_data$fit <- fitted_vals[, "Estimate"]
new_data$lower <- fitted_vals[, "Q2.5"]
new_data$upper <- fitted_vals[, "Q97.5"]

ggplot(new_data, aes(x = age_c, y = fit, col = as.factor(log_rt_0_c))) +
    geom_line(data = d_sub_prod, aes(x = age_c, y = prod, group=subject_id), col = "black", alpha=.1) + 
  geom_line(linewidth = 2) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = as.factor(log_rt_0_c), group = as.factor(log_rt_0_c)), 
              alpha = 0.3) +
  labs(x = "Centered Age", y = "Predicted prod (fixed effects only)") +
  theme_minimal() + 
  scale_color_solarized(name = "log RT at t0 (SD)") + 
  scale_fill_solarized(guide = FALSE)



# rename and scale variables
d_sub_s <- d_sub |>
  ungroup() |>
  select(dataset_name, subject_id, administration_id, age, 
         time_since_t0, delta_t,
         log_rt, log_rt_var, 
         long_window_accuracy, long_window_acc_var, prod, comp, ) |>
  rename(acc = long_window_accuracy, 
         acc_sd = long_window_acc_var, 
         log_rt_sd = log_rt_var) |>
  ungroup() |>
  mutate(across(all_of(c("log_rt", "log_rt_sd", "acc", "acc_sd", 
                         "prod", "comp")), 
                       ~ age_scale(.x, age))) 

d_sub_wide_fine <- d_sub_s |>
  mutate(t = cut(time_since_t0, breaks = c(0, 2, 4, 6, 8, 10, 30), 
                 include.lowest = TRUE)) |>
  filter(t != "(12,30]") |>
  mutate(t = as.numeric(t) -1) |>
  group_by(subject_id, dataset_name, t) |>
  summarise(across(all_of(c("log_rt", "log_rt_sd", "acc",
                            "acc_sd", "prod", "comp")),
                   ~ mean(.x, na.rm=TRUE))) |> 
  mutate(across(all_of(c("log_rt", "log_rt_sd", "acc",
                         "acc_sd", "prod", "comp")),
                ~ ifelse(is.nan(.x), NA, .x))) |>
  pivot_wider(id_cols = c("subject_id","dataset_name"), 
              names_from = "t",
              values_from = c("log_rt", "log_rt_sd", "acc", 
                              "acc_sd", "prod", "comp"), 
              names_prefix = "t")


slopes_model_long <- "

# regressions
accuracy_intercept =~ 1*acc_t0 + 1*acc_t1 + 1*acc_t2 + 1*acc_t3 + 1*acc_t4
accuracy_slope =~ 1*acc_t0 + 2*acc_t1 + 3*acc_t2 + 4*acc_t3 + 5*acc_t4
speed_intercept =~ 1*log_rt_t0 + 1*log_rt_t1 + 1*log_rt_t2 + 1*log_rt_t3 + 1*log_rt_t4
speed_slope =~ 1*log_rt_t0 + 2*log_rt_t1 + 3*log_rt_t2 + 4*log_rt_t3 + 5*log_rt_t4
vocab_intercept =~ 1*prod_t0 + 1*prod_t1 + 1*prod_t2 + 1*prod_t3 + 1*prod_t4
vocab_slope =~ 1*prod_t0 + 2*prod_t1 + 3*prod_t2 + 4*prod_t3 + 5*prod_t4
# let variances of each latent vary
accuracy_intercept ~~ NA*accuracy_intercept
accuracy_slope ~~ NA*accuracy_slope
speed_intercept ~~ NA*speed_intercept
speed_slope ~~ NA*speed_slope
vocab_intercept ~~ NA*vocab_intercept
vocab_slope ~~ NA*vocab_slope
"

slopes_long <- growth(slopes_model_long, d_sub_wide_fine, std.lv=TRUE, missing='fiml')

slopes_long_summary <- suppressMessages(summary(slopes_long, fit.measures=TRUE, standardize=TRUE))

layout <- read.csv(here("misc/layout_slopes_observed_growth.csv"), header = FALSE)

acc_color <- "#B2DF8A"
rt_color <- "#FB9A99"
prod_color <- "#FFFF99"

intercept_color <- "#1F78B4"
intercept_background <- "#A6CEE3"
slope_color <- "#FF7F00"
slope_background <- "#FDBF6F"

#"#A6CEE3" "#1F78B4" "#B2DF8A" "#33A02C"
# [5] "#FB9A99" "#E31A1C" "#FDBF6F" "#FF7F00"
# [9] "#CAB2D6" "#6A3D9A" "#FFFF99" "#B15928"
node_text <- tribble(~name, ~ pretty, ~fill, 
                     "acc_t0", "t0 Target Looking", acc_color,
                     "acc_t1", "t1 Target Looking", acc_color,
                     "acc_t2", "t2 Target Looking",acc_color,
                     "acc_t3", "t3 Target Looking",acc_color,
                     "acc_t4", "t4 Target Looking",acc_color,
                     "accuracy_intercept", "Accuracy intercept",acc_color,
                     "accuracy_slope", "Accuracy slope",acc_color,
                     "log_rt_t0", "t0 log RT",rt_color,
                     "log_rt_t1", "t1 log RT",rt_color,
                     "log_rt_t2", "t2 log RT",rt_color,
                     "log_rt_t3", "t3 log RT",rt_color,
                     "log_rt_t4", "t4 log RT",rt_color,
                     "prod_t0" , "t0 Production", prod_color,
                     "prod_t1" , "t1 Production",  prod_color,
                     "prod_t2" , "t2 Production" ,  prod_color,
                     "prod_t3", "t3 Production",         prod_color,
                     "prod_t4", "t4 Production" , prod_color,
                     "speed_intercept" , "Speed intercept" , rt_color,
                     "speed_slope" , "Speed slope",rt_color,
                     "vocab_intercept", "Vocabulary intercept", prod_color,
                     "vocab_slope", "Vocabulary slope",  prod_color ) |> 
  mutate(alpha=.1)

nodes <- get_nodes(slopes_long) |> tibble() |> left_join(node_text) |> mutate(label=pretty)


edges <- get_edges(slopes_long) |> mutate(label_location=.5) |> mutate(label=ifelse(arrow=="last", NA, label)) |>  mutate(connect_from=ifelse(to==from, "right",NA),
                    connect_to=connect_from) |> 
  mutate(color="black", label_hjust=.5, label_location=.5, label_fill="white")

edges[52,"curvature"]=85
#edges[52,"color"]="blue"
edges[61,"curvature"]=85
#edges[61,"color"]="blue"
edges[66,"curvature"]=85
#edges[66,"color"]="blue"
edges[52, "label_hjust"]=0
edges[61, "label_hjust"]=0
edges[66, "label_hjust"]=0



edges[54,"curvature"]=85
edges[54, "label_hjust"]=0

edges[63,"curvature"]=85
edges[63, "label_hjust"]=0


edges[53,"curvature"]=80
edges[53,"color"]=intercept_color
edges[53, "label_fill"]=intercept_background
edges[55,"curvature"]=82
edges[55,"label_location"]=.55

edges[55,"color"]=intercept_color
edges[55, "label_fill"]=intercept_background

edges[62,"curvature"]=80
edges[62,"color"]=intercept_color
edges[62, "label_fill"]=intercept_background

edges[53, "label_location"]=.6
edges[53, "label_hjust"]=0.4



edges[58,"curvature"]=80
edges[58,"color"]=slope_color
edges[58, "label_fill"]=slope_background

edges[60,"curvature"]=82
edges[60,"color"]=slope_color
edges[60, "label_fill"]=slope_background
edges[60,"label_location"]=.45
edges[65,"curvature"]=80
edges[65,"color"]=slope_color
edges[65, "label_fill"]=slope_background
edges[65, "label_location"]=.4
edges[53, "label_hjust"]=0.4



edges[59,"curvature"]=77

edges[64,"curvature"]=75
edges[56,"curvature"]=75
edges[57,"curvature"]=75


source(here("helper/modified_sem_plot.R"))
#View(edges)
prepare_graph(edges=edges, nodes=nodes, text_size = 2, layout = t(layout),angle=0,
          rect_width=1.5,
          rect_height=2,
          ellipses_width=1.5,
          ellipses_height=3,
          variance_width= .5,
          variance_height=1.3,
          var_arrow_angle=10,
          var_arrow_length=.05,
          arrow_angle=15,
          arrow_length=.07,
          variance_diameter=1,
          spacing_y=2.5,
          spacing_x=1.5,
          fix_coord=FALSE) |> 
  edit_edges({connect_from=case_when(
    (to==from & from %in% c("accuracy_intercept","accuracy_slope","speed_slope",
                          "speed_intercept", "vocab_slope", "vocab_intercept"))~"bottom",
    to==from ~ "right",
    !is.na(curvature)~"left",
    T ~ "right")}) |> 
  plot_happy_arrows()
