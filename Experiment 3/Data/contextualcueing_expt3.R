setwd('Z:/Contextual_Cueing/Experiment 3/Data')

library(tidyverse)
library(ez)
library(broom)
library(BayesFactor)

## Functions


every_nth <- function(x, nth, empty = TRUE, inverse = FALSE) 
{
  if (!inverse) {
    if(empty) {
      x[1:nth == 1] <- ""
      x
    } else {
      x[1:nth != 1]
    }
  } else {
    if(empty) {
      x[1:nth != 1] <- ""
      x
    } else {
      x[1:nth == 1]
    }
  }
}

all_files = list.files(pattern=".csv")
recognition_files = list.files(pattern="recognition.csv")
search_files = setdiff(all_files, c(recognition_files, "question2_resp.csv"))

search_data <- do.call(rbind, lapply(search_files, read.csv, header=TRUE))
recognition_data <- do.call(rbind, lapply(recognition_files, read.csv, header = TRUE))

q2_data <- read.csv("question2_resp.csv") %>%
  rename(sub_id_2 = "Var1",
         q2_resp = "Var2")


# we collected 1 extra participant, but we only need 20
# exclude last participant (28)

descrip <- search_data %>%
  filter(sub_id != 28) %>%
  mutate(hit = ifelse(Error == 0, 1, 0)) %>%
  group_by(sub_id) %>%
  summarise(accuracy = mean(hit)) 


individual_mean_rts <- search_data %>%
  filter(sub_id != 28) %>%
  filter(Error == 0) %>%
  group_by(sub_id) %>%
  summarise(meanRT = mean(RT)) 

group_sd <- sd(individual_mean_rts$meanRT)
group_mean <- mean(individual_mean_rts$meanRT)


bad_subs_accuracy <- (descrip %>%
               filter(accuracy<.9))$sub_id

bad_subs_rt <- (individual_mean_rts %>%
  mutate(upper_sd = group_mean + (2.5*group_sd),
         lower_sd = group_mean - (2.5*group_sd)) %>%
  mutate(exclude = if_else(meanRT < lower_sd | meanRT > upper_sd, 1, 0)) %>%
  filter(exclude == 1))$sub_id


## again, with subject 19 excluded because RT was beyond 2.5 SDs.
## now with subject 28 included

descrip <- search_data %>%
  filter(sub_id != bad_subs_rt) %>%
  mutate(hit = ifelse(Error == 0, 1, 0)) %>%
  group_by(sub_id) %>%
  summarise(accuracy = mean(hit)) 


individual_mean_rts <- search_data %>%
  filter(sub_id != bad_subs_rt) %>%
  filter(Error == 0) %>%
  group_by(sub_id) %>%
  summarise(meanRT = mean(RT)) 

group_sd <- sd(individual_mean_rts$meanRT)
group_mean <- mean(individual_mean_rts$meanRT)

bad_subs_rt <- (individual_mean_rts %>%
  mutate(upper_sd = group_mean + (2.5*group_sd),
         lower_sd = group_mean - (2.5*group_sd)) %>%
  mutate(exclude = if_else(meanRT < lower_sd | meanRT > upper_sd, 1, 0)) %>%
    filter(exclude == 1))$sub_id

bad_subs <- c(19, bad_subs_rt)

n_subs <- nrow(q2_data) - length(bad_subs)


#### MAIN ####

clean_data <- search_data %>%
  filter(!is.element(sub_id, bad_subs)) %>%
  filter(Error == 0) %>%
  group_by(repeat., d_setsize, sub_id) %>%
  mutate(upperlimit = mean(RT) + 2.5*sd(RT), lowerlimit = mean(RT) - 2.5*sd(RT)) %>%
  mutate(outlier = RT > upperlimit | RT < lowerlimit) %>%
  mutate(display = if_else(repeat. == 0, 'new', 'old')) %>%
  filter(outlier == FALSE) %>%
  mutate(epoch = ceiling(block/5))


clean_df <- clean_data %>%
  group_by(display, d_setsize, epoch, sub_id) %>%
  summarise(rt = mean(RT)) 

clean_df$display <- as.factor(clean_df$display)
clean_df$epoch <- as.factor(clean_df$epoch)


plot_df <- clean_df %>%
  group_by(display, d_setsize) %>%
  summarise(mean_rt = mean(rt), sem = sd(rt/sqrt(20)))


## setsizes are: 0, 4, 16, 26

ggplot(plot_df, aes(x=d_setsize, y=mean_rt, linetype = display, group=display)) + 
  geom_point(size=3, aes(shape = plot_df$display)) +
  stat_smooth(method="lm", formula=y~log(x+1), se=FALSE, color='#13294b', size = 2) +
  geom_errorbar(aes(ymin=mean_rt-sem, ymax=mean_rt+sem), width=2, size=1) +
  xlab("Lure set size") +
  ylab("RT") + coord_cartesian(ylim=c(460, 660)) +
  scale_y_continuous(breaks=seq(460, 660,10),
                     labels = every_nth(seq(460, 660,10), 5, inverse=TRUE)) +
  theme_bw() + 
  theme(panel.border = element_rect(linetype = 'solid', colour = 'grey', size=1.2),
        text = element_text(size=28)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position="none",
        axis.text=element_text(size=28)) +
  theme(panel.border = element_blank())
  

plot_df_epoch <- clean_df %>%
  group_by(epoch, display) %>%
  summarise(mean_rt = mean(rt), sem = sd(rt/sqrt(20)))

ggplot(plot_df_epoch, aes(x=epoch, y=mean_rt, linetype = display, shape=display, group=display)) + 
  geom_point(size=4, color="#E84A27") +
  geom_line(size=2, color="#E84A27") +
  # stat_smooth(method="lm", formula=y~log(x+1), se=FALSE, color='#E84A27', size = 3.5) +
  geom_errorbar(aes(ymin=mean_rt-sem, ymax=mean_rt+sem), width=0.3, size=1, color="#E84A27") +
  # facet_wrap(~ sub_id) +
  xlab("Epoch") +
  ylab("RT") + 
  coord_cartesian(ylim=c(460, 660)) +
  # scale_color_manual(values=c('#13294b', '#000099', '#00004C', '#000033', "000000")) +
  # scale_y_continuous(breaks=number_ticks(6)) +
  scale_y_continuous(breaks=seq(460, 660,10),
                     labels = every_nth(seq(460, 660,10), 5, inverse=TRUE)) +
  theme_bw() + 
  theme(panel.border = element_rect(linetype = 'solid', colour = 'grey', size=1.2),
        text = element_text(size=28)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position="none",
        axis.text=element_text(size=28)) +
  theme(panel.border = element_blank())

clean_df$d_setsize <- as.factor(clean_df$d_setsize)




##### Stats #####

clean_df$epoch <- as.factor(clean_df$epoch)
clean_df$d_setsize <- as.factor(clean_df$d_setsize)

ezANOVA(clean_df,
        dv = rt,
        wid = sub_id,
        within = c(epoch, d_setsize, display))

anovaBF(rt ~  display, data = clean_df)


descriptive_stats <- clean_df %>%
  group_by(epoch) %>%
  summarise(meanRT = mean(rt), 
            SD = sd(rt))

#### Noticing ####

# subject 19 was excluded in the main analysis for RTs beyond 2.5 SD
# subject 24 was excluded for not completing the recognition test


noticing <- merge(recognition_data, q2_data) %>% 
  filter(sub_id_2 != 19, sub_id_2!= 24) %>%
  group_by(sub_id_2) %>%
  summarize(noticed = mean(q1),
            percentage = mean(q2_resp, na.rm=TRUE)) %>%
  summarize(noticed = mean(noticed),
            percentage = mean(percentage))



## Dprime analysis ##

func <- function(x, y){
  
  return(qnorm(x) - qnorm(y))
  
}


# subject 19 was excluded in the main analysis for RTs beyond 2.5 SD
# subject 24 was excluded for not completing the recognition test

recognition_dprime<- recognition_data %>%
  filter(sub_id_2 != 19 & sub_id_2 != 24) %>%
  mutate(hit_count = if_else(repeat. == 1 & Resp_recog ==1, 1, 0),
         fa_count = if_else(repeat. == 0 & Resp_recog == 1, 1, 0),
         cr_count = if_else( repeat. == 0 & Resp_recog == 0, 1, 0),
         miss_count = if_else(repeat. == 1 & Resp_recog == 0, 1, 0)) %>%
  group_by(sub_id_2, d_setsize) %>%
  summarise(hit = mean(hit_count),
            fa = mean(fa_count),
            cr = mean(cr_count),
            miss = mean(miss_count)) %>%
  mutate(hit = if_else(hit == 0, 1/24, if_else(hit == 1, 1 - (1/24), hit)),
         fa = if_else(fa == 0, 1/24, if_else(fa == 1, 1 - (1/24), fa)),
         cr = if_else(cr == 0, 1/24, if_else(cr == 1, 1 - (1/24), cr)),
         miss = if_else(miss == 0, 1/24, if_else(miss == 1, 1 - (1/24), miss))) %>%
  mutate(dprime = mapply(func, list(hit), list(fa)))



# t tests for each set size to determine whether d' was significantly different from 0
t.test((recognition_dprime %>%  filter(d_setsize == 0))$dprime, mu =0)
t.test((recognition_dprime %>%  filter(d_setsize == 3))$dprime, mu=0)
t.test((recognition_dprime %>%  filter(d_setsize == 9))$dprime, mu = 0)
t.test((recognition_dprime %>%  filter(d_setsize == 19))$dprime, mu = 0)
t.test((recognition_dprime %>%  filter(d_setsize == 31))$dprime, mu =0)


######################
# Display-specific ###
######################

# subject 19 was excluded in the main analysis for RTs beyond 2.5 SD
# subject 24 was excluded for not completing the recognition test


# run it with and without block == 1 for the two tables in the manuscript
recognition_df <- merge(recognition_data, q2_data) %>%
  # filter(block == 1) %>%
  group_by(tloc, sub_id_2, repeat., d_setsize) %>%
  filter(sub_id_2 != 19 & sub_id_2 != 24) %>%
  summarise(q1 = mean(q1),
            noticed = mean(q2_resp),
    accuracy = mean(Hit_recog),
    percentage = mean(q2),
    conf = mean(Confidence_recog))

recognition_df_descriptive <- merge(recognition_data, q2_data) %>%
  filter(sub_id_2 !=24 & sub_id_2 != 19) %>%
  group_by(repeat., d_setsize) %>%
  summarise(accuracy = mean(Hit_recog))

perfect <- recognition_df %>% filter(accuracy == 1 & repeat. == 1) %>%
  filter(d_setsize != 0)



## Get contextual cueing effect for each display

individual_novel_rts <- clean_data %>%
  group_by(display, d_setsize, sub_id) %>%
  summarise(new_rt = mean(RT)) %>%
  filter(sub_id != 24) %>%
  filter(display == "new") 


individual_repeated_rts <- clean_data %>%
  filter(sub_id!= 19 & sub_id != 24) %>%
  group_by(sub_id, display, tloc, d_setsize) %>%
  summarise(repeat_rt = mean(RT)) %>%
  filter(display == "old")  

individual_cc <- merge(individual_novel_rts, individual_repeated_rts, by=c("sub_id", "d_setsize")) %>%
  mutate(cc = (repeat_rt - new_rt))


# get the number of displays that were perfectly recognized 

individual_display_recognition <- merge(recognition_data, q2_data) %>%
  filter(sub_id_2 != 19 & sub_id_2 != 24) %>%
  filter(d_setsize != 0) %>%
  # 19 bad subject, 24 didn't complete recognition test
  group_by(sub_id_2, repeat., tloc, d_setsize) %>%
  summarise(accuracy = mean(Hit_recog), conf = mean(Confidence_recog)) %>%
  filter(repeat. == 1) %>%
  rename(sub_id = "sub_id_2") 
  # filter(accuracy == 1) %>%
  # group_by(sub_id) %>%
  # count(sub_id)

# put the two into one df
individual_cc_df <- merge(individual_cc, individual_display_recognition) %>%
  group_by(accuracy, sub_id, d_setsize) %>%
  summarise(cc = mean(cc))


# check whether recognition accuracy affects cc effect
individual_cc_df$d_setsize <- as.factor(individual_cc_df$d_setsize)
individual_cc_df$accuracy <- as.factor(individual_cc_df$accuracy)


(anovaBF(cc ~ accuracy, data = individual_cc_df))




