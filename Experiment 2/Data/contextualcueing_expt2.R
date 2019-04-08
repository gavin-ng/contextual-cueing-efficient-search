setwd('Z:/Contextual_Cueing/Experiment 2/Data')

######## LIBRARIES ########

library(tidyverse)
library(ez)
library(broom)
library(BayesFactor)

####### FUNCTIONS ########


### to make custom number of ticks on axes
number_ticks <- function(n) {function(limits) pretty(limits, n)}

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


######## MAIN ########

all_files = list.files(pattern=".csv")

all_data <- do.call(rbind, lapply(all_files, read.csv, header=TRUE))

all_data$q1 <- as.numeric(all_data$q1)
all_data$q2 <- as.numeric(as.character(all_data$q2))

descrip <- all_data %>%
  mutate(hit = ifelse(Error == 0, 1, 0)) %>%
  group_by(sub_id) %>%
  summarise(accuracy = mean(hit))

bad_subs <- (descrip %>%
               filter(accuracy < .9))$sub_id

n_subs <- nrow(descrip) - length(bad_subs)

clean_data <- all_data %>%
  filter(!is.element(sub_id, bad_subs)) %>%
  filter(Error == 0) %>%
  group_by(repeat., d_setsize, sub_id) %>%
  mutate(upperlimit = mean(RT) + 2.5*sd(RT), lowerlimit = mean(RT) - 2.5*sd(RT)) %>%
  mutate(outlier = RT > upperlimit | RT < lowerlimit) %>%
  filter(outlier == FALSE) %>%
  mutate(epoch = ceiling(block/5)) 

clean_df <- clean_data %>%
  mutate(display = if_else(repeat. ==0, 'new', 'old')) %>%
  group_by(display, d_setsize, epoch, sub_id) %>%
  summarise(rt = mean(RT), q1 = mean(q1), q2 = mean(q2)) 

clean_df$display <- as.factor(clean_df$display)
clean_df$epoch <- as.factor(clean_df$epoch)

##### PLOT #####

overall_plot <- clean_df %>%
  group_by(display, epoch) %>%
  summarise(mean_rt = mean(rt), sem = sd(rt)/sqrt(20))

overall_plot$epoch <- as.factor(overall_plot$epoch)

## setsizes: 0, 4, 16, 26

ggplot(overall_plot , aes(x=epoch, y=mean_rt, linetype = display, group=display)) + 
  geom_point(size=5, color = '#E84A27') +
  geom_line(size=3, color = '#E84A27') +
  geom_errorbar(aes(ymin=mean_rt-sem, ymax=mean_rt+sem), width=.3, size=1, color = '#E84A27') +
  coord_cartesian(ylim=c(850, 1100)) +
  xlab("Epoch") +
  ylab("RT") +  
  scale_y_continuous(breaks=seq(850,1100,10),
                     labels = every_nth(seq(850,1100,10), 5, inverse=TRUE)) +
  theme_bw() + 
  theme(panel.border = element_rect(linetype = 'solid', colour = 'grey', size=1.2),
        text = element_text(size=28)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position="none",
        axis.text=element_text(size=28)) + 
  theme(panel.border = element_blank())


lure_plot <- clean_df %>%
  group_by(display, d_setsize) %>%
  summarise(mean_rt = mean(rt), sem = sd(rt)/sqrt(n_subs))


ggplot(lure_plot , aes(x=d_setsize, y=mean_rt, linetype = display, group=display)) + 
  geom_point(size=5) +
  stat_smooth(method="lm", formula=y~log(x+1), se=FALSE, size = 3.5, color='#13294B') +
  geom_errorbar(aes(ymin=mean_rt-sem, ymax=mean_rt+sem), width=1, size=1) +
  xlab("Lure setsize") +
  ylab("RT") +  
  coord_cartesian(ylim=c(850,1100)) +
  scale_y_continuous(breaks=seq(850,1100,10),
                     labels = every_nth(seq(850,1100,10), 5, inverse=TRUE)) +
  theme_bw() + 
  theme(panel.border = element_rect(linetype = 'solid', colour = 'grey', size=1.2),
        text = element_text(size=28)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position="none",
        axis.text=element_text(size=28)) +
  theme(panel.border = element_blank())


##### Stats #####

clean_df$epoch <- as.factor(clean_df$epoch)
clean_df$d_setsize <- as.factor(clean_df$d_setsize)

ezANOVA(clean_df,
        dv = rt,
        wid = sub_id,
        within = c(epoch, d_setsize, display))

descriptive_stats <- clean_df %>%
  spread(display, rt) %>%
  mutate(cc_effect = old - new) %>%
  group_by(epoch) %>%
  summarise(meanRT = mean(cc_effect),
            SD = sd(cc_effect))



## Follow-up t-tests for epoch x display interaction

all_ps <- list()
all_ts <- list()

for(i in 1:5){

  
  test<- t.test((clean_df %>% filter(epoch == i & display == "old"))$rt,
         (clean_df %>% filter(epoch == i & display == "new"))$rt, paired = TRUE)
  
  all_ps <- c(all_ps, test$p.value)
  all_ts <- c(all_ts, test$statistic)
  
  
  
  
}

#### Slopes ####

clean_df$d_setsize <- as.numeric(as.character(clean_df$d_setsize))

old_slopes <- clean_df %>%
  group_by(d_setsize, display, sub_id) %>%
  filter(display == "old") %>%
  summarise(meanRT = mean(rt)) %>%
  group_by(sub_id) %>%
  do(log_slope = lm(meanRT ~ log(d_setsize+1), data =.)) %>%
  tidy(log_slope) %>%
  filter(term == "log(d_setsize + 1)") %>%
  mutate(display = "old")

new_slopes <- clean_df %>%
  group_by(d_setsize, display, sub_id) %>%
  filter(display == "new") %>%
  summarise(meanRT = mean(rt)) %>%
  group_by(sub_id) %>%
  do(log_slope = lm(meanRT ~ log(d_setsize+1), data =.)) %>%
  tidy(log_slope) %>%
  filter(term == "log(d_setsize + 1)") %>%
  mutate(display = "new")
  

all_slopes <- rbind(old_slopes, new_slopes)

ezANOVA(all_slopes,
        wid = sub_id,
        dv = estimate,
        within = display)

# slopes not statistically significant
# look at Bayes Factors to provide support for null hypothesis

all_slopes$display <- as.factor(all_slopes$display)

anovaBF(estimate ~ display, data = all_slopes)


##### Noticing #####

noticing <- clean_df %>% 
  group_by(sub_id) %>%
  summarize(noticed = mean(q1),
            percentage = mean(q2, na.rm=TRUE)) %>%
  summarise(noticed = mean(noticed),
            percentage = mean(percentage, na.rm=TRUE))




