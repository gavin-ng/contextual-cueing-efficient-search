setwd('Z:/Contextual_Cueing/Experiment 1/Data')


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
all_data$q2 <- as.numeric(all_data$q2)

descrip <- all_data %>%
  mutate(hit = ifelse(Error == 0, 1, 0)) %>%
  group_by(sub_id) %>%
  summarise(accuracy = mean(hit))

bad_subs <- (descrip %>%
               filter(accuracy < .9))$sub_id

n_subs <- length(all_files) - length(bad_subs)

##### MAIN #####

clean_data <- all_data %>%
  filter(!is.element(sub_id, bad_subs)) %>%
  filter(Error == 0) %>%
  group_by(repeat., d_setsize, sub_id) %>%
  mutate(upperlimit = mean(RT) + 2.5*sd(RT), lowerlimit = mean(RT) - 2.5*sd(RT)) %>%
  mutate(outlier = RT > upperlimit | RT < lowerlimit) %>%
  filter(outlier == FALSE) %>%
  mutate(epoch = ceiling(block/5)) 

clean_df <- clean_data %>%
  group_by(repeat., d_setsize, epoch, sub_id) %>%
  summarise(rt = mean(RT), q1 = mean(q1), q2 = mean(q2)) %>%
  mutate(display = if_else(repeat. ==0, 'new', 'old'))

clean_df$display <- as.factor(clean_df$display)
clean_df$epoch <- as.factor(clean_df$epoch)


### Overall plot

plot_df <- clean_df %>%
  group_by(epoch, d_setsize, display) %>%
  summarise(mean_rt = mean(rt), sem = sd(rt/sqrt(n_subs)))

plot_df$epoch <- as.factor(plot_df$epoch)
plot_df$display <- as.factor(plot_df$display)
plot_df$d_setsize <- as.factor(plot_df$d_setsize)

## setsizes are: 0, 4, 16, 26

ggplot(plot_df, aes(x=epoch, y=mean_rt, linetype = display, group=interaction(d_setsize, display), color=d_setsize)) + 
  geom_point(data=plot_df, aes(color=d_setsize, shape = d_setsize), size=4) +
  geom_errorbar(aes(ymin=mean_rt-sem, ymax=mean_rt+sem), size=1, width=.3) +
  
  geom_line( size=1) +
  xlab("Epoch") +
  ylab("RT") +
  coord_cartesian(ylim=c(650,1050))+
  scale_color_manual(values=c('#13294b', '#E84A27'), labels = c("4", "8")) + 
  scale_y_continuous(breaks=seq(650,1050,10),
                     labels = every_nth(seq(650,1050,10), 5, inverse=TRUE)) +
  theme_bw() + 
  theme(panel.border = element_rect(linetype = 'solid', colour = 'grey', size=1.2),
        text = element_text(size=18)) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position="bottom",
        axis.text=element_text(size=18)) +
  theme(legend.position = "none")


## Statistical analyses

clean_df$d_setsize <- as.factor(clean_df$d_setsize)

anovaBF(rt ~ display * d_setsize *  epoch, data =clean_df)


ezANOVA(clean_df,
        dv = rt,
        wid = sub_id,
        within = .(d_setsize, display, epoch))



# change variable to get different descriptive stats
descriptive_stats <- clean_df %>%
  group_by(epoch) %>%
  summarise(meanRT = mean(rt), 
            SD = sd(rt))



#### Noticing ####

percent_noticed <- mean(clean_df$q1)



noticing <- clean_df %>% 
  group_by(sub_id) %>%
  summarize(noticed = mean(q1),
            percentage = mean(q2, na.rm=TRUE)) %>%
  summarize(noticed = mean(noticed),
            percentage = mean(percentage))



