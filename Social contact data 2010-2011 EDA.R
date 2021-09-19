# library
#-------------
library(dplyr)
library(tidyverse)
library(sqldf)
library(reshape)
library(ggplot2)
library(hrbrthemes)

# setwd
#-------------
setwd("C:/Users/MY ASUS/Downloads/0. UHasselt/1. PhD/1. Work/3. Social contact data/0. Flanders 2010-2011")

# input data
#-------------
cont_comm <- read.csv("2010_Willem_BELGIUM_contact_common.csv")
cont_ext <- read.csv("2010_Willem_BELGIUM_contact_extra.csv")
part_comm <- read.csv("2010_Willem_BELGIUM_participant_common.csv")
part_ext <- read.csv("2010_Willem_BELGIUM_participant_extra.csv")
sday <- read.csv("2010_Willem_BELGIUM_sday.csv")

# 1 = Yes, 2 = No of physical contact
# Yes and No of hh member
table(cont_ext$cnt_hh_member, cont_comm$phys_contact)

# join contact common and contact extra dataset
contact <- cont_comm %>% inner_join(cont_ext)

# 1
#------------
# Figure 1(a) - not correct
contact_hh <- contact %>% 
  group_by(cnt_hh_member, phys_contact) %>% 
  summarise(n = n()) %>%
  drop_na() %>% 
  mutate(sum = sum(n, na.rm = T),
         phys_contact = ifelse(phys_contact == "1", "Yes", "No"),
         cnt_hh_member = ifelse(cnt_hh_member == "N", "non HH members", "HH members"))

contact_hh$phys_contact <- as.factor(contact_hh$phys_contact)
contact_hh$phys_contact <- factor(contact_hh$phys_contact,
                                  levels = c("Yes", "No"))

ggplot(contact_hh, aes(x = cnt_hh_member, y = n, fill = phys_contact)) +
  geom_col(position = "fill") +
  labs(fill = "Physical contact", x = "", y = "") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2))

# Figure 1(b) - duration
contact_sday <- contact %>% inner_join(sday)

contact_freq <- contact_sday %>% 
  group_by(phys_contact, frequency_multi) %>% 
  summarise(n = n()) %>% 
  drop_na() %>% 
  mutate(phys_contact = ifelse(phys_contact == "1", "Yes", "No"),
         frequency_multi = ifelse(frequency_multi == "1", "daily", 
                          ifelse(frequency_multi == "2", "weekly",
                          ifelse(frequency_multi == "3", "monthly",
                          ifelse(frequency_multi == "4", "few times", "first time")))))
         
contact_freq$phys_contact <- as.factor(contact_freq$phys_contact)
contact_freq$phys_contact <- factor(contact_freq$phys_contact,
                                  levels = c("Yes", "No"))
contact_freq$frequency_multi <- factor(contact_freq$frequency_multi,
                                  levels = c("daily", "monthly", "weekly",
                                             "first time", "few times"))

ggplot(contact_freq, aes(x = frequency_multi, y = n, fill = phys_contact)) +
  geom_col(position = "fill") +
  labs(fill = "Physical contact", x = "", y = "") + 
  scale_y_continuous(breaks = seq(0, 1, by = 0.2))

# Figure 1(c) - duration multi - good
contact_multi <- contact_sday %>% 
  group_by(duration_multi, phys_contact) %>% 
  summarise(n = n()) %>% 
  drop_na() %>% 
  mutate(phys_contact = ifelse(phys_contact == "1", "Yes", "No"),
         duration_multi = ifelse(duration_multi == "1", "<5 min", 
                           ifelse(duration_multi == "2", "5-15 min",
                           ifelse(duration_multi == "3", "15 min-1hour",
                           ifelse(duration_multi == "4", "1-4hours", ">=4 hours")))))

contact_multi$phys_contact <- as.factor(contact_multi$phys_contact)
contact_multi$phys_contact <- factor(contact_multi$phys_contact,
                                    levels = c("Yes", "No"))
contact_multi$duration_multi <- factor(contact_multi$duration_multi,
                                       levels = c("<5 min", "5-15 min", "15 min-1hour",
                                                  "1-4hours", ">=4 hours"))

ggplot(contact_multi, aes(x = duration_multi, y = n, fill = phys_contact)) +
  geom_col(position = "fill") +
  labs(fill = "Physical contact", x = "", y = "") + 
  scale_y_continuous(breaks = seq(0, 1, by = 0.2))


# Figure 1(d) - location - good
count_home <- contact_sday %>% 
  group_by(phys_contact, cnt_home) %>%
  count(cnt_home) %>% 
  drop_na() %>% 
  filter(cnt_home == 1)

count_work <- contact_sday %>% 
  group_by(phys_contact, cnt_work) %>%
  count(cnt_work) %>% 
  drop_na() %>% 
  filter(cnt_work == 1)

count_leis <- contact_sday %>% 
  group_by(phys_contact, cnt_leisure) %>%
  count(cnt_leisure) %>% 
  drop_na() %>% 
  filter(cnt_leisure == 1) 

count_sch <- contact_sday %>% 
  group_by(phys_contact, cnt_school) %>%
  count(cnt_school) %>% 
  drop_na() %>% 
  filter(cnt_school == 1)

count_trans <- contact_sday %>% 
  group_by(phys_contact, cnt_transport) %>%
  count(cnt_transport) %>% 
  drop_na() %>% 
  filter(cnt_transport == 1)

count_oth <- contact_sday %>%
  group_by(phys_contact, cnt_otherplace) %>%
  count(cnt_otherplace) %>% 
  drop_na() %>% 
  filter(cnt_otherplace == 1)

# if more than one column is True, than count.
count_multi <- contact_sday %>%
  select(phys_contact, cnt_home, cnt_school, cnt_leisure, cnt_work,
         cnt_otherplace, cnt_transport) %>% 
  rowwise() %>% 
  mutate(sum = sum(c(cnt_home, cnt_school, cnt_leisure, cnt_work,
                        cnt_otherplace, cnt_transport))) %>% 
  filter(sum > 1) %>% 
  group_by(phys_contact) %>% count() %>%
  add_column(cnt_multi = "TRUE",
             .after = "phys_contact") %>% drop_na()
  

count_all <- rbind(count_home, count_leis, count_oth,
                   count_sch, count_trans, count_work,
                   count_multi) %>% as.data.frame()

count_all <- melt(count_all, id.vars = c("n", "phys_contact"),
             measure.vars = c("cnt_home", "cnt_leisure", 
                              "cnt_otherplace", "cnt_school", 
                              "cnt_transport", "cnt_work",
                              "cnt_multi")) %>% drop_na() %>% 
              mutate(phys_contact = ifelse(phys_contact == "1", "Yes", "No"))

count_all$phys_contact <- as.factor(count_all$phys_contact)
count_all$phys_contact <- factor(count_all$phys_contact,
                                     levels = c("Yes", "No"))
count_all$variable <- factor(count_all$variable,
                                       levels = c("cnt_home", "cnt_school", "cnt_work",
                                                  "cnt_transport", "cnt_leisure", "cnt_otherplace",
                                                  "cnt_multi"))

ggplot(count_all, aes(x = variable, y = n, fill = phys_contact)) +
  geom_col(position = "fill") +
  labs(fill = "Physical contact", x = "", y = "") + 
  scale_y_continuous(breaks = seq(0, 1, by = 0.2))

#------------
# 2
#------------

# age of contact = cnt_age_exact
# age of respondent = part_age 
contact_part <- part_comm %>% inner_join(cont_comm) %>% 
  select(part_id, part_age, part_gender, cont_id, cnt_age_exact,
         cnt_age_est_min, cnt_age_est_max, cnt_gender) %>% 
  mutate(cnt_age = ifelse(is.na(cnt_age_exact) == T, (cnt_age_est_min + cnt_age_est_max)/2, 
                           cnt_age_exact)) %>% 
  select(cont_id, part_id, part_age, part_gender, cont_id, cnt_gender, cnt_age) %>% drop_na()

# create binning for the age(s)
breaks <- seq(0, 100, by = 5) -1
tags <- c("[0,5)", "[5,10)", "[10,15)", "[15,20)", "[20,25)",
          "[25,30)", "[30,35)", "[35,40)", "[40,45)", "[45,50)",
          "[50,55)", "[55,60)", "[60,65)", "[65,70)", "[70,75)",
          "[75,80)", "[80,85)", "[85,90)", "[90,95)", "[95,100)")

contact_part$part_agec <- cut(contact_part$part_age, breaks = breaks,
                              labels = tags)
contact_part$cnt_agec <- cut(contact_part$cnt_age, breaks = breaks,
                              labels = tags)


contact_part[is.na(contact_part$cnt_agec),]

# contact_part_contact_respondent
# age of contact = male, age of respondent = male
# (a) contact_part_male_male - done

contact_part_m_m <- contact_part %>% 
  filter(part_gender == "M", 
         cnt_gender == "M") %>% 
  group_by(cnt_agec, part_agec) %>% 
  count() %>% drop_na()

ggplot(contact_part_m_m, aes(part_agec, cnt_agec)) + 
  geom_tile(aes(fill = n)) + 
  labs(fill = "# of contacts", x = "age of respondent", y = "age of contacts") +
  scale_fill_gradient(low="white", high="red") +
  theme_ipsum() + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


# age of contact = male, age of respondent = female
# (b) contact_part_m_f - done

contact_part_f_m <- contact_part %>% 
  filter(part_gender == "F", 
         cnt_gender == "M") %>% 
  group_by(cnt_agec, part_agec) %>% 
  count() %>% drop_na()

ggplot(contact_part_f_m, aes(part_agec, cnt_agec)) + 
  geom_tile(aes(fill = n)) + 
  labs(fill = "# of contacts", x = "age of respondent", y = "age of contacts") +
  scale_fill_gradient(low="white", high="red") +
  theme_ipsum() + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

# age of contact = female, age of respondent = male
# (c) contact_part_f_m - done

contact_part_m_f <- contact_part %>% 
  filter(part_gender == "M", 
         cnt_gender == "F") %>% 
  group_by(cnt_agec, part_agec) %>% 
  count() %>% drop_na()

ggplot(contact_part_m_f, aes(part_agec, cnt_agec)) + 
  geom_tile(aes(fill = n)) + 
  labs(fill = "# of contacts", x = "age of respondent", y = "age of contacts") +
  scale_fill_gradient(low="white", high="red") +
  theme_ipsum() + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

# age of contact = female, age of respondent = female
# (d) contact_part_f_f - done

contact_part_f_f <- contact_part %>% 
  filter(part_gender == "F", 
         cnt_gender == "F") %>% 
  group_by(cnt_agec, part_agec) %>% 
  count() %>% drop_na()

ggplot(contact_part_f_f, aes(part_agec, cnt_agec)) + 
  geom_tile(aes(fill = n)) + 
  labs(fill = "# of contacts", x = "age of respondent", y = "age of contacts") +
  scale_fill_gradient(low="white", high="red") +
  theme_ipsum() + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


#------------
# 3
#------------

contact_s_part <- contact_part %>% inner_join(sday) %>% 
  select(cont_id, part_id, part_agec, cnt_agec, dayofweek, holiday) %>% 
  mutate(holiday = factor(ifelse(holiday == "1", "Yes", "No")),
         dayofweek = factor(
                      ifelse(dayofweek == "0", "Sunday",
                      ifelse(dayofweek == "1", "Monday",
                      ifelse(dayofweek == "2", "Tuesday",
                      ifelse(dayofweek == "3", "Wednesday",
                      ifelse(dayofweek == "4", "Thursday",
                      ifelse(dayofweek == "5", "Friday", "Saturday")))))))
         ) %>% drop_na()

contact_s_part[is.na(contact_s_part$part_agec),]

# weekday
contact_s_part_wd <- contact_s_part %>% 
  filter(dayofweek != "Sunday" & dayofweek != "Saturday") %>% 
  group_by(cnt_agec, part_agec) %>% 
  count() %>% drop_na()

ggplot(contact_s_part_wd, aes(part_agec, cnt_agec)) + 
  geom_tile(aes(fill = n)) + 
  labs(fill = "# of contacts", x = "age of respondent", y = "age of contacts") +
  scale_fill_gradient(low="white", high="red") +
  theme_ipsum() + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))    

# weekend
contact_s_part_we <- contact_s_part %>% 
  filter(dayofweek == "Sunday" | dayofweek == "Saturday")  %>% 
  group_by(cnt_agec, part_agec) %>% 
  count() %>% drop_na()

ggplot(contact_s_part_we, aes(part_agec, cnt_agec)) + 
  geom_tile(aes(fill = n)) + 
  labs(fill = "# of contacts", x = "age of respondent", y = "age of contacts") +
  scale_fill_gradient(low="white", high="red") +
  theme_ipsum() + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

# holiday weekday
contact_s_part_hwd <- contact_s_part %>% 
  filter(dayofweek != "Sunday" & dayofweek != "Saturday" & holiday == "Yes") %>% 
  group_by(cnt_agec, part_agec) %>% 
  count() %>% drop_na()

ggplot(contact_s_part_hwd, aes(part_agec, cnt_agec)) + 
  geom_tile(aes(fill = n)) + 
  labs(fill = "# of contacts", x = "age of respondent", y = "age of contacts") +
  scale_fill_gradient(low="white", high="red") +
  theme_ipsum() + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 
