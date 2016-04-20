# Arsenic change data for fake midline data for training exercise
# "Original" data from http://www.bgs.ac.uk/research/groundwater/health/arsenic/Bangladesh/data.html (1998)
# "New" data from the 2012/2013 MICS

CI = 1.96 # 95% confidence interval

# Pull in original data ---------------------------------------------------

baseline = read.csv('~/Documents/USAID/Bangladesh/Training/BGD_NationalSurveyData_Arsenic.csv')


ggplot(baseline, aes(x = WELL_DEPTH, y = `As_ug.l`)) +
  geom_point() +
  theme_xygrid()


# Check no NAs in [As] ----------------------------------------------------
summary(baseline$As_ug.l)


# Calculate [As] > threshold ----------------------------------------------

baseline = baseline %>% 
  mutate(above50ppb = ifelse(As_ug.l > 50, 1, 0),
         above10ppb = ifelse(As_ug.l > 10, 1, 0))



# Collapse original data to the Adm1 level --------------------------------
baseline_Adm1 = baseline %>% 
  group_by(DIVISION) %>% 
  summarise(num = n(),
            meanAs50ppb = mean(above50ppb),
            std50ppb = sd(above50ppb),
            se50ppb = CI * std50ppb/sqrt(num),
            lb50ppb = meanAs50ppb - se50ppb,
            ub50ppb = meanAs50ppb + se50ppb,
            meanAs10ppb = mean(above10ppb),
            std10ppb = sd(above10ppb),
            se10ppb = CI * std10ppb/sqrt(num),
            lb10ppb = meanAs10ppb - se10ppb,
            ub10ppb = meanAs10ppb + se10ppb)

# Create fake Rangpur data, since Rangpur didn't exist in 1998
baselineRangpur = baseline_Adm1 %>% 
  filter(DIVISION == 'Rajshahi') %>% 
  mutate(DIVISION = 'Rangpur')

baseline_Adm1 = rbind(baseline_Adm1, baselineRangpur)

# Collapse original data for the Adm2 level -------------------------------
baseline_Adm2 = baseline %>% 
  group_by(DIVISION, DISTRICT) %>% 
  summarise(num = n(),
            meanAs50ppb = mean(above50ppb),
            std50ppb = sd(above50ppb),
            se50ppb = CI * std50ppb/sqrt(num),
            lb50ppb = meanAs50ppb - se50ppb,
            ub50ppb = meanAs50ppb + se50ppb,
            meanAs10ppb = mean(above10ppb),
            std10ppb = sd(above10ppb),
            se10ppb = CI * std10ppb/sqrt(num),
            lb10ppb = meanAs10ppb - se10ppb,
            ub10ppb = meanAs10ppb + se10ppb)


# Pull in "midline" data --------------------------------------------------------
source('~/Documents/USAID/Bangladesh/Training/BGD_MICS.R')



# Collapse midline data to Adm1 -------------------------------------------

midline_Adm1 = hh %>% 
  rename(As = WQ9) %>% 
  filter(As < 9999, !is.na(As)) %>% 
  mutate(above50ppb = ifelse(As > 50, 1, 0),
         above10ppb = ifelse(As > 10, 1, 0)) %>% 
  group_by(div) %>% 
  summarise(num = n(),
            meanAs50ppb = mean(above50ppb),
            std50ppb = sd(above50ppb),
            se50ppb = CI * std50ppb/sqrt(num),
            lb50ppb = meanAs50ppb - se50ppb,
            ub50ppb = meanAs50ppb + se50ppb,
            meanAs10ppb = mean(above10ppb),
            std10ppb = sd(above10ppb),
            se10ppb = CI * std10ppb/sqrt(num),
            lb10ppb = meanAs10ppb - se10ppb,
            ub10ppb = meanAs10ppb + se10ppb)


# Collapse midline data to Adm2 -------------------------------------------

midline_Adm2 = hh %>% 
  rename(As = WQ9) %>% 
  filter(As < 9999, !is.na(As)) %>% 
  mutate(above50ppb = ifelse(As > 50, 1, 0),
         above10ppb = ifelse(As > 10, 1, 0)) %>% 
  group_by(div, district) %>% 
  summarise(num = n(),
            meanAs50ppb = mean(above50ppb),
            std50ppb = sd(above50ppb),
            se50ppb = CI * std50ppb/sqrt(num),
            lb50ppb = meanAs50ppb - se50ppb,
            ub50ppb = meanAs50ppb + se50ppb,
            meanAs10ppb = mean(above10ppb),
            std10ppb = sd(above10ppb),
            se10ppb = CI * std10ppb/sqrt(num),
            lb10ppb = meanAs10ppb - se10ppb,
            ub10ppb = meanAs10ppb + se10ppb)


# Merge baseline and midline together -------------------------------------
adm1 = full_join(baseline_Adm1, midline_Adm1, 
                 by = c("DIVISION" = "div"))

ggplot(adm1, aes(xend =  1998, x = 2012, yend = meanAs50ppb.x, y =   meanAs50ppb.y,
                 colour = DIVISION, fill = DIVISION, label = DIVISION)) +
  geom_segment(data = adm1 %>% filter(!DIVISION %in% c('Rangpur', 'Rajshahi'))) +
  geom_segment(linetype = 2,
               colour = '#B983FF',
               data = adm1 %>% filter(DIVISION %in% c('Rangpur', 'Rajshahi'))) +
  geom_point(size = 3,             
             shape = 21,
             colour= grey90K) +
  geom_segment(aes(x = 1998, xend = 1998, y = lb50ppb.x, yend = ub50ppb.x),
               alpha = 0.4, size = 1.5) +
  geom_segment(aes(x = 2012, xend = 2012, y = lb50ppb.y, yend = ub50ppb.y),
               alpha = 0.4, size = 1.5) +
  geom_point(aes(y = meanAs50ppb.x, x = 1998), 
             colour= grey90K,
             shape = 21,
             size = 3) +
  geom_label(nudge_x = 1, label.size = 0, fill = 'white',
             family = 'Segoe UI Semilight', size = 4.5) + 
  theme_ygrid() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(limits = c(1998, 2014),
                     breaks = c(1998, 2012))

# Merge baseline and midline together -------------------------------------
adm2 = full_join(baseline_Adm2, midline_Adm2, 
                 by = c("DIVISION" = "div", "DISTRICT" = "district"))

ggplot(adm2 %>% filter(DIVISION %in% c('Khulna', 'Sylhet', 'Chittagong')), 
       aes(x =  1998, xend = 2012, y = meanAs50ppb.x, yend =   meanAs50ppb.y,
           colour = DISTRICT, label = DISTRICT)) +
  geom_segment() +
  geom_point(size = 3) +
  geom_point(aes(y = meanAs50ppb.y, x = 2012), size = 3) +
  geom_text(nudge_x = -1, 
            family = 'Segoe UI Semilight', size = 4.5) + 
  theme_ygrid() +
  facet_wrap(~DIVISION) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(limits = c(1996, 2012),
                     breaks = c(1998, 2012))
