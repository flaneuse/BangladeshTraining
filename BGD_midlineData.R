# Arsenic change data for fake midline data for training exercise
# "Original" data from http://www.bgs.ac.uk/research/groundwater/health/arsenic/Bangladesh/data.html (1998)
# "New" data from the 2012/2013 MICS

library(llamar)
loadPkgs()

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
         above10ppb = ifelse(As_ug.l > 10, 1, 0),
         year = 1998) %>% 
  rename(div = DIVISION,
         district = DISTRICT)



# Collapse original data to the Adm1 level --------------------------------
baseline_Adm1 = baseline %>% 
  group_by(div) %>% 
  summarise(num = n(),
            meanAs = mean(`As_ug.l`),
            meanAs50ppb = mean(above50ppb),
            std50ppb = sd(above50ppb),
            se50ppb = CI * std50ppb/sqrt(num),
            lb50ppb = meanAs50ppb - se50ppb,
            ub50ppb = meanAs50ppb + se50ppb,
            meanAs10ppb = mean(above10ppb),
            std10ppb = sd(above10ppb),
            se10ppb = CI * std10ppb/sqrt(num),
            lb10ppb = meanAs10ppb - se10ppb,
            ub10ppb = meanAs10ppb + se10ppb, 
            year = '1998/1999')

# Create fake Rangpur data, since Rangpur didn't exist in 1998
baselineRangpur = baseline_Adm1 %>% 
  filter(div == 'Rajshahi') %>% 
  mutate(div = 'Rangpur')

baseline_Adm1 = rbind(baseline_Adm1, baselineRangpur)

# Collapse original data for the Adm2 level -------------------------------
baseline_Adm2 = baseline %>% 
  group_by(div, district) %>% 
  summarise(num = n(),
            meanAs = mean(`As_ug.l`),
            meanAs50ppb = mean(above50ppb),
            std50ppb = sd(above50ppb),
            se50ppb = CI * std50ppb/sqrt(num),
            lb50ppb = meanAs50ppb - se50ppb,
            ub50ppb = meanAs50ppb + se50ppb,
            meanAs10ppb = mean(above10ppb),
            std10ppb = sd(above10ppb),
            se10ppb = CI * std10ppb/sqrt(num),
            lb10ppb = meanAs10ppb - se10ppb,
            ub10ppb = meanAs10ppb + se10ppb,
            year = '1998/1999')


# Pull in "midline" data --------------------------------------------------------
source('~/GitHub/BangladeshTraining/BGD_MICS.R')



# Collapse midline data to Adm1 -------------------------------------------

midline_Adm1 = hh %>% 
  rename(As = WQ9) %>% 
  filter(As < 9999, !is.na(As)) %>% 
  mutate(above50ppb = ifelse(As > 50, 1, 0),
         above10ppb = ifelse(As > 10, 1, 0)) %>% 
  group_by(div) %>% 
  summarise(num = n(),
            meanAs = mean(As),
            meanAs50ppb = mean(above50ppb),
            std50ppb = sd(above50ppb),
            se50ppb = CI * std50ppb/sqrt(num),
            lb50ppb = meanAs50ppb - se50ppb,
            ub50ppb = meanAs50ppb + se50ppb,
            meanAs10ppb = mean(above10ppb),
            std10ppb = sd(above10ppb),
            se10ppb = CI * std10ppb/sqrt(num),
            lb10ppb = meanAs10ppb - se10ppb,
            ub10ppb = meanAs10ppb + se10ppb,
            year = '2012/2013')


# Collapse midline data to Adm2 -------------------------------------------

midline_Adm2 = hh %>% 
  rename(As = WQ9) %>% 
  filter(As < 9999, !is.na(As)) %>% 
  mutate(above50ppb = ifelse(As > 50, 1, 0),
         above10ppb = ifelse(As > 10, 1, 0)) %>% 
  group_by(div, district) %>% 
  summarise(num = n(),
            meanAs = mean(As),
            meanAs50ppb = mean(above50ppb),
            std50ppb = sd(above50ppb),
            se50ppb = CI * std50ppb/sqrt(num),
            lb50ppb = meanAs50ppb - se50ppb,
            ub50ppb = meanAs50ppb + se50ppb,
            meanAs10ppb = mean(above10ppb),
            std10ppb = sd(above10ppb),
            se10ppb = CI * std10ppb/sqrt(num),
            lb10ppb = meanAs10ppb - se10ppb,
            ub10ppb = meanAs10ppb + se10ppb, 
            year = '2012/2013')


# Merge baseline and midline together -------------------------------------
adm1 = full_join(baseline_Adm1, midline_Adm1, 
                 by = c("div" = "div"))


# Merge baseline and midline together -------------------------------------
adm2 = full_join(baseline_Adm2, midline_Adm2, 
                 by = c("div" = "div", "district" = "district"))

ggplot(adm2 %>% filter(div %in% c('Khulna', 'Sylhet', 'Chittagong')), 
       aes(x =  1998, xend = 2012, y = meanAs50ppb.x, yend =   meanAs50ppb.y,
           colour = district, label = district)) +
  geom_segment() +
  geom_point(size = 3) +
  geom_point(aes(y = meanAs50ppb.y, x = 2012), size = 3) +
  geom_text(nudge_x = -1, 
            family = 'Segoe UI Semilight', size = 4.5) + 
  theme_ygrid() +
  facet_wrap(~div) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(limits = c(1996, 2012),
                     breaks = c(1998, 2012))

adm1_tidy = rbind(baseline_Adm1, midline_Adm1)
adm2_tidy = rbind(baseline_Adm2, midline_Adm2)


# high level stats --------------------------------------------------------
y1 = baseline %>% summarise(mean(above50ppb))
y2 = hh %>% 
  rename(As = WQ9) %>% 
  filter(As < 9999, !is.na(As)) %>% 
  mutate(above50ppb = ifelse(As > 50, 1, 0),
         above10ppb = ifelse(As > 10, 1, 0)) %>% 
  summarise(mean(above50ppb))

(y2-y1)/y1

# save data ---------------------------------------------------------------

write.csv(adm1, '~/Documents/USAID/Bangladesh/Training/dataout/BGD_training_Asmidline_adm1.csv')
write.csv(adm2, '~/Documents/USAID/Bangladesh/Training/dataout/BGD_training_Asmidline_adm2.csv')

write.csv(adm1_tidy, '~/Documents/USAID/Bangladesh/Training/dataout/BGD_training_Asmidline_adm1_tidy.csv')
write.csv(adm2_tidy, '~/Documents/USAID/Bangladesh/Training/dataout/BGD_training_Asmidline_adm2_tidy.csv')
