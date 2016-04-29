library(llamar)
loadPkgs()

ch = read_sav('~/Documents/USAID/Bangladesh/Training/Bangladesh_MICS5_Datasets/Bangladesh MICS 2012-13 SPSS Datasets/ch.sav')
hh = read_sav('~/Documents/USAID/Bangladesh/Training/Bangladesh_MICS5_Datasets/Bangladesh MICS 2012-13 SPSS Datasets/hh.sav')

# hh = removeAttributes(hh)
# ch = removeAttributes(ch)
# ch_merged = left_join(ch, hh, by = c('HH1', 'HH2'))

distNames = attr(hh$HH7A, 'labels')

districts = data.frame(district = as.character(plyr::mapvalues(hh$HH7A, from = distNames, 
                                                               to = labels(distNames))))


div = data.frame(div = as.character(plyr::mapvalues(ch$HH7, from = c(10, 20, 30, 40, 50, 55, 60), 
                to = c('Barisal', 'Chittagong',      'Dhaka',     'Khulna',   'Rajshahi',    'Rangpur',     'Sylhet'))))


# children ----------------------------------------------------------------


# ch = cbind(ch, div, districts)

# Stunting by division, sex
# ch = ch %>% 
  # filter(HAZ2 > -10, HAZ2 < 10,
         # HL4 !=9) %>% 
  # mutate(stunted = HAZ2 <= -2) 

# stunting = ch %>% 
#   group_by(div, HL4) %>% 
#   summarise(pctStunted = mean(stunted, na.rm = TRUE), 
#             std = sd(stunted, na.rm = TRUE),
#             num = n(),
#             se = std/sqrt(num)*1.96)
# 
# stuntingOrder = stunting %>% 
#   ungroup() %>% 
#   filter(HL4 == 1) %>% 
#   arrange(desc(pctStunted))
# 
# stunting$div= factor(stunting$div, levels = stuntingOrder$div)
# 
# ch %>% 
#   filter()
#   group_by(AG2) %>% 
#   summarise(n())
# 
# ggplot(stunting, aes(x = div, y = pctStunted, 
#                      ymin = pctStunted - se,
#                      ymax = pctStunted + se,
#                      colour = factor(HL4))) +
#   geom_pointrange() +
# facet_wrap(~HL4) +
#   scale_colour_manual(values = c('1' = 'blue', '2' = 'pink')) +
#   theme_ygrid() +
#   scale_y_continuous(limits = c(0, 0.6))+
#   ggtitle('2012/2013 MICS in Bangladesh')
# 

# Arsenic
div = data.frame(div = as.character(plyr::mapvalues(hh$HH7, from = c(10, 20, 30, 40, 50, 55, 60), 
                                                    to = c('Barisal', 'Chittagong',      'Dhaka',     'Khulna',   'Rajshahi',    'Rangpur',     'Sylhet'))))

hh = cbind(hh, div, districts)


# [As] > 50 ug/L, by Division/ District ---------------------------------------------


# "Arsenic level (ppb) in water sample provided by the respondent"
# By urban/rural
hh %>% 
  filter(WQ9 < 9999, !is.na(WQ9)) %>% 
  group_by(HH6, As = WQ9 >= 50) %>% 
  summarise(num = n()) %>% 
  mutate(tot = sum(num),
         pct = num / tot) %>% 
  filter(As == TRUE) %>% 
  ungroup() %>% 
  arrange(desc(pct))

# By Division
hh %>% 
  filter(WQ9 < 9999, !is.na(WQ9)) %>% 
  group_by(div, HH6, As = WQ9 >= 50) %>% 
  summarise(num = n()) %>% 
  mutate(tot = sum(num),
          pct = num / tot) %>% 
  filter(As == TRUE) %>% 
  ungroup() %>% 
  arrange(desc(pct))

hh_filtered = hh %>% 
  filter(WQ9 < 9999, !is.na(WQ9)) %>% 
  mutate(concAs = as.numeric(WQ9))
  
ggplot(hh_filtered, aes(x = log10(concAs))) +
  geom_density() + 
  facet_wrap(~div, scales = 'free_y')

# By District
hh %>% 
       filter(WQ9 < 9999, !is.na(WQ9)) %>% 
       group_by(div, district, As = WQ9 >= 50) %>% 
       summarise(num = n()) %>% 
       mutate(pct = num / sum(num)) %>% 
       filter(As == TRUE) %>%
       ungroup() %>% 
       arrange(desc(pct))


# "Arsenic level (ppb) in source water sample"
hh %>% 
  filter(WQ13 < 9999, !is.na(WQ13)) %>% 
  group_by(WQ13 >= 50) %>% 
  summarise(num = n()) %>% 
  mutate(pct = num / sum(num))

hh %>% 
  filter(WQ13 < 9999, !is.na(WQ13)) %>% 
  group_by(div, As = WQ13 >= 50) %>% 
  summarise(num = n()) %>% 
  mutate(pct = num / sum(num)) %>% 
  filter(As == TRUE) %>% 
  ungroup() %>% 
  arrange(desc(pct))


# As level at Adm2 --------------------------------------------------------
hh %>% 
       filter(WQ9 < 9999, !is.na(WQ9),
              div %in% c('Sylhet')) %>% 
       group_by(HH7A, As = WQ9 >= 50) %>% 
       summarise(num = n()) %>% 
       mutate(pct = num / sum(num)) %>% 
       filter(As == TRUE) %>%
       ungroup() %>% 
       arrange(desc(pct))


hh %>% 
       filter(WQ9 < 9999, !is.na(WQ9),
              div %in% c('Chittagong')) %>% 
       group_by(HH7A, As = WQ9 >= 50) %>% 
       summarise(num = n()) %>% 
       mutate(pct = num / sum(num)) %>% 
       filter(As == TRUE) %>%
       ungroup() %>% 
       arrange(desc(pct))

hh %>% 
       filter(WQ9 < 9999, !is.na(WQ9),
              div %in% c('Khulna')) %>% 
       group_by(HH7A, As = WQ9 >= 50) %>% 
       summarise(num = n()) %>% 
       mutate(pct = num / sum(num)) %>% 
       filter(As == TRUE) %>%
       ungroup() %>% 
       arrange(desc(pct))


# Adm2 levels -------------------------------------------------------------

adm2 = read.csv('~/Documents/USAID/Bangladesh/Training/BGD_adm2.csv')

adm2_mics = data.frame(district = names(attr(hh$HH7A, 'labels')))

x = full_join(adm2_mics, adm2, by = c("district"="NAME_2"))



# Infant mortality --------------------------------------------------------


# Percentage of deaths by cause -------------------------------------------

infMort = read.csv('~/Documents/USAID/Bangladesh/Training/BGD_infant_deaths.csv') %>% 
  rename(cause = `Cause.of.death`,
         year = Year,
         firstMonth = `X0.27.days`,
         firstQuarter = `X1.59.months`,
         year5 = `X0.4.years`)

# Deaths in first month
ggplot(infMort, aes(x = year, y = firstMonth, group = cause)) +
  geom_line()+
  facet_wrap(~cause)+
  theme_xygrid()

ggplot(infMort, aes(x = year, y = firstQuarter, group = cause)) +
  geom_line()+
  facet_wrap(~cause)+
  theme_xygrid()


# infMort$cause = factor(infMort$cause, 
                           # levels = orderRate$cause)

# Proportion of deaths for children under 5
ggplot(infMort, aes(x = year, y = year5, group = cause)) +
  geom_line()+
  facet_wrap(~cause)+
  theme_xygrid() + 
  ggtitle('Percent of deaths for children under 5')



# Rate of death per 1000 births -------------------------------------------

infMortRate = read_csv('~/Documents/USAID/Bangladesh/Training/BGD_infantMort_rates_GHO.csv') %>% 
  rename(cause = `Cause of death`, year = Year)

orderRate = infMortRate %>% 
  filter(year == 2015) %>% 
  arrange(desc(`0-4 years`))

infMortRate$cause = factor(infMortRate$cause, 
                           levels = orderRate$cause)

ggplot(infMortRate, aes(x = year, y = `0-4 years`, 
                        group = cause)) +
  geom_line()+
  facet_wrap(~cause)+
  theme_xygrid() +
  ggtitle('Rates (deaths per 1000 births)')

ggplot(aes(x = year, y = `0-4 years`, 
                        group = cause,
                        colour = cause),
       data = infMortRate %>% filter(cause %in% c('Acute lower respiratory infections',
                                                  'Birth asphyxia and birth trauma',
                                                  'Prematurity',
                                                  'Diarrhoeal diseases'))) +
  geom_line()+
  theme_basic() +
  ggtitle('Rates of deaths for children under 5')


# Proportion deaths top 4 -------------------------------------------------

ggplot(aes(x = year, y = year5, 
           group = cause,
           colour = cause),
       data = infMort%>% filter(cause %in% c('Acute lower respiratory infections',
                                                  'Birth asphyxia and birth trauma',
                                                  'Prematurity',
                                                  'Diarrhoeal diseases'))) +
  geom_line()+
  theme_xygridlight() +
  facet_wrap(~ cause) +
  ggtitle('Proportion of deaths for children under 5') +
  coord_cartesian(ylim = c(0, 22))


# change in death rates over time
infMortRate_tidy = infMortRate %>% 
  gather(age, rate, -year, -cause) %>% 
  group_by(age, year) %>% 
  summarise(totRate = sum(rate))

ggplot(infMortRate_tidy, aes(x = year, y = totRate, 
                        group = age, colour = age)) +
  geom_line()+
  facet_wrap(~age)+
  theme_xygrid()

ggplot(infMortRate_tidy %>% filter(year == 2015), aes(x = age, y = totRate)) +
  geom_bar(stat = 'identity') +
  theme_ygrid()
