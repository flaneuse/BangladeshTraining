library(llamar)
loadPkgs()

# Import midline data --------------------------------------------------
source('BGD_midlineData.R')

adm1 = adm1 %>% 
  mutate(chgAs50 = meanAs50ppb.y - meanAs50ppb.x)

# Merge in pop data
source('BGD_population.R')

pop2011 = pop %>% filter(year == 2011)

adm1 = left_join(adm1, pop2011, by = 'div')


# Merge in stunting data
bgStunted = read.csv('~/Documents/USAID/Bangladesh/Training/dataout/BGD_DHSstunting2014.csv') %>% 
  filter(SurveyYear == 2011,
         CharacteristicLabel != 'Rajshahi/Rangpur') %>% 
  mutate(Value = Value /100,
         CharacteristicLabel = as.character(CharacteristicLabel),
         CharacteristicLabel = ifelse(str_detect(CharacteristicLabel, '\\.'),
                                      str_replace_all(CharacteristicLabel, '\\.', ''),
                                      CharacteristicLabel)) %>% 
  select(div = CharacteristicLabel,
         stunting = Value,
         stuntingYr = SurveyYear,
         denomStunted = DenominatorUnweighted)

# fake money data
money = read_excel('~/GitHub/BangladeshTraining/BGD_midlinedata.xlsx',
                   sheet = 1) %>% 
  filter(division != 'All Bangladesh')

adm1 = left_join(adm1, bgStunted, by = 'div')


adm1_tidy = left_join(adm1_tidy, bgStunted, by = 'div')
adm1_tidy = left_join(adm1_tidy, pop, by = 'div')
adm1_tidy = left_join(adm1_tidy, money, by = c('div' = 'division'))


# plot: change [As] as arrows ---------------------------------------------
order1 = adm1 %>% 
  arrange(desc(meanAs50ppb.y))

adm1$div = factor(adm1$div, 
                  levels = order1$div)


ggplot(adm1, aes(x = div, xend = div,
                 y = meanAs50ppb.x, yend = meanAs50ppb.y,
                 colour = meanAs50ppb.y)) +
  geom_segment(arrow = arrow(length = unit(0.25,"cm"))) +
  geom_point(size = 1.5) +
  geom_label(aes(label = '1998/\n 1999'),
             nudge_x = 0.35,
             label.size = 0,
             data = adm1 %>% filter(div == 'Khulna')) +
  geom_label(aes(label = '2012/ \n2013',
                 y = meanAs50ppb.y),
             label.size = 0,
             nudge_x = 0.35,
             data = adm1 %>% filter(div == 'Khulna')) +
  
  scale_colour_gradientn(colours = brewer.pal(9, 'Oranges')[4:9]) +
  scale_y_continuous(labels = scales::percent, name = '') + 
  ggtitle('Households with contaminated drinking water decreased in 2012/2013') +
  theme_ygrid()


# plot: change [As] as arrows: Adm2 ---------------------------------------------
order3 = adm2 %>% 
  arrange(desc(meanAs50ppb.y))

adm2$district = factor(adm2$district, 
                       levels = order3$district)


ggplot(adm2 %>% filter(div %in% c('Chittagong', 'Dhaka', 'Sylhet')), aes(x = district, xend = district,
                                                                         y = meanAs50ppb.x, yend = meanAs50ppb.y,
                                                                         colour = meanAs50ppb.y)) +
  geom_segment(arrow = arrow(length = unit(0.25,"cm"))) +
  geom_point(size = 1.5) +
  # geom_label(aes(label = '1998/\n 1999'),
  # nudge_x = 0.35,
  # label.size = 0,
  # data = adm2 %>% filter(div == 'Khulna')) +
  # geom_label(aes(label = '2012/ \n2013',
  # y = meanAs50ppb.y),
  # label.size = 0,
  # nudge_x = 0.35,
  # data = adm2 %>% filter(div == 'Khulna')) +
  facet_wrap(~div, scales = 'free_x') +
  scale_colour_gradientn(colours = brewer.pal(9, 'Oranges')[4:9]) +
  scale_y_continuous(labels = scales::percent, name = '') + 
  ggtitle('Households with contaminated drinking water decreased in 2012/2013') +
  theme_ygrid()


# bump chart --------------------------------------------------------------

ggplot(adm1, aes(xend =  1998, x = 2012, yend = meanAs50ppb.x, y =   meanAs50ppb.y,
                 colour = div, fill = div, label = div)) +
  geom_segment(data = adm1 %>% filter(!div %in% c('Rangpur', 'Rajshahi'))) +
  geom_segment(linetype = 2,
               colour = '#B983FF',
               data = adm1 %>% filter(div %in% c('Rangpur', 'Rajshahi'))) +
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


# [As] vs. contaminated ---------------------------------------------------

ggplot(adm2_tidy %>% filter(year %like% '2012'), 
       aes(x = meanAs,  y = meanAs50ppb,
           colour = div, shape = year)) +
  annotate(geom = 'rect', xmin = 50, xmax = 150,
           ymin = 0, ymax = 0.5, fill = grey30K, alpha = 0.2) +
  geom_point(size = 3, alpha = 0.7) +
  theme_basic()


# 10 ppb v 50 ppb ---------------------------------------------------------

ggplot(adm2_tidy %>% filter(year %like% '2012'), 
       aes(x = meanAs10ppb,  y = meanAs50ppb,
           colour = div, shape = year)) +
  geom_abline(slope = 1, intercept = 0, 
              size = 0.25,
              colour = grey75K) +
  geom_point(size = 3, alpha = 0.7) +
  theme_xygridlight() +
  coord_equal()



# heatmap -----------------------------------------------------------------
order2 = adm2_tidy %>% 
  arrange(meanAs50ppb)

adm2_tidy$district = factor(adm2_tidy$district,
                            levels = order2$district)

ggplot(adm2_tidy, aes(y = district, 
                      x = year, 
                      fill = meanAs50ppb)) +
  geom_tile() +
  facet_wrap(~div, scales = 'free_y') +
  scale_fill_gradientn(colours = brewer.pal(9, 'Oranges')) +
  theme_labelsOnly()

# hist
ggplot(adm2_tidy %>% filter(year %like% '2012'),
       aes(x = meanAs50ppb)) +
  geom_histogram(binwidth = 0.15) +
  facet_wrap(~div) +
  theme_ygrid()

# distribution among districts --------------------------------------------
order4 = adm1_tidy %>% 
  filter(year == '2012/2013') %>% 
  arrange(desc(meanAs50ppb))

adm2_tidy$div = factor(adm2_tidy$div,
                       levels = order4$div)

adm1$div = factor(adm1$div,
                  levels = order4$div)

ggplot(adm2_tidy %>% filter(year %like% '2012'),
       aes(y = meanAs50ppb,
           x = div, colour = div)) +  
  geom_point(size = 5, alpha = 0.4, colour = NA) +
  geom_segment(aes(x = as.numeric(div) - 0.25,
                   xend = as.numeric(div) + 0.25,
                   y = meanAs50ppb.y,
                   yend = meanAs50ppb.y,
                   colour = div),
               size = 1.5,
               data = adm1) +
  scale_y_continuous(labels = scales::percent, 
                     limits = c(0, 0.5),
                     name = '') + 
  theme_ygrid()

ggplot(adm2_tidy %>% filter(year %like% '2012'),
       aes(y = meanAs50ppb,
           x = div, colour = div)) +
  geom_point(size = 5, alpha = 0.4) +
  geom_segment(aes(x = as.numeric(div) - 0.25,
                   xend = as.numeric(div) + 0.25,
                   y = meanAs50ppb.y,
                   yend = meanAs50ppb.y,
                   colour = div),
               size = 1.5,
               data = adm1) +
  scale_y_continuous(labels = scales::percent, 
                     limits = c(0, 0.5),
                     name = '') + 
  theme_ygrid()

# 1998

order5 = adm1_tidy %>% 
  filter(year == '1998/1999') %>% 
  arrange(desc(meanAs50ppb))

adm2_tidy$div = factor(adm2_tidy$div,
                       levels = order5$div)
adm1$div = factor(adm1$div,
                  levels = order5$div)


ggplot(adm2_tidy %>% filter(year %like% '1998'),
       aes(y = meanAs50ppb,
           x = div, colour = div)) +
  geom_point(size = 5, alpha = 0.4) +
  geom_segment(aes(x = as.numeric(div) - 0.25,
                   xend = as.numeric(div) + 0.25,
                   y = meanAs50ppb.x,
                   yend = meanAs50ppb.x,
                   colour = div),
               size = 1.5,
               data = adm1) +
  scale_y_continuous(labels = scales::percent, name = '') + 
  theme_ygrid()


# Scatter: stunting, As, pop ----------------------------------------------

ggplot(adm1, aes(x = meanAs50ppb.y, 
                 y = stunting, 
                 size = population, 
                 colour = population,
                 label = div)) +
  geom_point() +
  geom_text(family = 'Segoe UI Semilight',
            size = 6, 
            nudge_x = 0.025,
            nudge_y =  0.02) +
  scale_colour_gradientn(colours = brewer.pal(9, 'PuRd')[5:9]) +
  scale_x_continuous(labels = scales::percent,
                     name = 'households with arsenic-contaminated drinking water') +
  scale_y_continuous(labels = scales::percent, 
                     name = 'stunted children',
                     limits = c(0, 0.52)) +
  scale_size(range = c(3, 20)) +
  theme_xygridlight() +
  ggtitle('2011 - 2013 data')
