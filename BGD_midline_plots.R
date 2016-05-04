library(llamar)
loadPkgs()

# Import midline data --------------------------------------------------
source('~/GitHub/BangladeshTraining/BGD_midlineData.R')

adm1 = adm1 %>% 
  mutate(chgAs50 = meanAs50ppb.y - meanAs50ppb.x,
         pctChg = abs(chgAs50 / meanAs50ppb.x))

# Merge in pop data
source('~/GitHub/BangladeshTraining/BGD_population.R')

pop2011 = pop %>% filter(year == 2011)

adm1 = left_join(adm1, pop2011, by = 'div')
adm1_tidy = left_join(adm1_tidy, pop2011 %>% rename(yearPop = year), by = 'div')


# Merge in stunting data
bgStuntedAll = read.csv('~/Documents/USAID/Bangladesh/Training/dataout/BGD_DHSstunting2014.csv') %>% 
  mutate(Value = Value /100,
         CharacteristicLabel = as.character(CharacteristicLabel),
         CharacteristicLabel = ifelse(str_detect(CharacteristicLabel, '\\.'),
                                      str_replace_all(CharacteristicLabel, '\\.', ''),
                                      CharacteristicLabel)) %>% 
  select(div = CharacteristicLabel,
         stunting = Value,
         stuntingYr = SurveyYear,
         denomStunted = DenominatorUnweighted)

# Replicating data for Rajshahi / Rangpur split
raj = bgStuntedAll %>% 
  filter(div == 'Rajshahi/Rangpur',
         stuntingYr < 2011) %>% 
  mutate(div = 'Rajshahi')

rangpur = bgStuntedAll %>% 
  filter(div == 'Rajshahi/Rangpur', 
         stuntingYr < 2011) %>% 
  mutate(div = 'Rangpur')


bgStuntedAll = rbind(bgStuntedAll, raj)
bgStuntedAll = rbind(bgStuntedAll, rangpur) %>% 
  filter(div != 'Rajshahi/Rangpur')

bgStunted = read.csv('~/Documents/USAID/Bangladesh/Training/dataout/BGD_DHSstunting2014.csv') %>% 
  filter(SurveyYear == 2011,
         
         !CharacteristicLabel %in% c('Rajshahi/Rangpur', 'Total')) %>% 
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
  filter(division != 'All Bangladesh') %>% 
  filter(year =='1998/1999') %>% 
  select(division, contains('funding')) %>% 
  mutate(funding_SBC = 10/7,
         `total funding` = funding_SBC + funding_filtration + funding_community_treatment)

adm1 = left_join(adm1, bgStunted, by = 'div')


adm1_tidy = left_join(adm1_tidy, bgStunted, by = 'div')

# Merge in money
adm1 = left_join(adm1, money, by = c('div' = 'division'))
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
  geom_label(nudge_x = 1, 
             nudge_y = -0.055,
             label.size = 0, fill = 'white',
             family = 'Segoe UI Semilight', size = 4.5) + 
  geom_text(aes(label = paste0(percent(pctChg, 0)),
                 hjust = 1,
                 nudge_x = 1, 
                 family = 'Segoe UI Semilight', size = 4.5)) + 
  theme_ygrid() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(limits = c(1998, 2014),
                     breaks = c(1998, 2012)) +
  facet_wrap(~div)

ggsave(filename = '~/Documents/USAID/Bangladesh/Training/Training docs/arsenic_bump.pdf',
       width = 7, height = 5,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)


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


# heatmap -----------------------------------------------------------------
adm1_long = adm1 %>% 
  select(`As > 50ppb` = meanAs50ppb.y, 
         `As > 10ppb` = meanAs10ppb.y, 
         stunting, 
         div) %>% 
  gather(indicator, pct, -div) %>% 
  mutate(colourVal = ifelse(pct > mean(pct),
                            'white', grey90K))

orderHeat = adm1_long %>% 
  filter(indicator == 'As > 10ppb') %>% 
  arrange((pct))

adm1_long$div = factor(adm1_long$div,
                       levels = orderHeat$div)


adm1_long$indicator = factor(adm1_long$indicator,
                       levels = (c('As > 10ppb',
                                  'As > 50ppb','stunting')))

ggplot(adm1_long, aes(fill = pct, 
                      x = div,
                      y = indicator,
                      label = percent(pct, 0),
                      colour = colourVal)) +
  scale_fill_gradientn(colours = brewer.pal(9, 'YlGnBu'),
                       limits = c(min(adm1_long$pct), max(adm1_long$pct))) +
  geom_tile(colour = 'white', size = 0.5) +
  geom_text(size = 6, family = 'Segoe UI') +
  scale_color_identity() +
  coord_flip() +
  theme_xylab()


ggsave(filename = '~/Documents/USAID/Bangladesh/Training/Training docs/heatmapAs.pdf',
       width = 4., height = 6,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

# heatmap -----------------------------------------------------------------
adm1_long = adm1 %>% 
  select(`As > 50ppb` = meanAs50ppb.y, 
         `As > 10ppb` = meanAs10ppb.y, 
         stunting, 
         div) %>% 
  gather(indicator, pct, -div) %>% 
  mutate(colourVal = ifelse(pct > mean(pct),
                            'white', grey90K))

orderHeat = adm1_long %>% 
  filter(indicator == 'As > 10ppb') %>% 
  arrange((pct))

adm1_long$div = factor(adm1_long$div,
                       levels = orderHeat$div)


adm1_long$indicator = factor(adm1_long$indicator,
                             levels = (c('As > 10ppb',
                                         'As > 50ppb','stunting')))

ggplot(adm1_long, aes(fill = pct, 
                      x = div,
                      y = indicator,
                      label = percent(pct, 0),
                      colour = colourVal)) +
  scale_fill_gradientn(colours = brewer.pal(9, 'RdPu'),
                       limits = c(min(adm1_long$pct), max(adm1_long$pct))) +
  geom_tile(colour = 'white', size = 0.5) +
  geom_text(size = 6, family = 'Segoe UI') +
  scale_color_identity() +
  coord_flip() +
  theme_xylab()


ggsave(filename = '~/Documents/USAID/Bangladesh/Training/Training docs/heatmapAs_pink.pdf',
       width = 4., height = 6,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)


# heatmap - pop -----------------------------------------------------------


adm1$div = factor(adm1$div,
                       levels = orderHeat$div)

ggplot(adm1, aes(fill = population, 
                      x = div,
                      y = 1,
                      label = paste0(round(population/1e6,1), ' M'))) +
  scale_fill_gradient(low = grey25K, high = grey90K) +
  geom_tile(colour = 'white', size = 0.5) +
  geom_text(size = 6, family = 'Segoe UI') +
  scale_color_identity() +
  theme_xylab() + 
  coord_flip()

ggsave(filename = '~/Documents/USAID/Bangladesh/Training/Training docs/heatmapPop.pdf',
       width = 1.75, height = 6,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

# money plots -------------------------------------------------------------
widthMoney = 2.5
colourMoney = '#33a02c'

ggplot(adm1, aes(x = `total funding`, 
                 y = chgAs50,
                 colour = `total funding`,
                 label = div)) +
  geom_smooth(colour = grey75K, 
              fill = NA,
              size = 0.5,
              linetype = 2) +
  geom_point(size = 5) +
  geom_text(size = 5, 
            nudge_x = 1,
            hjust = 0,
            family = 'Segoe UI Semilight') +
  scale_colour_gradientn(colours = brewer.pal(9, 'YlGn')[3:8]) +
  theme_xygridlight() +
  scale_y_continuous(labels = scales::percent,
                     breaks = c(-0.4, -0.2, 0),
                     name = '') +
  ggtitle('change in percent') +
  coord_cartesian(xlim = c(0, 18), ylim = c(-0.4, 0)) +
  xlab('total funding (millions USD)')

ggsave(filename = '~/Documents/USAID/Bangladesh/Training/Training docs/funding_total.pdf',
       width = widthMoney, height = widthMoney,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

ggplot(adm1, aes(x = funding_community_treatment, 
                 y = chgAs50,
                 label = div,
                 colour = `total funding`)) +
  geom_smooth(colour = grey75K, 
              fill = NA,
              size = 0.5,
              linetype = 2) +
  geom_point(size = 5) +
  geom_text(size = 5, 
            nudge_x = 1,
            hjust = 0,
            family = 'Segoe UI Semilight') +
  scale_colour_gradientn(colours = brewer.pal(9, 'YlGn')[3:8]) +
  theme_xygridlight() +
  scale_y_continuous(labels = scales::percent,
                     breaks = c(-0.4, -0.2, 0),
                     name = '') +
  ggtitle('change in percent') +
  coord_cartesian(xlim = c(0, 18), ylim = c(-0.4, 0)) +
  xlab('community treatment funding (millions USD)')

ggsave(filename = '~/Documents/USAID/Bangladesh/Training/Training docs/funding_community.pdf',
       width = widthMoney, height = widthMoney,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

ggplot(adm1, aes(x = funding_filtration, y = chgAs50,
                 colour = `total funding`,
                 label = div)) +
  geom_smooth(colour = grey75K, 
              fill = NA,
              size = 0.5,
              linetype = 2) +
  geom_point(size = 5) +
  geom_text(size = 5, 
            nudge_x = 1,
            hjust = 0,
            family = 'Segoe UI Semilight') +
  scale_colour_gradientn(colours = brewer.pal(9, 'YlGn')[3:8]) +
  theme_xygridlight() +
  scale_y_continuous(labels = scales::percent,
                     breaks = c(-0.4, -0.2, 0),
                     name = '') +
  ggtitle('change in percent') +
  coord_cartesian(xlim = c(0, 18), ylim = c(-0.4, 0)) +
  xlab('rainwater harvesting funding (millions USD)')

ggsave(filename = '~/Documents/USAID/Bangladesh/Training/Training docs/funding_rainwater.pdf',
       width = widthMoney, height = widthMoney,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)


# sparklines: arsenic -----------------------------------------------------
adm1$div = factor(adm1$div,
                  levels = rev(orderHeat$div))

ggplot(adm1, aes(xend =  1998, x = 2012, yend = meanAs50ppb.x, y =   meanAs50ppb.y,
                 colour = meanAs50ppb.y, fill = meanAs50ppb.y)) +
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
               alpha = 0.7, size = 1.5) +
  geom_point(aes(y = meanAs50ppb.x, x = 1998), 
             colour= grey90K,
             shape = 21,
             size = 3) +
  theme_ygrid() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(limits = c(1998, 2014),
                     breaks = c(1998, 2012)) +
  facet_wrap(~div, ncol = 1) +
  scale_colour_gradientn(colours = brewer.pal(9, 'YlGnBu'),
                         limits = c(min(adm1_long$pct), max(adm1_long$pct))) +
  scale_fill_gradientn(colours = brewer.pal(9, 'YlGnBu'),
                         limits = c(min(adm1_long$pct), max(adm1_long$pct))) +
  facet_wrap(~div, ncol = 1) +
  geom_text(aes(y = meanAs50ppb.x,
                x = 1998,
                label = percent(meanAs50ppb.x)),
    size = 3.5,
            family = 'Segoe UI Light',
            nudge_y  = 0.09) +
  geom_text(aes(label = percent(meanAs50ppb.y)),
            size = 3.5,
            family = 'Segoe UI Light',
            nudge_y  = 0.09) +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank())
        # rect = element_rect(fill = NA, colour = grey50K, size = NULL, linetype = 1),
        # panel.border = element_rect(size = 0))


ggsave(filename = '~/Documents/USAID/Bangladesh/Training/Training docs/arsenic_sparks.pdf',
       width = 0.8, height = 6,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

# sparklines: stunting ----------------------------------------------------

bgStuntedPlot = bgStuntedAll %>% 
  filter(stuntingYr < 2012)

bgStunted2011 = bgStuntedPlot %>% 
  filter(stuntingYr == 2011) %>% 
  select(stunting2011 = stunting, div)

bgStuntedTotal = bgStuntedPlot %>% 
  filter(div == 'Total') %>% 
  select(stuntingTotal = stunting, stuntingYr)

bgStuntedPlot  = full_join(bgStunted2011, bgStuntedPlot)
bgStuntedPlot  = full_join(bgStuntedTotal, bgStuntedPlot) %>% 
  filter(div != 'Total')

bgStuntedPlot$div = factor(bgStuntedPlot$div,
                          levels = rev(orderHeat$div))

widthLine = 0.35

# -- Plot --

ggplot(bgStuntedPlot, aes(colour = stunting2011,
                          x = stuntingYr, y = stunting, 
                          group = div,
                          label = percent(stunting))) +
  # # -- Country --
  # geom_line(aes(y = stuntingTotal),
  #           size = widthLine,
  #           colour = grey50K) +
  # geom_point(aes(y = stuntingTotal),
  #            colour = 'white',
  #            size = 4) +
  # geom_point(aes(y = stuntingTotal),
  #            colour = grey50K,
  #            size = 3,
  #            fill = 'white',
  #            shape = 21) +
  
  # -- Regions --
  geom_line(size = widthLine,
    data = bgStuntedPlot %>% filter(stuntingYr < 2011 | !div %in% c('Rangpur','Rajshahi'))) +
  geom_line(size = widthLine,
    data = bgStuntedPlot %>% filter(div %in% c('Rangpur','Rajshahi')),
    linetype = 2) +
  geom_point(colour = 'white',
             size = 4) +
  geom_point(
    size = 3,
    fill = 'white',
    shape = 21) +
  geom_point(size = 3,
             data = bgStuntedPlot %>% filter(stuntingYr == 2011)) +
  # -- Labels --
  geom_text(size = 3.5,
            family = 'Segoe UI Light',
            nudge_y  = 0.09) +
  theme_ygrid() +
  coord_cartesian(xlim = c(2004, 2012)) +
  scale_y_continuous(limits = c(0.3, 0.6),
    breaks = seq(0.15, 0.75, by= 0.15)) +
  scale_colour_gradientn(colours = brewer.pal(9, 'RdPu'),
                         limits = c(min(adm1_long$pct), max(adm1_long$pct))) +
  facet_wrap(~div, ncol = 1) +
  theme(axis.title = element_blank(),
        axis.text = element_blank())

ggsave(filename = '~/Documents/USAID/Bangladesh/Training/Training docs/stunting_sparks.pdf',
       width = 0.8, height = 6,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)




# colors for money --------------------------------------------------------
moneyTidy = data.frame(x = unique(c(money$funding_community_treatment, money$funding_filtration, money$`total funding`)))

moneyTidy = moneyTidy %>% filter(x !=0)
ggplot(moneyTidy, aes(x = x, y = 1, colour = x)) +
  geom_point(size = 25) +
  scale_colour_gradientn(colours = brewer.pal(9, 'YlGn')[3:8],
                         breaks = seq(0,20, by = 5)) +
  theme_basic() +
  theme(legend.position = 'left')

