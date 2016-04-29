library(llamar)
loadPkgs()

# Plots for midline data --------------------------------------------------
source('BGD_midlineData.R')

adm1 = adm1 %>% 
  mutate(chgAs50 = meanAs50ppb.y - meanAs50ppb.x)


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
