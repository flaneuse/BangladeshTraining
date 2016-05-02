source('~/GitHub/Misc. projects/Import shapefile/shp2csv.R')

adm2_map = shp2csv(workingDir = '~/Documents/USAID/Bangladesh/Training/BGD_Adm2/',
               layerName = 'DistrictA',
               labelVar = 'District',
               exportData = FALSE)

# Pull out the lat/lon data
adm2_df = adm2_map$df


# merge in arsenic data ---------------------------------------------------
adm2_df = left_join(adm2_df, adm2,
                    by = c("District" = "district")) %>% 
  mutate(chgAs50 = meanAs50ppb.y - meanAs50ppb.x)


# plot map ----------------------------------------------------------------

p = ggplot(adm2_df, aes(x = long, 
                   y = lat, 
                   group = group, 
                   fill = meanAs50ppb.y)) +
  geom_polygon() +
  theme_void() + 
  coord_equal() +
  scale_fill_gradientn(colours = brewer.pal(9, 'YlOrBr'),
                       limits = c(0, 0.9)) +
  theme(legend.position = 'none')

fileName = '~/Documents/USAID/Bangladesh/Training/Training docs/BGD_midlineAs_choro.pdf'
plotWidth = 5
plotHeight = 6

ggsave(filename = fileName,
       width = plotWidth, height = plotHeight,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)


# baseline map ------------------------------------------------------------

p = ggplot(adm2_df, aes(x = long, 
                        y = lat, 
                        group = group, 
                        fill = meanAs50ppb.x)) +
  geom_polygon() +
  theme_void() + 
  coord_equal() +
  scale_fill_gradientn(colours = brewer.pal(9, 'YlOrBr'),
                       limits = c(0, 0.9)) +
  theme(legend.position = 'none')

fileName = '~/Documents/USAID/Bangladesh/Training/Training docs/BGD_baselineAs_choro.pdf'
plotWidth = 5
plotHeight = 6

ggsave(filename = fileName,
       width = plotWidth, height = plotHeight,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)


# change ------------------------------------------------------------------

p = ggplot(adm2_df, aes(x = long, 
                        y = lat, 
                        group = group, 
                        fill = chgAs50)) +
  geom_polygon() +
  theme_void() + 
  coord_equal() +
  scale_fill_gradientn(colours = rev(brewer.pal(9, 'RdYlBu')),
                       limits = c(-0.9, 0.9)) +
  theme(legend.position = 'none')

fileName = '~/Documents/USAID/Bangladesh/Training/Training docs/BGD_changeAs_choro.pdf'
plotWidth = 5
plotHeight = 6

ggsave(filename = fileName,
       width = plotWidth, height = plotHeight,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

