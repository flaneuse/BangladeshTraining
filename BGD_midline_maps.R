source('~/GitHub/Misc. projects/Import shapefile/shp2csv.R')

adm2 = shp2csv(workingDir = '~/Documents/USAID/Bangladesh/Training/BGD_Adm2/',
               layerName = 'DistrictA',
               labelVar = 'District',
               exportData = FALSE)

# Pull out the lat/lon data
adm2_df = adm2$df


# merge in arsenic data ---------------------------------------------------
adm2_df = left_join(adm2_df, adm2,
                    by = c("District" = "district"))


# plot map ----------------------------------------------------------------

p = ggplot(adm2_df, aes(x = long, 
                   y = lat, 
                   group = group, 
                   fill = meanAs50ppb)) +
  geom_polygon() +
  theme_void() + 
  coord_equal() +
  scale_fill_gradientn(colours = brewer.pal(9, 'YlGnBu')) +
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
