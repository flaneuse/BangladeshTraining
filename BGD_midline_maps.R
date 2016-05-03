source('~/GitHub/Misc. projects/Import shapefile/shp2csv.R')

adm2_map = shp2csv(workingDir = '~/Documents/USAID/Bangladesh/Training/BGD_Adm2/',
               layerName = 'DistrictA',
               labelVar = 'District',
               exportData = FALSE)

adm1_map = shp2csv(workingDir = '~/Documents/USAID/Bangladesh/Training/BGD_Adm1/',
                   layerName = 'DivisionA',
                   labelVar = 'Division',
                   exportData = FALSE)

# Pull out the lat/lon data
adm2_df = adm2_map$df
adm1_df = adm1_map$df

adm2_centroids = adm2_map$centroids
adm1_centroids = adm1_map$centroids


# clean names in the Districts --------------------------------------------
baseline_Adm2 = baseline_Adm2 %>% 
  mutate(district = as.character(district),
         District = ifelse(district == 'Netrokona', 'Netrakona', 
                           ifelse(district == 'Jhalakati', 'Jhalokati',
                                  ifelse(district == "Cox's Bazar", "Cox'S Bazar",
                                         district))))


midline_Adm2 = midline_Adm2 %>% 
  mutate(district = as.character(district),
         District = ifelse(district == 'Kishorganj', 'Kishoreganj', 
                           ifelse(district == 'Brahmanbaria', 'Brahamanbaria',
                                  ifelse(district == "Cox's Bazar", "Cox'S Bazar",
                           district))))


# merge in arsenic data ---------------------------------------------------

adm2_baseline = left_join(adm2_df, baseline_Adm2 %>% select(-div),
                    by = c("District" = "District"))

adm2_midline = left_join(adm2_df, midline_Adm2 %>% select(-div),
                          by = c("District" = "District"))

adm2_df = left_join(adm2_baseline, midline_Adm2 %>% select(-div),
                    by = c("District" = "District")) %>% 
  mutate(chgAs50 = meanAs50ppb.y - meanAs50ppb.x)


# midline map  (50 ppb) ----------------------------------------------------------------
colPal = 'OrRd'

p = ggplot(adm2_midline, aes(x = long, 
                              y = lat)) +
  geom_polygon(aes(fill = meanAs50ppb, 
                   group = group)) +
  geom_path(aes(group = group),
            colour = grey90K,
            data = adm1_df, size = 0.2) +
  geom_text(aes(label = label),
            colour = grey90K,
            data = adm1_centroids, size = 4) +
  theme_void() + 
  coord_equal() + theme(legend.direction = 'horizontal') +
  scale_fill_gradientn(colours = brewer.pal(9, colPal),
                       na.value = grey25K,
                       labels = scales::percent,
                       limits = c(0, 0.95)) +
  guides(fill = guide_colorbar(ticks = FALSE)) +
  ggtitle('Percent of households with contaminated drinking water (2012/2013)')


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


# midline (10 ppb) --------------------------------------------------------

p = ggplot(adm2_midline, aes(x = long, 
                             y = lat)) +
  geom_polygon(aes(fill = meanAs10ppb, 
                   group = group)) +
  geom_path(aes(group = group),
            colour = grey90K,
            data = adm1_df, size = 0.2) +
  geom_text(aes(label = label),
            colour = grey90K,
            data = adm1_centroids, size = 4) +
  theme_void() + 
  coord_equal() + theme(legend.direction = 'horizontal') +
  scale_fill_gradientn(colours = brewer.pal(9, colPal),
                       na.value = grey25K,
                       labels = scales::percent,
                       limits = c(0, 0.95)) +
  guides(fill = guide_colorbar(ticks = FALSE)) +
  ggtitle('Percent of households with contaminated drinking water (2012/2013)')



fileName = '~/Documents/USAID/Bangladesh/Training/Training docs/BGD_midlineAs10ppb_choro.pdf'
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


# midline (mean As) -------------------------------------------------------


p = ggplot(adm2_midline, aes(x = long, 
                             y = lat)) +
  geom_polygon(aes(fill = meanAs, 
                   group = group)) +
  geom_path(aes(group = group),
            colour = grey90K,
            data = adm1_df, size = 0.2) +
  geom_text(aes(label = label),
            colour = grey90K,
            data = adm1_centroids, size = 4) +
  theme_void() + 
  coord_equal() + theme(legend.direction = 'horizontal') +
  scale_fill_gradientn(colours = brewer.pal(9, colPal),
                       na.value = grey25K,
                       limits = c(0, 375)) +
  guides(fill = guide_colorbar(ticks = FALSE)) +
  ggtitle('Percent of households with contaminated drinking water (2012/2013)')



fileName = '~/Documents/USAID/Bangladesh/Training/Training docs/BGD_midlineAsmean_choro.pdf'
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

# baseline map (50 ppb) ------------------------------------------------------------

p = ggplot(adm2_baseline, aes(x = long, 
                        y = lat)) +
  geom_polygon(aes(fill = meanAs50ppb, 
                   group = group)) +
  geom_path(aes(group = group),
            colour = grey90K,
            data = adm1_df, size = 0.2) +
  geom_text(aes(label = label),
            colour = grey90K,
            data = adm1_centroids, size = 4) +
  theme_void() + 
  coord_equal() + theme(legend.direction = 'horizontal') +
  scale_fill_gradientn(colours = brewer.pal(9, colPal),
                       na.value = grey25K,
                       labels = scales::percent,
                       limits = c(0, 0.95)) +
  guides(fill = guide_colorbar(ticks = FALSE)) +
  ggtitle('Percent of households with contaminated drinking water (1998/1999)')

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


# baseline (10ppb) --------------------------------------------------------

p = ggplot(adm2_baseline, aes(x = long, 
                              y = lat)) +
  geom_polygon(aes(fill = meanAs10ppb, 
                   group = group)) +
  geom_path(aes(group = group),
            colour = grey90K,
            data = adm1_df, size = 0.2) +
  geom_text(aes(label = label),
            colour = grey90K,
            data = adm1_centroids, size = 4) +
  theme_void() + 
  coord_equal() + theme(legend.direction = 'horizontal') +
  scale_fill_gradientn(colours = brewer.pal(9, colPal),
                       na.value = grey25K,
                       labels = scales::percent,
                       limits = c(0, 0.95)) +
  guides(fill = guide_colorbar(ticks = FALSE)) +
  ggtitle('Percent of households with contaminated drinking water (1998/1999)')

fileName = '~/Documents/USAID/Bangladesh/Training/Training docs/BGD_baselineAs10ppb_choro.pdf'
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


# baseline (mean As) ------------------------------------------------------

p = ggplot(adm2_baseline, aes(x = long, 
                              y = lat)) +
  geom_polygon(aes(fill = meanAs, 
                   group = group)) +
  geom_path(aes(group = group),
            colour = grey90K,
            data = adm1_df, size = 0.2) +
  geom_text(aes(label = label),
            colour = grey90K,
            data = adm1_centroids, size = 4) +
  theme_void() + 
  coord_equal() + theme(legend.direction = 'horizontal') +
  scale_fill_gradientn(colours = brewer.pal(9, colPal),
                       na.value = grey25K,
                       limits = c(0, 375)) +
  guides(fill = guide_colorbar(ticks = FALSE)) +
  ggtitle('Percent of households with contaminated drinking water (1998/1999)')

fileName = '~/Documents/USAID/Bangladesh/Training/Training docs/BGD_baselinemeanAs_choro.pdf'
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
                        y = lat)) +
  geom_polygon(aes(fill = chgAs50, 
                   group = group)) +
  geom_path(aes(group = group),
            colour = grey90K,
            data = adm1_df, size = 0.2) +
  geom_text(aes(label = label),
            colour = grey90K,
            data = adm1_centroids, size = 4) +
  theme_void() + 
  coord_equal() + 
  theme(legend.direction = 'horizontal') +
  guides(fill = guide_colorbar(ticks = FALSE)) +
  ggtitle('Change in percent of households with contaminated drinking water (1998 to 2013)') +
  coord_equal() + theme(legend.direction = 'horizontal') +
  scale_fill_gradientn(colours = rev(brewer.pal(9, 'RdYlBu')),
                       limits = c(-0.9, 0.9))





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

