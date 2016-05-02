source('~/GitHub/Misc. projects/Import shapefile/shp2csv.R')

adm2_map = shp2csv(workingDir = '~/Documents/USAID/Bangladesh/Training/BGD_Adm2/',
               layerName = 'DistrictA',
               labelVar = 'District',
               exportData = FALSE)

# Pull out the lat/lon data
adm2_df = adm2_map$df

adm2_centroids = adm2_map$centroids


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


# plot map ----------------------------------------------------------------
colPal = 'OrRd'

p = ggplot(adm2_midline, aes(x = long, 
                   y = lat, 
                   group = group, 
                   fill = meanAs50ppb)) +
  geom_polygon() +
  theme_void() + 
  coord_equal() +
  scale_fill_gradientn(colours = brewer.pal(9, colPal),
                       limits = c(0, 0.95)) +
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


p = ggplot(adm2_midline, aes(x = long, 
                             y = lat, 
                             group = group, 
                             fill = meanAs10ppb)) +
  geom_polygon() +
  theme_void() + 
  coord_equal() +
  scale_fill_gradientn(colours = brewer.pal(9, colPal),
                       limits = c(0, 0.95)) +
  theme(legend.position = 'none')

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


p = ggplot(adm2_midline, aes(x = long, 
                             y = lat, 
                             group = group, 
                             fill = meanAs)) +
  geom_polygon() +
  theme_void() + 
  coord_equal() +
  scale_fill_gradientn(colours = brewer.pal(9, colPal),
                       limits = c(0, 375)) +
  theme()

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

# baseline map ------------------------------------------------------------

p = ggplot(adm2_baseline, aes(x = long, 
                        y = lat, 
                        group = group, 
                        fill = meanAs50ppb)) +
  geom_polygon() +
  theme_void() + 
  coord_equal() +
  scale_fill_gradientn(colours = brewer.pal(9, colPal),
                       limits = c(0, 0.95)) +
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


p = ggplot(adm2_baseline, aes(x = long, 
                              y = lat, 
                              group = group, 
                              fill = meanAs10ppb)) +
  geom_polygon() +
  theme_void() + 
  coord_equal() +
  scale_fill_gradientn(colours = brewer.pal(9, colPal),
                       limits = c(0, 0.95)) +
  theme(legend.position = 'none')

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

p = ggplot(adm2_baseline, aes(x = long, 
                              y = lat, 
                              group = group, 
                              fill = meanAs)) +
  geom_polygon() +
  theme_void() + 
  coord_equal() +
  scale_fill_gradientn(colours = brewer.pal(9, colPal),
                       limits = c(0, 375)) +
  theme()

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

