
# import rainfall data from World Bank ------------------------------------
# Country-wide

rain = read_excel('~/Documents/USAID/Bangladesh/Training/BGD_rainfall_WB.xls')


ggplot(rain, aes(x = `\tYear`, y = `Rainfall (mm)`,
                 group = `\tYear`)) +
  stat_summary(fun.y  = 'sum', geom = 'point')

ggplot(rain, aes(x = ` Month`, y = `Rainfall (mm)`,
                 colour = `\tYear`)) +
  geom_line() +
  facet_wrap(~`\tYear`) +
  theme_xygrid()



# Weather -----------------------------------------------------------------

# Jan	11	24.5	11	1
# Feb	13.1	27.7	17	2
# Mar	17.8	33.3	24	2
# Apr	22.8	36.3	63	5
# May	24.3	35	137	9
# Jun	25.8	33.5	257	14
# Jul	26	32.1	327	19
# Aug	26	32.3	268	18
# Sep	25.6	32.2	297	15
# Oct	23.2	31.7	113	6
# Nov	17.9	29.3	17	1
# Dec	12.6	25.8	12	0
