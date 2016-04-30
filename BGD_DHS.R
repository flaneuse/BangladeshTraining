
# Bangladesh DHS ----------------------------------------------------------

apiKey = 'USAAID-405632'
indicator = c('CM_ECMT_C_NNR',
              'CN_NUTS_C_HA2',
'CM_ECMT_C_PNR',
'CM_HRFB_C_NON','CM_HRFB_C_FOB',
'CM_HRFB_C_A18','CM_HRFB_C_A34',
'CM_HRFB_C_B24','CM_HRFB_C_BO3',
'CM_HRFB_C_SNG','CM_HRFB_C_A1B',
'CM_HRFB_C_A2B','CM_HRFB_C_ABO',
'CM_HRFB_C_A3B','CM_HRFB_C_BIO',
'CM_HRFB_C_MLT','CM_HRFB_C_ANY',
'CM_ECMT_C_IMR','CM_ECMT_C_CMR','CM_ECMT_C_U5M','CM_ECMR_C_NNR','CM_ECMR_C_PNR', 'CM_HRFB_C_TOT','CM_HRFB_C_NON',
'CM_ECMR_C_IMR','CM_ECMR_C_CMR','CM_ECMR_C_U5M','CM_PNMR_C_NSB','CM_PNMR_C_NEN', 'RH_ANCP_W_NON', 'RH_ANCP_W_SKP',
'CM_PNMR_C_PMR','CH_ARIS_C_ADV','CH_ARIS_C_ABI','CH_ARIS_C_NM2','CH_ARIS_C_UN2','CH_DIAR_C_DIA','RH_DELP_C_DHF')


countries = 'BD'
years = paste0(seq(1998,2015), collapse = ',')

subnatl = NULL

for (i in seq_along(indicator)) {
  print(i)
  
  temp = loadDHS(breakdown = 'all', indicators = indicator[i], countries = countries, 
                 years = years, apiKey = apiKey, numResults = 5000)
  
  subnatl = rbind(temp, subnatl)  
}

bgDHS = subnatl %>% 
  filter(SurveyYear == 2014,
         !(CharacteristicLabel %in% c('Chittagong/Sylhet', 'Rajshahi/Rangpur'))) # Removing old groupings of areas


bgDHS2 = subnatl %>% 
  filter(
         !(CharacteristicLabel %in% c('Chittagong/Sylhet'))) # Removing old groupings of areas


# Mortality rates -------------------------------------------------------------
bgFiltered = bgDHS %>% 
  filter(IndicatorId %in% c('CM_ECMR_C_CMR',  'CM_ECMR_C_NNR', 'CM_ECMR_C_PNR'),
         CharacteristicCategory == 'Region') # Weighted # children in the last 5 y.

kable(bgFiltered %>% 
        group_by(Indicator) %>% 
        select(Indicator, CharacteristicLabel, Value) %>% 
        arrange(desc(Value)))


bgFiltered = bgDHS %>% 
  filter(IndicatorId %in% c('CM_ECMR_C_NNR')) # Weighted # children in the last 5 y.

kable(bgFiltered %>% 
        group_by(CharacteristicCategory) %>% 
        select(CharacteristicCategory, CharacteristicLabel, Value) %>% 
        arrange(desc(Value)))



# Prematurity / Early births ---------------------------------------------
bgFiltered = bgDHS %>% 
  filter(IndicatorId %in% c('CM_HRFB_C_TOT', 'CM_HRFB_C_NON', 'RH_ANCP_W_NON'),
         CharacteristicCategory == 'Region') # Weighted # children in the last 5 y.

kable(bgFiltered %>% 
        group_by(Indicator) %>% 
        select(Indicator, CharacteristicLabel, Value) %>% 
        arrange(desc(Value)))

bgFiltered = bgDHS %>% 
  filter(IndicatorId %in% c('RH_ANCP_W_NON')) # no antenatal care

kable(bgFiltered %>% 
        group_by(CharacteristicCategory) %>% 
        select(CharacteristicCategory, CharacteristicLabel, Value) %>% 
        arrange(desc(Value))) 

bgFiltered = bgDHS %>% 
  filter(IndicatorId %in% c('CM_HRFB_C_NON', 'CM_HRFB_C_B24', 'CM_HRFB_C_A34'), # high-risk birhts
CharacteristicCategory == 'Region') 

kable(bgFiltered %>% 
        group_by(Indicator) %>% 
        select(Indicator, CharacteristicLabel, Value) %>% 
        arrange(desc(Value)))

# Respiratory infections --------------------------------------------------
bgFiltered = bgDHS %>% 
  filter(IndicatorId %in% c('CH_ARIS_C_NM2', 'CH_ARIS_C_ADV'),
         ByVariableLabel == 'Five years preceding the survey',
         CharacteristicCategory == 'Region') # Weighted # children in the last 5 y.

kable(bgFiltered %>% 
  group_by(Indicator) %>% 
  select(Indicator, CharacteristicLabel, Value) %>% 
  arrange(desc(Value)))


# Stunting ----------------------------------------------------------------
bgStunted = bgDHS2 %>% 
  filter(IndicatorId %in% c('CN_NUTS_C_HA2'),
         CharacteristicCategory == 'Region') # Weighted # children in the last 5 y.

write.csv(bgStunted, '~/Documents/USAID/Bangladesh/Training/dataout/BGD_DHSstunting2014.csv')


ggplot(bgStunted, aes(x = SurveyYear, y = Value, 
                      group = CharacteristicLabel,
                      label = CharacteristicLabel)) +
  geom_line(colour = grey40K) +
  geom_point(colour = grey60K,
            size = 2,
            data = bgStunted %>% filter(SurveyYear == 2014)) +
  geom_point(colour = 'red',
             size = 2,
             data = bgStunted %>% filter(SurveyYear == 2014,
                                         CharacteristicLabel == '..Sylhet')) +
  geom_text(colour = grey60K,
            family = 'Segoe UI Light', 
            size = 4,
            nudge_x = 0.2,
            hjust = 0,
            data = bgStunted %>% filter(SurveyYear == 2014)) +

  geom_text(colour = 'red',
            family = 'Segoe UI Light', 
            size = 4,
            nudge_x = 0.2,
            hjust = 0,
            data = bgStunted %>% filter(SurveyYear == 2014,
                                        CharacteristicLabel == '..Sylhet')) +
  geom_line(colour = 'red',
            data = bgStunted %>% filter(CharacteristicLabel == '..Sylhet')) +
  theme_xygridlight() +
  coord_cartesian(xlim = c(2004, 2016)) +
  ggtitle('Stunting in Sylhet has increased in the past 5 years')