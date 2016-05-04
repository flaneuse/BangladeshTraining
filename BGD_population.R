# Sources -----------------------------------------------------------------
# Data from Wiki from the BGD 2011 censuses: https://en.wikipedia.org/wiki/Barisal_Division
# 2011 data: http://203.112.218.65/WebTestApplication/userfiles/Image/National%20Reports/Union%20Statistics.pdf

# Wiki data.
# r1 = c('Barisal', 8173718,	8325666)
# r2 = c('Chittagong', 24290384,	28423019)
# r3 = c('Dhaka',	39044716 + 9864665,	47424418 + 11370000)
# # r4 = c('Mymensingh',		9864665,	11370000)
# 
# r5 = c('Rangpur', NA, 15787758)
# r6 = c('Sylhet',	 7939343,	9910219)
# r7 = c('Rajshahi', NA, 18484858) 
# r8 = c('Khulna',	14705223,	15687759)


r1 = c('Barisal'   , 8325666 )
r2 = c('Chittagong', 28423019)
r3 = c('Dhaka'   , 47424418)
r5 = c('Khulna'   , 15687759)
r6 = c('Rajshahi', 18484858)
r7 = c('Rangpur', 15787758)
r8 = c('Sylhet', 9910219 )

pop = rbind(r1, r2)
pop = rbind(pop, r3)
# pop = rbind(pop, r4)
pop = rbind(pop, r5)
pop = rbind(pop, r6)
pop = rbind(pop, r7)
pop = rbind(pop, r8)

pop = data.frame(pop) %>% 
  rename(div = X1, 
         # `2001` = X2, 
         `2011` = X2) %>% 
  gather(year, population, -div) %>% 
  mutate(population = as.numeric(population))