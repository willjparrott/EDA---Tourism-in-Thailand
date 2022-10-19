thailand_temprain<- read.csv("/Users/williamparrott/Desktop/Documents/R/R practice/Thailand_test_data.csv")

#for some reason there are empty rows and columns

thailand_temprain <- thailand_temprain[1:5] %>% filter(!is.na(month))

thailand_temprain <- thailand_temprain[1:5] %>% filter(!is.na(Rainfall))

thailand_temprain %>% ggplot(aes(International.tourists..2016., Temperature))+
  geom_point()

#change all to numeric

thailand_temprain$Temperature <- as.numeric(thailand_temprain$Temperature)

thailand_temprain$Rainfall <- as.numeric(thailand_temprain$Rainfall)

thailand_temprain$International.tourists..2016. <- as.numeric(gsub(",", "",thailand_temprain$International.tourists..2016. ))




cor.test(thailand_temprain$International.tourists..2016., thailand_temprain$Temperature)

cor.test(thailand_temprain$International.tourists..2016., thailand_temprain$Rainfall)

cor.test(thailand_temprain$International.tourists..2016., thailand_temprain$T.R)

temp <- thailand_temprain %>% 
  mutate(Month = factor(Month, levels = month.abb)) %>%
  ggplot(aes(x= Month, y = International.tourists..2016./1000000))+
  geom_bar(stat = "identity", fill = "#009CFF")+
  geom_line(aes(x = Month, y = (Temperature/10.5), group = 1), col = "red")+
  scale_y_continuous(breaks = seq(1,3.3, by = 0.2))+
  coord_cartesian(ylim=c(2.0, NA))+
  theme_bw()


temp <- thailand_temprain %>% 
  mutate(Month = factor(Month, levels = month.abb)) %>%
  ggplot(aes(x= Month, y = International.tourists..2016./1000000))+
  geom_bar(stat = "identity", fill = "#009CFF")+
  geom_line(aes(x = Month, y = (log10(thailand_temprain$Rainfall) +1), group = 1), col = "blue",linetype = 2)+
  scale_y_continuous(breaks = seq(1,3.3, by = 0.2), sec.axis = sec_axis(~10^(.-1), name = "Rainfall", breaks = seq(0,250,25)))+
  coord_cartesian(ylim=c(2.0, NA))+
  labs(title = "Monthly rainfall versus tourist numbers in Thailand.")+
  ylab("International tourists (millions)")

temp

rain <- thailand_temprain %>% 
  mutate(Month = factor(Month, levels = month.abb)) %>%
  ggplot(aes(x= Month, y = thailand_temprain$Rainfall +1)))+
  geom_line(aes(group = 1), col = "blue",linetype = 2)

rain

temp 

thailand_temprain$Temperature/15

+  scale_color_brewer("Blues")  



visit_2016nat <- read.csv("/Users/williamparrott/Desktop/Documents/R/R practice/2016-visitors-to-Thailand.csv")

visit_2016nat$Number <- as.numeric(gsub(",", "",visit_2016nat$Number))
visit_2016nat$X..Share <- as.numeric(visit_2016nat$X..Share)

visit_2016nat %>% ggplot(aes(Region,Number, fill = Country))+
  geom_bar(stat = "identity")


visit_2016thai

visit_2016nat %>% mutate(Year = 2016) %>% ggplot(aes(x = Region, y = Number/1000, col = Region))+
  geom_boxplot(alpha = 0.7, outlier.size = -1)+
  geom_jitter(width = 0.1, alpha = 0.4, col = "blue")+
  geom_text(data = subset(visit_2016nat, X..Share > 2.25), aes(Region, Number, label = Country))+
  scale_y_continuous(trans = "log10")+
  theme_bw()+
  labs(title = "Number of tourist by region (2016)", y = "Number of toursts (thousands)")+
  theme(legend.position = "none")

#top 10 countries by share %  - I have omitted Africa, Oceania and Middle East as none
#are top 10


top10_countries <-  visit_2016nat %>% filter(!Region %in% exclregion) %>% mutate(Year = 2016) %>% ggplot(aes(x = Region, y = Number, col = Region))+
  geom_boxplot(alpha = 0.7, outlier.size = -1)+
  geom_jitter(data = subset(visit_2016nat, X..Share > 2.5), width = 0.1, alpha = 0.4, col = "blue")+
  geom_text_repel(data = subset(visit_2016nat, X..Share > 2.5), aes(Region, Number, label = Country))+
  theme_bw()+
  labs(title = "Number of tourist by region (2016)", y = "Number of toursts (thousands)")+
  theme(legend.position = "none")

top10_countries + scale_y_continuous(trans = "log10")

exclregion <- c("Africa", "Oceania", "Middle East")

#now we do a bar-plot showing share

visit_2016nattop10 <-  visit_2016nat  %>%  slice_max(n = 10, order_by = X..Share) 

Other <- data.frame(Country = "Other", Region = "", Number = (sum(visit_2016nat$Number) -sum(visit_2016nattop10$Number)), X..Share = (100- sum(visit_2016nattop10$X..Share)))

visit_2016nattop10 <- rbind (visit_2016nattop10, Other)

top_10_share <- visit_2016nattop10 %>% mutate(Year = 2016) %>% ggplot(aes(x = reorder(Country,X..Share), y= X..Share, fill = Country))+
  geom_bar(stat = "identity", position = "stack")+
  scale_fill_brewer(palette = "Set3")

top_10_share

#Top 15 share

visit_2016nattop15 <-  visit_2016nat  %>%  slice_max(n = 15, order_by = X..Share) 

Other15 <- data.frame(Country = "Other", Region = "", Number = (sum(visit_2016nat$Number) -sum(visit_2016nattop15$Number)), X..Share = (100- sum(visit_2016nattop15$X..Share)))

visit_2016nattop15 <- rbind (visit_2016nattop15, Other15)

top_15_share <- visit_2016nattop15 %>% mutate(Year = 2016) %>% ggplot(aes(x = reorder(Country,desc(X..Share)), y= X..Share))+
  geom_bar(stat = "identity", position = "stack")+
  scale_y_continuous(breaks = seq(0,25,5))+
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "Tourist share by country (2016)", x = "Country", y = "Share (%)")

top_15_share

#can use themr to globally set theme 

ggthemr("grass")

c25 <- c(
  "dodgerblue2", "#E31A1C", # red
  "green4",
  "#6A3D9A", # purple
  "#FF7F00", # orange
  "black", "gold1",
  "skyblue2", "#FB9A99", # lt pink
  "palegreen2",
  "#CAB2D6", # lt purple
  "#FDBF6F", # lt orange
  "gray70", "khaki2",
  "maroon", "orchid1", "darkorange4")



# data from different regions 

regional <- read.csv("/Users/williamparrott/Desktop/Documents/R/R practice/tourism by region.csv")

regional$Number <- as.numeric(gsub(",", "", regional$Number))

Regional_visits <- regional %>% filter(Year == 2016) %>% group_by(Region) %>% summarise(Number = sum(Number)) %>% ggplot(aes(reorder(Region,desc(Number)), Number/10000000))+
  geom_bar(stat = "identity")+
  labs( title = "Tourists by region (2016)", x = "Region", y = "Tourists (millions)")
Regional_visits

#so I have found the data for ALL tourism, but now I want to see the difference between Thai and foreigner tourism

regional_thaiforeigner <- regional %>% mutate(Thai_foreigner =  dummy)

dummy <- ifelse(regional$Nationality == "Thai", "Thai", "Foreigner")


Regional_visitsthai <- regional_thaiforeigner %>% filter(Year == 2016) %>% group_by(Region, Thai_foreigner)%>% summarise(Number = sum(Number)) %>% 
  ggplot(aes(reorder(Region,desc(Number)), Number/10000000, fill = Thai_foreigner))+
  geom_bar(stat = "identity")+
  labs( title = "Tourists by region (2016)", x = "Region", y = "Tourists (millions)")+
  theme(legend.title=element_blank())

Regional_visitsthai

#probabilities for region and thai_vs_foreign.

regional_thai.v.foreigner <- regional_thaiforeigner %>% filter(Year == 2016) %>% group_by(Region, Thai_foreigner)%>% summarise(Number = sum(Number)) %>% pivot_wider(names_from = Region, values_from = Number)

regional_thai.v.foreigner <-  as.data.frame(regional_thai.v.foreigner)

prob_for_group <- rbind(prop.table(regional_thai.v.foreigner[1,1:7]),prop.table(regional_thai.v.foreigner[2,1:7]))

prob_for_region <- cbind(prop.table(regional_thai.v.foreigner[6]),prop.table(regional_thai.v.foreigner[1]),prop.table(regional_thai.v.foreigner[3]),prop.table(regional_thai.v.foreigner[4]),prop.table(regional_thai.v.foreigner[5]),prop.table(regional_thai.v.foreigner[7]),prop.table(regional_thai.v.foreigner[2]))

#tourism - minus thai tourists


Regional_visitsminusThai<- regional %>% filter(!Nationality == "Thai" & Year == 2016) %>% group_by(Region)%>% summarise(Number = sum(Number)) %>% 
  ggplot(aes(reorder(Region,desc(Number)), Number/10000000))+
  geom_bar(stat = "identity")+
  labs( title = "Foreign tourists by region (2016)", x = "Region", y = "Tourists (millions)")+
  theme(legend.title=element_blank())

Regional_visitsminusThai

#now with top 10 countries

Regional_visitstop10<- regional %>% filter(!Nationality == "Thai" & Year == 2016, Nationality %in% top_10) %>% group_by(Region, Nationality)%>% summarise(Number = sum(Number)) %>%
  ggplot(aes(reorder(Region,desc(Number)), Number/10000000, fill= Nationality))+
  geom_bar(stat = "identity", position = "stack")+
  labs( title = "Foreign tourists by region (2016)", x = "Region", y = "Tourists (millions)")

Regional_visitstop10


#some regions were small so not all detail could be seen.

Regional_visits_top10_NCWNE <-  regional %>% filter(!Nationality == "Thai" & Year == 2016, Nationality %in% top_10 & !Region %in% c("Bangkok", "South", "East", "North")) %>% group_by(Region, Nationality)%>% summarise(Number = sum(Number)) %>%
  ggplot(aes(reorder(Region,desc(Number)), Number/10000000, fill= Nationality))+
  geom_bar(stat = "identity", position = "stack")+
  labs( title = "Foreign tourists by region (2016)", x = "Region", y = "Tourists (millions)")

Regional_visits_top10_NCWNE

Regional_visit_China <- regional %>% filter(Nationality == "China" & Year == 2016, Nationality %in% top_10) %>% group_by(Region, Nationality)%>% summarise(Number = sum(Number)) %>%
  ggplot(aes(reorder(Region,desc(Number)), Number/10000000))+
  geom_bar(stat = "identity", position = "stack")+
  labs( title = "Chinese tourists by region (2016)", x = "Region", y = "Tourists (millions)")+
  scale_y_continuous(trans = )+
  theme(legend.position = "none")

Regional_visit_China

ggthemr("dust")

p <- regional %>% filter(!Nationality == "Thai" & Year == 2016) %>% group_by(Nationality)%>% summarise(Number = sum(Number)) %>% arrange(desc(Number))
p

Regional_visits_top10 <- regional %>% filter(Year == 2016 & Nationality %in% top_10) %>% group_by(Region) %>% summarise(Number = sum(Number)) %>% ggplot(aes(reorder(Region,desc(Number)), Number/10000000))+
  geom_bar(stat = "identity")+
  labs( title = "Tourists by region (2016)", x = "Region", y = "Tourists (millions)")
Regional_visits_top10

set_swatch(c25)

regions <- c("Bangkok", "South")





########################All visitors to Thailand 2013-2022.######################

Int.visit1322 <- read.csv("/Users/williamparrott/Desktop/Documents/R/Thailand travel data/Arrivals/Int.Arrivals.2013-2022.csv")

Int.visit1322$Number <- as.numeric(gsub(",","", Int.visit1322$Number))

Int.visit1322 <- Int.visit1322 %>% mutate(Year = as.factor(Year))
Int.visit1322 <- Int.visit1322 %>% mutate(Month = as.factor(Month))

Tourists_peryear <- Int.visit1322 %>% group_by(Year) %>% summarise(Tourists = sum(Number))

Tourists_peryear <- Tourists_peryear %>% filter(!is.na(Tourists))

mutate(Year = as.factor(Year))

Tourists_peryear %>% mutate(Year = as.factor(Year)) %>% ggplot(aes(Year, Tourists/1000000))+
  geom_bar(stat = "identity")
labs(title = "Tourism to Thailand by year", y = "Tourists (millions)")


Int.visit1322 %>% ggplot(aes(x= Month, y = Number))+
  geom_line()

#Converting month year into a date

Int.visit1322 <- Int.visit1322 %>% mutate(Date = paste0(Year, "-", Month, "-01"))

Int.visit1322$Date <- as.Date(Int.visit1322$Date)

#####2013-2022#####

Int.visit1322  %>% ggplot(aes(x=Date, y= Number/1000000))+
  geom_line(stat = "identity")+
  labs(title = "2013-May 20222 - International tourists to Thailand", y = "Tourists (millions)")

#######2019-May2022######

Int.visit1322  %>% filter(Year %in% c(2019,2020,2021,2022)) %>% ggplot(aes(x=Date, y= Number/1000000))+
  geom_bar(stat = "identity")+
  labs(title = "Jan 2019 - May 2020 - International tourists to Thailand", y = "Tourists (millions)")

#####Popular months######

Int.visit1322 %>% group_by(Month) %>% ggplot(aes(x= Month, y = Number))+
  geom_bar(stat= "identity")



#####visitor by year and nationality######

visitnat1322 <- read.csv("/Users/williamparrott/Desktop/Documents/R/Thailand travel data/Arrivals/Arrivalnationality1322.csv")

visitnat1322$Year <- as.factor(visitnat1322$Year)

#####raw number - nationality arrival top 10 (2013-2022)######

visitnat1322 %>% filter(Country %in% top_15, !is.na(Number)) %>% ggplot(aes(x = Year, y = Number, fill = Country))+
  geom_bar(stat= "identity", position = "stack")

#####raw number - nationality arrival top 10 (20121-2022)######

visitnat1322 %>% filter(Country %in% top_15, !is.na(Number), Year %in% c(2021,2022)) %>% ggplot(aes(x = Year, y = Number, fill = Country))+
  geom_bar(stat= "identity", position = "stack")

####¬ine graph######

visitnat1322 %>% filter(Country %in% top_10, !is.na(Number)) %>% ggplot(aes(x = Year, y = Number, group = Country))+
  geom_line(aes(col = Country))

#####interestingly China's number of tourists have not recovered.

visitnat1322 %>% filter(Country %in% top_10, !is.na(Number), Year %in% c(2021,2022)) %>% ggplot(aes(x = Year, y = Number, group = Country))+
  geom_line(aes(col = Country))+
  labs(title = "Tourist numbers (2021-May 2022", subtitle = "China has gone from being the largest tourist group to 8th", y = 'Tourists')



visitnat1322 %>% filter(Country %in% top_10, !is.na(Number))

#by 'fill'. Important to note that because of very low number of travellers in 2021, this have given a misleading indicator in some shares of tourist.
# For example Germany's share of international tourism looks like a 5/6x increase from 2020, but the total number is actually 5 times smaller! Same with
#2022, while the share is less than half, it is almost double in actual tourist numbers.

visitnat1322 %>% filter(Country %in% top_15 & !Country == "China", !is.na(Number)) %>% ggplot(aes(x = Year, y = Number, fill = Country))+
  geom_bar(stat= "identity", position = "fill")+
  labs(title = "Proportion of tourist by nationality", subtitle = "Countries included that have been in a yearly Top 10 twice", y = "Share")




top_15 <- c( "China","Malaysia","Korea", "Japan" , "Germany", "Russia" , "United Kingdom" ,"Australia", "France","USA","India",   "Laos", "Singapore", "Vietnam", "East Europe")       


visitnat1322 %>% filter(Country %in% top_15, !is.na(Number))

set_swatch(c25)

visitnat1322 %>% filter(Country %in% top_15, !is.na(Number)) %>% ggplot(aes(x = Year, y = X, fill = Country))+
  geom_bar(stat= "identity", position = "stack")+
  labs(title = 'Popularity of country for visit', subtitle = "Proportion of population that visited Thailand in a given year", y = "Proportion")

visitnat1322 %>% filter(!is.na(Number), Region == "East Asia", !Country %in% c("Others - East Asia", "Others - East Asia", "Myanmar")) %>% ggplot(aes(x = Country, y = X, fill = Country))+
  geom_bar(stat= "identity", position = "dodge", width = 0.5)+
  labs(title = 'Popularity of country for visit', subtitle = "Proportion of population that visited Thailand in a given year", y = "Proportion")

visitpopasia <- visitnat1322 %>% filter(!is.na(Number), Region == "East Asia", !Country %in% c("Others - East Asia", "Others - East Asia ")) %>% ggplot(aes(x = Year, y = X, group = Country))+
  geom_line(aes(col= Country))+
  labs(title = 'Popularity of country for visit', subtitle = "Proportion of population that visited Thailand in a given year", y = "Proportion")+
  facet_wrap(Country~.)+
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))


visitpopeurope <- visitnat1322 %>% filter(!is.na(Number), Region == "Europe", !Country %in% c("Others - Europe")) %>% ggplot(aes(x = Year, y = X, group = Country))+
  geom_line(aes(col= Country))+
  labs(title = 'Popularity of country for visit', subtitle = "Proportion of population that visited Thailand in a given year", y = "Proportion")+
  facet_wrap(Country~.)+
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))


Perctotaltourism <- natvsprov2 %>% group_by(Nationality) %>% summarise(Total = sum(Number)) %>% pull(Total) %>% prop.table()
Perctotaltourismrep <- rep(Perctotaltourism, times = 77)

natvsprov2 <- natvsprov2 %>% mutate(Perctotaltrsm = (Perctotaltourismrep*100))


natvsprov2 <- natvsprov2 %>% mutate(Diff = (Perc - Perctotaltrsm))



