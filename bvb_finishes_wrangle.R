library(XML)
library(tidyverse)

#Working directory
setwd("~/R/Projects")
#CSV
df <- read.csv('bvb_finishes.csv')
#Class
df$X7 <- as.character(df$X7)
#Get tournaments in a new column
df$test <- binhf::shift(df$X7, 1, 'right')
df$test[df$test==""] <- NA
df <- zoo::na.locf(df)
#Gender column
df$Gender <- ifelse(grepl("Women",df$test),'W', 
                    ifelse(grepl("Men's", df$test), 'M',NA))
#Circut Column
df$Circut <- 
  ifelse(grepl("HWN", df$test), 'AVP HWN',
  ifelse(grepl("AVP", df$test) & (grepl("of the Beach", df$test)),'AVP',
  ifelse(grepl("NVL", df$test) & (grepl("of the Beach", df$test)),'NVL', 
  ifelse(grepl("WPVA", df$test) & (grepl("of the Beach", df$test)),'WPVA',
  ifelse(grepl("FIVB", df$test) & (grepl("Under", df$test)),'FIVB AGWC',
  ifelse(grepl("SG", df$test) & (grepl("of the Beach", df$test)),'Star Games', 
  ifelse(grepl("PBVA", df$test), 'PBVA/AVP',
  ifelse(grepl("PAN AM", df$test), 'PAN AM',
  ifelse(grepl("NORCECA", df$test), 'NORCECA',
  ifelse(grepl("WPBVA", df$test), 'WPBVA', 
  ifelse(grepl("AVP", df$test) & (grepl("Young Guns", df$test)),'AVP Young Guns', 
  ifelse(grepl("AVP", df$test) & (grepl("YG", df$test)),'AVP Young Guns', 
  ifelse(grepl("CEV C&S", df$test), 'CEV C&S',
  ifelse(grepl("FIVB C&S", df$test), 'FIVB C&S',
  ifelse(grepl("p1440", df$test), 'p1440',
  ifelse(grepl("WPVA", df$test), 'WPVA',
  ifelse(grepl("VolleyHut", df$test), 'BVA',
  ifelse(grepl("AVP", df$test), 'AVP',
  ifelse(grepl("FIVB", df$test), 'FIVB',
  ifelse(grepl("NVL", df$test), 'NVL',
  ifelse(grepl("Ocean Beach Kirin Open", df$test), 'P&R',
  ifelse(grepl("Olympic Challenge Series", df$test), 'USAV',
  ifelse(grepl("Commonwealth", df$test), 'Other',
  ifelse(grepl("USAV IDQ", df$test), 'USAV IDQ',
  ifelse(grepl("Young Guns", df$test), 'AVP Young Guns',
  ifelse(grepl("Pro Beach", df$test), 'Pro Beach',
  ifelse(grepl("CEV", df$test) & (grepl("Youth", df$test)),'CEV Jr/Youth', 
  ifelse(grepl("CEV", df$test), 'CEV',
  ifelse(grepl("Style Beer Chicago Open", df$test), 'P&R',
  ifelse(grepl("Long Beach President", df$test), 'FIVB',
  ifelse(grepl("FISU", df$test), 'Other',
  ifelse(grepl("Sportswear World Championship", df$test), 'P&R',
  ifelse(grepl("Oldsmobile Alero Beach", df$test), 'USAV',
  ifelse(grepl("U.S.", df$test), 'USAV',
  ifelse(grepl("Cincinnati Players Championship", df$test), 'P&R',
  ifelse(grepl("Women's Laguna Beach Open", df$test), 'P&R',
  ifelse(grepl("Olympic Qualification", df$test), 'FIVB',
  ifelse(grepl("Wide Open", df$test), 'Wide Open',
  ifelse(grepl("BVA", df$test), 'BVA',
  ifelse(grepl("Kirin", df$test), 'P&R',
  ifelse(grepl("Women's Santa Monica Open", df$test), 'P&R',
  ifelse(grepl("Laguna Mist", df$test), 'P&R',
  ifelse(grepl("Lake Tahoe", df$test), 'P&R',
  ifelse(grepl("TBD", df$test), 'P&R',
  ifelse(grepl("Penrods Super Bowl", df$test), 'P&R',
  NA)))))))))))))))))))))))))))))))))))))))))))))
df$Circut[is.na(df$Circut)] <- 'P&R'

#filer out non tournament information
df <- df %>% 
  filter(!grepl('Summary', test))
#column 0 is not needed
df <- df[df$X!=0,]

#filter out this noise
df <- df %>% filter(X1 != "")
df <- df %>% filter(X1 != ". Grindis")
df <- df %>% filter(X0 != "Tournament:")

#class change
df$X0 <- as.character(df$X0)
df$X1 <- as.character(df$X1)
df$X2 <- as.character(df$X2)
df$X3 <- as.character(df$X3)
df$X4 <- as.character(df$X4)
df$X5 <- as.character(df$X5)
df$X6 <- as.character(df$X6)
###########################

#This section wrangles the columns. Needed to put all tournaments in same columns
##Change X2 seed
x2 <- df %>% filter(X2 == 'Seed')
x2_tournament <- unique(as.data.frame(x2$test)) %>% rename(V1 = `x2$test`)
this <- df %>% filter(test %in% x2_tournament$V1)
this <- this %>% add_column(Partner = NA, .after=3)
this <- this %>% add_column(Country = NA, .after=4)
this1 <- dplyr::select(this, 'X' = 'X', 'Finish' = 'X0', 'Player' = 'X1', 'Partner' = 'Partner', 'Country' = 'Country', 'Seed' = 'X2', 'Winnings' = 'X3', 'Points' = 'X4', 'Tournament' = 'test','Gender' = 'Gender', 'Circut' = 'Circut')

x1	<- df %>% filter(X == 1, X0 == 'Finish', X1 == 'Player', X2 == '', X3 == '', X4 == '', X5 == '', X6 == '')
x1_tourn <- unique(as.data.frame(x1$test)) %>% rename(V1 = `x1$test`)
this <- df %>% filter(test %in% x1_tourn$V1)
x1.1 <- dplyr::select(this, 'X' = 'X', 'Finish' = 'X0', 'Player' = 'X1', 'Partner' = 'X2', 'Country' = 'X3', 'Seed' = 'X4', 'Points' = 'X5', 'Winnings' = 'X6', 'Tournament' = 'test','Gender' = 'Gender', 'Circut' = 'Circut')

x2	<- df %>% filter(X == 1, X0 == 'Finish', X1 == 'Player', X2 == 'Winnings', X3 == '', X4 == '', X5 == '', X6 == '')
x2_tourn <- unique(as.data.frame(x2$test)) %>% rename(V1 = `x2$test`)
this <- df %>% filter(test %in% x2_tourn$V1)
this <- this %>% select(X, X0, X1, X3, X4, X5, X6, X2, Gender, Circut, test)
x2.1 <- dplyr::select(this,'X' = 'X','Finish' = 'X0','Player' = 'X1','Partner' = 'X3','Country' = 'X4','Seed' = 'X5','Points' = 'X6','Winnings' = 'X2','Tournament' = 'test','Gender' = 'Gender','Circut' = 'Circut')


x3	<- df %>% filter(X == 1, X0 == 'Finish', X1 == 'Player', X2 == 'Partner', X3 == '', X4 == '', X5 == '', X6 == '')
x3_tourn <- unique(as.data.frame(x3$test)) %>% rename(V1 = `x3$test`)
this <- df %>% filter(test %in% x3_tourn$V1)
this <- this %>% select(X, X0, X1, X2, X3, X4, X5, X6, X7, Gender, Circut, test)
x3.1 <- dplyr::select(this,'X' = 'X','Finish' = 'X0','Player' = 'X1','Partner' = 'X2','Country' = 'X3','Seed' = 'X4','Points' = 'X5','Winnings' = 'X6','Tournament' = 'test','Gender' = 'Gender','Circut' = 'Circut')


x4	<- df %>% filter(X == 1, X0 == 'Finish', X1 == 'Player', X2 == 'Partner', X3 == 'Winnings', X4 == '', X5 == '', X6 == '')
x4_tourn <- unique(as.data.frame(x4$test)) %>% rename(V1 = `x4$test`)
this <- df %>% filter(test %in% x4_tourn$V1)
this <- this %>% select(X, X0, X1, X2, X4, X5, X6, X7,X3, Gender, Circut, test)
x4.1 <- dplyr::select(this,'X' = 'X','Finish' = 'X0','Player' = 'X1','Partner' = 'X2','Country' = 'X4','Seed' = 'X5','Points' = 'X6','Winnings' = 'X3','Tournament' = 'test','Gender' = 'Gender','Circut' = 'Circut')

x5	<- df %>% filter(X == 1, X0 == 'Finish', X1 == 'Player', X2 == 'Partner', X3 == 'Seed', X4 == '', X5 == '', X6 == '')
x5_tourn <- unique(as.data.frame(x5$test)) %>% rename(V1 = `x5$test`)
this <- df %>% filter(test %in% x5_tourn$V1)
this <- this %>% select(X, X0, X1, X2, X4, X3, X5, X6, X7, Gender, Circut, test)
x5.1 <- dplyr::select(this,'X' = 'X','Finish' = 'X0','Player' = 'X1','Partner' = 'X2','Country' = 'X4','Seed' = 'X3','Points' = 'X6','Winnings' = 'X7','Tournament' = 'test','Gender' = 'Gender','Circut' = 'Circut')


x6	<- df %>% filter(X == 1, X0 == 'Finish', X1 == 'Player', X2 == 'Seed', X3 == 'Winnings', X4 == '', X5 == '', X6 == '')
x6_tourn <- unique(as.data.frame(x6$test)) %>% rename(V1 = `x6$test`)
this <- df %>% filter(test %in% x6_tourn$V1)
x6.1 <- dplyr::select(this,'X' = 'X','Finish' = 'X0','Player' = 'X1','Partner' = 'X7','Country' = 'X4','Seed' = 'X2','Points' = 'X5','Winnings' = 'X3','Tournament' = 'test','Gender' = 'Gender','Circut' = 'Circut')

x7	<- df %>% filter(X == 1, X0 == 'Finish', X1 == 'Player', X2 == 'Partner', X3 == 'Country', X4 == '', X5 == '', X6 == '')
x7_tourn <- unique(as.data.frame(x7$test)) %>% rename(V1 = `x7$test`)
this <- df %>% filter(test %in% x7_tourn$V1)
this <- this %>% select(X, X0, X1, X2, X3, X4, X5, X6, X7, Gender, Circut, test)
x7.1 <- dplyr::select(this,'X' = 'X','Finish' = 'X0','Player' = 'X1','Partner' = 'X2','Country' = 'X3','Seed' = 'X4','Points' = 'X5','Winnings' = 'X6','Tournament' = 'test','Gender' = 'Gender','Circut' = 'Circut')


x9	<- df %>% filter(X == 1, X0 == 'Finish', X1 == 'Player', X2 == 'Partner', X3 == 'Seed', X4 == 'Winnings', X5 == '', X6 == '')
x9_tourn <- unique(as.data.frame(x9$test)) %>% rename(V1 = `x9$test`)
this <- df %>% filter(test %in% x9_tourn$V1)
this <- this %>% select(X, X0, X1, X2, X7, X3, X5, X4, Gender, Circut, test)
x9.1 <- dplyr::select(this,'X' = 'X','Finish' = 'X0','Player' = 'X1','Partner' = 'X2','Country' = 'X7','Seed' = 'X3','Points' = 'X5','Winnings' = 'X4','Tournament' = 'test','Gender' = 'Gender','Circut' = 'Circut')


x10	<- df %>% filter(X == 1, X0 == 'Finish', X1 == 'Player', X2 == 'Partner', X3 == 'Country', X4 == 'Winnings', X5 == '', X6 == '')
x10_tourn <- unique(as.data.frame(x10$test)) %>% rename(V1 = `x10$test`)
this <- df %>% filter(test %in% x10_tourn$V1)
this <- this %>% select(X, X0, X1, X2, X3, X5,X6,X7, X4, Gender, Circut, test)
x10.1 <- dplyr::select(this,'X' = 'X','Finish' = 'X0','Player' = 'X1','Partner' = 'X2','Country' = 'X3','Seed' = 'X5','Points' = 'X6','Winnings' = 'X4','Tournament' = 'test','Gender' = 'Gender','Circut' = 'Circut')

x11	<- df %>% filter(X == 1, X0 == 'Finish', X1 == 'Player', X2 == 'Partner', X3 == 'Winnings', X4 == 'Points', X5 == '', X6 == '')
x11_tourn <- unique(as.data.frame(x11$test)) %>% rename(V1 = `x11$test`)
this <- df %>% filter(test %in% x11_tourn$V1)
this <- this %>% select(X, X0, X1, X2, X6, X7,X6,X4, X3, Gender, Circut, test)
x11.1 <- dplyr::select(this,'X' = 'X','Finish' = 'X0','Player' = 'X1','Partner' = 'X2','Country' = 'X6','Seed' = 'X7','Points' = 'X4','Winnings' = 'X3','Tournament' = 'test','Gender' = 'Gender','Circut' = 'Circut')

x12	<- df %>% filter(X == 1, X0 == 'Finish', X1 == 'Player', X2 == 'Partner', X3 == 'Seed', X4 == 'Points', X5 == '', X6 == '')
x12_tourn <- unique(as.data.frame(x12$test)) %>% rename(V1 = `x12$test`)
this <- df %>% filter(test %in% x12_tourn$V1)
this <- this %>% select(X, X0, X1, X2, X6, X7, X3, X4, X7, Gender, Circut, test)
x12.1 <- dplyr::select(this,'X' = 'X','Finish' = 'X0','Player' = 'X1','Partner' = 'X2','Country' = 'X6','Seed' = 'X3','Points' = 'X4','Winnings' = 'X7','Tournament' = 'test','Gender' = 'Gender','Circut' = 'Circut')

x13	<- df %>% filter(X == 1, X0 == 'Finish', X1 == 'Player', X2 == 'Partner', X3 == 'Country', X4 == 'Seed', X5 == '', X6 == '')
x13_tourn <- unique(as.data.frame(x13$test)) %>% rename(V1 = `x13$test`)
this <- df %>% filter(test %in% x13_tourn$V1)
x13.1 <- dplyr::select(this,'X' = 'X','Finish' = 'X0','Player' = 'X1','Partner' = 'X2','Country' = 'X3','Seed' = 'X4','Points' = 'X5','Winnings' = 'X7','Tournament' = 'test','Gender' = 'Gender','Circut' = 'Circut')

x14	<- df %>% filter(X == 1, X0 == 'Finish', X1 == 'Player', X2 == 'Partner', X3 == 'Country', X4 == 'Points', X5 == '', X6 == '')
x14_tourn <- unique(as.data.frame(x14$test)) %>% rename(V1 = `x14$test`)
this <- df %>% filter(test %in% x14_tourn$V1)
x14.1 <- dplyr::select(this,'X' = 'X','Finish' = 'X0','Player' = 'X1','Partner' = 'X2','Country' = 'X3','Seed' = 'X5','Points' = 'X4','Winnings' = 'X7','Tournament' = 'test','Gender' = 'Gender','Circut' = 'Circut')

x16	<- df %>% filter(X == 1, X0 == 'Finish', X1 == 'Player', X2 == 'Partner', X3 == 'Country', X4 == 'Winnings', X5 == 'Points', X6 == '')
x16_tourn <- unique(as.data.frame(x16$test)) %>% rename(V1 = `x16$test`)
this <- df %>% filter(test %in% x16_tourn$V1)
x16.1 <- dplyr::select(this,'X' = 'X','Finish' = 'X0','Player' = 'X1','Partner' = 'X2','Country' = 'X3','Seed' = 'X6','Points' = 'X5','Winnings' = 'X4','Tournament' = 'test','Gender' = 'Gender','Circut' = 'Circut')

x17	<- df %>% filter(X == 1, X0 == 'Finish', X1 == 'Player', X2 == 'Partner', X3 == 'Country', X4 == 'Seed', X5 == 'Winnings', X6 == '')
x17_tourn <- unique(as.data.frame(x17$test)) %>% rename(V1 = `x17$test`)
this <- df %>% filter(test %in% x17_tourn$V1)
x17.1 <- dplyr::select(this,'X' = 'X','Finish' = 'X0','Player' = 'X1','Partner' = 'X2','Country' = 'X3','Seed' = 'X4','Points' = 'X7','Winnings' = 'X5','Tournament' = 'test','Gender' = 'Gender','Circut' = 'Circut')

x18	<- df %>% filter(X == 1, X0 == 'Finish', X1 == 'Player', X2 == 'Partner', X3 == 'Country', X4 == 'Seed', X5 == 'Points', X6 == '')
x18_tourn <- unique(as.data.frame(x18$test)) %>% rename(V1 = `x18$test`)
this <- df %>% filter(test %in% x18_tourn$V1)
x18.1 <- dplyr::select(this,'X' = 'X','Finish' = 'X0','Player' = 'X1','Partner' = 'X2','Country' = 'X3','Seed' = 'X4','Points' = 'X5','Winnings' = 'X6','Tournament' = 'test','Gender' = 'Gender','Circut' = 'Circut')

x19	<- df %>% filter(X == 1, X0 == 'Finish', X1 == 'Player', X2 == 'Partner', X3 == 'Seed', X4 == 'Winnings', X5 == 'Points', X6 == '')
x19_tourn <- unique(as.data.frame(x19$test)) %>% rename(V1 = `x19$test`)
this <- df %>% filter(test %in% x19_tourn$V1)
x19.1 <- dplyr::select(this,'X' = 'X','Finish' = 'X0','Player' = 'X1','Partner' = 'X2','Country' = 'X7','Seed' = 'X3','Points' = 'X5','Winnings' = 'X4','Tournament' = 'test','Gender' = 'Gender','Circut' = 'Circut')

x21	<- df %>% filter(X == 1, X0 == 'Finish', X1 == 'Player', X2 == 'Partner', X3 == 'Country', X4 == 'Seed', X5 == 'Winnings', X6 == 'Points')
x21_tourn <- unique(as.data.frame(x21$test)) %>% rename(V1 = `x21$test`)
this <- df %>% filter(test %in% x21_tourn$V1)
x21.1 <- dplyr::select(this,'X' = 'X','Finish' = 'X0','Player' = 'X1','Partner' = 'X2','Country' = 'X3','Seed' = 'X4','Points' = 'X6','Winnings' = 'X5','Tournament' = 'test','Gender' = 'Gender','Circut' = 'Circut')

df <- bind_rows(x1.1,	x2.1,	x3.1,	x4.1,	x5.1,	x6.1,	x7.1,	x9.1,	x10.1,	x11.1,	x12.1,	x13.1,	x14.1,	x16.1,	x17.1,	x18.1,	x19.1,	x21.1)

###############################
#Creating 'Date' column
df$Month <- ifelse(grepl("January",df$Tournament),'01',
                    ifelse(grepl("February",df$Tournament),'02',
                           ifelse(grepl("March",df$Tournament),'03',
                                  ifelse(grepl("April",df$Tournament),'04',
                                         ifelse(grepl("May",df$Tournament),'05', 
                                                ifelse(grepl("June", df$Tournament), '06',
                                                       ifelse(grepl("July", df$Tournament), '07',
                                                              ifelse(grepl("August", df$Tournament), '08',
                                                                     ifelse(grepl("September", df$Tournament), '09',
                                                                            ifelse(grepl("October", df$Tournament), '10',
                                                                                   ifelse(grepl("November", df$Tournament), '11',
                                                                                          ifelse(grepl("December", df$Tournament), '12',NA))))))))))))

df$Date_old <- ifelse(grepl("January",df$Tournament),str_extract(df$Tournament, 'January .....'),
                   ifelse(grepl("February",df$Tournament),str_extract(df$Tournament, 'February .....'),
                          ifelse(grepl("March",df$Tournament),str_extract(df$Tournament, 'March .....'),
                                 ifelse(grepl("April",df$Tournament),str_extract(df$Tournament, 'April .....'),
                                        ifelse(grepl("May",df$Tournament),str_extract(df$Tournament, 'May .....'),
                                               ifelse(grepl("June", df$Tournament),str_extract(df$Tournament, 'June .....'),
                                                      ifelse(grepl("July", df$Tournament),str_extract(df$Tournament, 'July .....'),
                                                             ifelse(grepl("August", df$Tournament),str_extract(df$Tournament, 'August .....'),
                                                                    ifelse(grepl("September", df$Tournament),str_extract(df$Tournament, 'September .....'),
                                                                           ifelse(grepl("October", df$Tournament),str_extract(df$Tournament, 'October .....'),
                                                                                  ifelse(grepl("November", df$Tournament),str_extract(df$Tournament, 'November .....'),
                                                                                         ifelse(grepl("December", df$Tournament),str_extract(df$Tournament, 'December .....'),
                                                                                                NA))))))))))))


df$Year <- (str_extract_all(df$Tournament, "\\b, \\d{4}", simplify = T))
df$Year <- format(as.Date(df$Year, format=", %Y"), format = "%Y")

df$Day <- sub(".*? ", "", df$Date_old)
df$Day <- str_sub(df$Day, 1, 2)
df$Day <- str_replace(df$Day, '-', '')
df$Day <- str_replace(df$Day, ',', '')
df$Day <- str_replace(df$Day, '\\$', '')
df$Date <- paste0(df$Year, '-', df$Month, '-', df$Day)
df$Date <- as.Date(df$Date)
sst <- df
sst$rr <- ((zoo::na.locf(sst$Date)) + 7)
ss <- ifelse(is.na(sst$Date), sst$rr, sst$Date)
str(ss)
class(ss) <- 'Date'
str(ss)
as.Date(ss)
df$Date <- ss
df$Date_old <- NULL
df$Year <- NULL
df$Day <- NULL
df$Month <- NULL

#Change the finishes that have CQ in them. I want finish column to be a integar. This actually makes a little noise for the plots. Possible to filter out later on
df$Finish <- ifelse(df$Finish == 'CQ', df$X, df$Finish)
df$Finish <- as.integer(as.character(df$Finish))

#Column not needed
df <- df[df$X!=1,]

#need player and partner one more time so I can melt the data and still have partner. 
df$Player.1 <- df$Player
df$Partner.1 <- df$Partner

#Type of tournament at FIVB level column
df$Type <- 
  ifelse(grepl("Grand Slam", df$Tournament), 'Grand Slam',
         ifelse(grepl("Chelem", df$Tournament), 'Grand Slam',
                ifelse(grepl("Goodwill", df$Tournament), 'Goodwill',
                       ifelse(grepl("KATARA", df$Tournament), 'Four Star',
                              ifelse(grepl("Katara", df$Tournament), 'Four Star',
                                     ifelse(grepl("Major", df$Tournament), 'Major',
                                            ifelse(grepl("Presidents Cup", df$Tournament), 'Presidents Cup',
                                                   ifelse(grepl("FIVB", df$Tournament) & (grepl("Open", df$Tournament)),'Open',
                                                          ifelse(grepl("FIVB", df$Tournament) & (grepl("Challeng", df$Tournament)),'Challenger', 
                                                                 ifelse(grepl("FIVB", df$Tournament) & (grepl("World Series", df$Tournament)),'World Series',
                                                                        ifelse(grepl("FIVB", df$Tournament) & (grepl("World Championship", df$Tournament)),'World Championship',
                                                                               ifelse(grepl("FIVB", df$Tournament) & (grepl("Olympic", df$Tournament)),'Olympics', 
                                                                                      ifelse(grepl("FIVB", df$Tournament) & (grepl("Satellite", df$Tournament)),'Satellite',
                                                                                             ifelse(grepl("FIVB", df$Tournament) & (grepl("Continental", df$Tournament)),'Continental',
                                                                                                    ifelse(grepl("FIVB", df$Tournament) & (grepl("Junior", df$Tournament)),'Junior',
                                                                                                           ifelse(grepl("FIVB", df$Tournament) & (grepl("Youth", df$Tournament)),'Youth',
                                                                                                                  ifelse(grepl("FIVB", df$Tournament) & (grepl("World Tour", df$Tournament)),'World Tour',
                                                                                                                         ifelse(grepl("FIVB", df$Tournament) & (grepl("One Star", df$Tournament)),'One Star',
                                                                                                                                ifelse(grepl("FIVB", df$Tournament) & (grepl("Two Star", df$Tournament)),'Two Star',
                                                                                                                                       ifelse(grepl("FIVB", df$Tournament) & (grepl("Three Star", df$Tournament)),'Three Star',
                                                                                                                                              ifelse(grepl("FIVB", df$Tournament) & (grepl("Four Star", df$Tournament)),'Four Star',
                                                                                                                                                     ifelse(grepl("FIVB", df$Tournament) & (grepl("Five Star", df$Tournament)),'Five Star',
                                                                                                                                                            ifelse(grepl("Youth", df$Tournament) & (grepl("Olympic", df$Tournament)),'Youth Olympics',
                                                                                                                                                                   NA)))))))))))))))))))))))
df$Type[is.na(df$Type)] <- 'Other'

df <- df %>% rename(Circuit = Circut)


#melting
w_df <- reshape2::melt(df, id.vars = c('X','Finish','Tournament', 'Gender', 'Circut', 'Date', 'Country', 'Seed', 'Points', 'Winnings', 'Player.1', 'Partner.1', 'Type'))
w_df$Partner <- ifelse(w_df$value == w_df$Player.1, w_df$Partner.1, w_df$Player.1)
#get rid of the new columns after melt
w_df$Partner.1 <- NULL
w_df$Player.1 <- NULL
#change symbols. Not used right now, may change later
w_df$Winnings <- str_replace(w_df$Winnings, 'Â€', '€')
w_df$Points <- as.integer(as.character(w_df$Points))
#no longer need variable column
w_df$variable <- NULL
#rename column
w_df <- w_df %>% rename(Player = value)
#I want to change winnings to a integar class
w_df$Winnings <- substring(w_df$Winnings, 2)
w_df$Winnings <- str_replace(w_df$Winnings, ',', '')
w_df$Winnings <- as.integer(as.character(w_df$Winnings))
#some players are blank due to king of the beach events
w_df <- w_df %>% filter(Player != '')

#write final df
write.csv(w_df, 'beach_volley_finish_history.csv')
