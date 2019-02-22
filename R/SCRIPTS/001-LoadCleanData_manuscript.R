###------Load and Clean Data-----
## @knitr LoadCleanData


# Importing the Japanese shapefile and setting the bounding box to exclude many of the outlying islands
japan <- shapefile("../R/DATA-RAW/japanprefecture/japan_prefecture_boundary")
japan_bb <- bb(japan, xlim=c(.15, .85), ylim=c(0.35, 1), relative = TRUE)

# Importing the migration data

a_2016 <- read_csv("../R/DATA-RAW/migration/migod_2016.csv") %>%
  gather(origin, migrants, 2:74) %>% # Going from Wide to Tall
  mutate(year = 2016) %>% # Setting the year to 2016
  dplyr::select(X1, origin, migrants, year) # Selecting the Destination, Origin, # of Migrants, and the Year.

a_2015 <- read_csv("../R/DATA-RAW/migration/migod_2015.csv") %>%
  gather(origin, migrants, 2:74) %>%
  mutate(year = 2015) %>%
  dplyr::select(X1, origin, migrants, year)

a_2014 <- read_csv("../R/DATA-RAW/migration/migod_2014.csv") %>%
  gather(origin, migrants, 2:74) %>%
  mutate(year = 2014) %>%
  dplyr::select(X1, origin, migrants, year)

a_2013 <- read_csv("../R/DATA-RAW/migration/migod_2013.csv") %>%
  gather(origin, migrants, 2:74) %>%
  mutate(year = 2013) %>%
  dplyr::select(X1, origin, migrants, year)

a_2012 <- read_csv("../R/DATA-RAW/migration/migod_2012.csv") %>%
  gather(origin, migrants, 2:74) %>%
  mutate(year = 2012) %>%
  dplyr::select(X1, origin, migrants, year)

a_2011 <- read_csv("../R/DATA-RAW/migration/migod_2011.csv") %>%
  gather(origin, migrants, 2:73) %>%
  mutate(year = 2011) %>%
  dplyr::select(X1, origin, migrants, year)

a_2010 <- read_csv("../R/DATA-RAW/migration/migod_2010.csv") %>%
  gather(origin, migrants, 2:73) %>%
  mutate(year = 2010) %>%
  dplyr::select(X1, origin, migrants, year)

a_2009 <- read_csv("../R/DATA-RAW/migration/migod_2009.csv") %>%
  gather(origin, migrants, 2:72) %>%
  mutate(year = 2009) %>%
  dplyr::select(X1, origin, migrants, year)

a_2008 <- read_csv("../R/DATA-RAW/migration/migod_2008.csv") %>%
  gather(origin, migrants, 2:70) %>%
  mutate(year = 2008) %>%
  dplyr::select(X1, origin, migrants, year)

a_2007 <- read_csv("../R/DATA-RAW/migration/migod_2007.csv") %>%
  gather(origin, migrants, 2:70) %>%
  mutate(year = 2007) %>%
  dplyr::select(X1, origin, migrants, year)

a_2006 <- read.xlsx("../R/DATA-RAW/migration/Copy of a012-2.xlsx", sheet =3) %>%
  gather(origin, migrants, 3:49) %>%
  mutate(year = 2006) %>%
  dplyr::select(X1, origin, migrants, year)

a_2005 <- read.xlsx("../R/DATA-RAW/migration/a012_2005.xlsx", sheet =3) %>%
  gather(origin, migrants, 3:49) %>%
  mutate(year = 2005) %>%
  dplyr::select(X1, origin, migrants, year)

a_2004 <- read.xlsx("../R/DATA-RAW/migration/a013_2004.xlsx", sheet =3) %>%
  gather(origin, migrants, 2:49) %>%
  mutate(year = 2004) %>%
  dplyr::select(X1, origin, migrants, year)

# Joining all of the migration data together.
migdata <- full_join(a_2016, a_2015) %>%
  full_join(., a_2014) %>%
  full_join(., a_2013) %>%
  full_join(., a_2012) %>%
  full_join(., a_2011) %>%
  full_join(., a_2010) %>%
  full_join(., a_2009) %>%
  full_join(., a_2008) %>%
  full_join(., a_2007) %>%
  full_join(., a_2006) %>%
  full_join(., a_2005) %>%
  full_join(., a_2004) %>%
  filter(!grepl("Total", origin)) %>% #Excluding the Totals
  filter(!grepl("Total", X1))

# Fixing some of the oddly worded variables.
migdata2 <- migdata %>%
  mutate_at(funs(as.character(.)), .vars="migrants") %>%
  mutate_at(funs(gsub(",", "", .)), .vars = "migrants") %>%
  mutate_at(funs(gsub("-ken", "", .)), .vars = "X1") %>%
  mutate_at(funs(gsub("-ken", "", .)), .vars = "origin") %>%
  mutate_at(funs(as.numeric(.)), .vars = "migrants") %>%
  mutate(year = paste0("X",as.character(year))) %>%
  #spread(year, migrants) %>%
  filter(!grepl("Total", origin)) %>%
  filter(!grepl("Total", X1)) %>%
  filter(!grepl("-shi", origin )) %>%
  filter(!grepl("-shi", X1 )) %>%
  filter(!grepl("area", origin )) %>%
  filter(!grepl("Area", origin )) %>%
  filter(!grepl("area", X1 )) %>%
  filter(!grepl("Area", X1 )) %>%
  filter(!grepl("(Thousands)", origin)) %>%
  filter(!grepl("area", X1)) %>%
  filter(!grepl("(Thousands)", X1)) %>%
  filter(!grepl("Others", X1)) %>%
  filter(!grepl("Others", origin)) %>%
  spread(year, migrants)

# Going from Wide to Tall.
migdata3 <- migdata2 %>%
  gather(year, estimate, X2004:X2016) %>%
  mutate(Year = as.Date(as.yearmon(as.numeric(substr(year, 2,5)))),
         OD = paste0(origin, X1))

# Getting the total number of migrants and the probability of migrating to each prefecture.
migdata4 <- migdata3 %>%
  group_by(origin, year) %>%
  dplyr::summarize(dest_tot = sum(estimate, na.rm=TRUE)) %>%
  left_join(., migdata3) %>%
  mutate(dest_per = estimate/dest_tot)

# Getting the origin number of migrants and the probability of migrating from each prefecture.
migdata5 <- migdata3 %>%
  group_by(X1, year) %>%
  dplyr::summarize(origin_tot = sum(estimate, na.rm=TRUE)) %>%
  left_join(., migdata3) %>%
  mutate(origin_per = estimate/origin_tot)

# Joining the Origin and Destination numbers together.
migdata5 <- left_join(migdata5, migdata4, by = c("X1", "year", "origin", "Year", "OD")) %>%
  dplyr::select(-estimate.y, estimate = estimate.x)

# Evauee Data 
# Evacuee data taken from Takahsi Oda's Snapshot of Fukushima Evacuees.
# http://tohokugeo.jp/articles/e-contents24.pdf

evacuees <- tribble(
  ~X1, ~origin, ~Evacuees,
  "Hokkaido", "Fukushima",	1335,
  "Aomori","Fukushima",	579,
  "Iwate","Fukushima",	401,
  "Miyagi","Fukushima",	1091,
  "Akita","Fukushima",	865,
  "Yamagata","Fukushima",	5518,
  "Ibaraki","Fukushima",	1905,
  "Tochigi","Fukushima",	2624,
  "Gumma","Fukushima",	2510,
  "Saitama","Fukushima",	2666,
  "Chiba","Fukushima",	3024,
  "Tokyo-to","Fukushima",	5102,
  "Kanagawa","Fukushima",	1146,
  "Niigata","Fukushima",	7219,
  "Toyama","Fukushima",	402,
  "Ishikawa","Fukushima",	424,
  "Fukui","Fukushima",	414,
  "Yamanashi","Fukushima",	723,
  "Nagano","Fukushima",	929,
  "Gifu","Fukushima",	261,
  "Shizuoka","Fukushima",	1007,
  "Aichi","Fukushima",	814,
  "Mie","Fukushima",	145,
  "Shiga","Fukushima",	254,
  "Kyoto-fu","Fukushima",	477,
  "Osaka-fu","Fukushima",	564,
  "Hyogo","Fukushima",	498,
  "Nara","Fukushima",	78,
  "Wakayama","Fukushima",	50,
  "Tottori","Fukushima",	70,
  "Shimane","Fukushima",	143,
  "Okayama","Fukushima",	132,
  "Hiroshima","Fukushima",	235,
  "Yamaguchi","Fukushima",	94,
  "Tokushima","Fukushima",	58,
  "Kagawa","Fukushima",	49,
  "Ehime","Fukushima",	149,
  "Kochi","Fukushima",	56,
  "Fukuoka","Fukushima",	299,
  "Saga","Fukushima",	60,
  "Nagasaki","Fukushima",	111,
  "Kumamoto","Fukushima",	72,
  "Oita","Fukushima",	181,
  "Miyazaki","Fukushima",	81,
  "Kagoshima","Fukushima",	128,
  "Okinawa","Fukushima",	299
)

# Calculating the evacuees probability.
evacueeper <- evacuees %>%
  mutate(evac_per = Evacuees/ sum(Evacuees))

# Setting the base year as 2010.
basemig <- migdata5 %>%
  filter(year == "X2010") %>%
  dplyr::select(origin, X1, OD, num2010 = estimate, originper2010 = origin_per, destper2010 = dest_per)

# Setting a second year as 2011.
mig2011 <- migdata5 %>%
  filter(year == "X2011") %>%
  dplyr::select(origin, X1, OD, num2011 = estimate, destper2011 = dest_per, originper2011 = origin_per)

mig2012 <- migdata5 %>%
  filter(year == "X2012") %>%
  dplyr::select(origin, X1, OD, num2012 = estimate, destper2012 = dest_per, originper2012 = origin_per)

mig2013 <- migdata5 %>%
  filter(year == "X2013") %>%
  dplyr::select(origin, X1, OD, num2013 = estimate, destper2013 = dest_per, originper2013 = origin_per)

# Joing the pre- with the per- and post-disaster periods
regdat <- left_join(basemig, mig2011) %>%
  left_join(., mig2012) %>%
  left_join(., mig2013) %>%
  na.omit

# Subsetting just the Fukushima data and joining with the Evacuees.
Fukushimadat <- regdat %>%
  filter(origin == "Fukushima") %>%
  left_join(., evacueeper) %>%
  na.omit