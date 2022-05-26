
library(tidyverse)
library(tibble) 
library(dplyr)
library(matrixStats)
library(plyr)
library(tidyr)
library(stringr)
library(stargazer)
library(data.table)

setwd("C:\\Users\\91036\\Desktop\\725project")



## Part 1 Data

### We choose one year and one quarter (in total five quarters) data before merge and one year (in total four quarters) after merge.
### And then include 2019 4th quarter data for JetBlue-Frontier airline.

#read in all data subsetted for AS and VX airlines
data_2015_q1 <- fread(file = "AS&VX data/2015Q1.csv") %>% 
  subset(TICKET_CARRIER=="AS" | TICKET_CARRIER=="VX")

data_2015_q2 <- fread(file = "AS&VX data/2015Q2.csv") %>% 
  subset(TICKET_CARRIER=="AS" | TICKET_CARRIER=="VX")

data_2015_q3 <- fread(file = "AS&VX data/2015Q3.csv") %>% 
  subset(TICKET_CARRIER=="AS" | TICKET_CARRIER=="VX")

data_2015_q4 <- fread(file = "AS&VX data/2015Q4.csv") %>% 
  subset(TICKET_CARRIER=="AS" | TICKET_CARRIER=="VX")

data_2016_q1 <- fread(file="AS&VX data/2016Q1.csv") %>% 
  subset(TICKET_CARRIER=="AS" | TICKET_CARRIER=="VX")

data_2017_q1 <- fread(file = "AS&VX data/2017Q1.csv") %>% 
  subset(TICKET_CARRIER=="AS" | TICKET_CARRIER=="VX")

data_2017_q2 <- fread(file = "AS&VX data/2017Q2.csv") %>% 
  subset(TICKET_CARRIER=="AS" | TICKET_CARRIER=="VX")

data_2017_q3 <- fread(file = "AS&VX data/2017Q3.csv") %>% 
  subset(TICKET_CARRIER=="AS" | TICKET_CARRIER=="VX")

data_2017_q4 <- fread(file = "AS&VX data/2017Q4.csv") %>% 
  subset(TICKET_CARRIER=="AS" | TICKET_CARRIER=="VX")


data_2019_q4 <- fread(file = "AS&VX data/2019Q4.csv") %>% 
  subset(TICKET_CARRIER=="B6" | TICKET_CARRIER=="F9")

# merge
data_pre <- rbind.fill(data_2015_q1, data_2015_q2, data_2015_q3, data_2015_q4, data_2016_q1)
data_post <- rbind.fill(data_2017_q1, data_2017_q2, data_2017_q3, data_2017_q4, data_2019_q4)


#lower the column names for ease of use
colnames(data_pre) <- tolower(colnames(data_pre))
colnames(data_post) <- tolower(colnames(data_post))

# remove tickets that have a ticket carrier change

data_pre_1 <- subset(data_pre,data_pre$tk_carrier_change == 0)
data_post_1 <- subset(data_post,data_post$tk_carrier_change == 0)


# We do not filter the extreme value of the fare to ensure the concentration of our data 
# concatenate market IDs to get a market indicator
data_pre_1<- data_pre_1%>% 
  mutate(Market_Ind = paste(data_pre_1$origin_airport_id,data_pre_1$dest_airport_id,sep=""))

data_post_1<- data_post_1%>% 
  mutate(Market_Ind = paste(data_post_1$origin_airport_id,data_post_1$dest_airport_id,sep=""))

# remove outlier point
data_pre_1 <- subset(data_pre_1, data_pre_1$market_fare >= 25.0 & data_pre_1$market_fare<=2500.0)
data_post_1 <- subset(data_post_1, data_post_1$market_fare >= 25.0 & data_post_1$market_fare<=2500.0)


#eliminate small markets

data_pre_2<- data_pre_1 %>% group_by(Market_Ind)%>%dplyr::summarise(sum_passengers=sum(passengers)) %>%filter(sum_passengers*(4/365)>5)

data_pre_3 <- inner_join(data_pre_1, data_pre_2,by="Market_Ind")

data_post_2<- data_post_1 %>% group_by(Market_Ind)%>%dplyr::summarise(sum_passengers=sum(passengers)) %>%filter(sum_passengers*(4/365)>5)

data_post_3 <- inner_join(data_post_1, data_post_2,by="Market_Ind")


#Market-airline level
#need to weight the price and dist since passengers can be > 1
data_air_pre <- data_pre_3 %>% group_by(Market_Ind, ticket_carrier) %>% dplyr::summarize(ave_price=mean(market_fare, na.rm=T),tol_air_pass=sum(passengers),ave_distance=mean(market_distance, na.rm=T))

data_air_post<- data_post_3 %>% group_by(Market_Ind, ticket_carrier) %>% dplyr::summarize(ave_price=mean(market_fare, na.rm=T),tol_air_pass=sum(passengers),ave_distance=mean(market_distance, na.rm=T))



#Market level
data_mkt_pre <- data_pre_3 %>%group_by(Market_Ind, year, quarter) %>%dplyr::summarize(ave_price=mean(market_fare, na.rm=T),ave_distance=mean(market_distance, na.rm=T),ave_passengers=mean(sum_passengers, na.rm=T))

data_mkt_post <- data_post_3 %>%group_by(Market_Ind, year, quarter) %>%dplyr::summarize(ave_price=mean(market_fare, na.rm=T),ave_distance=mean(market_distance, na.rm=T),ave_passengers=mean(sum_passengers, na.rm=T))

# Next we will bring in the populations data
load("AS&VX data/populations.R")
populations_1 <- populations %>% 
  mutate(Market_Ind = paste(origin_airport_id,dest_airport_id,sep=""))

data_mkt_pre <- left_join(data_mkt_pre, populations_1, by="Market_Ind") %>% na.omit()
data_mkt_post <- left_join(data_mkt_post, populations_1, by="Market_Ind") %>% na.omit()


# merge hub data
load("AS&VX data/lookup_and_hub_r.R")

hubcodes = lookup_and_hub
hubcodes$hub_mutate = rowMaxs(as.matrix(hubcodes[,c(4:134)]))
hubcodes_1 <- hubcodes %>% select(Code, Description, airport,hub_mutate)

names(hubcodes_1)[1]=names(data_mkt_pre)[7]
hubdata_pre = merge(data_mkt_pre,hubcodes_1,names(data_mkt_pre[7]))

names(hubcodes_1)[1]=names(data_mkt_pre)[8]
hubdata_pre= merge(hubdata_pre,hubcodes_1,names(hubdata_pre[8])) 

hubdata_pre$hub_flag = rowMaxs(as.matrix(hubdata_pre[,c(12,15)]))
hubdata_pre <- select(hubdata_pre,-hub_mutate.x,-hub_mutate.y,  -airport.x, -airport.y)

hubdata_pre <- hubdata_pre %>% 
  dplyr::rename(o_city=Description.x,
                d_city=Description.y)

hubdata_pre <- hubdata_pre %>% 
  separate(o_city, c("origin_city",NA), sep = ":") %>% 
  separate(d_city, c("dest_city",NA), sep=":") 


names(hubcodes_1)[1]=names(data_mkt_post)[7]
hubdata_post = merge(data_mkt_post,hubcodes_1,names(data_mkt_post[7]))

names(hubcodes_1)[1]=names(data_mkt_post)[8]
hubdata_post= merge(hubdata_post,hubcodes_1,names(hubdata_post[8])) 

hubdata_post$hub_flag = rowMaxs(as.matrix(hubdata_post[,c(12,15)]))
hubdata_post <- select(hubdata_post,-hub_mutate.x,-hub_mutate.y, -airport.x, -airport.y)


hubdata_post <- hubdata_post %>% 
  dplyr::rename(o_city=Description.x,
                d_city=Description.y)

hubdata_post <- hubdata_post %>% 
  separate(o_city, c("origin_city",NA), sep = ":") %>% 
  separate(d_city, c("dest_city",NA), sep=":") 


# then the vacation spot data
load("AS&VX data/vacations.R")

vacationcodes = vacations #create a backup dataframe
names(vacationcodes)[1] = names(hubdata_pre[10]) 
vacationdata_pre = merge(hubdata_pre, vacationcodes, names(hubdata_pre[10])) #merge data  with original airport's data
names(vacationcodes)[1] = names(hubdata_pre)[11]
vacationdata_pre = merge(vacationdata_pre, vacationcodes, names(hubdata_pre[11])) #merge data  with destination airport's data
vacationdata_pre <- vacationdata_pre %>% mutate(vacation_flag=pmax(vacation_spot.x , vacation_spot.y)) #create the final indicator variable
vacationdata_pre <- vacationdata_pre %>% select(-vacation_spot.x, -vacation_spot.y) # remove the separate data 


names(vacationcodes)[1] = names(hubdata_post[10]) 
vacationdata_post = merge(hubdata_post, vacationcodes, names(hubdata_post[10])) 
names(vacationcodes)[1] = names(hubdata_post)[11]
vacationdata_post = merge(vacationdata_post, vacationcodes, names(hubdata_post[11])) 
vacationdata_post <- vacationdata_post %>% mutate(vacation_flag=pmax(vacation_spot.x , vacation_spot.y))  
vacationdata_post <- vacationdata_post %>% select(-vacation_spot.x, -vacation_spot.y) 

# merge income data
load("AS&VX data/data_income.R")

incomecodes = msa_income #create a backup dataframe
names(incomecodes)[1] = names(vacationdata_pre)[2]
incomedata_pre = merge(vacationdata_pre, incomecodes, names(vacationdata_pre[2])) #merge data  with original airport's data
names(incomecodes)[1] = names(vacationdata_pre)[1]
incomedata_pre = merge(incomedata_pre, incomecodes, names(vacationdata_pre[1]))#merge data  with destination airport's data
incomedata_pre <- incomedata_pre %>% mutate(income_geo_mean = sqrt(median_income.x * median_income.y)) #calculate the geometric mean of the median income of the market¡¯s endpoints
incomedata_pre <- incomedata_pre %>% select(-median_income.x, -median_income.y) # remove the separate data 

names(incomecodes)[1] = names(vacationdata_post)[2]
incomedata_post = merge(vacationdata_post, incomecodes, names(vacationdata_pre[2])) #merge data  with original airport's data
names(incomecodes)[1] = names(vacationdata_post)[1]
incomedata_post = merge(incomedata_post, incomecodes, names(vacationdata_post[1]))#merge data  with destination airport's data
incomedata_post <- incomedata_post %>% mutate(income_geo_mean = sqrt(median_income.x * median_income.y)) #calculate the geometric mean of the median income of the market¡¯s endpoints
incomedata_post <- incomedata_post %>% select(-median_income.x, -median_income.y) # remove the separate data 

# merge the slot controlled data
load("AS&VX data/slot_controlled.R")

slotcodes = slot_controlled #create a backup dataframe
names(slotcodes)[1] = names(incomedata_pre)[4]
slotdata_pre = merge(incomedata_pre, slotcodes, names(incomedata_pre[4])) #merge data  with original airport's data
names(slotcodes)[1] = names(incomedata_pre)[3]
slotdata_pre = merge(slotdata_pre, slotcodes, names(incomedata_pre[3]))#merge data  with destination airport's data
slotdata_pre <- slotdata_pre %>% mutate(slot_flag=pmax(slot_controlled.x , slot_controlled.y)) #create the final indicator variable
slotdata_pre <- slotdata_pre %>% select(-slot_controlled.x, -slot_controlled.y) # remove the separate data 

slotcodes = slot_controlled #create a backup dataframe
names(slotcodes)[1] = names(incomedata_post)[4]
slotdata_post = merge(incomedata_post, slotcodes, names(incomedata_post[4])) #merge data  with original airport's data
names(slotcodes)[1] = names(incomedata_post)[3]
slotdata_post = merge(slotdata_post, slotcodes, names(incomedata_post[3]))#merge data  with destination airport's data
slotdata_post <- slotdata_post %>% mutate(slot_flag=pmax(slot_controlled.x , slot_controlled.y)) #create the final indicator variable
slotdata_post <- slotdata_post %>% select(-slot_controlled.x, -slot_controlled.y) # remove the separate data 

# sort the data and add indicator for pre/post period
mkt_data_pre <- slotdata_pre %>% 
  arrange(origin_airport_id, dest_airport_id) %>% 
  mutate(period=0)
mkt_data_post <- slotdata_post %>% 
  arrange(origin_airport_id, dest_airport_id) %>% 
  mutate(period=1)

# stack the data (and remove NAs)!
mkt_data_all <- rbind.fill(mkt_data_pre,mkt_data_post) %>% na.omit()

# split out the full data and the 2019 data
mkt_data_counter <- mkt_data_all %>% 
  subset(year == 2019)

mkt_data_fact <- mkt_data_all %>% 
  subset(year != 2019)

# output data file
write.csv(mkt_data_counter, file="mkt_data_counter.csv")
write.csv(mkt_data_fact, file="mkt_data_fact.csv")

