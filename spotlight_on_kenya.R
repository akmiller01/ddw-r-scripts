list.of.packages <- c("data.table","readr","dplyr","DescTools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

library("xlsx")


rawfolder <- "/Users/boss/Dev_Musings/devinit/ddw_update/rscripts/ddw-r-scripts/sok_raw/"
outputfolder <- "/Users/boss/Dev_Musings/devinit/ddw_update/rscripts/ddw-r-scripts/output_sok/"
sok_folder <- '/Users/boss/Dev_Musings/devinit/spotlight_on_kenya/Naphlin'
  
setwd("/Users/boss/Dev_Musings/devinit/ddw_update/rscripts/ddw-r-scripts");
source("baseYearConstants.R")
#source("connect.R")

setwd(rawfolder)
#ref_kenya_districts <- ddw('spotlight_on_kenya_2017.ref_kenya_district')
ref_kenya_districts <- read.csv('ref_kenya_districts.csv')


#Total Expenditures
keep <- c('district_id','year_c','expenditure')
exp_2015  <- read.xlsx("Expenditures.xlsx",sheetName ="2014-15")
exp_2015$NA. <- NULL
exp_2015 <- merge(exp_2015,ref_kenya_districts, by.x= 'County',by.y = 'name',all.x = T) %>%
  setnames(c('id','Total.Expenditure','FY'),c('district_id','expenditure','year'))
exp_2015$year_c <- NA
exp_2015$year_c[which(exp_2015$year == '2014/15')] <- 2015
exp_2015 <- exp_2015[,keep]

exp_2016 <- read.xlsx("Expenditures.xlsx",sheetName ="2015-16")
exp_2016 <- merge(exp_2016,ref_kenya_districts, by.x= 'County',by.y = 'name',all.x = T) %>% 
  setnames(c('id','Total.Expenditure','FY'),c('district_id','expenditure','year'))
exp_2016$year_c <- NA
exp_2016$year_c[which(exp_2016$year == '2015/16')] <- 2016
exp_2016 <- exp_2016[,keep]

exp_2017  <- read.xlsx("Expenditures.xlsx",sheetName ="2016-17")
exp_2017 <- merge(exp_2017,ref_kenya_districts, by.x= 'County',by.y = 'name',all.x = T) %>% 
  setnames(c('id','Total.Expenditure','FY'),c('district_id','expenditure','year'))
exp_2017$year_c <- NA
exp_2017$year_c[which(exp_2017$year == '2016/17')] <- 2017
exp_2017 <- exp_2017[,keep]

exp_2018  <- read.xlsx("Expenditures.xlsx",sheetName ="2017-18")
exp_2018 <- merge(exp_2018,ref_kenya_districts, by.x= 'County',by.y = 'name',all.x = T) %>% 
  setnames(c('id','Total.Expenditure','FY'),c('district_id','expenditure','year'))
exp_2018$year_c <- NA
exp_2018$year_c[which(exp_2018$year == '2017/18')] <- 2018
exp_2018 <- exp_2018[,keep]

expediture <- bind_rows(exp_2015,exp_2016,exp_2017,exp_2018);
setnames(expediture,'year_c','year')

#Work on agriculture
setwd(rawfolder)
agric <- read.xlsx("Agriculture expenditure.xlsx",sheetName ="Agriculture expenditure")
agric$year_c <- NA
agric$year_c[which(agric$year == '2014/15')] <- 2015
agric$year_c[which(agric$year == '2015/16')] <- 2016
agric$year_c[which(agric$year == '2016/17')] <- 2017
agric$year_c[which(agric$year == '2017/18')] <- 2018

keep <-c('id','year_c','budget_type','value')
agric <- agric[,keep]
setnames(agric,c('id','year_c'),c('district_id','year'))


agric <- merge(expediture,agric,by=c('district_id','year'))
agric$percentage_of_revenue <- (agric$value/agric$expenditure)*100

agric_compare <- ddw('spotlight_on_kenya_2017.kenya_agri_percent')

agric_compare <- merge(agric,agric_compare,by= c('district_id','year'))
agric_compare$comparison <- agric_compare$percentage_of_revenue - agric_compare$value.y


#Work on Eduction
educ <- read.xlsx("Education expenditure.xlsx",sheetName ="Education expenditure")

educ$year_c <- NA
educ$year_c[which(educ$year == '2014/15')] <- 2015
educ$year_c[which(educ$year == '2015/16')] <- 2016
educ$year_c[which(educ$year == '2016/17')] <- 2017
educ$year_c[which(educ$year == '2017/18')] <- 2018

educ <- educ[,keep]

setnames(educ,c('id','year_c'),c('district_id','year'))
educ <- merge(expediture,educ,by=c('district_id','year'))
educ$percentage_of_revenue <- (educ$value/educ$expenditure)*100

educ_compare <- ddw('spotlight_on_kenya_2017.kenya_educ_percent')
educ_compare <- merge(educ,educ_compare,by= c('district_id','year','budget_type'))
educ_compare$comparison <- educ_compare$percentage_of_revenue - educ_compare$value.y


#Compare with old

#Work on health
health <- read.xlsx("Health expenditure.xlsx",sheetName ="Health expenditure")
health$year_c <- NA
health$year_c[which(health$year == '2014/15')] <- 2015
health$year_c[which(health$year == '2015/16')] <- 2016
health$year_c[which(health$year == '2016/17')] <- 2017
health$year_c[which(health$year == '2017/18')] <- 2018

health <- health[,keep]

setnames(health,c('id','year_c'),c('district_id','year'))
health <- merge(expediture,health,by=c('district_id','year'))
health$percentage_of_revenue <- (health$value/health$expenditure)*100

#Work on water
water <- read.xlsx("Water expenditure.xlsx",sheetName ="Water expenditure")

water$year_c <- NA
water$year_c[which(water$year == '2014/15')] <- 2015
water$year_c[which(water$year == '2015/16')] <- 2016
water$year_c[which(water$year == '2016/17')] <- 2017
water$year_c[which(water$year == '2017/18')] <- 2018

water <- water[,keep]

setnames(water,c('id','year_c'),c('district_id','year'))
water <- merge(expediture,water,by=c('district_id','year'))
water$percentage_of_revenue <- (water$value/water$expenditure)*100


#Work on finance
finance <- read.xlsx("Finance_file_kenya.xlsx",sheetName ="Finance_file_kenya")
setnames(finance,'X..value.ksh..','value')
finance$NA. <- NULL

finance <- finance[,c('County','year','L1','L2','value')]

setwd(outputfolder)

keep<- c('district_id','year','budget_type','percentage_of_revenue')
write.csv(agric[,keep],'spotlight_on_kenya.kenya_agric_percent.csv',na = "",row.names = FALSE)
write.csv(educ[,keep],'spotlight_on_kenya.kenya_educ_percent.csv',na = "",row.names = FALSE)
write.csv(health[,keep],'spotlight_on_kenya.kenya_health_agric_percent.csv',na = "",row.names = FALSE)
write.csv(water[,keep],'spotlight_on_kenya.kenya_water_agric_percent.csv',na = "",row.names = FALSE)





# Work on the mapped files
setwd('/Users/boss/Dev_Musings/devinit/spotlight_on_kenya/Naphlin')
dataset <- 'mapping_data_2.xlsx';
mapdetails <- read.xlsx(dataset,sheetName ="map_details")


for(i in 1:dim(mapdetails)[1]){
  
  # Process each row generating the graph for it
  indicator_ <- as.vector(mapdetails[[1]])[i]
  
  
  print(paste0('-------- Processing indicator : ',as.character(indicator_),' --------------'))
  
  assign(indicator_,read.xlsx(dataset,sheetName = as.character(indicator_)))
  
}


for(i in 1:dim(mapdetails)[1]){
  
  # Process each row generating the graph for it
  indicator_ <- as.vector(mapdetails[[1]])[i]

  print(paste0('-------- Processing indicator : ',as.character(indicator_),' --------------'))

  tmp_value <- merge(get(indicator_),ref_kenya_districts, by.x= 'county',by.y = 'name',all.x = T)
  
  assign(paste0(indicator_,'_m'),tmp_value)

  print(indicator_)
  rm(indicator_)
}


setwd(outputfolder)


#Working on population
valid_years=c(paste0('X',2010:2015))
valid_years=append(valid_years,c('X2020','X2025'))

total_population <- reshape(total_population_m,varying=valid_years,
                            times =  valid_years,new.row.names = 1:1000,
                            v.names = "value",timevar="year",direction="long")

total_population <- total_population[,c('id','year','value')]

total_population$year <- gsub('X','',total_population$year)
setnames(total_population,'id','district_id')

write.csv(x = total_population,'kenya_total_pop.csv',na = '',row.names = F)
rm(total_population,total_population_m)

#This is the lazy way, the map file has all the column names so this could be automated

#Removed population because it needs reshaping

for(i in 1:dim(mapdetails)[1]){
  
  # Process each row generating the graph for it
  indicator_ <- as.vector(mapdetails[[1]])[i]
  column_to_keep <- as.character(as.vector(mapdetails[[4]])[i])
  
  print(paste0('-------- Creating columns and names: ',as.character(indicator_),' --------------'))
 
  tmp_value <- get(paste0(indicator_,'_m'))[,c('id',column_to_keep)]
  setnames(tmp_value,c('id',column_to_keep),c('district_id','value'))
  tmp_value$year <- 2016
  write.csv(x = tmp_value,paste0('kenya_',indicator_,'.csv'),na = '',row.names = F)
}


## Work on finance file
options(scipen=999)
setwd(sok_folder)
local_revenue <- read.csv('locally_generated_revenue.csv',na.strings = '')

local_revenue <- merge(local_revenue,ref_kenya_districts, by.x= 'county',by.y = 'name',all.x = T)
keep<- c('id','year','value')
local_revenue <- local_revenue[,keep]

setwd(sok_folder)
#Equitable revenue
equitable <- read.csv('equitable_share_revenue.csv',na.strings = '')

equitable <-  merge(equitable,ref_kenya_districts, by.x= 'county',by.y = 'name',all.x = T)
equitable <- equitable[,keep]

#Condititional Revenue revenue
conditional_revenue <- read.csv('conditional_revenue.csv',na.strings = '')

conditional_revenue <- merge(conditional_revenue,ref_kenya_districts,by.x='X...county',by.y='name',all.x =T)
conditional_revenue <- conditional_revenue[,keep]

finance <- merge(equitable,conditional_revenue,by=c('id','year'))
setnames(finance,c('value.x','value.y'),c('equitable','conditional'))  
finance <- merge(finance,local_revenue,by=c('id','year'))
finance$total_revenue <- finance$equitable +finance$conditional +finance$value


#Load donor funds and merge with revenue data to get percentage of revenue
donor_contribution <- read.csv('donations_to_county.csv',na.strings = '')
donor_contribution <- merge(donor_contribution,ref_kenya_districts,by.x='X...county',by.y='name',all.x =T)
donor_contribution <- donor_contribution[,keep]


finance <- merge(finance,donor_contribution,by=c('id','year'),all.x = T)

finance$donor_percent <- (finance$value.y/finance$total_revenue)*100
donor_percent <- finance[which(!is.na(finance$donor_percent)),c('id','year','donor_percent')]

setwd(outputfolder)
donor_percent$budget_type <- 'actual'
write.csv(x=donor_percent,file='kenya_donor_percent.csv',na ='',row.names = F)

setwd(sok_folder)

finance$local_percent <- (finance$value.x/finance$total_revenue)*100
local_percent <- finance[which(!is.na(finance$local_percent)),c('id','year','local_percent')]
local_percent$budget_type <- 'actual'

setwd(outputfolder)
write.csv(x=local_percent,file='kenya_local_percent.csv',na ='',row.names = F)

setnames(local_revenue,'value','value_ncu')
local_revenue$value <- local_revenue$value_ncu/128.4786036
write.csv(local_revenue,'kenya_igf_resources.csv',na = '',row.names = F)


