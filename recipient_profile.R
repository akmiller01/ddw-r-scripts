list.of.packages <- c("data.table","readr","dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

options(scipen = 999)

source("load_configs.R")
base_year <- configs$base_year
current_year <- configs$current_year
wb_data_start_year <- configs$wb_data_start_year
setwd(configs$wd)
#This file can only be processed after fact table has been processed successfully
# source("baseYearConstants.R")
source("connect.R")


#Assumptions to generate the donor profile data, the following assumption is made
#1 You have successfully generated oda (official development assistance) data and this data has been loaded to fact.oda database of your test enviroment
#2 You have successfully generated oda_constant (oda rebased to constant year; which can be set from baseYearConstants.R file)
#3 World bank data has been successfully processed and fact.population_total is populated
#4 gdp (gross domestic product has been calulated) and fact.gdp_usd_current has been generated
#5 manually generated data_series data_series.poor_people_190 should be up to date

#The following order will be followed to process the oda 

#For now this file will be pulled from db but it should be in working directory ideally

#oda <- ddw('fact.oda')
#oda_constant <- ddw('fact.oda_constant')
oda <- read.csv('output/fact.oda.csv',na.strings = "") #%>% data.table
oda_constant <- read.csv('output/fact.oda_constant.csv',na.strings = "") #%>% data.table

'%!in%' <- function(x,y)!('%in%'(x,y))

# -- 1
get_oda_per_captita <- function(isConstantYearCalculation,excludingNonTransfer){
  
  population_total <- ddw('fact.population_total') %>% data.table
  
  if(excludingNonTransfer){
    
    oda_sum <- if(isConstantYearCalculation) oda_constant[which(oda_constant$bundle %!in% c('non-transfer')),] else oda[which(oda$bundle %!in% c('non-transfer')),]
    oda_sum <- aggregate(oda_sum$value,by=list(di_id=oda_sum$to_di_id,year=oda_sum$year),FUN=sum)
    setnames(oda_sum,'x','total_oda')
    
    #merge the sum calculated with population in order  to calculate per capita values
    oda_per_captia <- merge(oda_sum,population_total,by=c('di_id','year'))
    rm(oda_sum,population_total)
    
    oda_per_captia$per_capita_value <- oda_per_captia$total_oda/oda_per_captia$value %>% round(digits=2)
    keep <- c('di_id','year','per_capita_value')
    oda_per_captia <- oda_per_captia[,keep]
    setnames(oda_per_captia,'per_capita_value','value')
    
    of <- if(isConstantYearCalculation) 'oda_per_capita_excl_non_transfer_constant.csv' else 'oda_per_capita_excl_non_transfer.csv'
    write.csv(oda_per_captia,row.names = F,na = "",file=paste0(wd,'/output/','recipient_profile.',of))
    
  }else{
  
    oda_sum <- if(isConstantYearCalculation) aggregate(oda_constant$value,by=list(di_id=oda_constant$to_di_id,year=oda_constant$year),FUN=sum) else aggregate(oda$value,by=list(di_id=oda$to_di_id,year=oda$year),FUN=sum)
    setnames(oda_sum,'x','total_oda')
    
    #merge the sum calculated with population in order  to calculate per capita values
    oda_per_captia <- merge(oda_sum,population_total,by=c('di_id','year'))
    rm(oda_sum,population_total)
    
    oda_per_captia$per_capita_value <- oda_per_captia$total_oda/oda_per_captia$value %>% round(digits=2)
    keep <- c('di_id','year','per_capita_value')
    oda_per_captia <- oda_per_captia[,keep]
    setnames(oda_per_captia,'per_capita_value','value')
    
    of <- if(isConstantYearCalculation) 'oda_per_capita_constant.csv' else 'oda_per_capita.csv'
    write.csv(oda_per_captia,row.names = F,na = "",file=paste0(wd,'/output/','recipient_profile.',of))
  
  }
  
}

# --2
get_oda_per_percent_gdp <- function(excludingNonTransfer){
  
  gdp_usd_current <- read.csv(paste(wd,'output/wb/fact.gdp-usd-current.csv',sep="/"),stringsAsFactors = F,na.strings = "")  #%>% data.table
  
  
  #get sum of oda to each country
  oda_sum <- if(excludingNonTransfer) oda[which(oda$bundle %!in% c('non-transfer')),]
  per_percent_gdp <- oda_sum <- if(excludingNonTransfer) aggregate(oda_sum$value,by=list(di_id=oda_sum$to_di_id,year=oda_sum$year),FUN=sum) 
  else aggregate(oda$value,by=list(di_id=oda$to_di_id,year=oda$year),FUN=sum)
  setnames(per_percent_gdp,'x','total_oda')
  
  #merge the sum calculated with population in order  to calculate per capita values
  per_percent_gdp <- merge(per_percent_gdp,gdp_usd_current,by=c('di_id','year'))
  
  per_percent_gdp$per_capita_value <- per_percent_gdp$total_oda/per_percent_gdp$value %>% round(digits=4)
  keep <- c('di_id','year','per_capita_value')
  per_percent_gdp <- per_percent_gdp[,keep]
  setnames(per_percent_gdp,'per_capita_value','value')
  
  of <- if(excludingNonTransfer)  'oda_per_percent_gdp_excluding_non_transfer.csv' else 'oda_per_percent_gdp.csv'
  write.csv(per_percent_gdp,row.names = F,na = "",file=paste0(wd,'/output/recipient_profile/','recipient_profile.',of))
}


# Bundle Code A
# -- 3
get_oda_per_poor_people <- function(isConstantYearCalculation,excludingNonTransfer){
  
  #poor_people_190 <- ddw('data_series.poor_people_190')  %>% data.table
  poor_people_190 <- read.csv('output/recipient_profile.pop_in_pov.csv',na.strings = "") #%>% data.table
  setnames(poor_people_190,"Poorpop.Interp","value")
  setnames(poor_people_190,"Year","year")
  
  
  if(excludingNonTransfer){
    
    #Calculate values with non-transfers removed
   
    oda_sum <- if(isConstantYearCalculation) oda_constant[which(oda_constant$bundle %!in% c('non-transfer')),] else oda[which(oda$bundle %!in% c('non-transfer')),]
    oda_sum <- aggregate(oda_sum$value,by=list(di_id=oda_sum$to_di_id,year=oda_sum$year),FUN=sum)
    setnames(oda_sum,'x','total_oda')
    
    #merge the sum calculated with population in order  to calculate per capita values
    oda_per_poor_people <- merge(oda_sum,poor_people_190,by=c('di_id','year'))
    rm(oda_sum,poor_people_190)
    
    oda_per_poor_people$per_pp_value <- oda_per_poor_people$total_oda/oda_per_poor_people$value %>% round(digits=4)
    keep <- c('di_id','year','per_pp_value')
    oda_per_poor_people <- oda_per_poor_people[,keep]
    setnames(oda_per_poor_people,'per_pp_value','value')
    
    of <- if(isConstantYearCalculation)  'oda_per_poor_people_constant_excl_non_tranfer.csv' else 'oda_per_poor_people_excl_non_tranfer.csv'
    write.csv(oda_per_poor_people,row.names = F,na = "",file=paste0(wd,'/output/','recipient_profile.',of))
    
    
  }else{
    
    
    #get sum of oda to each country
    oda_sum <- if(isConstantYearCalculation) aggregate(oda_constant$value,by=list(di_id=oda_constant$to_di_id,year=oda_constant$year),FUN=sum) else aggregate(oda_constant$value,by=list(di_id=oda_constant$to_di_id,year=oda_constant$year),FUN=sum)
    setnames(oda_sum,'x','total_oda')
 
    #merge the sum calculated with population in order  to calculate per capita values
    oda_per_poor_people_int <- merge(oda_sum,poor_people_190,by=c('di_id','year'))
    rm(oda_sum,poor_people_190)
    
    oda_per_poor_people_int$per_pp_value <- oda_per_poor_people_int$total_oda/oda_per_poor_people_int$value %>% round(digits=4)
    keep <- c('di_id','year','per_pp_value')
    oda_per_poor_people_int <- oda_per_poor_people_int[,keep]
    setnames(oda_per_poor_people_int,'per_pp_value','value')
    
    of <- if(isConstantYearCalculation)  'oda_per_poor_people_constant.csv' else 'oda_per_poor_people.csv'
    write.csv(oda_per_poor_people_int,row.names = F,na = "",file=paste0(wd,'/output/','recipient_profile.',of))
    
  }
  
}

get_oda_per_captita(T,T)
get_oda_per_captita(T,F)
get_oda_per_captita(F,T)
get_oda_per_captita(F,F)

get_oda_per_percent_gdp(T)
get_oda_per_percent_gdp(F)

get_oda_per_poor_people(T,T)
get_oda_per_poor_people(T,F)
get_oda_per_poor_people(F,T)
get_oda_per_poor_people(F,F)


