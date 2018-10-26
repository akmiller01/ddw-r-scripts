list.of.packages <- c("data.table","readr","dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd='C:/git/ddw-r-scripts'
setwd(wd)
#This file can only be processed after fact table has been processed successfully
source("baseYearConstants.R")
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
##write.csv(oda,"output/fact.oda.csv",na="",row.names=F)
#oda_constant <- ddw('fact.oda_constant')
##write.csv(oda_constant,"output/fact.oda_constant.csv",na="",row.names=F)
oda <- read.csv('output/fact.oda.csv',na.strings="") %>% data.table
oda_constant <- read.csv('output/fact.oda_constant.csv',na.strings="") %>% data.table

# -- 1
get_oda_per_captita <- function(isConstantYearCalculation,excludingNonTransfer){
  
  population_total <- ddw('fact.population_total') %>% data.table
  
  if(excludingNonTransfer){
    
    oda_sum <- if(isConstantYearCalculation) oda_constant[,.(value=sum(value),bundle),by=c('to_di_id','year')] else oda[,.(value=sum(value),bundle),by=c('to_di_id','year')]
    
    #get sum of oda to each country
    oda_sum <- oda_sum[bundle != 'non_transfer',.(value=sum(value)),by=c('to_di_id','year')]
    setnames(oda_sum,c('to_di_id','value'),c('di_id','total_oda'))
    #merge the sum calculated with population in order  to calculate per capita values
    oda_per_captia <- merge(oda_sum,population_total,by=c('di_id','year'))
    rm(oda_sum,population_total)
    
    oda_per_captia$per_capita_value <- oda_per_captia$total_oda/oda_per_captia$value %>% round(digits=2)
    keep <- c('di_id','year','per_capita_value')
    oda_per_captia <- oda_per_captia[,..keep]
    setnames(oda_per_captia,'per_capita_value','value')
    
    of <- if(isConstantYearCalculation) 'oda_per_capita_excl_non_transfer_constant.csv' else 'oda_per_capita_excl_non_transfer.csv'
    write.csv(oda_per_captia,row.names = F,file=paste0(wd,'/output/','recipient_profile.',of))
    
  }else{
  
    oda_sum <- if(isConstantYearCalculation) oda_constant[,.(value=sum(value)),by=c('to_di_id','year')] else oda[,.(value=sum(value)),by=c('to_di_id','year')]

    setnames(oda_sum,c('to_di_id','value'),c('di_id','total_oda'))
    #merge the sum calculated with population in order  to calculate per capita values
    oda_per_captia <- merge(oda_sum,population_total,by=c('di_id','year'))
    rm(oda_sum,population_total)
    
    oda_per_captia$per_capita_value <- oda_per_captia$total_oda/oda_per_captia$value %>% round(digits=2)
    keep <- c('di_id','year','per_capita_value')
    oda_per_captia <- oda_per_captia[,..keep]
    setnames(oda_per_captia,'per_capita_value','value')
    
    of <- if(isConstantYearCalculation) 'oda_per_capita_constant.csv' else 'oda_per_capita.csv'
    write.csv(oda_per_captia,row.names = F,file=paste0(wd,'/output/','recipient_profile.',of))
  
  }
  
}

# --2
get_oda_per_percent_gdp <- function(excludingNonTransfer){
  
  gdp_usd_current <- ddw('fact.gdp_usd_current')  %>% data.table
  
  
  #get sum of oda to each country
  per_percent_gdp <- if(excludingNonTransfer) oda[bundle != 'non-transfer',.(value=sum(value)),by=c('to_di_id','year')] else oda[,.(value=sum(value)),by=c('to_di_id','year')]
  
  setnames(per_percent_gdp,c('to_di_id','value'),c('di_id','total_oda'))
  #merge the sum calculated with population in order  to calculate per capita values
  per_percent_gdp <- merge(per_percent_gdp,gdp_usd_current,by=c('di_id','year'))
  
  per_percent_gdp$per_capita_value <- per_percent_gdp$total_oda/per_percent_gdp$value %>% round(digits=4)
  keep <- c('di_id','year','per_capita_value')
  per_percent_gdp <- per_percent_gdp[,..keep]
  setnames(per_percent_gdp,'per_capita_value','value')
  
  of <- if(excludingNonTransfer)  'oda_per_percent_gdp_excluding_non_transfer.csv' else 'oda_per_percent_gdp.csv'
  write.csv(per_percent_gdp,row.names = F,file=paste0(wd,'/output/','recipient_profile.',of))
}


# -- 3
get_oda_per_poor_people <- function(isConstantYearCalculation,excludingNonTransfer){
  
  poor_people_190 <- ddw('data_series.poor_people_190')  %>% data.table

  if(!excludingNonTransfer){
    #get sum of oda to each country
    oda_sum <- if(isConstantYearCalculation) oda_constant[,.(value=sum(value),bundle),by=c('to_di_id','year')] else oda[,.(value=sum(value)),by=c('to_di_id','year')]
    
    setnames(oda_sum,c('to_di_id','value'),c('di_id','total_oda'))
    #merge the sum calculated with population in order  to calculate per capita values
    oda_per_poor_people <- merge(oda_sum,poor_people_190,by=c('di_id','year'))
    rm(oda_sum,poor_people_190)
    
    oda_per_poor_people$per_pp_value <- oda_per_poor_people$total_oda/oda_per_poor_people$value %>% round(digits=4)
    keep <- c('di_id','year','per_pp_value')
    oda_per_poor_people <- oda_per_poor_people[,..keep]
    setnames(oda_per_poor_people,'per_pp_value','value')
    
    of <- if(isConstantYearCalculation)  'oda_per_poor_people_constant.csv' else 'oda_per_poor_people_constant.csv'
    write.csv(oda_per_poor_people,row.names = F,file=paste0(wd,'/output/','recipient_profile.',of))
  
  }else{
    #Calculate values with non-transfers removed
    oda_sum <- if(isConstantYearCalculation) oda_constant[bundle != 'non-transfer',.(value=sum(value)),by=c('to_di_id','year')] else oda[bundle != 'non-transfer',.(value=sum(value)),by=c('to_di_id','year')]
    
    setnames(oda_sum,c('to_di_id','value'),c('di_id','total_oda'))
    #merge the sum calculated with population in order  to calculate per capita values
    oda_per_poor_people <- merge(oda_sum,poor_people_190,by=c('di_id','year'))
    rm(oda_sum,poor_people_190)
    
    oda_per_poor_people$per_pp_value <- oda_per_poor_people$total_oda/oda_per_poor_people$value %>% round(digits=4)
    keep <- c('di_id','year','per_pp_value')
    oda_per_poor_people <- oda_per_poor_people[,..keep]
    setnames(oda_per_poor_people,'per_pp_value','value')
    
    of <- if(isConstantYearCalculation)  'oda_per_poor_people_constant_excl_non_tranfer.csv' else 'oda_per_poor_people_constant_excl_non_tranfer.csv'
    write.csv(oda_per_poor_people,row.names = F,file=paste0(wd,'/output/','recipient_profile.',of))
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


