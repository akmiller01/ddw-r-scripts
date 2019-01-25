
# Before running this script the following dimensions from warehouse db have to be upto date
# dimension.oecd_donor
# dimension.oecd_donor_to_di_id_map

list.of.packages <- c("data.table","readr","dplyr","DescTools","stringr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)


wd <- "~/git/ddw-r-scripts"
setwd(wd)
source("baseYearConstants.R")
source("connect.R")

dac1 <- fread("~/ddw_update/mirrors/dac1.csv",na.strings="")
dac2a <- fread("~/ddw_update/mirrors/dac2a.csv",na.strings="")
dac2b <- fread("~/ddw_update/mirrors/dac2b.csv",na.strings="")
'%!in%' <- function(x,y)!('%in%'(x,y))

getSscPercentGni <- function(){
  
  #filters for ssc_percent
  donor_code_lt_ <- 20001
  donor_code_eq_ <- 20006
  part_code_ <- 1
  aid_type_code_ <- 2
  flows_ <- 1140
  amount_type_code_ <- 'A'
  
  ssc_percent <- filter(dac1,(dac1$donor_code < donor_code_lt_ | dac1$donor_code == donor_code_eq_) &
                        dac1$part_code == part_code_ &
                        dac1$aid_type_code  == aid_type_code_ &
                        dac1$flows == flows_ &
                        dac1$amount_type_code == amount_type_code_ &
                        !is.na(value) &
                        value != 0
                        )[,c('donor_code','year','value')]
  
  
  dimension_query <- 'select oecd_donor.donor_code,map.di_id from dimension.oecd_donor AS "oecd_donor"  LEFT JOIN dimension.oecd_donor_to_di_id_map AS "map" ON "oecd_donor".donor_code = "map".donor_code
  WHERE ( "oecd_donor".donor_type IN ( \'Non DAC\' )
  OR "oecd_donor".donor_code = 20006 )';
  
  donor_to_di_id_map <- dbGetQuery(con,dimension_query)
  
  ssc_percent <- merge(ssc_percent,donor_to_di_id_map,by = 'donor_code')
  ssc_percent$value <- round(ssc_percent$value, 4 )
  keep <- c('di_id','year','value')
  ssc_percent <- ssc_percent[,keep]
  
  #Write result of the filter to file
  fwrite(ssc_percent,"output/international-official-finance/ssc_percent_gni.csv")
}


#For constant year pass 'D' as datatype
getOutSscNet <- function(data_type_ = 'A',filename_='fact.out_ssc_net.csv'){
  #filters for ssc_percent
  donor_code_lt_ <- 20001
  donor_code_eq_ <- 20006
  recipient_code_ <- 10200
  part_code_ <- 1
  aid_type_code_ <- 206
  
  
  out_ssc_net <- filter(dac2a,(dac2a$donor_code < donor_code_lt_ | dac2a$donor_code == donor_code_eq_)
                        & dac2a$recipient_code == recipient_code_ 
                        & dac2a$part_code == part_code_
                        & dac2a$aid_type_code == aid_type_code_
                        & dac2a$data_type == data_type_)
  
  
  out_ssc_net <- aggregate(out_ssc_net$value,
                           by=list('donor_code'=out_ssc_net$donor_code,'year'=out_ssc_net$year),FUN=sum)
  setnames(out_ssc_net,'x','value')
  
  dimension_query <- 'select oecd_donor.donor_code,map.di_id from dimension.oecd_donor AS "oecd_donor"  LEFT JOIN dimension.oecd_donor_to_di_id_map AS "map" ON "oecd_donor".donor_code = "map".donor_code
  WHERE ( "oecd_donor".donor_type IN ( \'Non DAC\' )
  OR "oecd_donor".donor_code = 20006 )';
  
  donor_to_di_id_map <- dbGetQuery(con,dimension_query)
  
  out_ssc_net <-  merge(out_ssc_net,donor_to_di_id_map,by = 'donor_code')
  
  out_ssc_net <- aggregate(out_ssc_net$value,
                           by=list('di_id'=out_ssc_net$di_id,'year'=out_ssc_net$year),FUN=sum)
 
  setnames(out_ssc_net,'x','value') 
  out_ssc_net$value <- round(out_ssc_net$value,0) * 10**6
  
  fwrite(out_ssc_net,paste0("output/international-official-finance/",filename_))
}


# For constant year values, data_type should be 'D'
getOutOOFNet <- function(data_type_ = 'A',filename_='fact.out_oof_net.csv'){
  
  recipient_code_ <- 10100
  part_code_ <- 1
  aid_type_code_ <- 296

  
  out_oof_net <- filter(dac2b, dac2b$recipient_code == recipient_code_ & dac2b$part_code == part_code_
                        & dac2b$aid_type_code == aid_type_code_
                        & dac2b$data_type == data_type_)
  
  out_oof_net <- out_oof_net[,c('donor_code','year','value')]
  
  
  dimension_query <- 'select oecd_donor.donor_code,map.di_id from dimension.oecd_donor AS "oecd_donor"  LEFT JOIN dimension.oecd_donor_to_di_id_map AS "map" ON "oecd_donor".donor_code = "map".donor_code
  WHERE ( "oecd_donor".donor_type IN ( \'DAC\',\'Non DAC\' )
  OR "oecd_donor".donor_code != 918 )';
  
  donor_to_di_id_map <- dbGetQuery(con,dimension_query)
  
  out_oof_net <-  merge(out_oof_net,donor_to_di_id_map,by = 'donor_code')
  out_oof_net <- out_oof_net[,c('di_id','year','value')]
  
  out_oof_net$value <- round((out_oof_net$value * 10**6),0)
  
  #Write result to file
  fwrite(out_oof_net,paste0("output/international-official-finance/",filename_))
}

# For constant year values, data_type should be 'D'
getOutODANet <- function(data_type_ = 'A',filename_='fact.out_oda_net.csv'){
  
  donor_code_lteq <- 20001
  recipient_code_ <- 10200
  part_code_ <- 1
  aid_type_code_ <- 206
  
  
  out_oda_net <- filter(dac2a,dac2a$donor_code <= donor_code_lteq
                        & dac2a$recipient_code == recipient_code_ 
                        & dac2a$part_code == part_code_
                        & dac2a$aid_type_code == aid_type_code_
                        & dac2a$data_type == data_type_)
  
  
  out_oda_net <- aggregate(out_oda_net$value,
                           by=list('donor_code'=out_oda_net$donor_code,'year'=out_oda_net$year),FUN=sum)
  setnames(out_oda_net,'x','value')
  out_oda_net$value <- coalesce(out_oda_net$value,0) * 10**6
  
  dimension_query <- 'select oecd_donor.donor_code,map.di_id from dimension.oecd_donor AS "oecd_donor"  LEFT JOIN dimension.oecd_donor_to_di_id_map AS "map" ON "oecd_donor".donor_code = "map".donor_code
  WHERE ( "oecd_donor".donor_type IN ( \'DAC\' )
  OR "oecd_donor".donor_code = 20001 )';
  
  donor_to_di_id_map <- dbGetQuery(con,dimension_query)
  
  out_oda_net <-  merge(out_oda_net,donor_to_di_id_map,by = 'donor_code')
  out_oda_net <- aggregate(out_oda_net$value,
                           by=list('di_id'=out_oda_net$di_id,'year'=out_oda_net$year),FUN=sum)
  
  setnames(out_oda_net,'x','value')
  out_oda_net <- out_oda_net[,c('di_id','year','value')]
  
  #Write result to file
  fwrite(out_oda_net,paste0("output/international-official-finance/",filename_))
}


# For constant year values, data_type should be 'D'
getOutODAGross <- function(data_type_ = 'A',filename_='fact.out_oda_gross.csv'){
  
  donor_code_lteq <- 20001
  recipient_code_ <- 10200
  part_code_ <- 1
  aid_type_code_ <- 240
  
  
  out_oda_gross <- filter(dac2a,dac2a$donor_code <= donor_code_lteq
                        & dac2a$recipient_code == recipient_code_ 
                        & dac2a$part_code == part_code_
                        & dac2a$aid_type_code == aid_type_code_
                        & dac2a$data_type == data_type_
                        & !is.na(dac2a$value) &
                          dac2a$value != 0)
  
  
  out_oda_gross <- aggregate(out_oda_gross$value,
                           by=list('donor_code'=out_oda_gross$donor_code,'year'=out_oda_gross$year),FUN=sum)
  setnames(out_oda_gross,'x','value')
  out_oda_gross$value <- coalesce(out_oda_gross$value,0) * 10**6
  
  dimension_query <- 'select oecd_donor.donor_code,map.di_id from dimension.oecd_donor AS "oecd_donor"  LEFT JOIN dimension.oecd_donor_to_di_id_map AS "map" ON "oecd_donor".donor_code = "map".donor_code
  WHERE ( "oecd_donor".donor_type IN ( \'DAC\' )
  OR "oecd_donor".donor_code = 20001 )';
  
  donor_to_di_id_map <- dbGetQuery(con,dimension_query)
  
  out_oda_gross <-  merge(out_oda_gross,donor_to_di_id_map,by = 'donor_code')
  out_oda_gross <- aggregate(out_oda_gross$value,
                           by=list('di_id'=out_oda_gross$di_id,'year'=out_oda_gross$year),FUN=sum)
  
  setnames(out_oda_gross,'x','value')
  out_oda_gross <- out_oda_gross[,c('di_id','year','value')]
  
  #Write result to file
  fwrite(out_oda_gross,paste0("output/international-official-finance/",filename_))
}

# For constant year values, data_type should be 'D'
getOutDebtRelief <- function(data_type_ = 'A',filename_='fact.out_debt_relief.csv'){
  
  #First get the values of net oda without debt relief
  #filters for ssc_percent
  part_code_ <- 1
  aid_type_code_ <- 1010
  flows_ <- 1140
  amount_type_code_ <- 'A'
  
  #Get value of net oda from dac1
  net_oda_dac_1 <- filter(dac1,
                          dac1$part_code == part_code_ &
                          dac1$aid_type_code  == aid_type_code_ &
                          dac1$flows == flows_ &
                          dac1$amount_type_code == amount_type_code_ &
                          !is.na(value) &
                          value != 0
  )[,c('donor_code','donor_name','year','value')]
  
  net_oda_dac_1$value <- coalesce(net_oda_dac_1$value,0) * 10**6
  
  aid_type_code_ <- 1600
  
  # Get value of debt relief from dac1
  debt_relief_dac_1 <-  filter(dac1,
                          dac1$part_code == part_code_ &
                            dac1$aid_type_code  == aid_type_code_ &
                            dac1$flows == flows_ &
                            dac1$amount_type_code == amount_type_code_ &
                            !is.na(value) &
                            value != 0)[,c('donor_code','year','value')]
  
  
  debt_relief_dac_1$value <- coalesce(debt_relief_dac_1$value,0) * 10**6
  
  dimension_query <- 'select * from  dimension.oecd_donor_to_di_id_map';
  
  donor_to_di_id_map <- dbGetQuery(con,dimension_query)
  
  net_oda_excl_debt_relief <- merge(net_oda_dac_1,debt_relief_dac_1,by=c('donor_code','year'),all.x = T)
  
  #GEt actual net oda excluding debt relief by subtracting debt relief from net oda
  net_oda_excl_debt_relief$value <- (coalesce(net_oda_excl_debt_relief$value.x,0) - coalesce(net_oda_excl_debt_relief$value.y,0)) %>% round(0)
  net_oda_excl_debt_relief <- merge(net_oda_excl_debt_relief,donor_to_di_id_map,by='donor_code',all.x = T)
  net_oda_excl_debt_relief$aid_type <- 'out-oda-net-excl-debt-relief'
  net_oda_excl_debt_relief <- net_oda_excl_debt_relief[,c('di_id','aid_type','year','value')]
  
  
  #Next get the value of debt_relief from dac 1 with debt relief
  debt_relief_dac_1 <- merge(debt_relief_dac_1,donor_to_di_id_map,by='donor_code',all.x = T)
  debt_relief_dac_1$aid_type <- 'out-debt-relief'
  debt_relief_dac_1 <- debt_relief_dac_1[,c('di_id','aid_type','year','value')]
  
  out_debt_relief <- rbind(net_oda_excl_debt_relief,debt_relief_dac_1)
  
  #Write result of the filter to file
  fwrite(out_debt_relief,paste0("output/international-official-finance/",filename_))
  
}

odaPercentGni <- function(){
  
  donor_code_ <- 20001
  part_code_ <- 1
  aid_type_code_ <- 2
  flows_ <- 1140
  amount_type_code_ <- 'A'
  
  
  oda_percent_gni <- filter(dac1,dac1$donor_code <= donor_code_ &
                          dac1$part_code == part_code_ &
                          dac1$aid_type_code  == aid_type_code_ &
                          dac1$flows == flows_ &
                          dac1$amount_type_code == amount_type_code_ &
                          !is.na(value) &
                          value != 0
  )[,c('donor_code','year','value')]
  
  
  dimension_query <- 'select * from  dimension.oecd_donor_to_di_id_map';
  
  donor_to_di_id_map <- dbGetQuery(con,dimension_query)
  
  oda_percent_gni <- merge(oda_percent_gni,donor_to_di_id_map,by='donor_code',all.x = T) 
  oda_percent_gni <- oda_percent_gni[,c('di_id','year','value')]
  
  #Write output to file
  
  fwrite(oda_percent_gni,"output/international-official-finance/oda_percent_gni.csv")
  
}


odaToLdcsPctGni <- function(){
  
  #From oda_2a
  donor_code_ <- 20001
  recipient_code_ <- 10016
  part_code_ <- 1
  aid_type_code_ <- c(106,206)
  data_type_ = 'A'
  
  # Get the donor's total net ODA to all the LDCs ((recipient_code, recipient_name) = (10016, 'LDCs, Total'))
  oda_to_ldcs <- filter(dac2a,dac2a$donor_code < donor_code_
                          & dac2a$recipient_code == recipient_code_ 
                          & dac2a$part_code == part_code_
                          & dac2a$aid_type_code %in% aid_type_code_
                          & dac2a$data_type == data_type_
                          & !is.na(value) &
                            value != 0)
  
  oda_to_ldcs$value <- coalesce(oda_to_ldcs$value) * 10**6
  
  oda_to_ldcs <- aggregate(oda_to_ldcs$value,by=list('donor_code'=oda_to_ldcs$donor_code,'year'=oda_to_ldcs$year),FUN=sum)
  setnames(oda_to_ldcs,'x','value')
  
  # The donor's GNI from DAC 1
  
  part_code_ <- 1
  aid_type_code_ <- 1
  flows_ <- 1140
  
  donor_gni <-  filter(dac1,dac1$part_code == part_code_ &
                                 dac1$aid_type_code  == aid_type_code_ &
                                 dac1$flows == flows_ &
                                 dac1$amount_type_code == data_type_ 
                                )[,c('donor_code','year','value')]
  
  donor_gni$value <- coalesce(donor_gni$value,0) * 10**6

  
  oecd_donor_di_id_map_q <- 'select donor_code, di_id from  dimension.oecd_donor_to_di_id_map';
  oecd_donor_q <- 'SELECT donor_code, donor_type FROM dimension.oecd_donor'
  
  
  oecd_donor_di_id_map <- dbGetQuery(con,oecd_donor_di_id_map_q)
  oecd_donor <- dbGetQuery(con,oecd_donor_q)
  
  oda_to_ldcs <- merge(oda_to_ldcs,donor_gni,by=c('donor_code','year'))
  oda_to_ldcs <- merge(oda_to_ldcs,oecd_donor_di_id_map,by='donor_code')
  oda_to_ldcs <- merge(oda_to_ldcs,oecd_donor,by='donor_code')
  
  oda_to_ldcs <- filter(oda_to_ldcs,oda_to_ldcs$donor_type == 'DAC' & oda_to_ldcs$donor_code != 918)
  
  #calculate actual value
  
  oda_to_ldcs$value <- (oda_to_ldcs$value.x / oda_to_ldcs$value.y)*100 %>% round(2)
  oda_to_ldcs <- oda_to_ldcs[,c('di_id','year','value')]
  
  #Write result to file
  fwrite(oda_to_ldcs,"output/international-official-finance/oda_to_ldcs_percent_gni.csv")
  
}


getInODANet <- function(data_type_ = 'A',file_name_ = 'fact.in_oda_net.csv'){
  #filters for ssc_percent
  donor_code_lte_ <- 1601
  recipient_code_lt_ <- 900
  recipient_code_eq_ <- 9998
  recipient_code_ni_ <-  c( 237, 650, 752, 878 )
  recipient_name_ni_ <- ' Unspecified|Other '
  part_code_ <- 1
  aid_type_code_ <- 206
  
  
  in_oda_net <- dac2a[which(!grepl(recipient_name_ni_,dac2a$recipient_name,ignore.case  = FALSE)),]
  
  in_oda_net <- filter(in_oda_net, (in_oda_net$recipient_code < recipient_code_lt_ | 
                                      in_oda_net$recipient_code == recipient_code_eq_)
                     & in_oda_net$recipient_code %!in% recipient_code_ni_
                     & in_oda_net$donor_code <= donor_code_lte_
                     & in_oda_net$part_code == part_code_ 
                     & in_oda_net$aid_type_code == aid_type_code_ & 
                       in_oda_net$data_type == data_type_)
                          
                            
  in_oda_net$value = coalesce(in_oda_net$value,0)
  in_oda_net <- aggregate(in_oda_net$value,by=list('donor_code'=in_oda_net$donor_code,
                                                   'recipient_code'=in_oda_net$recipient_code,
                                                   'year'=in_oda_net$year),FUN=sum)
  
  setnames(in_oda_net,'x','value')
  
  dimension_query <- 'Select * from dimension.oecd_recipient_to_di_id_map'
   recipient_to_di_id_map <- dbGetQuery(con,dimension_query)
  dimension_query <- 'select * from dimension.oecd_donor'
  oecd_donor <- dbGetQuery(con,dimension_query)
  
  in_oda_net <- merge(in_oda_net,oecd_donor,by='donor_code',all.x=T)
  in_oda_net <- merge(in_oda_net,recipient_to_di_id_map,by='recipient_code',all.x = T)
  in_oda_net <- filter(in_oda_net,in_oda_net$donor_type %in% c( 'DAC', 'Multilateral' ))
  
  in_oda_net <- aggregate(in_oda_net$value,by=list('di_id'=in_oda_net$di_id,'year'=in_oda_net$year),FUN=sum)
  setnames(in_oda_net,'x','value')
  in_oda_net$value <- in_oda_net$value * 10**6
  
  #Swrite out output
  fwrite(in_oda_net,paste0("output/international-official-finance/",file_name_))
  
}



getInODAGross <- function(data_type_='D',file_name_ = 'fact.in_oda_gross.csv'){
  #filters for ssc_percent
  donor_code_lte_ <- 1601
  recipient_code_lt_ <- 900
  recipient_code_eq_ <- 9998
  recipient_code_ni_ <-  c( 237, 650, 752, 878 )
  recipient_name_ni_ <- ' Unspecified|Other '
  part_code_ <- 1
  aid_type_code_ <- 240
  
  
  in_oda_net <- dac2a[which(!grepl(recipient_name_ni_,dac2a$recipient_name,ignore.case  = FALSE)),]
  
  in_oda_net <- filter(in_oda_net, (in_oda_net$recipient_code < recipient_code_lt_ |
                                      in_oda_net$recipient_code == recipient_code_eq_)
                       & in_oda_net$recipient_code %!in% recipient_code_ni_
                       & in_oda_net$donor_code <= donor_code_lte_
                       & in_oda_net$part_code == part_code_ 
                       & in_oda_net$aid_type_code == aid_type_code_ 
                       & in_oda_net$data_type == data_type_
                       )
  
  
  in_oda_net$value = coalesce(in_oda_net$value,0)
  in_oda_net <- aggregate(in_oda_net$value,by=list('donor_code'=in_oda_net$donor_code,
                                                   'recipient_code'=in_oda_net$recipient_code,
                                                   'year'=in_oda_net$year),FUN=sum)
  
  setnames(in_oda_net,'x','value')
  
  dimension_query <- 'Select * from dimension.oecd_recipient_to_di_id_map'
  recipient_to_di_id_map <- dbGetQuery(con,dimension_query)
  dimension_query <- 'select * from dimension.oecd_donor'
  oecd_donor <- dbGetQuery(con,dimension_query)
  
  in_oda_net <- merge(in_oda_net,oecd_donor,by='donor_code',all.x=T)
  in_oda_net <- merge(in_oda_net,recipient_to_di_id_map,by='recipient_code',all.x = T)
  in_oda_net <- filter(in_oda_net,in_oda_net$donor_type %in% c( 'DAC', 'Multilateral' ))
  
  in_oda_net <- aggregate(in_oda_net$value,by=list('di_id'=in_oda_net$di_id,'year'=in_oda_net$year),FUN=sum)
  setnames(in_oda_net,'x','value')
  in_oda_net$value <- in_oda_net$value * 10**6
  
  #Swrite out output
  fwrite(in_oda_net,paste0('output/international-official-finance/',file_name_))
  
}

getInOOFGross <- function(data_type_='A',file_name_='fact.in_oof_gross.csv'){
  
  donor_code_ <- c(20001 # DAC Countries, Total
                   , 20002 # Multilateral, Total
                   , 20006) #Non-DAC Countries, Total
  recipient_code_lt_ <- 10001
  recipient_code_ni_ <- c(
    79 # CEEC/NIS Unspecified
    , 98 # Other Multilateral (Part II)
    , 101 # CEEC Unspecified
    , 102 # NIS Unspecified
    , 105 # MADCT Unspecified
    , 237 # East African Community
    , 901 # IBRD
    , 909 # IDB
    , 910 # CABEI
    , 913 # Af. D B
    , 915 # Asian Dev. Bank
    , 989 # Other Multilateral
    , 990 # EBRD
    , 996 # CEC FOR CEEC/NIS
    , 752 # Mekong Delta Project
  )
  
  part_code_ <- 1
  aid_type_code_ <- c( 204, 292 )
  
  
  in_oof_gross <- dac2a[which(dac2a$recipient_code < recipient_code_lt_ & dac2a$recipient_code %!in% recipient_code_ni_),]
  
  in_oof_gross <- filter( in_oof_gross,in_oof_gross$donor_code %in% donor_code_
                       & in_oof_gross$part_code == part_code_ 
                       & in_oof_gross$aid_type_code %in% aid_type_code_ & 
                         in_oof_gross$data_type == data_type_
                       & !is.na(in_oof_gross$value) &
                         in_oof_gross$value != 0)[,c('recipient_code','year','value')]
  
  in_oof_gross$value <- in_oof_gross$value * 10**6
  
  
  dimension_query <- 'Select * from dimension.oecd_recipient_to_di_id_map'
  recipient_to_di_id_map <- dbGetQuery(con,dimension_query)
  
  in_oof_gross <- merge(in_oof_gross,recipient_to_di_id_map,by='recipient_code')
  
  in_oof_gross <- aggregate(in_oof_gross$value,by=list('di_id'=in_oof_gross$di_id,'year'=in_oof_gross$year),FUN=sum)
  setnames(in_oof_gross,'x','value')
  
  #Write out value
  fwrite(in_oof_gross,paste0('output/international-official-finance/',file_name_))
}

getSscPercentGni()
getOutSscNet()
getOutSscNet('D','fact.out_ssc_net_constant.csv')
getOutOOFNet()
getOutOOFNet('D','fact.out_oof_net_constant.csv')
getOutODANet()
getOutODANet('D','fact.out_oda_net_constant.csv')
getOutODAGross()
getOutODAGross('D','fact.out_oda_gross_constant.csv')
getOutDebtRelief()
getOutDebtRelief('D','fact.out_debt_relief_constant.csv')
odaPercentGni()
odaToLdcsPctGni()
getInODANet()
getInODANet('D','fact.in_oda_net_constant.csv')
getInODAGross()
getInODAGross('D','fact.in_oda_gross_constant.csv')
getInOOFGross()
getInOOFGross('D','fact.in_oof_gross_constant.csv')



compare <- fread('output/international-official-finance/fact.in_oda_gross_constant.csv',na.strings="")

