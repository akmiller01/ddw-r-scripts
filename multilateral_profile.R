list.of.packages <- c("data.table","readr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

#database_connection
source("connect.R")

create_oda_oof_trend = function(){
  
  dac2 <- fread("mirrors/dac2a.csv",data.table=getOption("datatable.fread.datatable", TRUE),
                select = c('donor_code','Year','value','recipient_code','part_code','aid_type_code','data_type'))
  setnames(dac2,'Year','year');
  
  multilateral_donor <- ddw("multilateral_profile.master_multilateral_donor")
  donor_di_id_map <- ddw("dimension.oecd_donor_to_di_id_map")
  
  #Required fields and values from DAC2 for this
  
  oda_trend <- dac2[(data_type=="A" & aid_type_code==240 & part_code==1 & recipient_code==10100),
               .(donor_code,year,value=round(coalesce(value,as.numeric(0L))*10**6,2))]
  
  #This is one way of doing join on the three columns, however i prefer the step by step since
  #there is visibility
  #oda_trend <- Reduce(function(x,y)merge(x=x,y=y,by='donor_code'),list(oda_trend,multilateral_donor,donor_di_id_map))
 
   oda_trend <- merge(oda_trend,multilateral_donor,by='donor_code')[donor_type=='Multilateral',oda_oof_flow:='oda']
  
  oda_trend <- merge(oda_trend,donor_di_id_map,by='donor_code')[,c("donor_code","donor_name","donor_type"):=NULL]
  oda_trend <- oda_trend[,.(oda_oof_flow,value=sum(value)),by=.(di_id,parent,year)]
  
                                                               
 # dac2 <- dac2['aid_type_code'='A',
 #              .('donor_code','year','value','recipient_code','part_code','aid_type_code','data_type')]
  
}