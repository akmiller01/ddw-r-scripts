list.of.packages <- c("data.table","readr","dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd='/Users/boss/Dev_Musings/devinit/ddw_update/rscripts/ddw-r-scripts'
setwd(wd)
#This file can only be processed after fact table has been processed successfully
source("baseYearConstants.R")
source("connect.R")

# You will need to obtain the DAC regional and unspecified total
# breakdowns which are available under the recipient heading:
#   
# Europe, Total 
# North of Sahara, Total
# South of Sahara, Total
# North & Central America, Total
# South America, Total
# Far East Asia, Total
# South & Central Asia, Total
# Middle East, Total
# Oceania, Total
# Developing Countries Unspecified

#Recipient grouping
#These have their figures directly from DAC2A
recipient_codes <- c(10002, 10003, 10005, 10006, 10008, 10009, 10010, 10011, 10012, 9998);

#Aid type codes that we will be working with from dac1 for multilateral
aid_types <- c(2101, 2102, 2103, 2104, 2105, 2106, 2107, 2108)

#codes are for Africa,Asia,America
recipients_codes_impute <- c(298,498,798)

#imputed are contribute to shares of subregions South of Sahara and North of Sahara
africa_subregion <- c(10003,10002)
# North and Central America, South America
americas_subregion <- c(10005,10006)
#Far East Asia, Central Asia and Middle East
asia_subregions <- c(10008,10009,10011)

# All this calcualtions is based on DAC2A, data covers all disbursements for mulitlateral and bilaterals

dac2a <- data.table(read.csv("mirrors/dac2a.csv",na.strings = ""))
dac1 <- data.table(read.csv('mirrors/dac1.csv',na.strings = ""))
setnames(dac2a,'Year','year')

dac2a$value <- coalesce(dac2a$value,as.numeric(0L))*10**6
dac1$value <- coalesce(dac1$value,as.numeric(0L))*10**6

oecd_donor_to_di_id_map <- ddw("dimension.oecd_donor_to_di_id_map")


calculate_bilateral_disbursements <- function(dataType){
  
  #Filter dac2a data and retrieve recipients codes regional recipient codes only eg(Middle East, Total -- Sub Saharan Africa, Total),
  #We only need flows to developing countries, these have part_code 1 to annote the flow
  #  --- Added base year
  dac2a_bilateral_tmp <- dac2a[recipient_code %in% recipient_codes & part_code==1 & aid_type_code==240 & data_type==dataType & year == base_year,
                         .(donor_code,recipient_code,recipient_name,aid_type="bilateral",value,year)]

  #Get contributions that has been made to Africa sub regions only, the sub_regions are North of Sahara and South of Sahara
  africa_share_dt <- dac2a[recipient_code %in% africa_subregion & part_code==1 & aid_type_code==240 & data_type==dataType & year==base_year,
                               .(donor_code,recipient_code,recipient_name,
                                 value,year)]
 
  #Get the total contribution per donor to each region (group contribution by donor) 
  africa_share <- africa_share_dt[,.(value=sum(value)),by=donor_code]
  
  #In order to get the percentage contributibution to each Africa Subregion by each donor formula~(contrib_to_subregion/total_contribution_to_region)*100
  #Merge the africa_share_dt which is share to each sub region with africa_share which is total share to a sub region by the donor
  africa_share <- merge(africa_share,africa_share_dt,by='donor_code')
  rm(africa_share_dt)
  
  #Calculate actual shares for each donor to each Africa sub region, add impute_from_code to denote the region from which imputation is being done, in this case Africa
  africa_share <- africa_share[,.(donor_code,impute_from_code=as.numeric(recipients_codes_impute[1]),share=(value.y/value.x)*100,impute_to_code=recipient_code)]

  ##Repeat above process for Asia and Americas
  
  #Get share contribution for Asia
  asia_share_dt <- dac2a[recipient_code %in% asia_subregions & part_code==1 & aid_type_code==240 & data_type==dataType & year==base_year,
                           .(donor_code,recipient_code,recipient_name,
                             value,year)]
  
  asia_share <- asia_share_dt[,.(value=sum(value)),by=donor_code]
  asia_share <- merge(asia_share,asia_share_dt,by='donor_code')
  rm(asia_share_dt)
  
  
  asia_share <- asia_share[,.(donor_code,impute_from_code=as.numeric(recipients_codes_impute[3]),share=(value.y/value.x)*100,impute_to_code=recipient_code)]
  
  #Get share contributions for the Americas
  america_share_dt <- dac2a[recipient_code %in% americas_subregion & part_code==1 & aid_type_code==240 & data_type==dataType & year==base_year,
                         .(donor_code,recipient_code,recipient_name,
                           value,year)]
  
  america_share <- america_share_dt[,.(value=sum(value)),by=donor_code]
  america_share <- merge(america_share,america_share_dt,by='donor_code')
  rm(america_share_dt)
  america_share <- america_share[,.(donor_code,impute_from_code=as.numeric(recipients_codes_impute[2]),share=(value.y/value.x)*100,impute_to_code=recipient_code)]
  
  #Combine the result of share calculation to create the final imputation table
  impute_share <- rbind(africa_share,america_share,asia_share)
  rm(africa_share,asia_share,america_share)
  
  #Impute from code denotes the region of imputation
  #impute to code denotes the sub region of imputation
  
  to_impute <- dac2a[recipient_code %in% recipients_codes_impute & part_code==1 & aid_type_code==240 & data_type==dataType & year==base_year,
                     .(donor_code,recipient_code,recipient_name,
                       value,year)]
  setnames(impute_share,"impute_from_code","recipient_code")
  imputation <- merge(to_impute,impute_share,by=c('donor_code','recipient_code'),all.x = T,allow.cartesian = T)
  
  setnames(dac2a_bilateral_tmp,'recipient_code','impute_to_code')
  imputation <- merge(imputation,dac2a_bilateral_tmp,by=c('donor_code','impute_to_code'),all.y  = T,allow.cartesian = T)
  
  #Perform the imputation
  imputation$share_val <- imputation$value.x*(imputation$share/100L)
  imputation$share_val[which(is.na(imputation$share_val))] <- 0
  imputation$final_figure <- imputation$value.y +imputation$share_val
  
  #Keep only the necessary columns and that makes for disbursment by region
  keep <- c("donor_code","recipient_name","aid_type.x","year.y","final_figure")
  disbursement_by_region_bilateral <- merge(dac2a_bilateral_tmp,imputation, 
                                            by=c('donor_code','impute_to_code'),
                                            all.x=T)[,..keep]
  
  setnames(disbursement_by_region_bilateral,c("recipient_name","aid_type.x","year.y"),c("region_name","aid_type","year"))
  
  keep <- c("di_id","donor_code","region_name","aid_type","year","final_figure")
  disbursement_by_region_bilateral <- merge(disbursement_by_region_bilateral,
                                            oecd_donor_to_di_id_map,by='donor_code',
                                            all.x=T)[,..keep]
  setnames(disbursement_by_region_bilateral,'final_figure','value')
  #Replace names to di standard identifiers
  disbursement_by_region_bilateral$region_name <- disbursement_by_region_bilateral$region_name %>%
  {gsub('Europe, Total','europe',.)} %>%
  {gsub('Far East Asia, Total','far-east-asia',.)} %>%
  {gsub('Middle East, Total','middle-east',.)} %>%
  {gsub('North Africa, Total','north-of-sahara',.)} %>%
  {gsub('North of Sahara, Total','north-of-sahara',.)} %>%
  {gsub('North & Central America, Total','north-central-america',.)} %>%
  {gsub('Oceania, Total','oceania',.)} %>%
  {gsub('South America, Total','south-america',.)} %>%
  {gsub('South & Central Asia, Total','south-central-asia',.)} %>%
  {gsub('Sub-Saharan Africa, Total','south-of-sahara',.)} %>%
  {gsub('South of Sahara, Total','south-of-sahara',.)} %>%
  {gsub('Unspecified, Total','unspecified',.)} %>%
  {gsub('Developing countries, unspecified','unspecified',.)}
  
  return(disbursement_by_region_bilateral)

}


#Work on donoations to multilateral donors. This and combination with disbursement_by_region_bilateral 
#Should give us the final data for disbursement_by_region
calculate_multilateral_disbursements <- function(dataType,disbursement_by_region_bilateral){
  #Dependencies
  #Make sure donor_profile.dac2a_name_to_dac1_name_map is upto date before running this script
  #Get list of standard di id maps
  dac2a_dac1_map <- ddw("donor_profile.dac2a_name_to_dac1_name_map")
  setnames(dac2a_dac1_map,'code','donor_code')
  
  disbursement_brb_with_dac1_name <- merge(disbursement_by_region_bilateral,dac2a_dac1_map,by="donor_code")
  
  dac_1_ttl_to_region <- disbursement_brb_with_dac1_name[,.(dac_1_total_to_region=sum(value)),by=c('dac_1_name','region_name','year')]

  dac_1_ttl_to_world <- aggregate(disbursement_brb_with_dac1_name$value,list(dac_1_name=disbursement_brb_with_dac1_name$dac_1_name),FUN="sum")
  setnames(dac_1_ttl_to_world,'x','dac_1_total_to_world')
  
  
  dac_1_name_share_to_region <- merge(dac_1_ttl_to_region,dac_1_ttl_to_world,by='dac_1_name')
  dac_1_name_share_to_region$dac_1_share_to_region <- (dac_1_name_share_to_region$dac_1_total_to_region/dac_1_name_share_to_region$dac_1_total_to_world)*100
  
  dac_1_multilateral_cur <- dac1[aid_type_code %in% aid_types & part_code==1 & flows==1120 & amount_type_code==dataType & year==base_year,
                                  .(donor_code,donor_name,dac_1_name=aid_type_name,value,year)]
  
  dac_1_multilateral_cur$dac_1_name <- gsub('^(I\\.B\\.1\\.\\d+{1}\\.\\s+)','',dac_1_multilateral_cur$dac_1_name)
  dac_1_multilateral_cur$dac_1_name <- gsub('\\s+\\(96\\%\\)','',dac_1_multilateral_cur$dac_1_name)
  
  #get only unique region_names from bilateral data filtered
  unique_regions <- unique(disbursement_by_region_bilateral,by='region_name')
  unique_regions <- unique_regions[,'region_name']
  
  
  multilateral_imputation <- merge(as.data.frame(dac_1_multilateral_cur),as.data.frame(unique_regions),by=NULL)

  multilateral_imputation <- merge(multilateral_imputation,dac_1_name_share_to_region,
                                   by=c('region_name','dac_1_name'),allow.cartesian = TRUE)

  multilateral_imputation$oda_to_region_tmp <- (multilateral_imputation$dac_1_share_to_region/100)
  multilateral_imputation$oda_to_region <- multilateral_imputation$value *  multilateral_imputation$oda_to_region_tmp
  
  keep <- c('donor_code','donor_name','dac_1_name','year.x','region_name','oda_to_region')
  multilateral_imputation=multilateral_imputation[,keep]
  
  setnames(multilateral_imputation,'year.x','year')
  multilateral_imputation <-merge(multilateral_imputation,oecd_donor_to_di_id_map,by='donor_code',all.x = T,all.y=T)
  multilateral_imputation$aid_type<-'multilateral'
  
  multilateral_imputation$oda_to_region <- multilateral_imputation$oda_to_region %>% coalesce(0)
  
  disbursement_by_region_multilateral <- aggregate(multilateral_imputation$oda_to_region,list(
    di_id=multilateral_imputation$di_id,
    region_name=multilateral_imputation$region_name,
    aid_type=multilateral_imputation$aid_type,
   year=multilateral_imputation$year),FUN="sum")
  setnames(disbursement_by_region_multilateral,'x','value')
  
  return(disbursement_by_region_multilateral)
}

calculate_disbursment_by_region <- function(){
  
  disbursement_by_region_bilateral <- calculate_bilateral_disbursements('A')
  
  disbursement_by_region_multilateral <- calculate_multilateral_disbursements('A',disbursement_by_region_bilateral)
  
  keep <- c("di_id","region_name","aid_type","year","value")
  disbursement_by_region_bilateral <- disbursement_by_region_bilateral[,..keep]
  disbursments <- rbind(disbursement_by_region_bilateral,disbursement_by_region_multilateral)
  
  write.csv(disbursments,file='output/donor_profile.disbursements_by_region.csv',row.names = F)

}


calculate_disbursment_by_region_constant <- function(){
  
  disbursement_by_region_bilateral <- calculate_bilateral_disbursements('D')
  disbursement_by_region_multilateral <- calculate_multilateral_disbursements('D',disbursement_by_region_bilateral)
  
  keep <- c("di_id","region_name","aid_type","year","value")
  disbursement_by_region_bilateral <- disbursement_by_region_bilateral[,..keep]
  
  disbursments <- rbind(disbursement_by_region_bilateral,disbursement_by_region_multilateral)
  
  write.csv(disbursments,file='output/donor_profile.disbursements_by_region_constant.csv',row.names = F)
}

calculate_disbursment_by_region()
calculate_disbursment_by_region_constant()