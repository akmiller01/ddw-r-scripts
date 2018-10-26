list.of.packages <- c("data.table","readr","dplyr","DescTools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)


wd='/Users/boss/Dev_Musings/devinit/ddw_update/rscripts/ddw-r-scripts'
setwd(wd)
#This file can only be processed after fact table has been processed successfully
source("baseYearConstants.R")
#source("connect.R")

#Load oecd_crs data set that will be manipulated

crs <- fread('mirrors/crs_mirror.csv')


#donor_map <- ddw('dimension.oecd_donor_to_di_id_map');
#recipient_to_di_id_map <- ddw('dimension.oecd_recipient_to_di_id_map');
#sector_3_code_map <- ddw('dimension.oecd_crs_sector_code_3_digit_to_itep_sector_web_id_map')
#channel_code_5_web_map <- ddw('dimension.oecd_crs_channel_code_5_digit_to_itep_channel_web_id_map')


donor_map <- read.csv('output/dimension.oecd_donor_to_di_id_map');
recipient_to_di_id_map <- read.csv('output/dimension.oecd_recipient_to_di_id_map');
sector_3_code_map <- read.csv('output/dimension.oecd_crs_sector_code_3_digit_to_itep_sector_web_id_map')
channel_code_5_web_map <- read.csv('output/dimension.oecd_crs_channel_code_5_digit_to_itep_channel_web_id_map')

#Get only oda from crs
#Filters that will be used to get only ODA
flow_code_v <- c(11,13,19)
category_v <- 10

oda_filter <- crs[flow_code %in% flow_code_v & category == category_v & !is.na(usd_disbursement) & usd_disbursement > 0]
oda_filter$oda_donor_bundle <- NA
# Get vectors to generate ODA bundle codes
# Bundle Code F

aid_type_v <- c('E01','E02','G01','H01','H02')
finance_type_v <- c(210)
sector_code_v <- c(600)
purpose_code_v <- c(93010,99820)

oda_filter$oda_donor_bundle = NA
oda_filter[,oda_donor_bundle := as.character(oda_donor_bundle)]
oda_filter$oda_donor_bundle <- 'E'

# Filter using aid_type,finance_type,sector_code and purpose-code
oda_filter$oda_donor_bundle[which(oda_filter$aid_type %in% aid_type_v | oda_filter$finance_type %in% finance_type_v |
                                    oda_filter$sector_code %in% sector_code_v |
                                    oda_filter$purpose_code %in% purpose_code_v)] <- 'F'

# Bundle Code A

flow_code_v <- c(13,14)
oda_filter$oda_donor_bundle[which(oda_filter$flow_code %in%  flow_code_v)] <- 'A'

# Bundle Code B

aid_type_v <- c('A01','A02')
purpose_code_v <- 51010
donor_code_v <- c(901,905,906,907,909,912,913,914,915,916,921,951,976,990,1013)
pba_v <- 1
# also check for combination of aid_type and recipient code 
# aid_type B01 or B03 or Bo4 and !recipient code 998

#channel channel code to character and check if
channel_code_g <- '3%'
short_description_g <- c('%finance%','%fund%','%subsidy%')
long_description_g <- c('%finance%','%fund%','%subsidy%','% SWAP %')

#Check for aid type,purpose_code,donor_code,pba,short_description and long_description
oda_filter$oda_donor_bundle[which(oda_filter$aid_type %in%  aid_type_v |
                                    oda_filter$purpose_code ==  purpose_code_v |
                                    oda_filter$donor_code %in%  donor_code_v |
                                    oda_filter$pba ==  pba_v |
                                    as.character(oda_filter$channel_code) %like%  channel_code_g |
                                    oda_filter$long_description %like any%  long_description_g |
                                    oda_filter$short_description %like any%  short_description_g)] <- 'B'

# Check for aid type and recipient code
aid_type_v <- c('B01','B03','B04')
oda_filter$oda_donor_bundle[which(oda_filter$aid_type %in%  aid_type_v & oda_filter$recipient_code != 998)] <- 'B'
  

# Bundle Code C
purpose_code_v <- c(52010,53030,53040,72040)
short_description_g <- c('%vaccin%','%non-food%')
long_description_g <- c('%vaccin%','%non-food%')

# Check for purpose code, short_description and long_description
oda_filter$oda_donor_bundle[which(oda_filter$purpose_code %in%  purpose_code_v |
                                    oda_filter$long_description %like any%  long_description_g |
                                  oda_filter$short_description %like any%  short_description_g)] <- 'C'


# Bundle Code D
channel_code_g <- '51*'
long_description_g <- '%research%'
# grep %research% and recipient code 998
# aid type B01 or B03 or B04 and recipient code 998
aid_type_v <- c('B01','B03','B04')
# aid

#Check for channel code
oda_filter$oda_donor_bundle[which(as.character(oda_filter$channel_code) %like%  channel_code_g)] <- 'D'

oda_filter$oda_donor_bundle[which(oda_filter$long_description %like any%  long_description_g & oda_filter$recipient_code == 998)] <- 'D'
oda_filter$oda_donor_bundle[which(oda_filter$recipient_code == 998 & oda_filter$aid_type %in% aid_type_v)] <- 'D'

# Bundle G
short_description_g <- c('%consultancy%','%consultant%','%technical assistance%','%technical cooperation%','%training%')
long_description_g <- c('%consultancy%','%consultant%','%technical assistance%','%technical cooperation%','%training%')

#Search for the occurrences of short_description and long_description
oda_filter$oda_donor_bundle[which(oda_filter$long_description %like any%  long_description_g |
                                    oda_filter$short_description %like any%  short_description_g)] <- 'G'



oda_filter[,.('Number of Rows'=.N),by='oda_donor_bundle']


