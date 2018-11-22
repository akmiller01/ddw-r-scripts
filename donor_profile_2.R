list.of.packages <- c("data.table","readr","dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)


source('connect.R')

wd='/Users/boss/Dev_Musings/devinit/ddw_update/rscripts/ddw-r-scripts'
setwd(wd)

donor_bilateral <- read.csv(file="output/donor_profile.disbursements_by_region_bilateral.csv",na.strings = "")
donor_bilateral$donor_code <- NULL

donor_multilateral <- read.csv(file="output/donor_profile.disbursements_by_region_multilateral.csv",na.string="")
donor_multilateral <- donor_multilateral[,c('di_id','region_name','aid_type','year','value')]

disbursement_by_region <-  rbind(donor_bilateral,donor_multilateral)

write.csv(disbursement_by_region,'output/donor_profile.disbursement_by_region.csv',na = "")

oda_constant <- read.csv(file="output/oda_constant.csv",na.strings = "")
oda <- read.csv(file="output/oda.csv",na.strings = "")


donors <- randomQuery(" select map.di_id,map.donor_code,donor.donor_name,donor.donor_type from  dimension.oecd_donor_to_di_id_map AS \"map\" 
 LEFT JOIN dimension.oecd_donor AS \"donor\" ON \"map\".donor_code = \"donor\".donor_code")

oda <- oda[oda$year == 2016,]
oda_constant <- oda_constant[oda_constant$year == 2016,]

donor_bundle <- aggregate(oda$value,by=list(from_di_id=oda$from_di_id,bundle=oda$bundle,year=oda$year),FUN="sum")

setnames(donor_bundle,c('from_di_id','x'),c('di_id','value'))
dac_total <- merge(donor_bundle,donors,by='di_id')

dac_total <- dac_total[dac_total$di_id != 'EU' & dac_total$donor_type == 'DAC',]
dac_total <- aggregate(dac_total$value,by=list(bundle=dac_total$bundle),FUN='sum')

setnames(dac_total,'x','value')
dac_total$di_id <- 'dac-countries-total'
dac_total$year <- '2016'
dac_total <- dac_total[,c('di_id','bundle','year','value')]

donor_bundle <- rbind(donor_bundle,dac_total)

write.csv(donor_bundle,'output/donor_profile.donor_bundle.csv',na = "")


donor_bundle_constant <- aggregate(oda_constant$value,by=list(from_di_id=oda_constant$from_di_id,bundle=oda_constant$bundle,year=oda_constant$year),FUN="sum")


setnames(donor_bundle_constant,c('from_di_id','x'),c('di_id','value'))
dac_total <- merge(donor_bundle_constant,donors,by='di_id')

dac_total <- dac_total[dac_total$di_id != 'EU' & dac_total$donor_type == 'DAC',]
dac_total <- aggregate(dac_total$value,by=list(bundle=dac_total$bundle),FUN='sum')

setnames(dac_total,'x','value')
dac_total$di_id <- 'dac-countries-total'
dac_total$year <- '2016'
dac_total <- dac_total[,c('di_id','bundle','year','value')]

donor_bundle_constant <- rbind(donor_bundle_constant,dac_total)

write.csv(donor_bundle_constant,'output/donor_profile.donor_bundle_constant.csv',na = "")


# Work on recipient_bundle

oda_ranking <- oda[,c('from_di_id','to_di_id','year','value')]
oda_ranking_constant <- oda_constant[,c('from_di_id','to_di_id','year','value')]

oda_ranking <- aggregate(oda_ranking$value,by=list(from_di_id=oda_ranking$from_di_id,to_di_id=oda_ranking$to_di_id,year=oda_ranking$year),FUN='sum')
setnames(oda_ranking,'x','value')


oda_ranking_constant<- aggregate(oda_ranking_constant$value,by=list(from_di_id=oda_ranking_constant$from_di_id,
                                                                    to_di_id=oda_ranking_constant$to_di_id,
                                                                    year=oda_ranking_constant$year),FUN='sum')
setnames(oda_ranking_constant,'x','value')

#oda_tmp <- oda_ranking
#oda_tmp$order <- rank(oda_tmp$value,ties.method = "first")
#oda_tmp$order <- NULL

#oda_tmp <- oda_tmp[order(oda_tmp$from_di_id),]
#oda_tmp$order_by_group <- unlist(with(oda_tmp,tapply(value,from_di_id,function(x) rank(x,ties.method = "first"))))


Rank <- function(x) rank(-x,ties.method = "first")
oda_ranking <- transform(oda_ranking,rank=ave(value,from_di_id,FUN=Rank))
oda_ranking_constant <- transform(oda_ranking_constant,rank=ave(value,from_di_id,FUN=Rank))

recpt_to_di_map_with_icome_grp <- randomQuery("select * from dimension.oecd_recipient_to_di_id_map a left join dimension.oecd_recipient b on a.recipient_code =b.recipient_code left join 
                                              dimension.oecd_recipient_income_group k on k.income_group_name = b.income_group")

donor_details <- merge(donors,recpt_to_di_map_with_icome_grp,by='di_id')

donor_ranking <- merge(oda_ranking,donor_details,by.x='from_di_id',by.y='di_id')
donor_ranking <- donor_ranking[donor_ranking$income_group_code != 10024 & 
                                                   donor_ranking$donor_type == 'DAC',]

donor_ranking_constant <- merge(oda_ranking_constant,donor_details,by.x='from_di_id',by.y='di_id')
donor_ranking_constant <- donor_ranking_constant[donor_ranking_constant$income_group_code != 10024 & 
                                                   donor_ranking_constant$donor_type == 'DAC',]





