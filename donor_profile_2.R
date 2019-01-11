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
oda <- oda[oda$year == 2016,]
oda_constant <- oda_constant[oda_constant$year == 2016,]

donors <- randomQuery(" select map.di_id,map.donor_code,donor.donor_name,donor.donor_type from  dimension.oecd_donor_to_di_id_map AS \"map\" 
 LEFT JOIN dimension.oecd_donor AS \"donor\" ON \"map\".donor_code = \"donor\".donor_code")

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
oda_ranking_ <- oda[,c('from_di_id','to_di_id','bundle','year','value')]
oda_ranking_constant_ <- oda_constant[,c('from_di_id','to_di_id','bundle','year','value')]

oda_ranking_sum <- aggregate(oda_ranking_$value,by=list(from_di_id=oda_ranking_$from_di_id,to_di_id=oda_ranking_$to_di_id,year=oda_ranking_$year),FUN='sum')
setnames(oda_ranking_sum,'x','value')


oda_ranking_constant_sum<- aggregate(oda_ranking_constant_$value,by=list(from_di_id=oda_ranking_constant_$from_di_id,
                                                                    to_di_id=oda_ranking_constant_$to_di_id,
                                                                    year=oda_ranking_constant_$year),FUN='sum')


setnames(oda_ranking_constant_sum,'x','value')
oda_ranking <- oda_ranking_sum
oda_ranking_constant <- oda_ranking_constant_sum

recpt_to_di_map_with_icome_grp <- randomQuery("select * from dimension.oecd_recipient_to_di_id_map a left join dimension.oecd_recipient b on a.recipient_code =b.recipient_code left join 
                                              dimension.oecd_recipient_income_group k on k.income_group_name = b.income_group")

donor_details <- merge(donors,recpt_to_di_map_with_icome_grp,by='di_id',all.x = T)


'%!in%' <- function(x,y)!('%in%'(x,y))
keep <- c('di_id','to_di_id','year','rank')
Rank <- function(x) rank(-x,ties.method = "first")

oda_ranking <- transform(oda_ranking,rank=ave(value,from_di_id,FUN=Rank))
setnames(oda_ranking,'from_di_id','di_id');
oda_ranking <- merge(oda_ranking,donor_details, by='di_id',all.x = T)

oda_ranking <- oda_ranking[which(as.factor(oda_ranking$donor_type) == 'DAC'),]
oda_ranking <- oda_ranking[which(oda_ranking$rank < 11),]
oda_ranking <- oda_ranking[which(oda_ranking$income_group %!in% c('Part I unallocated by income')),]
oda_ranking <- oda_ranking[,keep]
setnames(oda_ranking,'di_id','from_di_id')

oda_ranking_constant <- transform(oda_ranking_constant,rank=ave(value,from_di_id,FUN=Rank))
setnames(oda_ranking_constant,'from_di_id','di_id');
oda_ranking_constant <- merge(oda_ranking_constant,donor_details, by='di_id',all.x = T)

oda_ranking_constant <- oda_ranking_constant[which(as.factor(oda_ranking_constant$donor_type) == 'DAC'),]
oda_ranking_constant <- oda_ranking_constant[which(oda_ranking_constant$rank < 11),]
oda_ranking_constant <- oda_ranking_constant[which(oda_ranking_constant$income_group %!in% c('Part I unallocated by income')),]

oda_ranking_constant <- oda_ranking_constant[,keep]
setnames(oda_ranking_constant,'di_id','from_di_id')

## Get the the actual bundle and value that the from_di_id gave to to_di_id

recipient_bundle <- merge(oda_ranking,oda_ranking_sum,by=c('from_di_id','to_di_id'),all.x = T)
setnames(recipient_bundle,'value','total_oda')

oda_bundlesum <- aggregate(oda_ranking_$value,by=list(from_di_id=oda_ranking_$from_di_id,to_di_id=oda_ranking_$to_di_id,
                                                      year=oda_ranking_$year,bundle=oda_ranking_$bundle),FUN='sum')
setnames(oda_bundlesum,'x','value')

recipient_bundle <- merge(recipient_bundle,oda_bundlesum,by=c('from_di_id','to_di_id'),all.x = T)
setnames(recipient_bundle,c('value','year.x'),c('bundle_value','year'))
recipient_bundle <- recipient_bundle[,c('from_di_id','to_di_id','year','rank','total_oda','bundle','bundle_value')]


##Add the unwanted recipients and call them other -- Anyone who did not appear in the top ten

row_11th <- merge(oda,oda_ranking,by=c('from_di_id','to_di_id','year'),all = T)
row_11th <- merge(row_11th,donor_details,by.x='from_di_id',by.y='di_id',all.x = T) %>% as.data.table()
row_11th <- row_11th[as.factor(row_11th$donor_type) == 'DAC' & is.na(row_11th$rank),] %>% as.data.frame

row_11th <- aggregate(row_11th$value,by=list(from_di_id=row_11th$from_di_id,
                                                      year=row_11th$year,
                                             bundle=row_11th$bundle),FUN='sum')
setnames(row_11th,'x','bundle_value')

row_11th_total_oda <- merge(oda,oda_ranking,by=c('from_di_id','to_di_id','year'),all.x = T,all =T) %>% as.data.table
row_11th_total_oda <- row_11th_total_oda[is.na(row_11th_total_oda$rank),] %>% as.data.frame

row_11th_total_oda <- aggregate(row_11th_total_oda$value,by=list(from_di_id=row_11th_total_oda$from_di_id,
                                             year=row_11th_total_oda$year
                                             ),FUN='sum')
setnames(row_11th_total_oda,'x','total_oda')

row_11th <- merge(row_11th,row_11th_total_oda,by=c('from_di_id','year'),all.x = T )
row_11th$to_di_id <- 'other'
row_11th$rank <- 11
keep <- c('from_di_id','to_di_id','year','rank','total_oda','bundle','bundle_value');
row_11th <- row_11th[,keep]


recipient_bundle <- rbind(recipient_bundle,row_11th)



## Get recipient bundle constant bundles ....

recipient_bundle_constant <- merge(oda_ranking_constant,oda_ranking_constant_sum,by=c('from_di_id','to_di_id'),all.x = T)
setnames(recipient_bundle_constant,'value','total_oda')

oda_bundlesum_constant <- aggregate(oda_ranking_constant_$value,by=list(from_di_id=oda_ranking_constant_$from_di_id,to_di_id=oda_ranking_constant_$to_di_id,
                                                                        year=oda_ranking_constant_$year,bundle=oda_ranking_constant_$bundle),FUN='sum')
setnames(oda_bundlesum_constant,'x','value')

recipient_bundle_constant <- merge(recipient_bundle_constant,oda_bundlesum_constant,by=c('from_di_id','to_di_id'),all.x = T)
setnames(recipient_bundle_constant,c('value','year.x'),c('bundle_value','year'))
recipient_bundle_constant <- recipient_bundle_constant[,c('from_di_id','to_di_id','year','rank','total_oda','bundle','bundle_value')]


##Add the unwanted recipients and call them other -- Anyone who did not appear in the top ten

row_11th_constant <- merge(oda_constant,oda_ranking_constant,by=c('from_di_id','to_di_id','year'),all = T)
row_11th_constant <- merge(row_11th_constant,donor_details,by.x='from_di_id',by.y='di_id',all.x = T) %>% as.data.table()
row_11th_constant <- row_11th_constant[as.factor(row_11th_constant$donor_type) == 'DAC' & is.na(row_11th_constant$rank),] %>% as.data.frame

row_11th_constant <- aggregate(row_11th_constant$value,by=list(from_di_id=row_11th_constant$from_di_id,
                                             year=row_11th_constant$year,
                                             bundle=row_11th_constant$bundle),FUN='sum')
setnames(row_11th_constant,'x','bundle_value')

row_11th_constant_total_oda <- merge(oda_constant,oda_ranking_constant,by=c('from_di_id','to_di_id','year'),all.x = T,all =T) %>% as.data.table
row_11th_constant_total_oda <- row_11th_constant_total_oda[is.na(row_11th_constant_total_oda$rank),] %>% as.data.frame

row_11th_constant_total_oda <- aggregate(row_11th_constant_total_oda$value,by=list(from_di_id=row_11th_constant_total_oda$from_di_id,
                                                                 year=row_11th_constant_total_oda$year
),FUN='sum')
setnames(row_11th_constant_total_oda,'x','total_oda')

row_11th_constant <- merge(row_11th_constant,row_11th_constant_total_oda,by=c('from_di_id','year'),all.x = T )
row_11th_constant$to_di_id <- 'other'
row_11th_constant$rank <- 11
keep <- c('from_di_id','to_di_id','year','rank','total_oda','bundle','bundle_value');
row_11th_constant <- row_11th_constant[,keep]


recipient_bundle_constant <- rbind(recipient_bundle_constant,row_11th_constant)


write.csv(recipient_bundle,'output/donor_profile.recipient_bundle.csv',na='',row.names = F)
write.csv(recipient_bundle_constant,'output/donor_profile.recipient_bundle_constant.csv',na = '',row.names = F)


