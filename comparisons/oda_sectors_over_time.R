list.of.packages <- c("data.table","openxlsx","WDI","varhandle","reshape2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd='C:/git/ddw-r-scripts'
setwd(wd)

#oda sectors over time
oda=read.xlsx("comparisons/D11 and D12 sector.xlsx",sheet="D12",startRow=1,na.strings = "")
oda=subset(oda,!is.na(di.id))
setnames(oda,"Donor","country")
oda=melt(oda,id.vars=c("di.id","country","ITEP.sector"),variable.name="year")
oda$value=as.numeric(oda$value)
oda$value=oda$value*1000000

oda_sectors_time.rp=read.csv("output/donor_profile.sectors_over_time_constant.csv",na.strings = "")
setnames(oda_sectors_time.rp,"di_id","di.id")
setnames(oda_sectors_time.rp,"sector","ITEP.sector")
oda_sectors_time.merge=merge(oda_sectors_time.rp,oda,by=c("di.id","year","ITEP.sector"),all.x=T)
oda_sectors_time.merge$diff=(oda_sectors_time.merge$value.x-oda_sectors_time.merge$value.y)/(oda_sectors_time.merge$value.x)

write.csv(oda_sectors_time.merge,"comparisons/oda_sectors_time__historical_comparison.csv",na="",row.names=F)