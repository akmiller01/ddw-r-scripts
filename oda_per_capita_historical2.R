list.of.packages <- c("data.table","openxlsx","WDI","varhandle","reshape2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd='C:/git/ddw-r-scripts'
setwd(wd)

#oda per capita historical
oda=read.xlsx("comparisons/OECD CRS ODA data.xlsx",sheet="OECD.Stat export",startRow=10,na.strings = "")
oda=subset(oda,!is.na(di.id))
setnames(oda,"Recipient","country")
oda=melt(oda,id.vars=c("di.id","country"),variable.name="year")

oda$value=oda$value*1000000
pop=WDI(country="all",indicator="SP.POP.TOTL",start=1995,end=2016,extra=T)
keep=c("iso2c","year","SP.POP.TOTL")

pop=pop[keep]
names(pop)=c("di.id","year","value")

oda_per_capita=merge(oda,pop,by=c("di.id","year"))
setdiff(unique(oda$di.id),unique(oda_per_capita$di.id))
setdiff(unique(oda_per_capita$di.id),unique(oda$di.id))

oda_per_capita$value.z=oda_per_capita$value.x/oda_per_capita$value.y
oda_per_capita$pop=NULL
oda_per_capita.rp=read.csv("output/recipient_profile.oda_per_capita_constant.csv",na.strings = "")
setnames(oda_per_capita.rp,"di_id","di.id")
oda_per_capita.merge=merge(oda_per_capita.rp,oda_per_capita,by=c("di.id","year"),all.x=T)
oda_per_capita.merge$value=as.numeric(unfactor(oda_per_capita.merge$value))
oda_per_capita.merge$diff=(oda_per_capita.merge$value-oda_per_capita.merge$value.z)/(oda_per_capita.merge$value)

write.csv(oda_per_capita.merge,"comparisons/oda_per_capita__historical_comparison.csv",na="",row.names=F)

