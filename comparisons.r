list.of.packages <- c("data.table","openxlsx","WDI","varhandle")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd='C:/git/ddw-r-scripts'
setwd(wd)

#oda per capita
oda=read.xlsx("comparisons/ODA per capita and percent GDP.xlsx",sheet="ODA",startRow=11,na.strings = "")
names(oda)=c("recipient","di_id","value")
oda$value=oda$value*1000000
pop=WDI(country="all",indicator="SP.POP.TOTL",start=2016,end=2016,extra=T)
keep=c("iso2c","SP.POP.TOTL")

pop=pop[keep]
names(pop)=c("di_id","pop")
oda_per_capita=merge(oda,pop,by="di_id")
setdiff(unique(oda$di_id),unique(oda_per_capita$di_id))
setdiff(unique(oda_per_capita$di_id),unique(oda$di_id))

oda_per_capita$value=oda_per_capita$value/oda_per_capita$pop
oda_per_capita$pop=NULL
oda_per_capita.rp=read.csv("output/recipient_profile.oda_per_capita_constant.csv",na.strings = "")
oda_per_capita.rp=subset(oda_per_capita.rp,year==2016)
oda_per_capita.merge=merge(oda_per_capita.rp,oda_per_capita,by="di_id",all.x=T)
oda_per_capita.merge$value.x=as.numeric(unfactor(oda_per_capita.merge$value.x))
oda_per_capita.merge$diff=(oda_per_capita.merge$value.x-oda_per_capita.merge$value.y)/(oda_per_capita.merge$value.x)

write.csv(oda_per_capita.merge,"comparisons/oda_per_capita_comparison.csv",na="",row.names=F)
