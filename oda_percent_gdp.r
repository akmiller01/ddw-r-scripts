list.of.packages <- c("data.table","openxlsx","WDI","varhandle","reshape2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd='C:/git/ddw-r-scripts'
setwd(wd)

#oda percent gdp
oda=read.xlsx("comparisons/ODA percent GDP.xlsx",sheet="ODA current",startRow=10,na.strings = "")
oda=subset(oda,!is.na(di.id))
setnames(oda,"Year","country")
oda=melt(oda,id.vars=c("di.id","country"),variable.name="year")

oda$value=oda$value*1000000
gdp=WDI(country="all",indicator="NY.GDP.MKTP.CD",start=1995,end=2016,extra=T)
keep=c("iso2c","year","NY.GDP.MKTP.CD")

gdp=gdp[keep]
names(gdp)=c("di.id","year","gdp")

oda_percent_gdp=merge(oda,gdp,by=c("di.id","year"))
setdiff(unique(oda$di.id),unique(oda_percent_gdp$di.id))
setdiff(unique(oda_percent_gdp$di.id),unique(oda$di.id))

oda_percent_gdp$value=oda_percent_gdp$value/oda_percent_gdp$gdp
oda_percent_gdp$gdp=NULL
oda_percent_gdp.rp=read.csv("output/recipient_profile.oda_per_percent_gdp.csv",na.strings = "")
setnames(oda_percent_gdp.rp,"di_id","di.id")
oda_percent_gdp.merge=merge(oda_percent_gdp.rp,oda_percent_gdp,by=c("di.id","year"),all.x=T)
oda_percent_gdp.merge$value.x=as.numeric(unfactor(oda_percent_gdp.merge$value.x))
oda_percent_gdp.merge$diff=(oda_percent_gdp.merge$value.x-oda_percent_gdp.merge$value.y)/(oda_percent_gdp.merge$value.x)

write.csv(oda_percent_gdp.merge,"comparisons/oda_percent_gdp_comparison.csv",na="",row.names=F)