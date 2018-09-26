list.of.packages <- c("data.table","reshape2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

# Change WD to git repo if needed
wd = "~/git/ddw-r-scripts"
setwd(wd)

# Set base_year here
base_year = 2016

# Load data, removing na strings
data_url = "https://www.imf.org/external/pubs/ft/weo/2018/01/weodata/WEOApr2018all.xls"
weo = read.csv(data_url,sep="\t",na.strings=c("","n/a","--"))

# Set our desired indicators with nice names
weo$indicator = NA
weo$indicator[which(weo$Subject.Descriptor== "Gross domestic product, deflator" & weo$Units == "Index")] = "deflator.2009"

# Grab just those indicators and relevant columns
indicators = subset(weo,!is.na(indicator))
keep = c("WEO.Country.Code","ISO","Country","indicator",paste0("X",c(1981:2023)))
indicators = indicators[,keep]

# Dataset has commas in numbers, which need to be removed and parsed as numbers
indicators[,paste0("X",c(1981:2023))] = as.numeric(sapply(indicators[,paste0("X",c(1981:2023))],gsub,pattern=",",replacement=""))

# From reshape2 package, melt turns dataset as long as it can go
indicators.m = melt(indicators,id.vars=c("WEO.Country.Code","ISO","Country","indicator"))

# dcast takes a molten dataframe and reshapes it given a formula, here we're recasting long
indicators.l = dcast(indicators.m,WEO.Country.Code+ISO+Country+variable~indicator)

# Remove the leading X now that year is no longer a variable name
indicators.l$year = as.numeric(substr(indicators.l$variable,2,5))
indicators.l$variable = NULL

indicators.l.base = subset(indicators.l,year==base_year)
setnames(indicators.l.base,"deflator.2009","deflator.base")
indicators.l.base$year=NULL

indicators.l = merge(indicators.l,indicators.l.base,by=c("WEO.Country.Code","ISO","Country"))
indicators.l$deflator= (indicators.l$deflator.2009/indicators.l$deflator.base)*100

# Drop unnecessary columns, rename, and write csv
keep = c("WEO.Country.Code","ISO","Country","year","deflator")
indicators.l = indicators.l[,keep]
names(indicators.l) = c("weo_country_code","iso_alpha_3_code","country_name","year","deflator")
write.csv(indicators.l,"output/usd_deflator_2014_2016_apr.csv",na="",row.names=F)