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
weo$indicator[which(weo$Subject.Descriptor== "Gross domestic product, current prices" & weo$Units == "U.S. dollars")] = "current_usd_gdp"
weo$indicator[which(weo$Subject.Descriptor== "Gross domestic product, constant prices" & weo$Units == "Percent change")] = "gdp_growth"

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

indicators.l$gdp_growth = 1+(indicators.l$gdp_growth/100)

calc_base_gdp = function(dt,base_year){
  results = c()
  row_len = nrow(dt)
  for(i in 1:row_len){
    row = dt[i,]
    this_year = row$year[[1]]
    this_gdp = row$current_usd_gdp[[1]]
    if(this_year<base_year){
      inter_year_range = subset(dt,year>this_year & year<=base_year)
      gdp_growths = inter_year_range$inflation
      gdp_growths_prod = prod(gdp_growths)
      gdp_constant_base = this_gdp * gdp_growths_prod
      results = c(results,gdp_constant_base)
    }
    if(this_year==base_year){
      results = c(results,this_gdp)
    }
    if(this_year>base_year){
      inter_year_range = subset(dt,year>base_year & year<=this_year)
      gdp_growths = inter_year_range$inflation
      gdp_growths_prod = prod(gdp_growths)
      gdp_constant_base = this_gdp / gdp_growths_prod
      results = c(results,gdp_constant_base)
    }
  }
  return(results)
}

# Reorder by country and year
indicators.l = indicators.l[order(indicators.l$WEO.Country.Code,indicators.l$year),]
indicators.l = data.table(indicators.l)
indicators.l[,gdp_growth_current:=1+(c(NA,diff(.SD$current_usd_gdp))/.SD$current_usd_gdp),by=.(WEO.Country.Code)]
indicators.l$inflation = 1+indicators.l$gdp_growth_current-indicators.l$gdp_growth
indicators.l = subset(indicators.l,!is.na(inflation))
indicators.l[,constant_usd_gdp:=calc_base_gdp(.SD,base_year),by=.(WEO.Country.Code)]

# # Calculate the deflator index
indicators.l$deflator = round( (indicators.l$current_usd_gdp/indicators.l$constant_usd_gdp) * 100, 6)

# Drop unnecessary columns, rename, and write csv
keep = c("WEO.Country.Code","ISO","Country","year","deflator")
indicators.l = indicators.l[,keep,with=F]
names(indicators.l) = c("weo_country_code","iso_alpha_3_code","country_name","year","deflator")
write.csv(indicators.l,"output/usd_deflator_2014_2016_apr.csv",na="",row.names=F)