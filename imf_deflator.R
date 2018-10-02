list.of.packages <- c("data.table", "reshape2")
new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages))
  install.packages(new.packages)
lapply(list.of.packages, require, character.only = T)

# Change WD to git repo if needed
wd = "/Users/boss/Dev_Musings/devinit/ddw_update/rscripts/ddw-r-scripts"
setwd(wd)

# Set base_year here
base_year = 2016

# Load data, removing na strings
#data_url = "https://www.imf.org/external/pubs/ft/weo/2018/01/weodata/WEOApr2018all.xls"

data_url="WEOApr2018all.xls"
weo = read.csv(data_url,
               sep = "\t",
               na.strings = c("", "n/a", "--","NA"))


#weo[Country=='Afghanistan' & WEO.Subject.Code=='NGDP_RPCH']$Subject.Notes

# Add a column to pick out indicators that will be useful to us
# Set our desired indicators with nice names
weo$indicator = NA
weo$indicator[which(
  weo$WEO.Subject.Code=='NGDPD'
)] = "current_usd_gdp"

weo$indicator[which(
  weo$WEO.Subject.Code=='NGDP_RPCH'
)] = "gdp_growth"

# Grab just those indicators and relevant columns
indicators = subset(weo, !is.na(indicator))
keep = c("WEO.Country.Code",
         "ISO",
         "Country",
         "indicator",
         paste0("X", c(1980:2023)))

indicators = indicators[,keep]


# Dataset has commas in numbers, which need to be removed and parsed as numbers
indicators[, paste0("X", c(1980:2023))] = as.numeric(sapply(indicators[, paste0("X", c(1980:2023))], 
                                                            gsub,pattern=",",replacement="",fixed=TRUE))



#indicators[, paste0("X", c(1980:2023))] = sapply(indicators[, paste0("X", c(1980:2023))],as.numeric)

# From reshape2 package, melt turns dataset as long as it can go
indicators.m = melt(indicators,
                    id.vars = c("WEO.Country.Code", "ISO", "Country", "indicator"))

# dcast takes a molten dataframe and reshapes it given a formula, here we're recasting long
indicators.l = dcast(indicators.m,
                     WEO.Country.Code + ISO + Country + variable ~ indicator)

# Remove the leading X now that year is no longer a variable name
indicators.l$year = substr(indicators.l$variable, 2, 5)
indicators.l$variable = NULL

# Reorder by country and year
indicators.l = indicators.l[order(indicators.l$WEO.Country.Code, indicators.l$year), ]
# Now that we're reordered, calculate leading and lagging gdp and growth rates
# This will allow us to calculate constant_usd_gdp all at once rather than 1 year at a time
indicators.l = data.table(indicators.l)[, c(
  "lag.current_usd_gdp",
  "lag.gdp_growth",
  "lead.current_usd_gdp",
  "lead.gdp_growth"
) := list(
  c(NA, current_usd_gdp[-.N])
  ,
  c(NA, gdp_growth[-.N])
  ,
  c(current_usd_gdp[-1], NA)
  ,
  c(gdp_growth[-1], NA)
)
, by = WEO.Country.Code]

# Start calculating constant gdp
indicators.l$constant_usd_gdp = NA
# For cases where the year is less than the base year
indicators.l$constant_usd_gdp[which(indicators.l$year < base_year)] = indicators.l$lead.current_usd_gdp[which(indicators.l$year <
                                                                                                                base_year)] / (1 + (indicators.l$lead.gdp_growth[which(indicators.l$year <
                                                                                                                                                                         base_year)] / 100))
# For cases where the year is equal to base year
indicators.l$constant_usd_gdp[which(indicators.l$year == base_year)] = indicators.l$current_usd_gdp[which(indicators.l$year ==
                                                                                                            base_year)]
# For cases where the year is greater than the base year
indicators.l$constant_usd_gdp[which(indicators.l$year > base_year)] = indicators.l$lag.current_usd_gdp[which(indicators.l$year >
                                                                                                               base_year)] * (1 + (indicators.l$gdp_growth[which(indicators.l$year > base_year)] /
                                                                                                                                     100))

# Calculate the deflator index
indicators.l$deflator = round((indicators.l$constant_usd_gdp / indicators.l$current_usd_gdp) * 100,
                              6)

# Drop unnecessary columns, rename, and write csv
keep = c("WEO.Country.Code", "ISO", "Country", "year", "deflator")
indicators.l = indicators.l[, keep, with = F]
names(indicators.l) = c("weo_country_code",
                        "iso_alpha_3_code",
                        "country_name",
                        "year",
                        "deflator")
write.csv(
  indicators.l,
  "output/usd_deflator_2014_2016_apr.csv",
  na = "",
  row.names = F
)