list.of.packages <- c("WDI","data.table")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

# Change WD to git repo if needed
# Make sure you create ddw.txt in wd formatted as such:
#
# postgres_user_name
# postgres_ip_address
# postgres_port
# postgres_user_pass
# postgres_db_name

wd = "~/git/ddw-r-scripts"
setwd(wd)

source("connect.R")

# Pull in id mappings and deflators. Performing merges to append di_id to the deflator
wdi_id_map = ddw("dimension.wb_wdi_country_to_di_id_map")
deflator = read.csv("output/usd_deflator_2014_2016_apr.csv")
deflator$iso_alpha_3_code = NULL
deflator$country_name = NULL
setnames(deflator,"weo_country_code","imf_weo_country_code")
imf_wdi_map = ddw("dimension.wb_wdi_country_to_imf_weo_country_map")

# Function to convert from constant to current USD
convert_wb_wdi_series_simple = function(df,base_year,precision){
  df = merge(df,imf_wdi_map,by="wb_wdi_country_code",all.x=T)
  base_year_deflator = subset(deflator,year==base_year)
  setnames(base_year_deflator,"deflator","base_year_deflator")
  setnames(base_year_deflator,"year","base_year")
  
  df = merge(df,deflator,by=c("imf_weo_country_code","year"),all.x=T)
  df = merge(df,base_year_deflator,by=c("imf_weo_country_code"),all.x=T)
  df$conversion_factor = df$base_year_deflator/df$deflator
  df$value = round(df$value,precision) * df$conversion_factor
  keep = c("di_id","year","value")
  df = df[keep]
  return(df)
}

# Function to pull from WDI API
dh_wdi = function(indicator,start=1960,end=2018){
  dat <- WDI(country = "all", 
             indicator = indicator, 
             start = start, 
             end = end,
             extra = TRUE
  )
  setnames(dat,"iso3c","wb_wdi_country_code")
  setnames(dat,indicator,"value")
  dat = merge(dat,wdi_id_map,by="wb_wdi_country_code",all.x=T)
  keep = c("di_id","wb_wdi_country_code","year","value")
  dat = dat[keep]
  dat = subset(dat, !is.na(di_id))
  return(dat)
}

# Function to combine WDI pulled dataframes
dh_combine = function(frame_list){
  var_names = names(frame_list)
  dat = Reduce(function(...) merge(..., all=T,by=c("di_id","year")), frame_list)
  names(dat)[3:length(dat)] = var_names
  return(dat)
}

# Example usage
# pop_total = dh_wdi("SP.POP.TOTL")
# pop_total$wb_wdi_country_code = NULL
# pop_male = dh_wdi("SP.POP.TOTL.MA.IN")
# pop_male$wb_wdi_country_code = NULL
# pop_total_and_male = dh_combine(
#   list(
#     "pop_total"=pop_total,
#     "pop_male"=pop_male
#     )
# )

tables_list = list()

# https://github.com/devinit/ddh_donata_scripts/blob/master/data_etl/dh/fact_wb/CREATE_TABLE_gdp_pc_usd_current.sql
gdp_pc_usd_current = dh_wdi("NY.GDP.PCAP.CD")
gdp_pc_usd_2016 = convert_wb_wdi_series_simple(gdp_pc_usd_current,2016,2)
gdp_pc_usd_current$wb_wdi_country_code = NULL

tables_list[["gdp-pc-usd-current"]] = gdp_pc_usd_current
tables_list[["gdp-pc-usd-2016"]] = gdp_pc_usd_2016

gni_usd_current = dh_wdi("NY.GNP.MKTP.CD")
gni_usd_2016 = convert_wb_wdi_series_simple(gni_usd_current,2016,2)

tables_list[["gni-usd-current"]] = gni_usd_current
tables_list[["gni-usd-2016"]] = gni_usd_2016


# https://github.com/devinit/ddh_donata_scripts/blob/master/data_etl/dh/fact_wb/CREATE_TABLE_gdp_usd_current.sql
series = dbReadTable(con,c("public","individual_wb_wdi_series_in_di_dh"))
for(i in 1:nrow(series)){
  serie = series[i,]
  message(serie$di_dh_series_id)
  dat = dh_wdi(serie$wb_wdi_series_code)
  if(grepl("current",serie$wb_wdi_indicator_name,ignore.case=T)){
    constant_name = gsub("current","2016",serie$di_dh_series_id)
    message(constant_name)
    dat_constant = convert_wb_wdi_series_simple(dat,2016,2)
    tables_list[[constant_name]] = dat_constant
  }
  dat$wb_wdi_country_code = NULL
  tables_list[[serie$di_dh_series_id]] = dat
}

setwd(paste0(wd,"/output"))
table_names = names(tables_list)
for(table_name in table_names){
  individual_table = tables_list[[table_name]]
  write.csv(individual_table,paste0(table_name,".csv"),na="",row.names=F)
}

# dbDisconnect(con)
