list.of.packages <- c("data.table","readr","dplyr","DescTools","stringr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)


source("load_configs.R")
# wd='~/git/ddw-r-scripts'
wd = configs$wd
setwd(wd)

crs <- fread(paste0(configs$ddw_update_path,'/mirrors/crs_mirror.csv'),na.strings="")

#Get only oda from crs
#Filters that will be used to get only ODA
flow_code_v <- c(11,13,19)
category_v <- 10

oda_filter <- crs[crs$flow_code %in% flow_code_v & crs$category == category_v & !is.na(crs$usd_disbursement) & crs$usd_disbursement != 0]
oda_filter$total_desc = paste(oda_filter$short_description, oda_filter$long_description)


fund_no_space = oda_filter[grepl("fund",oda_filter$total_desc,ignore.case=T,useBytes=T),]
fund_space = oda_filter[grepl(" fund ",oda_filter$total_desc,ignore.case=T,useBytes=T),]

fund_diff = setdiff(fund_no_space$total_desc,fund_space$total_desc)
fwrite(data.table(fund_diff),"output/fund_diff.csv")

vaccin_no_space = oda_filter[grepl("vaccin",oda_filter$total_desc,ignore.case=T,useBytes=T),]
vaccin_space= oda_filter[grepl(" vaccin",oda_filter$total_desc,ignore.case=T,useBytes=T),]

vaccin_diff = setdiff(vaccin_no_space$total_desc,vaccin_space$total_desc)
fwrite(data.table(vaccin_diff),"output/vaccin_diff.csv")
