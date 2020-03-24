list.of.packages <- c("data.table","readr","dplyr","DescTools","stringr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)


source("load_configs.R")
wd=configs$wd
setwd(wd)

crs <- fread(paste0(configs$ddw_update_path, '/mirrors/crs_mirror.csv'),na.strings="")

#Get only oda from crs
#Filters that will be used to get only ODA
flow_code_v <- c(11,13,19)
category_v <- 10

oda_filter <- crs[crs$flow_code %in% flow_code_v & crs$category == category_v & !is.na(crs$usd_disbursement) & crs$usd_disbursement != 0]

oda_filter$oda_donor_bundle <- 'E'
# Get vectors to generate ODA bundle codes

# Bundle Code F

aid_type_v <- c('E01','E02','G01','H01','H02')
finance_type_v <- c(210)
sector_code_v <- c(600)
purpose_code_v <- c(93010,99820)

#oda_filter$oda_donor_bundle = NA
#oda_filter[,oda_donor_bundle := as.character(oda_donor_bundle)]


# Filter using aid_type,finance_type,sector_code and purpose-code
oda_filter$oda_donor_bundle[which(oda_filter$aid_type %in% aid_type_v | oda_filter$finance_type %in% finance_type_v |
                                    oda_filter$sector_code %in% sector_code_v |
                                    oda_filter$purpose_code %in% purpose_code_v)] <- 'F'

# Bundle Code A
'%!in%' <- function(x,y)!('%in%'(x,y))

ignore_bundle_code <- c('F')
flow_code_v <- c(13,19)
oda_filter$oda_donor_bundle[which(oda_filter$flow_code %in%  flow_code_v 
                            & oda_filter$oda_donor_bundle %!in% ignore_bundle_code)] <- 'A'

# Bundle G
ignore_bundle_code <- append(ignore_bundle_code,'A')
aid_type_v <- 'D%'
ftc_v <- 1

oda_filter$oda_donor_bundle[which((oda_filter$aid_type %like%  aid_type_v |
  oda_filter$ftc ==  ftc_v) & oda_filter$oda_donor_bundle %!in% ignore_bundle_code)] <- 'G'



# Bundle Code B
ignore_bundle_code <- append(ignore_bundle_code,'G')
aid_type_v <- c('A01','A02')
oda_filter$oda_donor_bundle[which(oda_filter$aid_type %in%  aid_type_v  & 
                                    oda_filter$oda_donor_bundle %!in% ignore_bundle_code)] <- 'B'

# Bundle Code C
ignore_bundle_code <- append(ignore_bundle_code,'B')
purpose_code_v <- c(52010,53030,53040,72040)

oda_filter$oda_donor_bundle[which(oda_filter$purpose_code %in%  purpose_code_v &
                                    oda_filter$oda_donor_bundle %!in% ignore_bundle_code)] <- 'C'

  
  # Bundle Code B
ignore_bundle_code <- append(ignore_bundle_code,'C')
# Check for aid type and recipient code
aid_type_v <- c('B01','B03','B04')
oda_filter$oda_donor_bundle[which(oda_filter$aid_type %in%  aid_type_v & oda_filter$recipient_code != 9998 & 
                                    oda_filter$oda_donor_bundle %!in% ignore_bundle_code)] <- 'B'

# Bundle Code D
# aid type B01 or B03 or B04 and recipient code 998
aid_type_v <- c('B01','B03','B04')

oda_filter$oda_donor_bundle[which(oda_filter$recipient_code == 9998 & 
                                    oda_filter$aid_type %in% aid_type_v & 
                                    oda_filter$oda_donor_bundle %!in% ignore_bundle_code)] <- 'D'

# Bundle Code B
ignore_bundle_code <- append(ignore_bundle_code,'D')
purpose_code_v <- 51010
pba_v <- 1
# also check for combination of aid_type and recipient code 
# aid_type B01 or B03 or Bo4 and !recipient code 998

#channel channel code to character and check if
channel_code_g <- '3%'


#Check for aid type,purpose_code,donor_code,pba,short_description and long_description
oda_filter$oda_donor_bundle[which(( oda_filter$purpose_code ==  purpose_code_v |
                                    oda_filter$pba ==  pba_v |
                                    as.character(oda_filter$channel_code) %like%  channel_code_g) & 
                                    oda_filter$oda_donor_bundle %!in% ignore_bundle_code)] <- 'B'


# Bundle code D
channel_code_g <- '51%'

#Check for channel code
oda_filter$oda_donor_bundle[which(as.character(oda_filter$channel_code) %like%  channel_code_g & 
                                    oda_filter$oda_donor_bundle %!in% ignore_bundle_code)] <- 'D'

# Bundle Code B

donor_code_v <- c(901,905,906,907,909,912,913,914,915,916,921,951,953,958,976,990,1013)
swp <- ' SWAP '
description_g <- ' finance | fund | subsidy | financement | fonds |subsidie '

oda_filter$short_description_l <- str_to_lower(oda_filter$short_description)
oda_filter$long_description_l <- str_to_lower(oda_filter$long_description)

oda_filter$oda_donor_bundle[which(oda_filter$donor_code %in%  donor_code_v & oda_filter$usd_disbursement >= 1 & 
                                    oda_filter$oda_donor_bundle %!in% ignore_bundle_code)] <- 'B'


oda_filter$oda_donor_bundle[which(( grepl(description_g,oda_filter$short_description_l,ignore.case  = TRUE, useBytes = TRUE)) &
                                    oda_filter$oda_donor_bundle %!in% ignore_bundle_code)] <- 'B'

oda_filter$oda_donor_bundle[which(( grepl(description_g,oda_filter$long_description_l,ignore.case  = TRUE, useBytes = TRUE)) &
                                    oda_filter$oda_donor_bundle %!in% ignore_bundle_code)] <- 'B'

oda_filter$oda_donor_bundle[which(( grepl(swp,oda_filter$long_description,ignore.case  = FALSE, useBytes = TRUE)) &
                                    oda_filter$oda_donor_bundle %!in% ignore_bundle_code)] <- 'B'

length(which(oda_filter$oda_donor_bundle=="B"))
description_g = 'finance|fund|subsidy|financement|fonds|subsidie'

oda_filter$short_description_l <- str_to_lower(oda_filter$short_description)
oda_filter$long_description_l <- str_to_lower(oda_filter$long_description)

oda_filter$oda_donor_bundle[which(oda_filter$donor_code %in%  donor_code_v & oda_filter$usd_disbursement >= 1 & 
                                    oda_filter$oda_donor_bundle %!in% ignore_bundle_code)] <- 'B'


oda_filter$oda_donor_bundle[which(( grepl(description_g,oda_filter$short_description_l,ignore.case  = TRUE, useBytes = TRUE)) &
                                    oda_filter$oda_donor_bundle %!in% ignore_bundle_code)] <- 'B'

oda_filter$oda_donor_bundle[which(( grepl(description_g,oda_filter$long_description_l,ignore.case  = TRUE, useBytes = TRUE)) &
                                    oda_filter$oda_donor_bundle %!in% ignore_bundle_code)] <- 'B'

oda_filter$oda_donor_bundle[which(( grepl(swp,oda_filter$long_description,ignore.case  = FALSE, useBytes = TRUE)) &
                                    oda_filter$oda_donor_bundle %!in% ignore_bundle_code)] <- 'B'

length(which(oda_filter$oda_donor_bundle=="B"))

# Bundle Code D
long_description_g <- ' research'
# grep %research% and recipient code 998

oda_filter$oda_donor_bundle[which(grepl(long_description_g,oda_filter$long_description,ignore.case  = TRUE, useBytes = TRUE) & oda_filter$recipient_code == 9998 & 
                                    oda_filter$oda_donor_bundle %!in% ignore_bundle_code)] <- 'D'

length(which(oda_filter$oda_donor_bundle=="D"))
long_description_g <- 'research'
# grep %research% and recipient code 998

oda_filter$oda_donor_bundle[which(grepl(long_description_g,oda_filter$long_description,ignore.case  = TRUE, useBytes = TRUE) & oda_filter$recipient_code == 9998 & 
                                    oda_filter$oda_donor_bundle %!in% ignore_bundle_code)] <- 'D'

length(which(oda_filter$oda_donor_bundle=="D"))

# Bundle G
description_g <- ' consultancy | consultant | technical assistance | technical cooperation | training '

#Search for the occurrences of short_description and long_description
oda_filter$oda_donor_bundle[which((grepl(description_g,oda_filter$long_description_l,ignore.case  = TRUE, useBytes = TRUE) |
                                     grepl(description_g,oda_filter$short_description_l,ignore.case  = TRUE, useBytes = TRUE)) &
                                    oda_filter$oda_donor_bundle %!in% ignore_bundle_code)] <- 'G'

length(which(oda_filter$oda_donor_bundle=="G"))
description_g <- 'consultancy|consultant|technical assistance|technical cooperation|training'


#Search for the occurrences of short_description and long_description
oda_filter$oda_donor_bundle[which((grepl(description_g,oda_filter$long_description_l,ignore.case  = TRUE, useBytes = TRUE) |
                                     grepl(description_g,oda_filter$short_description_l,ignore.case  = TRUE, useBytes = TRUE)) &
                                    oda_filter$oda_donor_bundle %!in% ignore_bundle_code)] <- 'G'

length(which(oda_filter$oda_donor_bundle=="G"))

# Bundle Code C
description_g <-' vaccin| vacci| non-food | nonfood | non alimentaire '


# Check for purpose code, short_description and long_description
oda_filter$oda_donor_bundle[which( grepl(description_g,oda_filter$long_description_l,ignore.case  = TRUE, useBytes = TRUE) 
                                   |grepl(description_g,oda_filter$short_description_l,ignore.case  = TRUE, useBytes = TRUE) 
                                   & oda_filter$oda_donor_bundle %!in% ignore_bundle_code)] <- 'C'

length(which(oda_filter$oda_donor_bundle=="C"))

description_g <-'vaccin|vacci|non-food|nonfood|non alimentaire'


# Check for purpose code, short_description and long_description
oda_filter$oda_donor_bundle[which( grepl(description_g,oda_filter$long_description_l,ignore.case  = TRUE, useBytes = TRUE) 
                                    |grepl(description_g,oda_filter$short_description_l,ignore.case  = TRUE, useBytes = TRUE) 
                                  & oda_filter$oda_donor_bundle %!in% ignore_bundle_code)] <- 'C'

length(which(oda_filter$oda_donor_bundle=="C"))


oda_filter[,.('Number of Rows'=.N),by='oda_donor_bundle']

oda_filter$channel_code[which(as.character(oda_filter$channel_code) %like%  channel_code_g
                              & oda_filter$oda_donor_bundle %!in% ignore_bundle_code)]

save(oda_filter, file="output/oda_filter.RData")
# load("output/oda_filter.RData")

channel_map = data.table(read.csv("http://212.111.41.68:8000/single_table?indicator=oecd_crs_channel_code_5_digit_to_di_itep_channel_map&format=csv", as.is=T))
names(channel_map) = c("channel_code","itep_channel_name")
oda_filter = merge(oda_filter,channel_map, by="channel_code")

sector_map = data.table(read.csv("http://212.111.41.68:8000/single_table?indicator=oecd_crs_purpose_code_5_digit_to_di_itep_sector_map&format=csv", as.is=T))
names(sector_map) = c("purpose_code","itep_sector_name")
oda_filter = merge(oda_filter,sector_map, by="purpose_code")

oda_tab = oda_filter[,.(
  usd_disbursement_deflated=sum(usd_disbursement_deflated,na.rm=T),
  usd_disbursement=sum(usd_disbursement,na.rm=T),
  usd_commitment_deflated=sum(usd_commitment_deflated,na.rm=T),
  usd_commitment=sum(usd_commitment,na.rm=T)
  ),by=.(
    donor_name,
    recipient_name,
    itep_sector_name,
    oda_donor_bundle,
    itep_channel_name
  )]

bundle_map = c("Cash (loans/equity)","Cash grants", "Commodities and food", "GPGs and NNGOs", "Mixed project aid", "Non-transfer", "Technical cooperation")
names(bundle_map) = c("A","B","C","D","E","F","G")

oda_tab$bundle_name = bundle_map[oda_tab$oda_donor_bundle]
oda_tab$oda_donor_bundle = NULL

fwrite(oda_tab,"output/bundle_test.csv")
