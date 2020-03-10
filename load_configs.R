list.of.packages <- c("config")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)


# Set production as the default config. Any values not defined in the production section will fall back to defaults, which is acting as our test
Sys.setenv(R_CONFIG_ACTIVE = "production")
configs <- config::get()
