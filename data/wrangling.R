###Load packages

p_required <- c("dplyr", "ggplot2", "readxl", "tidyr", "lubridate", "stringr", "countrycode", "ISOcodes", "r2country", "DMwR2") #package names
packages <- rownames(installed.packages()) #vector with all existing packages
p_to_install <- p_required[!(p_required %in% packages)] #vector with all packages that are in p_required, but not                                                         packages
if (length(p_to_install) > 0) { 
  install.packages(p_to_install)
}#install all packages in p_to_install                                                 
sapply(p_required, require, character.only = TRUE)#make sure all objects only in character
rm(p_required, p_to_install, packages) #remove now unnecessary vectors

rm(list = ls())

aid_oecd <- read.csv2("datasources/Aid/oecd_aid.csv")
oecd_lbls <- read_xlsx("datasources/Aid/oecd_labels.xlsx")
wdi_food <- read.csv("datasources/WB/food_db.csv")
wgi <- read.csv("datasources/WB/wgi_data.csv")
wdi <- read.csv("datasources/WB/wdi.csv")
gdp_deflate_data <- read_xls("datasources/WB/gdp_deflator.xls", skip = 3)
ucdp_base <- read.csv("datasources/ucdp/UcdpPrioConflict_v24_1.csv")
ucdp_brd_c <- read.csv("datasources/ucdp/BattleDeaths_v24_1_conf.csv")
ucdp_geo <- read.csv("datasources/ucdp/GEDEvent_v24_1.csv")
vdem <- read.csv("datasources/V-Dem/vdem_full.csv")
emdat <- read_xlsx("datasources/emdat/emdat.xlsx")
fao_diet <- read.csv("datasources/fao/dietcost.csv")
fao_foodsec <- read.csv("datasources/fao/foodsecurity.csv")
fao_foodtrade <- read.csv("datasources/food_trade/Trade_DetailedTradeMatrix_E_All_Data.csv")
rainfall <- read.csv("datasources/rainfall/average-precipitation-per-year.csv")
road_net2017 <- read.csv2("datasources/road_net/density-by-land-area---2017.csv")
road_net2018 <- read.csv2("datasources/road_net/density-by-land-area---2018.csv")
road_net2019 <- read.csv2("datasources/road_net/density-by-land-area---2019.csv")
road_net2020 <- read.csv2("datasources/road_net/density-by-land-area---2020.csv")
road_net2021 <- read.csv2("datasources/road_net/density-by-land-area---2021.csv")

road_use2017 <- read.csv2("datasources/road_net/use-rate-by-network---2017.csv")
road_use2018 <- read.csv2("datasources/road_net/use-rate-by-network---2018.csv")
road_use2019 <- read.csv2("datasources/road_net/use-rate-by-network---2019.csv")
road_use2020 <- read.csv2("datasources/road_net/use-rate-by-network---2020.csv")
road_use2021 <- read.csv2("datasources/road_net/use-rate-by-network---2021.csv")

aiddat_sdgs <- read.csv("datasources/Aid/Financing_2030_Agenda_SDG_V1/Aggregates_Financing_the_2030_Agenda_for_Sustainable_Development_Dataset_Version_1_0.csv")
region_iso <- read.csv("datasources/ISO/region_iso.csv")
data(ISO_3166_1)

###########
#WDI Data##
###########

shape_wdi <- function(data, chartrue){
  extract_digits <- function(name) {
    str_extract(name, "\\d{4}")
  }
  
  data_names <- sapply(names(data), extract_digits)
  data_names[1:4] <- c("indx", "indx_code", "cntr", "cntr_code")
  names(data) <- data_names
  
  data <- data %>%
    mutate(indx_code = str_replace_all(str_to_lower(indx_code), "\\.", "_"))
  if (chartrue) {
    data <- data %>%
      mutate(across(everything(), ~ na_if(., '..')))
  }
  
  data_re <- data %>%
    pivot_longer(
      cols = matches("^\\d{4}$"),
      names_to = "year",
      values_to = "amount"
    )
  data_re$amount <- as.numeric(data_re$amount)
  indx_book <- data_re %>% distinct(indx_code, indx)
  
  data_re <- data_re %>%
    group_by(cntr, indx, year) %>%
    mutate(row = row_number()) %>%
    tidyr::pivot_wider(names_from = c(indx_code, indx), values_from = amount) %>%
    select(-row)
  
  names(data_re) <- sapply(names(data_re), function(x) {
    if (grepl("_", x)) {
      sub("^[^_]*_", "", x)
    } else {
      x
    }
  })
  
  return(data_re)
}

wdi_re <- shape_wdi(wdi,TRUE)

#economic downturn
wdi_re$ec_dwntrn <- NA
wdi_re$ec_dwntrn[wdi_re$`gdp_mktp_kd_zg_GDP growth (annual %)` > 0 ] <- 1 
wdi_re$ec_dwntrn[wdi_re$`gdp_mktp_kd_zg_GDP growth (annual %)` < 0 ] <- 0
wdi_re$log_gdp <- log(wdi_re$`gdp_mktp_kd_GDP (constant 2015 US$)`)
wdi_re$log_gdp_pc <- log(wdi_re$`gdp_pcap_kd_GDP per capita (constant 2015 US$)`)

################
#OECD Aid Data##
################

aid_oecd <- aid_oecd %>% select(DONOR, RECIPIENT, TIME_PERIOD, SECTOR, OBS_VALUE)
names(aid_oecd) <- c("oecd_dnr", "code", "year", "oecd_sctr", "value")
aid_oecd <- aid_oecd[aid_oecd$oecd_sctr != 520,]#food development assistance is duplicated
aid_oecd <- aid_oecd %>% arrange(code, year, oecd_dnr, oecd_sctr, value)

aid_oecd <- aid_oecd %>%
left_join(oecd_lbls, by = c("oecd_sctr" = "label")) %>%
select(code, year, oecd_dnr, value, name) %>%
rename(oecd_sec = name)

aid_oecd$value <- as.numeric(aid_oecd$value)
aid_oecd$value = aid_oecd$value * 1000

aid_oecd <- aid_oecd %>%
pivot_wider(
  names_from = oecd_sec, 
  values_from = value
)
aid_oecd <- aid_oecd[aid_oecd$oecd_dnr == "DAC",]

################################
#AidData Financing2030 AidData##
################################

aiddat_sdgs <- aiddat_sdgs %>%
            group_by(year, recipient_name) %>%
            summarise(ad_sdg2_aid = sum(sdg_2_sum),
                      ad_sdg2_proj = sum(sdg_2_n_proj),
                      ad_tot_aid = sum(disbursement),
                      ad_tot_proj = sum(total_proj))

aiddat_sdgs$ad_exc2_aid <- aiddat_sdgs$ad_tot_aid - aiddat_sdgs$ad_sdg2_aid
aiddat_sdgs$ad_exc2_proj <- aiddat_sdgs$ad_tot_proj - aiddat_sdgs$ad_sdg2_proj
aiddat_sdgs$code <- countrycode(aiddat_sdgs$recipient_name, "country.name", "iso3c")


###############
##GDP Deflate##
###############

gdp_deflate_data <- gdp_deflate_data %>%
  rowwise() %>%  # Apply operation row by row
  mutate(`2020` = ifelse(
    is.na(`2020`) & sum(!is.na(c(`2018`, `2019`, `2021`, `2022`))) >= 2,  # Only interpolate if 2020 is NA and at least two non-NA values exist
    approx(x = c(2018, 2019, 2021, 2022), 
           y = c(`2018`, `2019`, `2021`, `2022`), 
           xout = 2020)$y,  # Interpolate for 2020
    `2020`  # If 2020 is not NA, leave the value as is
  )) %>%
  ungroup()
gdp_deflate <-gdp_deflate_data %>% select("Country Name", "Country Code", "2015", "2020")
names(gdp_deflate) <- c("cntr", "code", "gdp_deflate_2015", "gdp_deflate_2020")

#######
##FAO##
#######

shape_fao <- function(dat){
  
  dat <- dat[dat$Element == "Value",]
  dat <- dat %>% select(Item, Area, Year.Code, Value)
  dat <- dat %>% mutate(year = str_sub(Year.Code, -4))
  dat <- dat %>% select(-Year.Code)
  dat <- dat %>%
    pivot_wider(
      names_from = Item,      # Columns to use as new column names
      values_from = Value     # Column to populate the values
    )
  dat$code <- countrycode(dat$Area, "country.name", "iso3c")
  dat <- dat[dat$Area != "China, mainland",] #duplicates
  
  dat <- dat %>% select(Area, year, code, everything())
  dat[4:ncol(dat)] <- lapply(dat[4:ncol(dat)], as.numeric)
  return(dat)
}

fao_foodsec_re <- shape_fao(fao_foodsec)
fao_diet_re <- shape_fao(fao_diet)

##################
##FAO Food Trade##
##################

#https://data.harvestportal.org/dataset/faostat-detailed-trade-matrix

# Select only columns that are either NOT year columns or are from 2000 onwards
fao_foodtrade <- fao_foodtrade %>%
  select(Reporter.Countries, Element, matches("^Y(200[0-9]|201[0-9]|202[0-9]|2023)$"))

# Convert to long format
fao_foodtrade <- fao_foodtrade %>%
  pivot_longer(cols = starts_with("Y"), names_to = "Year", values_to = "Value") %>%
  mutate(Year = as.numeric(sub("Y", "", Year)))  # Convert Year column to numeric

# Aggregate imports and exports separately
fao_foodtrade <- fao_foodtrade %>%
  filter(Element %in% c("Export value", "Import value")) %>%
  group_by(Reporter.Countries, Year, Element) %>%
  summarise(Total_Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Element, values_from = Total_Value, names_prefix = "Total_")


names(fao_foodtrade) <- c("country", "year", "food_exp", "food_imp")
fao_foodtrade$food_exp <- as.numeric(fao_foodtrade$food_exp)
fao_foodtrade$food_imp <- as.numeric(fao_foodtrade$food_imp*1000)

fao_foodtrade$code <- countrycode(fao_foodtrade$country, "country.name", "iso3c")
fao_foodtrade <- fao_foodtrade[fao_foodtrade$country!= "Ethiopia PDR",]
fao_foodtrade <- fao_foodtrade[fao_foodtrade$country!= "Sudan (former)",]
fao_foodtrade <- fao_foodtrade[fao_foodtrade$country!= "USSR",]
fao_foodtrade <- fao_foodtrade[!is.na(fao_foodtrade$code),]

########
##UCDP##
########

ucdp_geo <- ucdp_geo[ucdp_geo$type_of_violence == 3,]
ucdp_geo <- ucdp_geo %>%
         group_by(year, country) %>%
         summarise(
            bd_best = sum(`best`, na.rm = TRUE),
            bd_high = sum(`high`, na.rm = TRUE),
            bd_low = sum(`low`, na.rm = TRUE),
            Number_of_Events = n()                          
          ) %>%
         ungroup()
ucdp_geo$fight <- ifelse(ucdp_geo$bd_best>24,1,0)
ucdp_geo$code <- countrycode(ucdp_geo$country, "country.name", "iso3c")
ucdp_geo$code[ucdp_geo$country=="Yemen (North Yemen)"] <- "YEM"


#######
##WGI##
#######

wgi <- wgi %>% select(Series.Name, Series.Code, Country.Name, Country.Code, everything())
wgi_re <- shape_wdi(wgi, TRUE)
wgi_re$gov_score <- (wgi_re$`est_Control of Corruption: Estimate` + wgi_re$`est_Government Effectiveness: Estimate` + wgi_re$`est_Regulatory Quality: Estimate` + wgi_re$`est_Rule of Law: Estimate` + wgi_re$`est_Voice and Accountability: Estimate`) /5
wgi_re <- wgi_re %>% select(code, year, gov_score)

########
##VDEM##
########

vdem <- vdem %>% select(country_text_id, year, e_pt_coup, e_pt_coup_attempts, e_autoc, e_democ, e_p_polity, e_polcomp, e_polity2, e_regiongeo, e_regionpol, e_regionpol_6C, e_regionpol_7C)

#########
##EMDAT##
#########

emdat_re <- emdat %>%
  rowwise() %>%
  mutate(year = list(seq(`Start Year`, `End Year`))) %>%
  unnest(year)

emdat_re <- emdat_re %>%
  group_by(ISO, year) %>%
  summarise(
    Total_Affected = sum(`Total Affected`, na.rm = TRUE), 
    Number_of_Events = n()                          
  ) %>%
  ungroup()

#################
##Precipitation##
#################

#Calculate z-scores for annual precipitation
win_size <- 30
get_z_scores <- function(df, win_size) {

    z_scores <- rep(NA, nrow(df))
  
  for (i in (win_size + 1):nrow(df)) {

    win_dat <- df$Annual.precipitation[(i - win_size):(i - 1)]
    
    mean_win <- mean(win_dat)
    sd_win <- sd(win_dat)
    
    z_scores[i] <- (df$Annual.precipitation[i] - mean_win) / sd_win
  }
  
  df$z_score <- z_scores
  
  return(df)
}


rainfall <- rainfall %>%
  group_by(Code) %>%
  do(get_z_scores(., win_size))  

########
##Road##
########


road_net2017$year <- 2017
road_net2018$year <- 2018
road_net2019$year <- 2019
road_net2020$year <- 2020
road_net2021$year <- 2021
r_net_df <- rbind(road_net2017, road_net2018, road_net2019, road_net2020, road_net2021)

road_use2017$year <- 2017
road_use2018$year <- 2018
road_use2019$year <- 2019
road_use2020$year <- 2020
road_use2021$year <- 2021
r_use_df <- rbind(road_use2017, road_use2018, road_use2019, road_use2020, road_use2021)

roads_df <- merge(r_net_df, r_use_df, by = c("Category", "year"), all.x = TRUE)
roads_df$code <- countrycode(roads_df$Category, "country.name", "iso3c")
roads_df$code[roads_df$Category == "Kosovo"] <- "XKX"#hardcode one missing Iso

##########
##REGION##
##########

region_iso <- region_iso %>% select(alpha.3, region, sub.region)
names(region_iso)[1] <- "code"

#########
##MERGE##
#########


food_df <- merge(aiddat_sdgs, aid_oecd[aid_oecd$code %in% ISO_3166_1$Alpha_3,], by = c("code", "year"), all.x = TRUE)
food_df <- merge(food_df, gdp_deflate, by = c("code"), all.x = TRUE)
food_df <- merge(food_df, wdi_re, by = c("code", "year"), all.x = TRUE)
food_df <- merge(food_df, wgi_re, by = c("code", "year"), all.x = TRUE)
food_df <- merge(food_df, vdem, by.x = c("code", "year"), by.y = c("country_text_id", "year"), all.x = TRUE)
food_df <- merge(food_df, emdat_re, by.x = c("code", "year"), by.y = c("ISO", "year"), all.x = TRUE)
food_df <- merge(food_df, fao_foodsec_re, by = c("code", "year"), all.x = TRUE)
food_df <- merge(food_df, fao_diet_re, by = c("code", "year"), all.x = TRUE)
food_df <- merge(food_df, ucdp_geo, by = c("code", "year"), all.x = TRUE)
food_df <- merge(food_df, region_iso, by = c("code"), all.x = TRUE)
food_df <- merge(food_df, roads_df, by = c("code", "year"), all.x = TRUE)
food_df <- merge(food_df, rainfall, by.x = c("code", "year"),  by.y = c("Code", "Year"), all.x = TRUE)
food_df <- merge(food_df, fao_foodtrade, by = c("code", "year"), all.x = TRUE)


##########################
##Wrangle merged dataset##
##########################


#where conflict is NA it is 0, as not in UCDP database
food_df$fight[is.na(food_df$fight)] <- 0
#manually update Coup Data
food_df$e_pt_coup[is.na(food_df$e_pt_coup)] <- 0
food_df$e_pt_coup[food_df$country == "Mali" & food_df$year ==2020] <- 1
food_df$e_pt_coup[food_df$country == "Mali" & food_df$year ==2021] <- 1
food_df$e_pt_coup[food_df$country == "Tunisia" & food_df$year ==2021] <- 1
food_df$e_pt_coup[food_df$country == "Guinea" & food_df$year ==2021] <- 1
food_df$e_pt_coup[food_df$country == "Sudan" & food_df$year ==2021] <- 1
food_df$e_pt_coup[food_df$country == "Burkina Faso" & food_df$year ==2022] <- 1
food_df$e_pt_coup[food_df$country == "Niger" & food_df$year ==2023] <- 1
food_df$e_pt_coup[food_df$country == "Sierra Leone" & food_df$year ==2023] <- 1
food_df$e_pt_coup[food_df$country == "Gabon" & food_df$year ==2023] <- 1
food_df$e_pt_coup[food_df$country == "Myanmar" & food_df$year ==2021] <- 1

#regions as factors

#coups
# mali 2020
# mali 2021
# tunesia 2021
# guinea 2021
# sudan 2021
# burkina 2022
# niger 2023
# sierra leone 2023
# gabon 2023
# myanmar 2021

#assume disaster affected people are 0 where NA from nature of EMDAT

food_df$Total_Affected[is.na(food_df$Total_Affected)] <- 0

#adjust monetary variables to same base year

food_df <- food_df %>% select(code, recipient_name, year, `Prevalence of severe food insecurity in the total population (percent) (3-year average)`, 
                              `Prevalence of moderate or severe food insecurity in the total population (percent) (3-year average)`, 
                              `Number of moderately or severely food insecure people (million) (3-year average)`, 
                              `Number of severely food insecure people (million) (3-year average)`, `development food assistance`, `all sectors`, 
                      `cpi_totl_Consumer price index (2010 = 100)`, `val_food_zs_un_Food exports (% of merchandise exports)`, 
                      `val_food_zs_un_Food imports (% of merchandise imports)`, `prd_food_xd_Food production index (2014-2016 = 100)`, 
                      `klt_dinv_wd_gd_zs_Foreign direct investment, net inflows (% of GDP)`, `gdp_mktp_kd_GDP (constant 2015 US$)`, 
                      `gdp_mktp_kd_zg_GDP growth (annual %)`, `gdp_pcap_kd_zg_GDP per capita growth (annual %)`, `gdp_pcap_kd_GDP per capita (constant 2015 US$)`, 
                      `cpi_totl_zg_Inflation, consumer prices (annual %)`, `trf_pwkr_dt_gd_zs_Personal remittances, received (% of GDP)`, 
                      `pop_grow_Population growth (annual %)`, `pop_totl_Population, total`, log_gdp, log_gdp_pc, gov_score, e_pt_coup, 
                      sub.region, region, Total_Affected, z_score, Network.Density.by.Land.Area, food_exp, food_imp,
                      ad_sdg2_aid, ad_sdg2_proj, ad_tot_aid, ad_tot_proj, ad_tot_proj, ad_exc2_aid, ad_exc2_proj, fight, 
                      everything())

names <- c("code", "country", "year", "prev_sfi", "prev_msfi", "n_msfi", "n_sfi", "oecd_food_aid", "oecd_tot_aid",
           "cpi", "food_exp_shr_merch", "food_imp_shr_merch", "food_prod", "fdi_netin", "gdp", "gdp_grw", "gdp_pc_grw", "gdp_pc", "infl", "remmit_gdp", "pop_growth", "pop", 
           "log_gdp", "log_gdp_pc", "gov_score", "e_pt_coup", "subreg", "reg", "disast_affect", "prec_z", "net_dens")

all_names <- c(names, names(food_df)[(length(names) + 1):ncol(food_df)])
names(food_df) <- all_names

write.csv(food_df, "food_df_all.csv", row.names = FALSE)

##############################
###Wrangle for final analysis#
##############################

food_df <- read.csv("food_df_all.csv")

food_df <- food_df[food_df$year>2015 & food_df$year < 2021,]

food_df$food_exp <- as.numeric(food_df$food_exp)
food_df$food_imp <- as.numeric(food_df$food_imp)
food_df$pop <- as.numeric(food_df$pop)
food_df$na_prev <- ifelse(is.na(food_df$prev_msfi), 1, 0)

#Imputation for Covariates

cols_to_impute <- c("prev_msfi", "cpi", "food_prod", "gdp", "gdp_pc_grw", "gdp_pc", "infl", "pop_growth", "pop", "gov_score", "prec_z", "food_exp", "food_imp", "net_dens") #other covariates non-empty
cols_to_impute_2016 <- c("prev_msfi", "cpi", "food_prod", "gdp", "gdp_pc_grw", "gdp_pc", "infl", "pop_growth", "pop", "gov_score", "prec_z", "food_exp", "food_imp") #other covariates non-empty

food_df <- food_df[rowSums(is.na(food_df[c("cpi", "food_prod", "gdp", "gdp_pc_grw", "gdp_pc", "infl", "pop_growth", "pop", "gov_score", "prec_z", "food_exp", "food_imp")])) < 10, ] #omit observations where all Covariates NA

food_df <- food_df %>%
  group_by(country) %>%  # Group by country
  filter(n() >= 5 & any(!is.na(prev_msfi))) %>%  # Keep only if country appears at least 5 times and has at least one non-NA prev_msfi
  ungroup()

#Imputation
food_df <- food_df %>%
  group_split(year) %>%  
  lapply(function(df) {
    if (unique(df$year) > 2016) {  # Apply only if year > 2016
      df[cols_to_impute] <- knnImputation(df[cols_to_impute], k = 5)
    }
    if (unique(df$year) == 2016) {  # Apply only if year > 2016
      df[cols_to_impute_2016] <- knnImputation(df[cols_to_impute_2016], k = 5)
    }
    return(df)  # Return modified or unmodified dataframe
  }) %>%
  bind_rows()  # Combine back into a single dataframe

#get change rate in food ins
food_df <- food_df %>%
  group_by(code) %>% 
  arrange(year) %>% 
  mutate(
    change_msfi = (lag(prev_msfi) - prev_msfi) / lag(prev_msfi), 
    change_sfi = (lag(prev_sfi) - prev_sfi) / lag(prev_sfi),
    change_abs_msfi = (lag(n_msfi) - n_msfi) / lag(n_msfi), 
    change_abs_sfi = (lag(n_sfi) - n_sfi) / lag(n_sfi)
  )

#Get logged GDP values
food_df$log_gdp <- log(food_df$gdp)
food_df$log_gdp_pc <- log(food_df$gdp_pc)

#get aid data to 2015 dollars
food_df$ad_exc2_aid <- food_df$ad_exc2_aid * (food_df$gdp_deflate_2015/food_df$gdp_deflate_2020)
food_df$ad_sdg2_aid <- food_df$ad_sdg2_aid * (food_df$gdp_deflate_2015/food_df$gdp_deflate_2020)

#omit countries where no deflation was possible - only 2
food_df <- food_df %>%
  group_by(country) %>%
  filter(!any(is.na(gdp_deflate_2020))) %>%
  ungroup()

#get aid lags and aid as share of gdp
food_df$ad_sdg2_aid_gdp <- (food_df$ad_sdg2_aid / food_df$gdp) * 100
food_df$ad_exc2_aid_gdp <- (food_df$ad_exc2_aid / food_df$gdp) * 100

food_df <- food_df %>%
  group_by(code) %>% 
  arrange(year) %>% #
  mutate(
    ad_sdg2_aid_lag = lag(ad_sdg2_aid),
    ad_exc2_aid_lag = lag(ad_exc2_aid),
    ad_sdg2_aid_gdp_lag = lag(ad_sdg2_aid_gdp),
    ad_exc2_aid_gdp_lag = lag(ad_exc2_aid_gdp)
  )

#Imports and Exports as Share of GDP

food_df$food_imp_gdp <- food_df$food_imp / food_df$gdp
food_df$food_exp_gdp <- food_df$food_exp / food_df$gdp
food_df$food_imp_net <- food_df$food_imp - food_df$food_exp
food_df$food_imp_net_gdp <- (food_df$food_imp_net / food_df$gdp)*100

#convert units

food_df$ad_sdg2_aid_lag <- food_df$ad_sdg2_aid_lag / 1000000
food_df$ad_sdg2_aid <- food_df$ad_sdg2_aid / 1000000
food_df$ad_exc2_aid <- food_df$ad_exc2_aid / 1000000
food_df$ad_exc2_aid_lag <- food_df$ad_exc2_aid_lag / 1000000
food_df$pop <- food_df$pop / 1000000
food_df$disast_affect <- ifelse(food_df$disast_affect != 0, log(food_df$disast_affect), 0.00000000000000000001)

#Get relevant years and omit NAs
food_df <- food_df %>%
  group_by(country) %>%
  filter(all(c(2017, 2018, 2019,2020) %in% year) & 
           all(!is.na(change_msfi[year %in% c(2017, 2018, 2019,2020)]))) %>%
  ungroup()

food_df <- food_df[food_df$year == 2017 | food_df$year == 2018 | food_df$year == 2019 | food_df$year == 2020,]

#overall clean dataset 
write.csv(food_df, "food_df_rob_large.csv", row.names = FALSE)

#smaller dataset without NAs for DV
food_df_ana_small <- food_df %>%
  group_by(country) %>%
  filter(!any(na_prev == 1, na.rm = TRUE)) %>%
  ungroup()
write.csv(food_df_ana_small, "food_df_rob.csv", row.names = FALSE)

food_df_ana_small <- food_df_ana_small %>% select(country, year, change_msfi, ad_sdg2_aid_lag, ad_sdg2_aid, ad_exc2_aid_gdp, infl, food_prod, food_imp_net_gdp,
                                  log_gdp, log_gdp_pc, gdp_pc_grw, gov_score, e_pt_coup, disast_affect, fight, prec_z, net_dens)

write.csv(food_df_ana_small, "food_df_ana.csv", row.names = FALSE)

#for analysis
food_df_ana <- food_df %>% select(country, year, change_msfi, ad_sdg2_aid_lag, ad_sdg2_aid, ad_exc2_aid_gdp, infl, food_prod, food_imp_net_gdp,
                              log_gdp, log_gdp_pc, gdp_pc_grw, gov_score, e_pt_coup, disast_affect, fight, prec_z, net_dens)

write.csv(food_df_ana, "food_df_ana_large.csv", row.names = FALSE)


#########################
##Wrangle Borders Dataset
#########################


borders <- read.csv("borders.csv")
borders$country_code <- countrycode(borders$country_code, "iso2c", "iso3c")
borders$country_border_code <- countrycode(borders$country_border_code, "iso2c", "iso3c")


write.csv(borders, "borders_csv", row.names = FALSE)