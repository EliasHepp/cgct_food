###Load packages

p_required <- c("dplyr", "ggplot2", "tidyr", "wbstats", "stringr", "countrycode", "sf", "viridis", "faraway", "reticulate", "corrplot", "gridExtra","patchwork", "rnaturalearthdata", "rnaturalearth", "terra", "matrixcalc") #package names
packages <- rownames(installed.packages()) #vector with all existing packages
p_to_install <- p_required[!(p_required %in% packages)] #vector with all packages that are in p_required, but not                                                         packages
if (length(p_to_install) > 0) { 
  install.packages(p_to_install)
}#install all packages in p_to_install                                                 
sapply(p_required, require, character.only = TRUE)#make sure all objects only in character
rm(p_required, p_to_install, packages) #remove now unnecessary vectors

rm(list = ls())

food_df <- read.csv("food_df_all.csv")
fao_foodsec <- read.csv("datasources/fao/foodsecurity.csv")

###################################
###Large Dataset
###################################

###################################
###Food  
###################################

##################
##Wrangle FAO data

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

##Food Affected Population on Map

map_plot_prev <- fao_foodsec_re %>% filter(year == 2022) %>%  select(Area, `Prevalence of moderate or severe food insecurity in the total population (percent) (3-year average)`) 
world <- ne_countries(scale = "medium", returnclass = "sf")

map_plot_prev <- world %>%
  left_join(map_plot_prev, by = c("name" = "Area"))

map_msfi_2023 <- ggplot(map_plot_prev) +
  geom_sf(aes(fill = `Prevalence of moderate or severe food insecurity in the total population (percent) (3-year average)`), color = "gray", lwd = 0.1) + 
  scale_fill_gradient(low = "#007777", high = "#003333", na.value = "lightgray")  +
  theme_minimal() +
  theme_bw() +
  labs(title = "", fill = "Affected Population, %") +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())

ggsave("../outputs/map_msfi_2023.png", plot = map_msfi_2023, width = 8, height = 6, dpi = 300)


#Availability of DV

cntr_dv_count <- food_df %>%
                  group_by(year) %>%
                      summarise(unit_count = sum(!(is.na(prev_sfi))))

                                
###food aid over time

#Time Trends

plot_foodtime <- fao_foodsec_re %>%
  filter(year >2016 & year < 2023) %>%
  group_by(year) %>%
  summarize(avg_msfi = mean(`Prevalence of moderate or severe food insecurity in the total population (percent) (3-year average)`, na.rm = TRUE),
            avg_sfi = mean(`Prevalence of severe food insecurity in the total population (percent) (3-year average)`, na.rm = TRUE),
            tot_aff_msfi = sum(`Number of moderately or severely food insecure people (million) (3-year average)`, na.rm = TRUE),
            tot_aff_sfi = sum(`Number of severely food insecure people (million) (3-year average)`, na.rm = TRUE))
plot_foodtime$year <- as.numeric(plot_foodtime$year)            

#Average over Time
fi_avg_time <- ggplot(plot_foodtime) +
  geom_line(aes(x = year, y = avg_msfi, color = "Moderate or Severe Food Insecurity"), size = 1) +
  geom_line(aes(x = year, y = avg_sfi, color = "Severe Food Insecurity"), size = 1) +
  scale_color_manual(
    values = c("Severe Food Insecurity" = "#005555", "Moderate or Severe Food Insecurity" = "black"),  # Colors for each line
    name = "Food Insecurity Measure"  # Legend title
  ) +
  scale_x_continuous(
    breaks = seq(min(plot_foodtime$year), max(plot_foodtime$year), by = 1)  # Every 2nd year on the x-axis
  ) +
  scale_y_continuous(
    limits = c(0, 40),  # Start at 0, automatically determine the upper limit
    expand = c(0, 0)    # Remove extra space above and below the range
  ) +
  labs(
    title = "Average Food Insecurity",
    x = "Year",
    y = "Affected Population, %"
  ) +
  theme_bw() 

ggsave("../outputs/fi_avg_time.png", plot = fi_avg_time, width = 8, height = 6, dpi = 300)


#Total Over Time

fi_tot_time <- ggplot(plot_foodtime) +
  geom_line(aes(x = year, y = tot_aff_msfi, color = "Moderate or Severe Food Insecurity"), size = 1) +
  geom_line(aes(x = year, y = tot_aff_sfi, color = "Severe Food Insecurity"), size = 1) +
  scale_color_manual(
    values = c("Severe Food Insecurity" = "#005555", "Moderate or Severe Food Insecurity" = "black"),  # Colors for each line
    name = "Food Insecurity Measure"  # Legend title
  ) +
  scale_x_continuous(
    breaks = seq(min(plot_foodtime$year), max(plot_foodtime$year), by = 1)  # Every 2nd year on the x-axis
  ) +
  labs(
    title = "Total Food Insecurity",
    x = "Year",
    y = "Affected Population, Million"
  ) +
  theme_bw() 

ggsave("../outputs/fi_tot_time.png", plot = fi_tot_time, width = 8, height = 6, dpi = 300)


fi_time_plots <- (fi_avg_time | fi_tot_time) +  
  plot_layout(guides = "collect") +             
  plot_annotation(theme = theme(legend.position = "bottom"))

ggsave("../outputs/fi_time_plots.png", plot = fi_time_plots, width = 16, height = 8, dpi = 300)


##Food Ins per Country Development Group

gni_data <- wb_data(country = "all", indicator = "NY.GDP.PCAP.CD")

#thresholds from https://blogs.worldbank.org/en/opendata/new-world-bank-country-classifications-income-level-2022-2023
gni_data <- gni_data %>%
  mutate(income_level = case_when(
    `NY.GDP.PCAP.CD` < 1086 ~ "Low Income",
    `NY.GDP.PCAP.CD` >= 1086 & `NY.GDP.PCAP.CD` < 4256 ~ "Lower-Middle Income",
    `NY.GDP.PCAP.CD` >= 4256 & `NY.GDP.PCAP.CD` < 13206 ~ "Upper-Middle Income",
    `NY.GDP.PCAP.CD` >= 13206 ~ "High Income"
  ))
gni_data <- gni_data[gni_data$date == 2023,]


msfi_dev <- merge(fao_foodsec_re, gni_data, by.x = c("code", "year"), by.y = c("iso3c", "date"))

print(mean(msfi_dev$`Prevalence of moderate or severe food insecurity in the total population (percent) (3-year average)`[msfi_dev$income_level == "High Income"], na.rm =TRUE))
print(mean(msfi_dev$`Prevalence of moderate or severe food insecurity in the total population (percent) (3-year average)`[msfi_dev$income_level == "Low Income"], na.rm =TRUE))
print(mean(msfi_dev$`Prevalence of moderate or severe food insecurity in the total population (percent) (3-year average)`[msfi_dev$income_level == "Low Income" | msfi_dev$income_level == "Lower-Middle Income" | msfi_dev$income_level == "Upper-Middle Income"], na.rm =TRUE))


###################################
###Aid
###################################

#Countries

plot_aidcntrs <- food_df %>%
  filter(year >2010 & year < 2022) %>%
  group_by(code) %>%
  summarize(tot_food_aid = sum(ad_sdg2_aid, na.rm = TRUE)/1000000,
            tot_aid = sum(ad_tot_aid, na.rm = TRUE)/1000000,
            tot_notfood_aid = sum(ad_exc2_aid, na.rm = TRUE)/1000000,
            oecd_food_aid = sum(oecd_food_aid, na.rm = TRUE)/1000,
            oecd_tot_aid = sum(oecd_tot_aid, na.rm = TRUE)/1000,
            wb_tot_aid = sum(oda_odat_kd_Net.official.development.assistance.received..constant.2021.US.., na.rm = TRUE)/1000000)
plot_aidcntrs <- plot_aidcntrs[!is.na(plot_aidcntrs$code),]
#ggsave("../../plot_data/my_plot.png", plot = test, width = 8, height = 6, dpi = 300)


#by country
ggplot(plot_aidcntrs, aes(x = tot_food_aid)) + 
  geom_histogram(binwidth = 0.5, fill = "#005555", color = "black", alpha = 0.7) +
  labs(
    title = "Histogram of Food Aid",
    x = "Value",
    y = "Frequency"
  ) 

#scatterplot
ggplot(food_df, aes(x = year, y = ad_sdg2_aid)) +
  geom_point(color = "black", size = 2) + 
  labs(
    title = "Scatter Plot of x vs y",
    x = "X Variable",
    y = "Y Variable"
  ) +
  theme_minimal()  # Minimal theme


#outliers
mean_value <- mean(food_df$ad_sdg2_aid, na.rm = TRUE)
sd_value <- sd(food_df$ad_sdg2_aid, na.rm = TRUE)

# Identify outliers (values > 2 standard deviations from the mean)
outliers <- !(is.na(food_df$ad_sdg2_aid[abs(food_df$ad_sdg2_aid - mean_value) > 3 * sd_value]))


#2011-2022

#Time Trends

plot_aidtime <- food_df %>%
  filter(year >2010 & year < 2022) %>%
  group_by(year) %>%
  summarize(tot_food_aid = sum(ad_sdg2_aid, na.rm = TRUE)/1000000000,
            tot_aid = sum(ad_tot_aid, na.rm = TRUE)/1000000000,
            tot_notfood_aid = sum(ad_exc2_aid, na.rm = TRUE)/1000000000,
            oecd_food_aid = sum(oecd_food_aid, na.rm = TRUE)/1000000,
            oecd_tot_aid = sum(oecd_tot_aid, na.rm = TRUE)/1000000,
            wb_tot_aid = sum(oda_odat_kd_Net.official.development.assistance.received..constant.2021.US.., na.rm = TRUE)/1000000)


#AidData and OECD Data
ggplot(plot_aidtime) +
  geom_line(aes(x = year, y = tot_food_aid, color = "AidData"), size = 1) +
  geom_line(aes(x = year, y = oecd_food_aid, color = "OECD"), size = 1) +
  scale_color_manual(
    values = c("AidData" = "#005555", "OECD" = "black"),  # Colors for each line
    name = "Aid Type"  # Legend title
  ) +
  scale_x_continuous(
    breaks = seq(min(plot_aidtime$year), max(plot_aidtime$year), by = 2)  # Every 2nd year on the x-axis
  ) +
  labs(
    title = "Aid Trends Over Time",
    x = "Year",
    y = "Amount in Million USD"
  ) +
  theme_bw() 

#AidData - Food and Total

aid_tot_time <- ggplot(plot_aidtime) +
  geom_line(aes(x = year, y = tot_food_aid, color = "Food Aid"), size = 1) +
  geom_line(aes(x = year, y = tot_aid, color = "Total Aid"), size = 1) +
  scale_color_manual(
    values = c("Food Aid" = "#005555", "Total Aid" = "black"),  # Colors for each line
    name = "Aid Type"  # Legend title
  ) +
  scale_x_continuous(
    breaks = seq(min(plot_aidtime$year), max(plot_aidtime$year), by = 2)  # Every 2nd year on the x-axis
  ) +
  labs(
    title = "",
    x = "Year",
    y = "Amount in Billion USD"
  ) +
  theme_bw() 

ggsave("../outputs/aid_tot_time.png", plot = aid_tot_time, width = 8, height = 6, dpi = 300)


plot_aidtime_dvtimeframe <- plot_aidtime[plot_aidtime$year > 2015 & plot_aidtime$year < 2022,]
aid_tot_time <- ggplot(plot_aidtime_dvtimeframe) +
  geom_line(aes(x = year, y = tot_food_aid, color = "Food Aid"), size = 1) +
  geom_line(aes(x = year, y = tot_aid, color = "Total Aid"), size = 1) +
  scale_color_manual(
    values = c("Food Aid" = "#005555", "Total Aid" = "black"),  # Colors for each line
    name = "Aid Type"  # Legend title
  ) +
  scale_x_continuous(
    breaks = seq(min(plot_aidtime$year), max(plot_aidtime$year), by = 2)  # Every 2nd year on the x-axis
  ) +
  labs(
    title = "Total Aid Dispersed Over Time",
    x = "Year",
    y = "Amount in Billion USD"
  ) +
  theme_bw() 

ggsave("../outputs/aid_tot_timedv.png", plot = aid_tot_time, width = 8, height = 6, dpi = 300)


#Total Aid Comparison - AidData more comprehensive

ggplot(plot_aidtime) +
  geom_line(aes(x = year, y = tot_aid, color = "AidData Aid"), size = 1) +
  geom_line(aes(x = year, y = oecd_tot_aid, color = "OECD Aid"), size = 1) +
  geom_line(aes(x = year, y = wb_tot_aid, color = "WB Aid"), size = 1) +
  scale_color_manual(
    values = c("AidData Aid" = "red", "OECD Aid" = "black", "WB Aid" = "blue"),  # Colors for each line
    name = "Aid Type"  # Legend title
  ) +
  scale_x_continuous(
    breaks = seq(min(plot_aidtime$year), max(plot_aidtime$year), by = 2)  # Every 2nd year on the x-axis
  ) +
  labs(
    title = "Aid Trends Over Time",
    x = "Year",
    y = "Amount in Billion USD"
  ) +
  theme_bw() 


#################################################################################

#Countries

plot_aidcntrs <- food_df %>%
  filter(year >2010 & year < 2022) %>%
  group_by(code) %>%
  summarize(tot_food_aid = sum(ad_sdg2_aid, na.rm = TRUE)/1000000,
            tot_aid = sum(ad_tot_aid, na.rm = TRUE)/1000000,
            tot_notfood_aid = sum(ad_exc2_aid, na.rm = TRUE)/1000000,
            oecd_food_aid = sum(oecd_food_aid, na.rm = TRUE)/1000,
            oecd_tot_aid = sum(oecd_tot_aid, na.rm = TRUE)/1000,
            wb_tot_aid = sum(oda_odat_kd_Net.official.development.assistance.received..constant.2021.US.., na.rm = TRUE)/1000000)

#ggsave("../../plot_data/my_plot.png", plot = test, width = 8, height = 6, dpi = 300)

#by country
ggplot(plot_aidcntrs, aes(x = tot_food_aid)) + 
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black", alpha = 0.7) +
  labs(
    title = "Histogram of Food Aid",
    x = "Value",
    y = "Frequency"
  ) +
  theme_minimal()  # Minimal theme for a clean look


#scatterplot
ggplot(food_df, aes(x = year, y = ad_sdg2_aid)) +
  geom_point(color = "black", size = 2) +  # Customize point color and size
  labs(
    title = "Scatter Plot of x vs y",
    x = "X Variable",
    y = "Y Variable"
  ) +
  theme_minimal()  # Minimal theme


#outliers
mean_value <- mean(food_df$ad_sdg2_aid, na.rm = TRUE)
sd_value <- sd(food_df$ad_sdg2_aid, na.rm = TRUE)

# Identify outliers (values > 2 standard deviations from the mean)
outliers <- !(is.na(food_df$ad_sdg2_aid[abs(food_df$ad_sdg2_aid - mean_value) > 2 * sd_value]))

###################
###Analysis Dataset
###################

food_df <- read.csv("food_df_ana.csv")

mean_change <- mean(food_df$change_msfi[food_df$year == 2019], na.rm = TRUE)  # Remove NAs if any
sd_change <- sd(food_df$change_msfi[food_df$year == 2019], na.rm = TRUE)


ggplot(food_df, aes(x = change_msfi)) +
  geom_histogram(bins = 57, fill = "white", color = "black") +
  labs(
    title = "Relative Change in Moderate or Severe Food Insecurity 2019",
    x = "Value",
    y = "Count"
  ) +
  theme_minimal()  # Minimal theme

##Overview Statistics
var_stats <- food_df[c(3,5:ncol(food_df))]
statistics <- data.frame(
  Mean = sapply(var_stats, mean),
  SD = sapply(var_stats, sd),
  Median = sapply(var_stats, median),
  Min = sapply(var_stats, min),
  Max = sapply(var_stats, max)
)

print(statistics)

##On Map

food_df <- read.csv("food_df_rob.csv")
food_df <- food_df %>% select(country, year, change_msfi, prev_msfi, ad_sdg2_aid) %>% filter(year == 2018 | year == 2019)

map_plot <- food_df %>%
  pivot_wider(
    names_from = year,
    values_from = c(prev_msfi, change_msfi, ad_sdg2_aid),
    names_glue = "{.value}_{year}"
  )

# Print the reshaped data
world <- ne_countries(scale = "medium", returnclass = "sf")

world_data <- world %>%
  left_join(map_plot, by = c("name" = "country"))

# Create the map with a greyscale color palette
ggplot(world_data) +
  geom_sf(aes(fill = ad_sdg2_aid_2019), color = "gray", lwd = 0.1) + 
  scale_fill_gradient(low = "orange", high = "red", na.value = "lightgray") +  # Greyscale
  theme_minimal() +
  labs(title = "World Map Colored by Received Aid", fill = "Received Aid") +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())


ggplot(world_data) +
  geom_sf(aes(fill = change_msfi_2019), color = "gray", lwd = 0.1) + 
  scale_fill_gradient(low = "orange", high = "red", na.value = "lightgray") +  # Greyscale
  theme_minimal() +
  labs(title = "World Map Colored by Received Aid", fill = "Received Aid") +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())

#Treatment and outcome Distribution

food_df <- read.csv("food_df_ana.csv")
aid_hist <- ggplot(food_df, aes(x = ad_sdg2_aid)) + 
  geom_histogram(binwidth = 10, fill = "#005555", color = "black", alpha = 0.7) +
  labs(
    title = "Distribution of Received Aid",
    x = "Aid Received, Million USD",
    y = "Frequency"
  ) +
  theme_minimal() + 
  theme_bw()

msfi_hist <- ggplot(food_df, aes(x = change_msfi)) + 
  geom_histogram(binwidth = 0.005, fill = "#005555", color = "black", alpha = 0.7) +
  labs(
    title = "Distribution of Relative Reduction Rate",
    x = "Relative Reduction Rate",
    y = "Frequency"
  ) +
  theme_minimal() + 
  theme_bw()

dviv_hist_plots <- (aid_hist | msfi_hist) +  
  plot_layout(guides = "collect") +             
  plot_annotation(theme = theme(legend.position = "bottom"))

ggsave("../outputs/dviv_hist.png", plot = dviv_hist_plots, width = 16, height = 8, dpi = 300)

###########################
##Synthetic Data Comparison
###########################

# Load the .npz file
food_df <- read.csv("food_df_ana.csv")
food_df <- food_df[food_df$year == 2018 | food_df$year == 2019,]

var_stats <- food_df[c(3,5:ncol(food_df))]
statistics <- data.frame(
  Mean = sapply(var_stats, mean),
  SD = sapply(var_stats, sd),
  Median = sapply(var_stats, median),
  Min = sapply(var_stats, min),
  Max = sapply(var_stats, max)
)

print(statistics)


###########################
##Multicollinearity
###########################

food_df <- read.csv("food_df_ana.csv")
condition_number <- kappa(cor(food_df[,6:18]))

names(food_df)[6:18] <- c("Non Food Aid Share GDP", "Inflation", "Food Production", "Food Import Shares GDP", "Log GDP", "Log GDP P.C.", "GDP P.C. Growth", "Governance Score", "Successful Coup", "Log Disaster Affected", "Present Conflict", "Z-Score Precipitation", "Road Density")
corr_matrix <- cor(food_df[,6:18])
png("../outputs/corrplot.png", width = 4000, height = 4000, res = 300)
corrplot(corr_matrix, method = "color", tl.col = "black",  col = colorRampPalette(c("#550000", "white", "#005555"))(200), tl.cex = 1, addCoef.col = "black", number.cex = 1)
dev.off()

