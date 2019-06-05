# Visualisation exploration for SDG-regime shifts paper
# Juan Rocha
# 190605

library(tidyverse)
library(ggalluvial)

# read datasets
pop_biome <- readxl::read_excel(
    path = "~/Dropbox/regimeShift-SDG/GIS data/Population/Pop_RegimeShifts.xlsx",
    n_max = 11)

pop_regimeshift <- readxl::read_excel(
    path = "~/Dropbox/regimeShift-SDG/GIS data/Population/Pop_RegimeShifts.xlsx",
    range = "A16:AA27")

rs_sdg <- read_csv(file = "~/Dropbox/regimeShift-SDG/Data-May2018/RegimeShiftsSDGBiome.csv")
rs_land <- read_csv(file = "~/Dropbox/regimeShift-SDG/Data-May2018/RSLandPrecent.csv")

rsdb <- read_csv(
    file = "~/Dropbox/regimeShift-SDG/data-csv/Regime Shifts Database.CSV",
    )

dat <- rsdb %>% select(3,7,12,13,14) %>%
    rename(regime_shift = 1, drivers = 2, provisioning = 3, regulating = 4, cultural = 5) %>%
    mutate(
        provisioning = str_split(provisioning, pattern = ", " ),
        regulating = str_split(regulating, pattern = ", "),
        cultural = str_split(cultural, pattern = ", "),
        drivers = str_split(drivers, pattern = ", ")
           ) %>%
    unnest(drivers, .drop = FALSE) %>%
    unnest(provisioning, .drop = FALSE) %>%
    unnest(regulating, .drop = FALSE) %>%
    unnest(cultural, .drop = FALSE) %>%
    gather(key = "es_type", value = "ecosystem_service", provisioning, regulating, cultural) %>%
    mutate(obs = 1)


# Visualisation

ggplot(dat, aes(y = obs, axis1 = drivers, axis2 = regime_shift, axis3 = es_type)) +
    geom_alluvium(aes(fill = regime_shift), alpha = 0.7) +
    geom_stratum(width = 1/6) +
    #geom_label(stat = "stratum", label.strata = TRUE) +
    scale_x_discrete(limits = c("Drivers", "Regime shifts", "Ecosystem services")) +
    theme_light(base_size = 10)


