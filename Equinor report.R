# Equinor report figures


# setup -------------------------

library(tidyverse)
library(ggplot2)
library(dplyr)
library(suncalc)
library(suntools)
library(lubridate)
library(readr)
library(esquisse)
# library(MetBrewer)
# library(purrr)
# library(sp)
# library(sf)
# library(hms)
# library(maptools)
# library(ggcorrplot)

# read data in -------------------------------

cm25 <- read.csv("data/2025/cm_2025_night.csv")
cm24 <- read.csv("data/2024/cm_2024_night.csv")
cm26 <- read.csv("data/2026/cm_2026_night.csv")

ov25 <- read.csv("data/2025/overview_2025.csv")
ov25$type[ov25$Site == "CM-01"] <- "coast"
ov25$type[ov25$Site %in% c("CM-07", "CM-41")] <- "lake"
ov24 <- read.csv("data/2024/overview_2024.csv")
ov24$type[ov24$Site == "CM-01"] <- "coast"
ov24$type[ov24$Site %in% c("CM-07", "CM-41")] <- "lake"
ov26 <- read.csv("data/2026/overview_2026.csv")
ov26$type[ov26$Site == "CM-01"] <- "coast"
ov26$type[ov26$Site %in% c("CM-07", "CM-41")] <- "lake"

# 2025--------------------------------------

# 2025 Pnat plot all sites, check different match ratios

pnat25 <- cm25 %>% filter(autoid == "PIPNAT") %>% droplevels()
pnat25$type <- ov25$type[match(pnat25$Site, ov25$Site)]
pnat25$DATE <- as.Date(pnat25$DATE)

pnat25$datetime <- as.POSIXct(pnat25$datetime)

pnat.plot.25 <- ggplot(pnat25, aes(x = DATE)) + 
  stat_count(geom = "point", aes(y = ..count..),
             color = "cyan3", size = 4) +
  scale_x_date(
    limits = as.Date(c("2025-03-11", "2025-10-16"))) + 
  ylab("N recordings across all sites 2025") + 
  xlab("") + 
  ggtitle("Nathusius' pipistrelle (Pipistrellus nathusii) activity 2025") + 
  theme_minimal() 

pnat.plot.25

# match ratio 0.5 does not change it so much, most matches fall above that line

pnat25 %>%
  filter(MATCH_RATIO > 0.5,
         DATE >= as.Date("2025-03-11"),
         DATE <= as.Date("2025-10-16")) %>%
  ggplot(aes(x = DATE)) +
  stat_count(geom = "point", color = "cyan3", size = 4) +
  scale_x_date(
    limits = as.Date(c("2025-03-11", "2025-10-16"))) + 
  stat_smooth(method = "loess", col = "blue", fill = "lightblue", 
               se = TRUE, span = 0.65, level = 0.95)
  ylab("N recordings across all sites 2025") + 
  xlab("") + 
  ggtitle("Nathusius' pipistrelle (Pipistrellus nathusii) activity 2025") + 
  theme_minimal() 

pnat.plot.25

# activity minutes

pnat25_min <- pnat25 %>%
  filter(MATCH_RATIO > 0.4) %>%
  mutate(minute = floor_date(datetime, "minute"),
         DATE   = as.Date(minute)) %>%
  distinct(Site, minute, .keep_all = TRUE)   # one row per site-minute


pnat.plot.25.min <- ggplot(pnat25_min, aes(x = DATE)) + 
  stat_count(geom = "point", color = "cyan3", size = 4) +
  scale_x_date(limits = as.Date(c("2025-03-11", "2025-10-16"))) + 
  ylab("Activity minutes across all sites 2025") + 
  xlab("") + 
  ggtitle("Nathusius' pipistrelle (Pipistrellus nathusii) activity 2025") + 
  theme_minimal()

pnat.plot.25.min

# smooth plot


daily25 <- pnat25_min %>%
  count(DATE, name = "activity_min") %>%
  complete(DATE = seq(as.Date("2025-03-11"), as.Date("2025-10-16"), by = "day"),
           fill = list(activity_min = 0))

ggplot(daily25, aes(DATE, activity_min)) +
  geom_point(color = "cyan3", size = 3) +
  geom_smooth(method = "loess", span = 0.3, color = "black", fill = "grey70") +
  scale_x_date(limits = as.Date(c("2025-03-11", "2025-10-16"))) +
  ylab("Activity minutes across all sites 2025") +
  xlab("") +
  ggtitle("Nathusius' pipistrelle (Pipistrellus nathusii) activity 2025") +
  theme_minimal()

ggplot(daily25, aes(DATE, activity_min)) +
  geom_point(color = "cyan3", size = 3) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10),
              method.args = list(family = quasipoisson)) +
  scale_x_date(limits = as.Date(c("2025-03-11", "2025-10-16"))) +
  ylab("Activity minutes across all sites 2025") +
  xlab("") +
  ggtitle("Nathusius' pipistrelle (Pipistrellus nathusii) activity 2025") +
  theme_minimal()

# 2024 ----------------------------------------------

pnat24 <- cm24 %>% filter(autoid == "PIPNAT") %>% droplevels()
pnat24$type <- ov24$type[match(pnat24$Site, ov24$Site)]
pnat24$DATE <- as.Date(pnat24$DATE)

pnat24$datetime <- as.POSIXct(pnat24$datetime)

pnat.plot.24 <- ggplot(pnat24, aes(x = DATE)) + 
  stat_count(geom = "point", aes(y = ..count..),
             color = "green3", size = 4) +
  scale_x_date(
    limits = as.Date(c("2024-06-15", "2024-10-16"))) + 
  ylab("N recordings across all sites 2024") + 
  xlab("") + 
  ggtitle("Nathusius' pipistrelle (Pipistrellus nathusii) activity 2024") + 
  theme_minimal() 

pnat.plot.24

# activity minutes

pnat24_min <- pnat24 %>%
  filter(MATCH.RATIO > 0.4) %>%
  mutate(minute = floor_date(datetime, "minute"),
         DATE   = as.Date(minute)) %>%
  distinct(Site, minute, .keep_all = TRUE)   # one row per site-minute


pnat.plot.24.min <- ggplot(pnat24_min, aes(x = DATE)) + 
  stat_count(geom = "point", color = "green3", size = 4) +
  scale_x_date(limits = as.Date(c("2024-06-15", "2024-10-16"))) + 
  ylab("Activity minutes across all sites 2024") + 
  xlab("") + 
  ggtitle("Nathusius' pipistrelle (Pipistrellus nathusii) activity 2024") + 
  theme_minimal()

pnat.plot.24.min

# smooth plot


daily24 <- pnat24_min %>%
  count(DATE, name = "activity_min") %>%
  complete(DATE = seq(as.Date("2024-06-15"), as.Date("2024-10-16"), by = "day"),
           fill = list(activity_min = 0))

ggplot(daily24, aes(DATE, activity_min)) +
  geom_point(color = "green3", size = 3) +
  geom_smooth(method = "loess", span = 0.3, color = "black", fill = "grey70") +
  scale_x_date(limits = as.Date(c("2024-06-15", "2024-10-16"))) +
  ylab("Activity minutes across all sites 2024") +
  xlab("") +
  ggtitle("Nathusius' pipistrelle (Pipistrellus nathusii) activity 2024") +
  theme_minimal()

ggplot(daily24, aes(DATE, activity_min)) +
  geom_point(color = "green3", size = 3) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10),
              method.args = list(family = quasipoisson)) +
  scale_x_date(limits = as.Date(c("2024-06-15", "2024-10-16"))) +
  ylab("Activity minutes across all sites 2024") +
  xlab("") +
  ggtitle("Nathusius' pipistrelle (Pipistrellus nathusii) activity 2024") +
  theme_minimal()

# 2026-----------------------------------------------

pnat26 <- cm26 %>% filter(autoid == "PIPNAT") %>% droplevels()
pnat26$type <- ov26$type[match(pnat26$Site, ov26$Site)]
pnat26$DATE <- as.Date(pnat26$DATE)

pnat26$datetime <- as.POSIXct(pnat26$datetime)

pnat.plot.26 <- ggplot(pnat26, aes(x = DATE)) + 
  stat_count(geom = "point", aes(y = ..count..),
             color = "purple3", size = 4) +
  scale_x_date(
    limits = as.Date(c("2026-03-11", "2026-07-20"))) + 
  ylab("N recordings across all sites 2026") + 
  xlab("") + 
  ggtitle("Nathusius' pipistrelle (Pipistrellus nathusii) activity 2026") + 
  theme_minimal() 

pnat.plot.26

# activity minutes

pnat26_min <- pnat26 %>%
  filter(MATCH.RATIO > 0.4) %>%
  mutate(minute = floor_date(datetime, "minute"),
         DATE   = as.Date(minute)) %>%
  distinct(Site, minute, .keep_all = TRUE)   # one row per site-minute


pnat.plot.26.min <- ggplot(pnat26_min, aes(x = DATE)) + 
  stat_count(geom = "point", color = "purple3", size = 4) +
  scale_x_date(limits = as.Date(c("2026-03-11", "2026-07-20"))) + 
  ylab("Activity minutes across all sites 2026") + 
  xlab("") + 
  ggtitle("Nathusius' pipistrelle (Pipistrellus nathusii) activity 2026") + 
  theme_minimal()

pnat.plot.26.min

# smooth plot


daily26 <- pnat26_min %>%
  count(DATE, name = "activity_min") %>%
  complete(DATE = seq(as.Date("2026-03-11"), as.Date("2026-07-20"), by = "day"),
           fill = list(activity_min = 0))

ggplot(daily26, aes(DATE, activity_min)) +
  geom_point(color = "purple3", size = 3) +
  geom_smooth(method = "loess", span = 0.3, color = "black", fill = "grey70") +
  scale_x_date(limits = as.Date(c("2026-03-11", "2026-07-20"))) +
  ylab("Activity minutes across all sites 2026") +
  xlab("") +
  ggtitle("Nathusius' pipistrelle (Pipistrellus nathusii) activity 2026") +
  theme_minimal()

ggplot(daily26, aes(DATE, activity_min)) +
  geom_point(color = "purple3", size = 3) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10),
              method.args = list(family = quasipoisson)) +
  scale_x_date(limits = as.Date(c("2026-03-11", "2026-07-20"))) +
  ylab("Activity minutes across all sites 2026") +
  xlab("") +
  ggtitle("Nathusius' pipistrelle (Pipistrellus nathusii) activity 2026") +
  theme_minimal()

# put toghether all years------------------------------


all_years <- bind_rows(
  daily24 %>% mutate(year = "2024"),
  daily25 %>% mutate(year = "2025"),
  daily26 %>% mutate(year = "2026")
) %>%
  mutate(DATE_plot = as.Date(format(DATE, "2000-%m-%d")))  # same dummy year for all


ggplot(all_years, aes(DATE_plot, activity_min, color = year, fill = year)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "loess", span = 0.3, alpha = 0.15) +
  scale_x_date(limits = as.Date(c("2000-03-11", "2000-10-16")),
               date_breaks = "1 month", date_labels = "%b") +
  ylab("Activity minutes across all sites") +
  xlab("") +
  ggtitle("Nathusius' pipistrelle (Pipistrellus nathusii) activity") +
  theme_minimal()


