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
library(mgcv)
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
cmw <- read.csv("data/winter/cm_winter.csv")

cmw <- cmw %>%
  mutate(manual = id) %>%
  separate_rows(manual, sep = "_")
cmw <- cmw %>%
  mutate(
    datetime = as.POSIXct(paste(DATE, TIME),
                          format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT-2") %>%
      with_tz("Europe/Oslo"),
    datetime_12 = as.POSIXct(paste(DATE_12, TIME_12),
                             format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT-2") %>%
      with_tz("Europe/Oslo")
  )

# datetime into Posixct, truncated = 3 makes midnight an actual time.
cm25$datetime <- ymd_hms(cm25$datetime, tz = "Europe/Oslo", truncated = 3)
cm24$datetime <- ymd_hms(cm24$datetime, tz = "Europe/Oslo", truncated = 3)
cm26$datetime <- ymd_hms(cm26$datetime, tz = "Europe/Oslo", truncated = 3)

cm24 <- rename(cm24, MATCH_RATIO = MATCH.RATIO)
cm26 <- rename(cm26, MATCH_RATIO = MATCH.RATIO)

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

# NoID graph--------------------
spp <- c("PIPNAT", "PIPPYG", "EPTNIL", "NoID", "MYODAU")

make_daily <- function(dat, start, end) {
  dat %>%
    filter(autoid %in% spp,
           autoid == "NoID" | MATCH_RATIO > 0.4) %>%
    mutate(minute = floor_date(datetime, "minute"), DATE = as.Date(minute)) %>%
    distinct(Site, autoid, minute, .keep_all = TRUE) %>%
    count(DATE, autoid, name = "activity_min") %>%
    complete(DATE = seq(as.Date(start), as.Date(end), by = "day"),
             autoid = spp, fill = list(activity_min = 0))
}

# 2024

daily24_spp <- make_daily(cm24, "2024-06-15", "2024-10-16")

ggplot(daily24_spp, aes(DATE, activity_min, color = autoid, fill = autoid)) +
  geom_point(size = 1.5, alpha = 0.3) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10),
             method.args = list(family = quasipoisson), alpha = 0.15) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  ylab("Activity minutes across all sites 2024") +
  xlab("") +
  theme_minimal()

# 2025

daily25_spp <- make_daily(cm25, "2025-03-11", "2025-10-16")

ggplot(daily25_spp, aes(DATE, activity_min, color = autoid, fill = autoid)) +
  geom_point(size = 1.5, alpha = 0.3) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10),
              method.args = list(family = quasipoisson), alpha = 0.15) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  ylab("Activity minutes across all sites 2025") +
  xlab("") +
  theme_minimal()

# 2026

daily26_spp <- make_daily(cm26, "2026-03-11", "2026-07-20")

ggplot(daily26_spp, aes(DATE, activity_min, color = autoid, fill = autoid)) +
  geom_point(size = 1.5, alpha = 0.3) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10),
              method.args = list(family = quasipoisson), alpha = 0.15) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  ylab("Activity minutes across all sites 2026") +
  xlab("") +
  theme_minimal()

# activity minutes in 7 sites that were up all winter 2025/2026 from 2024-2026------------

sites <- c("CM-03", "CM-04", "CM-05", "CM-06", "CM-17", "CM-23", "CM-56")
start <- as.Date("2024-06-20")
end   <- as.Date("2026-07-30")

# automatic IDs (same confidence filter as before)
auto <- bind_rows(
  cm24 %>% mutate(Site = as.character(Site)),
  cm25 %>% mutate(Site = as.character(Site)),
  cm26 %>% mutate(Site = as.character(Site))
) %>%
  filter(autoid == "PIPNAT", MATCH_RATIO > 0.4) %>%
  select(Site, datetime)

# manual IDs
man <- cmw %>%
  mutate(Site = as.character(Site)) %>%
  filter(manual == "PNAT") %>%     # adapt to how the column is coded
  select(Site, datetime)

act <- bind_rows(auto, man) %>%
  filter(Site %in% sites) %>%
  mutate(minute = floor_date(datetime, "minute")) %>%
  distinct(Site, minute) %>%                 # also removes auto/manual duplicates
  mutate(DATE = as.Date(minute)) %>%
  filter(DATE >= start, DATE <= end) %>%
  count(Site, DATE, name = "activity_min") %>%
  complete(Site = sites,
           DATE = seq(start, end, by = "day"),
           fill = list(activity_min = 0)) %>%
  mutate(Site = factor(Site),
         day  = as.numeric(DATE - start))

m2 <- gam(activity_min ~ s(day, k = 80) + s(Site, bs = "re"),
          family = nb(), method = "REML", data = act)

gam.check(m2)

newd <- tibble(day = 0:as.numeric(end - start), Site = act$Site[1])
pr <- predict(m2, newd, type = "link", se.fit = TRUE, exclude = "s(Site)")

newd <- newd %>%
  mutate(DATE = start + day,
         fit = exp(pr$fit),
         lwr = exp(pr$fit - 1.96 * pr$se.fit),
         upr = exp(pr$fit + 1.96 * pr$se.fit))

daily_mean <- act %>% group_by(DATE) %>% summarise(activity_min = mean(activity_min))

ggplot() +
  geom_point(data = daily_mean, aes(DATE, activity_min),
             color = "cyan3", alpha = 0.4, size = 1.5) +
  geom_ribbon(data = newd, aes(DATE, ymin = lwr, ymax = upr), alpha = 0.2) +
  geom_line(data = newd, aes(DATE, fit), linewidth = 1) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %y") +
  ylab("Activity minutes per site per day") +
  xlab("") +
  theme_minimal()

ggplot() +
  geom_point(data = daily_mean, aes(DATE, activity_min),
             color = "cyan3", alpha = 0.4, size = 1.5) +
  geom_ribbon(data = newd, aes(DATE, ymin = lwr, ymax = upr), alpha = 0.2) +
  geom_line(data = newd, aes(DATE, fit), linewidth = 1) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %y") +
  coord_cartesian(ylim = c(0, 20)) +
  ylab("Activity minutes per site per day") +
  xlab("") +
  theme_minimal()

# with autoid for cmw too

keep <- function(d) {
  d %>%
    mutate(Site = as.character(Site)) %>%
    select(Site, autoid, MATCH_RATIO, datetime)
}

auto1 <- bind_rows(keep(cm24), keep(cm25), keep(cm26), keep(cmw)) %>%
  filter(autoid == "PIPNAT", MATCH_RATIO > 0.4) %>%
  select(Site, datetime)

act1 <- auto1 %>%
  filter(Site %in% sites) %>%
  mutate(minute = floor_date(datetime, "minute")) %>%
  distinct(Site, minute) %>%
  mutate(DATE = as.Date(minute)) %>%
  filter(DATE >= start, DATE <= end) %>%
  count(Site, DATE, name = "activity_min") %>%
  complete(Site = sites,
           DATE = seq(start, end, by = "day"),
           fill = list(activity_min = 0)) %>%
  mutate(Site = factor(Site),
         day  = as.numeric(DATE - start))

m21 <- gam(activity_min ~ s(day, k = 80) + s(Site, bs = "re"),
          family = nb(), method = "REML", data = act1)

gam.check(m21)

newd1 <- tibble(day = 0:as.numeric(end - start), Site = act1$Site[1])
pr1 <- predict(m21, newd1, type = "link", se.fit = TRUE, exclude = "s(Site)")

newd1 <- newd1 %>%
  mutate(DATE = start + day,
         fit = exp(pr1$fit),
         lwr = exp(pr1$fit - 1.96 * pr1$se.fit),
         upr = exp(pr1$fit + 1.96 * pr1$se.fit))



daily_mean1 <- act1 %>% group_by(DATE) %>% summarise(activity_min = mean(activity_min))

ggplot() +
  geom_point(data = daily_mean1, aes(DATE, activity_min),
             color = "cyan3", alpha = 0.4, size = 1.5) +
  geom_ribbon(data = newd1, aes(DATE, ymin = lwr, ymax = upr), alpha = 0.2) +
  geom_line(data = newd1, aes(DATE, fit), linewidth = 1) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %y") +
  ylab("Activity minutes per site per day") +
  xlab("") +
  theme_minimal()

# both:
  
both <- bind_rows(newd  %>% mutate(src = "autoid + manual (cmw)"),
                  newd1 %>% mutate(src = "autoid only (cmw too)"))

ggplot(both, aes(DATE, fit, color = src, fill = src)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1) +
  coord_cartesian(ylim = c(0, 20)) +
  theme_minimal()

# winter----------------

# sunset and sunrise times

cmw$DATE <- as.Date(cmw$DATE)

sun_input <- cmw %>%
  distinct(Site, date = DATE) %>%
  left_join(
    ov25 %>% select(Site, latitude, longitude),
    by = "Site"
  )

sun_times <- bind_cols(
  sun_input %>% select(Site, DATE = date),
  suncalc::getSunlightTimes(
    data = sun_input %>%
      transmute(date = date, lat = latitude, lon = longitude),
    keep = c("sunrise", "sunset"),
    tz = "Europe/Oslo"
  ) %>%
    select(sunrise, sunset)
)

cmw_sun <- cmw %>%
  left_join(sun_times, by = c("Site", "DATE"))

cmw_daytime <- cmw_sun %>%
  filter(
    !is.na(sunrise), !is.na(sunset),
    datetime >= sunrise,
    datetime <= sunset
  )

cmw_daytime$manual <- factor(cmw_daytime$manual)

summary(cmw_daytime$manual)

night_windows <- sun_times %>%
  arrange(Site, DATE) %>%
  group_by(Site) %>%
  mutate(
    next_sunrise = lead(sunrise)
  ) %>%
  ungroup() %>%
  filter(
    !is.na(sunset),
    !is.na(next_sunrise)
  ) %>%
  transmute(
    Site,
    night_start = sunset - 3600,
    night_end = next_sunrise + 3600
  )

cmw_night <- cmw %>%
  inner_join(
    night_windows,
    by = "Site",
    relationship = "many-to-many"
  ) %>%
  filter(
    datetime >= night_start,
    datetime <= night_end
  ) %>%
  select(-night_start, -night_end)

cmw_night$manual <- factor(cmw_night$manual)
cmw$manual <- factor(cmw$manual)

summary(cmw_night$manual)
summary(cmw$manual)

# second try

site_coords <- ov25 %>% distinct(Site, latitude, longitude)

sun_grid <- cmw %>%
  group_by(Site) %>%
  summarise(first = min(DATE) - 1, last = max(DATE) + 1, .groups = "drop") %>%
  rowwise() %>%
  mutate(DATE = list(seq(first, last, by = "day"))) %>%
  ungroup() %>%
  select(-first, -last) %>%
  unnest(DATE) %>%
  left_join(site_coords, by = "Site")


sun <- getSunlightTimes(
  data = sun_grid %>% transmute(date = DATE, lat = latitude, lon = longitude),
  keep = c("sunrise", "sunset"), tz = "Europe/Oslo"
)

sun_times <- bind_cols(sun_grid %>% select(Site, DATE),
                       sun %>% select(sunrise, sunset))

# night of date d = sunset(d) - 1h  to  sunrise(d+1) + 1h
night_windows <- sun_times %>%
  transmute(Site, night_date = DATE, night_start = sunset - 3600) %>%
  left_join(sun_times %>% transmute(Site, night_date = DATE - 1,
                                    night_end = sunrise + 3600),
            by = c("Site", "night_date")) %>%
  filter(!is.na(night_start), !is.na(night_end))

cmw_night <- cmw %>%
  inner_join(night_windows,
             by = join_by(Site, between(datetime, night_start, night_end))) %>%
  select(-night_start, -night_end)


cmw_night$manual <- factor(cmw_night$manual)
cmw$manual <- factor(cmw$manual)

summary(cmw_night$manual)
summary(cmw$manual)


# files daytime

ppyg_missing <- cmw %>%
  filter(manual == "PPYG") %>%
  anti_join(cmw_night, by = "filename") %>%
  distinct(filename, .keep_all = TRUE)

ppyg_missing %>% select(file, Site, datetime)

# check if new nighttime dataset differs from the old one, cm is full, cm24 is old night

cm <- read.csv("C:/Users/mapa/OneDrive - Norwegian University of Life Sciences/Documents/R for data/Overview of sites/cm_all.csv")

site_coords24 <- ov24 %>% distinct(Site, latitude, longitude)

cm$DATE <- as.Date(cm$DATE)

sun_grid24 <- cm %>%
  group_by(Site) %>%
  summarise(first = min(DATE) - 1, last = max(DATE) + 1, .groups = "drop") %>%
  rowwise() %>%
  mutate(DATE = list(seq(first, last, by = "day"))) %>%
  ungroup() %>%
  select(-first, -last) %>%
  unnest(DATE) %>%
  left_join(site_coords24, by = "Site")


sun24 <- getSunlightTimes(
  data = sun_grid24 %>% transmute(date = DATE, lat = latitude, lon = longitude),
  keep = c("sunrise", "sunset"), tz = "Europe/Oslo"
)

sun_times24 <- bind_cols(sun_grid24 %>% select(Site, DATE),
                       sun24 %>% select(sunrise, sunset))

# night of date d = sunset(d) - 1h  to  sunrise(d+1) + 1h
night_windows24 <- sun_times24 %>%
  transmute(Site, night_date = DATE, night_start = sunset - 3600) %>%
  left_join(sun_times24 %>% transmute(Site, night_date = DATE - 1,
                                    night_end = sunrise + 3600),
            by = c("Site", "night_date")) %>%
  filter(!is.na(night_start), !is.na(night_end))

cm <- cm %>%
  mutate(
    datetime = as.POSIXct(
      paste(DATE, TIME),
      format = "%Y-%m-%d %H:%M:%S",
      tz = "Europe/Oslo"))

cm_night24 <- cm %>%
  inner_join(night_windows24,
             by = join_by(Site, between(datetime, night_start, night_end))) %>%
  select(-night_start, -night_end)

cm24_missing <- cm24 %>%
  anti_join(cm_night24, by = "filename") %>%
  distinct(filename, .keep_all = TRUE)

cm24 %>%
  count(filename) %>%
  filter(n > 1) %>%
  summarise(
    duplicated_filenames = n(),
    extra_rows = sum(n - 1)
  )

cm_night24 %>%
  count(filename) %>%
  filter(n > 1) %>%
  summarise(
    duplicated_filenames = n(),
    extra_rows = sum(n - 1)
  )

n_distinct(cm24$filename)
n_distinct(cm_night24$filename)
n_distinct(cm24_missing$filename)