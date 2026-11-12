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
    datetime = as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Oslo"),
    datetime_12 = as.POSIXct(datetime_12, format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Oslo")
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
summary(is.na(pnat25$DATE))

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


# activity minutes

pnat25_min <- pnat25 %>%
  filter(MATCH_RATIO > 0.5) %>%
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
  filter(MATCH.RATIO > 0.5) %>%
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
  filter(MATCH.RATIO > 0.5) %>%
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


# check for manual data----------------

winter <- read.csv("data/winter/cm_winter_total.csv")
spring <- winter %>%
  filter(DATE > "2026-03-24")

spring <- spring %>%
  separate_rows(id, sep = "_")

winter <- winter %>%
  separate_rows(id, sep = "_")

id_map <- c(PNAT = "PIPNAT",
            PPYG = "PIPPYG",
            ENIL = "EPTNIL",     
            MDAU = "MYODAU",
            VMUR = "VESMUR",
            PAUR = "PLEAUR",
            MYSP = "MYDAU",
            NNOC = "NYCNOC")     

winter <- winter %>%
  mutate(id_clean = toupper(trimws(as.character(id))),
         id_std   = coalesce(unname(id_map[id_clean]), id_clean))

spring <- spring %>%
  mutate(id_clean = toupper(trimws(as.character(id))),
         id_std   = coalesce(unname(id_map[id_clean]), id_clean))

table(spring$autoid, spring$id_std)

table(winter$autoid, winter$id_std)

table(winter$autoid, winter$id_std)   # a look at agreement and confusion

sp <- "EPTNIL"
d  <- winter %>% filter(!is.na(id_std))

eval_thr <- function(mr, p) {
  keep <- (d$autoid %in% sp) & (d$MATCH.RATIO >= mr) & (d$PULSES >= p)
  keep[is.na(keep)] <- FALSE
  tp <- sum(keep & d$id_std %in% sp)
  fp <- sum(keep & !(d$id_std %in% sp))
  fn <- sum(d$id_std %in% sp) - tp
  tibble(mr, p, tp, fp, fn,
         precision = tp / (tp + fp),
         recall    = tp / (tp + fn),
         f1        = 2 * tp / (2 * tp + fp + fn))
}


grid <- expand_grid(mr = seq(0, 1, 0.05), p = 1:20)
res  <- bind_rows(purrr::pmap(grid, eval_thr))

res %>% arrange(desc(f1)) %>% head(10)

ggplot(res, aes(mr, p, fill = f1)) +
  geom_tile() + scale_fill_viridis_c() + theme_minimal() +
  labs(x = "min match ratio", y = "min pulses")

res %>%
  filter(p == 1) %>%
  select(mr, tp, fp, precision, recall, f1) %>%
  print(n = 25)

res %>%
  filter(p == 1) %>%
  tidyr::pivot_longer(c(precision, recall, f1)) %>%
  ggplot(aes(mr, value, color = name)) + geom_line() + theme_minimal()

#Judith MS-----------
#PPYG-------
#Logistic regression to determine cut-off

ppyg_data <- spring %>%
  filter(autoid == "PIPPYG") %>%
  mutate(
    MATCH.RATIO = as.numeric(MATCH.RATIO),
    correct = ifelse(id == "PPYG", 1, 0)) %>%
  filter(!is.na(MATCH.RATIO))


ppyg_glm <- glm(
  correct ~ MATCH.RATIO,
  family = binomial,
  data = ppyg_data)

summary(ppyg_glm)


# calculates the match ratio cut-off by inverting the logistic regression
get_cutoff <- function(p, model) {
  beta0 <- coef(model)[1]
  beta1 <- coef(model)[2]
  (log(p / (1 - p)) - beta0) / beta1
}

# target probabilities for which we want to know the match ratio
target_probs <- c(0.5, 0.6, 0.7, 0.8, 0.9)

# create table with probability column and fill with values from get-cutoff + lost and retained calls + percentage
loss_table <- data.frame(Probability = target_probs) %>%
  mutate(
    Cutoff_Ratio = pmin(
      sapply(Probability, get_cutoff, model = ppyg_glm),
      1),
    Calls_retained = sapply(Cutoff_Ratio, function(c) 
      sum(ppyg_data$MATCH.RATIO >= c)),
    Calls_lost = sapply(Cutoff_Ratio, function(c) 
      sum(ppyg_data$MATCH.RATIO < c)),
    Percent_loss = round(
      (Calls_lost / nrow(ppyg_data)) * 100, 2 ) )

loss_table <- loss_table %>%
  mutate(
    Retained_correct = sapply(Cutoff_Ratio, function(c)
      sum(ppyg_data$correct[ppyg_data$MATCH.RATIO >= c], na.rm = TRUE)),
    Retained_false   = Calls_retained - Retained_correct,
    Precision        = round(Retained_correct / Calls_retained, 3),
    True_calls_lost  = sum(pnat_data$correct, na.rm = TRUE) - Retained_correct,
    Recall           = round(Retained_correct / sum(pnat_data$correct, na.rm = TRUE), 3),
  )

print(loss_table)

ggplot(loss_table, aes(Probability, Percent_loss)) +
  geom_line(linewidth = 1, color = "steelblue") +
  geom_point(aes(shape = Cutoff_Ratio >= 1), size = 3.5, color = "steelblue") +
  geom_text(aes(label = paste0("≥ ", round(Cutoff_Ratio, 2))),
            vjust = -1.1, size = 3.3) +
  scale_x_continuous(labels = scales::percent) +        # if Probability is 0-1
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.12))) +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 4),
                     labels = c("reached", "capped at 1"),
                     name = "Target") +
  labs(x = "Target probability of a correct ID",
       y = "Calls lost (%)",
       subtitle = "Labels show the minimum match ratio required") +
  theme_minimal()


loss_table %>%
  pivot_longer(c(Cutoff_Ratio, Percent_loss),
               names_to = "metric", values_to = "value") %>%
  mutate(metric = recode(metric,
                         Cutoff_Ratio = "Required match ratio",
                         Percent_loss = "Calls lost (%)")) %>%
  ggplot(aes(Probability, value)) +
  geom_line(color = "steelblue") +
  geom_point(size = 3, color = "steelblue") +
  facet_wrap(~ metric, ncol = 1, scales = "free_y", strip.position = "left") +
  scale_x_continuous(labels = scales::percent) +
  labs(x = "Target probability of a correct ID", y = NULL) +
  theme_minimal() +
  theme(strip.placement = "outside")

loss_table %>%
  pivot_longer(c(Calls_retained, Calls_lost), names_to = "status", values_to = "n") %>%
  ggplot(aes(factor(Probability), n, fill = status)) +
  geom_col() +
  scale_fill_manual(values = c(Calls_retained = "steelblue", Calls_lost = "grey70"),
                    labels = c("Lost", "Retained"), name = NULL) +
  labs(x = "Target probability", y = "Calls") +
  theme_minimal()


# plot

ppyg_data2 <- spring %>%
  filter(autoid == "PIPPYG") %>%                 # KPRO's PPYG calls
  mutate(correct = as.integer(id_std == "PIPPYG"))   # 1 = manual ID agrees

ppyg_glm <- glm(correct ~ MATCH.RATIO, data = ppyg_data, family = binomial)

ggplot(ppyg_data, aes(MATCH.RATIO, correct)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "glm", method.args = list(family = binomial),
              formula = y ~ x, se = FALSE, color = "blue") +
  labs(title = "Comparing KPRO vs Manual ID for Soprano pipistrelles bats",
       x = "KPRO match ratio",
       y = "Predicted probability of correct") +
  theme_minimal()

#ENIL------
#Logistic regression to determine cut-off,  


enil_data <- spring %>%
  filter(autoid == "EPTNIL") %>%
  mutate(
    MATCH.RATIO = as.numeric(MATCH.RATIO),
    correct = ifelse(id == "ENIL", 1, 0)) %>%
  filter(!is.na(MATCH.RATIO))


enil_glm <- glm(
  correct ~ MATCH.RATIO,
  family = binomial,
  data = enil_data)

summary(enil_glm)


# calculates the match ratio cut-off by inverting the logistic regression
get_cutoff <- function(p, model) {
  beta0 <- coef(model)[1]
  beta1 <- coef(model)[2]
  (log(p / (1 - p)) - beta0) / beta1
}

# target probabilities for which we want to know the match ratio
target_probs <- c(0.5, 0.6, 0.7, 0.8, 0.9)

# create table with probability column and fill with values from get-cutoff + lost and retained calls + percentage
loss_table <- data.frame(Probability = target_probs) %>%
  mutate(
    Cutoff_Ratio = pmin(
      sapply(Probability, get_cutoff, model = enil_glm),
      1),
    Calls_retained = sapply(Cutoff_Ratio, function(c) 
      sum(enil_data$MATCH.RATIO >= c)),
    Calls_lost = sapply(Cutoff_Ratio, function(c) 
      sum(enil_data$MATCH.RATIO < c)),
    Percent_loss = round(
      (Calls_lost / nrow(enil_data)) * 100, 2 ) )

enil_glm2 <- glm(correct ~ MATCH.RATIO + PULSES + MATCHING,
                 family = binomial, data = enil_data)
summary(enil_glm2)

rng <- range(enil_data$MATCH.RATIO)

loss_table <- data.frame(Probability = target_probs) %>%
  mutate(
    Cutoff_Ratio = sapply(Probability, get_cutoff, model = enil_glm),
    Cutoff_Ratio = ifelse(Cutoff_Ratio >= rng[1] & Cutoff_Ratio <= rng[2],
                          Cutoff_Ratio, NA)           # NA = target unreachable
  )

print(loss_table)

ggplot(loss_table, aes(Probability, Percent_loss)) +
  geom_line(linewidth = 1, color = "steelblue") +
  geom_point(aes(shape = Cutoff_Ratio >= 1), size = 3.5, color = "steelblue") +
  geom_text(aes(label = paste0("≥ ", round(Cutoff_Ratio, 2))),
            vjust = -1.1, size = 3.3) +
  scale_x_continuous(labels = scales::percent) +        # if Probability is 0-1
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.12))) +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 4),
                     labels = c("reached", "capped at 1"),
                     name = "Target") +
  labs(x = "Target probability of a correct ID",
       y = "Calls lost (%)",
       subtitle = "Labels show the minimum match ratio required") +
  theme_minimal()


loss_table %>%
  pivot_longer(c(Cutoff_Ratio, Percent_loss),
               names_to = "metric", values_to = "value") %>%
  mutate(metric = recode(metric,
                         Cutoff_Ratio = "Required match ratio",
                         Percent_loss = "Calls lost (%)")) %>%
  ggplot(aes(Probability, value)) +
  geom_line(color = "steelblue") +
  geom_point(size = 3, color = "steelblue") +
  facet_wrap(~ metric, ncol = 1, scales = "free_y", strip.position = "left") +
  scale_x_continuous(labels = scales::percent) +
  labs(x = "Target probability of a correct ID", y = NULL) +
  theme_minimal() +
  theme(strip.placement = "outside")

loss_table %>%
  pivot_longer(c(Calls_retained, Calls_lost), names_to = "status", values_to = "n") %>%
  ggplot(aes(factor(Probability), n, fill = status)) +
  geom_col() +
  scale_fill_manual(values = c(Calls_retained = "steelblue", Calls_lost = "grey70"),
                    labels = c("Lost", "Retained"), name = NULL) +
  labs(x = "Target probability", y = "Calls") +
  theme_minimal()


# plot

ppyg_data2 <- spring %>%
  filter(autoid == "PIPPYG") %>%                 # KPRO's PPYG calls
  mutate(correct = as.integer(id_std == "PIPPYG"))   # 1 = manual ID agrees

ppyg_glm <- glm(correct ~ MATCH.RATIO, data = ppyg_data, family = binomial)

ggplot(ppyg_data, aes(MATCH.RATIO, correct)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "glm", method.args = list(family = binomial),
              formula = y ~ x, se = FALSE, color = "blue") +
  labs(title = "Comparing KPRO vs Manual ID for Soprano pipistrelles bats",
       x = "KPRO match ratio",
       y = "Predicted probability of correct") +
  theme_minimal()

#PNAT-------
#Logistic regression to determine cut-off

pnat_data <- spring %>%
  filter(autoid == "PIPNAT") %>%
  mutate(
    MATCH.RATIO = as.numeric(MATCH.RATIO),
    correct = ifelse(id == "PNAT", 1, 0)) %>%
  filter(!is.na(MATCH.RATIO))


pnat_glm <- glm(
  correct ~ MATCH.RATIO,
  family = binomial,
  data = pnat_data)

summary(pnat_glm)


# calculates the match ratio cut-off by inverting the logistic regression
get_cutoff <- function(p, model) {
  beta0 <- coef(model)[1]
  beta1 <- coef(model)[2]
  (log(p / (1 - p)) - beta0) / beta1
}

# target probabilities for which we want to know the match ratio
target_probs <- c(0.5, 0.6, 0.7, 0.8, 0.9)

# create table with probability column and fill with values from get-cutoff + lost and retained calls + percentage
loss_table <- data.frame(Probability = target_probs) %>%
  mutate(
    Cutoff_Ratio = pmin(
      sapply(Probability, get_cutoff, model = pnat_glm),
      1),
    Calls_retained = sapply(Cutoff_Ratio, function(c) 
      sum(pnat_data$MATCH.RATIO >= c)),
    Calls_lost = sapply(Cutoff_Ratio, function(c) 
      sum(pnat_data$MATCH.RATIO < c)),
    Percent_loss = round(
      (Calls_lost / nrow(pnat_data)) * 100, 2 ) )

print(loss_table)

ggplot(loss_table, aes(Probability, Percent_loss)) +
  geom_line(linewidth = 1, color = "steelblue") +
  geom_point(aes(shape = Cutoff_Ratio >= 1), size = 3.5, color = "steelblue") +
  geom_text(aes(label = paste0("≥ ", round(Cutoff_Ratio, 2))),
            vjust = -1.1, size = 3.3) +
  scale_x_continuous(labels = scales::percent) +        # if Probability is 0-1
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.12))) +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 4),
                     labels = c("reached", "capped at 1"),
                     name = "Target") +
  labs(x = "Target probability of a correct ID",
       y = "Calls lost (%)",
       subtitle = "Labels show the minimum match ratio required") +
  theme_minimal()


loss_table %>%
  pivot_longer(c(Cutoff_Ratio, Percent_loss),
               names_to = "metric", values_to = "value") %>%
  mutate(metric = recode(metric,
                         Cutoff_Ratio = "Required match ratio",
                         Percent_loss = "Calls lost (%)")) %>%
  ggplot(aes(Probability, value)) +
  geom_line(color = "steelblue") +
  geom_point(size = 3, color = "steelblue") +
  facet_wrap(~ metric, ncol = 1, scales = "free_y", strip.position = "left") +
  scale_x_continuous(labels = scales::percent) +
  labs(x = "Target probability of a correct ID", y = NULL) +
  theme_minimal() +
  theme(strip.placement = "outside")

loss_table %>%
  pivot_longer(c(Calls_retained, Calls_lost), names_to = "status", values_to = "n") %>%
  ggplot(aes(factor(Probability), n, fill = status)) +
  geom_col() +
  scale_fill_manual(values = c(Calls_retained = "steelblue", Calls_lost = "grey70"),
                    labels = c("Lost", "Retained"), name = NULL) +
  labs(x = "Target probability", y = "Calls") +
  theme_minimal()


# plot

pnat_data2 <- spring %>%
  filter(autoid == "PIPPYG") %>%                 # KPRO's PPYG calls
  mutate(correct = as.integer(id_std == "PIPPYG"))   # 1 = manual ID agrees

pnat_glm2 <- glm(correct ~ MATCH.RATIO, data = pnat_data, family = binomial)

ggplot(pnat_data, aes(MATCH.RATIO, correct)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "glm", method.args = list(family = binomial),
              formula = y ~ x, se = FALSE, color = "blue") +
  labs(title = "Comparing KPRO vs Manual ID for Soprano pipistrelles bats",
       x = "KPRO match ratio",
       y = "Predicted probability of correct") +
  theme_minimal()
summary(pnat_glm2)

# false detections

loss_table <- loss_table %>%
  mutate(
    Retained_correct = sapply(Cutoff_Ratio, function(c)
      sum(pnat_data$correct[pnat_data$MATCH.RATIO >= c], na.rm = TRUE)),
    Retained_false   = Calls_retained - Retained_correct,
    Precision        = round(Retained_correct / Calls_retained, 3),
    True_calls_lost  = sum(pnat_data$correct, na.rm = TRUE) - Retained_correct,
    Recall           = round(Retained_correct / sum(pnat_data$correct, na.rm = TRUE), 3),
  )

loss_table %>% select(Probability, Cutoff_Ratio, Calls_retained,
                      Retained_false, Precision, True_calls_lost, Recall, Percent_loss)

Expected_precision = sapply(Cutoff_Ratio, function(c)
  mean(predict(pnat_glm2,
               newdata = pnat_data[pnat_data$MATCH.RATIO >= c, ],
               type = "response")))
# plots-------------

winter <- winter %>% 
  mutate(
    datetime = as.POSIXct(
      paste(DATE, TIME),
      format = "%Y-%m-%d %H:%M:%S",
      tz = "Europe/Oslo"
    )
  )
spring <- spring %>% 
  mutate(
    datetime = as.POSIXct(
      paste(DATE, TIME),
      format = "%Y-%m-%d %H:%M:%S",
      tz = "Europe/Oslo"
    )
  )

winter$DATE <- as.Date(winter$DATE, format =  "%d/%m/%Y")
winter <- winter %>%
  mutate(id = if_else(DATE == "2026-01-05" & id == "ENIL",
                      "Noise",
                      id))
spring$DATE <- as.Date(spring$DATE, format =  "%d/%m/%Y")

ggplot(winter %>% filter(id %in% c("ENIL", "MDAU", "PPYG", "PNAT", "VMUR", "MYSP", "NNOC") ), aes(x = DATE, y = Site, color = id)) +
  geom_point(size = 2) +
  theme_minimal() +
  scale_x_date() +
  scale_y_discrete(expand = expansion(mult = c(0.01, 0.01)))+
  labs(title = "Observed bats in the winter")

ggplot(
  spring %>% filter(id %in% c("PNAT")),
  aes(x = datetime, y = Site, color = id)
) +
  geom_point(size = 2) +
  theme_minimal() +
  scale_x_datetime() +
  scale_y_discrete(expand = expansion(mult = c(0.01, 0.01))) +
  labs(title = "Observed bats in the spring", x = "Date")


# Valerie data --------------

cm14 <- read.csv("data/2025/id14.csv")

cm14 <- cm14 %>%
  mutate(id = MANUAL.ID) %>%
  separate_rows(id, sep = "_")

cm14 <- cm14 %>% 
  mutate(
    datetime = as.POSIXct(
      paste(DATE, TIME),
      format = "%Y-%m-%d %H:%M:%S",
      tz = "Europe/Oslo"
    )
  )

ggplot(
  cm14 %>% filter(id %in% c("CNIL", "MDAU", "PPYG", "PNAT", "VMUR", "MYSP", "NNOC", "NYCT")),
  aes(x = datetime, y = id, color = id)) +
  geom_point(size = 2) +
  theme_minimal() +
  scale_x_datetime() +
  scale_y_discrete(expand = expansion(mult = c(0.01, 0.01))) +
  labs(title = "Observed bats in CM-14 2025", x = "Date", y = "Species")

cm14 <- cm14 %>%
  mutate(id = as.factor(id))

summary(cm14$id)

cm14 <- cm14 %>%
  mutate(AUTO.ID. = as.factor(AUTO.ID.))

summary(cm14$AUTO.ID.)

#PPYG-------
#Logistic regression to determine cut-off


ppyg_data <- cm14 %>%
  filter(AUTO.ID. == "PIPPYG") %>%
  mutate(
    MATCH.RATIO = as.numeric(MATCH.RATIO),
    correct = ifelse(id == "PPYG", 1, 0)) %>%
  filter(!is.na(MATCH.RATIO))%>%
  filter(!is.na(id), trimws(id) != "")



ppyg_glm <- glm(
  correct ~ MATCH.RATIO,
  family = binomial,
  data = ppyg_data)

summary(ppyg_glm)


# calculates the match ratio cut-off by inverting the logistic regression
get_cutoff <- function(p, model) {
  beta0 <- coef(model)[1]
  beta1 <- coef(model)[2]
  (log(p / (1 - p)) - beta0) / beta1
}

# target probabilities for which we want to know the match ratio
target_probs <- c(0.5, 0.6, 0.7, 0.8, 0.9)

# create table with probability column and fill with values from get-cutoff + lost and retained calls + percentage
loss_table <- data.frame(Probability = target_probs) %>%
  mutate(
    Cutoff_Ratio = pmin(
      sapply(Probability, get_cutoff, model = ppyg_glm),
      1),
    Calls_retained = sapply(Cutoff_Ratio, function(c) 
      sum(ppyg_data$MATCH.RATIO >= c)),
    Calls_lost = sapply(Cutoff_Ratio, function(c) 
      sum(ppyg_data$MATCH.RATIO < c)),
    Percent_loss = round(
      (Calls_lost / nrow(ppyg_data)) * 100, 2 ) )

loss_table <- loss_table %>%
  mutate(
    Retained_correct = sapply(Cutoff_Ratio, function(c)
      sum(ppyg_data$correct[ppyg_data$MATCH.RATIO >= c], na.rm = TRUE)),
    Retained_false   = Calls_retained - Retained_correct,
    Precision        = round(Retained_correct / Calls_retained, 3),
    True_calls_lost  = sum(ppyg_data$correct, na.rm = TRUE) - Retained_correct,
    Recall           = round(Retained_correct / sum(ppyg_data$correct, na.rm = TRUE), 3),
  )

print(loss_table)


# Pnat

pnat_data <- cm14 %>%
  filter(AUTO.ID. == "PIPNAT", !is.na(id)) %>%
  mutate(
    MATCH.RATIO = as.numeric(MATCH.RATIO),
    correct = ifelse(id == "PNAT", 1, 0)) %>%
  filter(!is.na(MATCH.RATIO))%>%
  filter(!is.na(id), trimws(id) != "")


pnat_glm <- glm(
  correct ~ MATCH.RATIO,
  family = binomial,
  data = pnat_data)

summary(pnat_glm)

# create table with probability column and fill with values from get-cutoff + lost and retained calls + percentage
loss_table <- data.frame(Probability = target_probs) %>%
  mutate(
    Cutoff_Ratio = pmin(
      sapply(Probability, get_cutoff, model = pnat_glm),
      1),
    Calls_retained = sapply(Cutoff_Ratio, function(c) 
      sum(pnat_data$MATCH.RATIO >= c)),
    Calls_lost = sapply(Cutoff_Ratio, function(c) 
      sum(pnat_data$MATCH.RATIO < c)),
    Percent_loss = round(
      (Calls_lost / nrow(pnat_data)) * 100, 2 ) )

loss_table <- loss_table %>%
  mutate(
    Retained_correct = sapply(Cutoff_Ratio, function(c)
      sum(pnat_data$correct[pnat_data$MATCH.RATIO >= c], na.rm = TRUE)),
    Retained_false   = Calls_retained - Retained_correct,
    Precision        = round(Retained_correct / Calls_retained, 3),
    True_calls_lost  = sum(pnat_data$correct, na.rm = TRUE) - Retained_correct,
    Recall           = round(Retained_correct / sum(pnat_data$correct, na.rm = TRUE), 3),
  )

print(loss_table)

# Enil

enil_data <- spring %>%
  filter(autoid == "PIPNAT", !is.na(id)) %>%
  mutate(
    MATCH.RATIO = as.numeric(MATCH.RATIO),
    correct = ifelse(id == "PNAT", 1, 0)) %>%
  filter(!is.na(MATCH.RATIO))


pnat_glm <- glm(
  correct ~ MATCH.RATIO,
  family = binomial,
  data = pnat_data)

summary(pnat_glm)

# create table with probability column and fill with values from get-cutoff + lost and retained calls + percentage
loss_table <- data.frame(Probability = target_probs) %>%
  mutate(
    Cutoff_Ratio = pmin(
      sapply(Probability, get_cutoff, model = pnat_glm),
      1),
    Calls_retained = sapply(Cutoff_Ratio, function(c) 
      sum(pnat_data$MATCH.RATIO >= c)),
    Calls_lost = sapply(Cutoff_Ratio, function(c) 
      sum(pnat_data$MATCH.RATIO < c)),
    Percent_loss = round(
      (Calls_lost / nrow(pnat_data)) * 100, 2 ) )

loss_table <- loss_table %>%
  mutate(
    Retained_correct = sapply(Cutoff_Ratio, function(c)
      sum(pnat_data$correct[pnat_data$MATCH.RATIO >= c], na.rm = TRUE)),
    Retained_false   = Calls_retained - Retained_correct,
    Precision        = round(Retained_correct / Calls_retained, 3),
    True_calls_lost  = sum(pnat_data$correct, na.rm = TRUE) - Retained_correct,
    Recall           = round(Retained_correct / sum(pnat_data$correct, na.rm = TRUE), 3),
  )

print(loss_table)

# pnat activity minutes------------

pnat_data <- spring %>%
  filter(autoid == "PIPNAT") %>%
  mutate(
    MATCH.RATIO = as.numeric(MATCH.RATIO),
    correct = ifelse(id == "PNAT", 1, 0)) %>%
  filter(!is.na(MATCH.RATIO))

pnat_min <- pnat_data %>%
  mutate(minute = floor_date(datetime, "minute"))      # use your datetime column

manutotal_min <- n_distinct(pnat_min$Site, pnat_min$minute)
true_min  <- pnat_min %>% filter(correct == 1) %>%
  distinct(Site, minute) %>% nrow()

minute_stats <- function(c) {
  kept <- pnat_min %>% filter(MATCH.RATIO >= c)
  tibble(
    Minutes_retained         = n_distinct(kept$Site, kept$minute),
    Minutes_retained_correct = kept %>% filter(correct == 1) %>%
      distinct(Site, minute) %>% nrow()
  )
}

pnat_glm <- glm(
  correct ~ MATCH.RATIO,
  family = binomial,
  data = pnat_data)

loss_table <- data.frame(Probability = target_probs) %>%
  mutate(
    Cutoff_Ratio = pmin(
      sapply(Probability, get_cutoff, model = pnat_glm),
      1),
    Calls_retained = sapply(Cutoff_Ratio, function(c) 
      sum(pnat_data$MATCH.RATIO >= c)),
    Calls_lost = sapply(Cutoff_Ratio, function(c) 
      sum(pnat_data$MATCH.RATIO < c)),
    Percent_loss = round(
      (Calls_lost / nrow(pnat_data)) * 100, 2 ) )

loss_table <- loss_table %>%
  mutate(
    Retained_correct = sapply(Cutoff_Ratio, function(c)
      sum(pnat_data$correct[pnat_data$MATCH.RATIO >= c], na.rm = TRUE)),
    Retained_false   = Calls_retained - Retained_correct,
    Precision        = round(Retained_correct / Calls_retained, 3),
    True_calls_lost  = sum(pnat_data$correct, na.rm = TRUE) - Retained_correct,
    Recall           = round(Retained_correct / sum(pnat_data$correct, na.rm = TRUE), 3),
  )


loss_table_min <- loss_table %>%
  select(Probability, Cutoff_Ratio) %>%
  mutate(stats = lapply(Cutoff_Ratio, minute_stats)) %>%
  unnest(stats) %>%
  mutate(
    Minutes_lost           = total_min - Minutes_retained,
    Percent_loss           = round(Minutes_lost / total_min * 100, 2),
    Minutes_retained_false = Minutes_retained - Minutes_retained_correct,
    Precision              = round(Minutes_retained_correct / Minutes_retained, 3),
    True_minutes_lost      = true_min - Minutes_retained_correct,
    Recall                 = round(Minutes_retained_correct / true_min, 3)
  )

loss_table_min %>% select(Probability, Cutoff_Ratio, Minutes_retained,
                          Minutes_retained_false, Precision,
                          True_minutes_lost, Recall, Percent_loss)

pnat_minlevel <- pnat_min %>%
  group_by(Site, minute) %>%
  summarise(MATCH.RATIO = max(MATCH.RATIO), correct = max(correct), .groups = "drop")

pnat_glm_min <- glm(correct ~ MATCH.RATIO, family = binomial, data = pnat_minlevel)
summary(pnat_glm_min)

c0 <- 0.86
c0 <- 0.75
c0 <- 0.66

minute_stats(c0) %>%
  mutate(
    Minutes_lost           = total_min - Minutes_retained,
    Percent_loss           = round(Minutes_lost / total_min * 100, 2),
    Minutes_retained_false = Minutes_retained - Minutes_retained_correct,
    Precision              = round(Minutes_retained_correct / Minutes_retained, 3),
    True_minutes_lost      = true_min - Minutes_retained_correct,
    Recall                 = round(Minutes_retained_correct / true_min, 3)
  )

sum(pnat_data$id == "PNAT", na.rm = TRUE)

# with big dataset

big <- read.csv("data/CM_manualIDs_allavailable_subsets.csv")

head(big)

big  <- big %>%
  mutate(id = manualid) %>%
  separate_rows(id, sep = "_")

big <- big %>% 
  mutate(
    datetime = as.POSIXct(
      paste(DATE, TIME),
      format = "%d/%m/%Y %H:%M:%S",
      tz = "Europe/Oslo"
    )
  )

pnat_data <- big %>% # from here--------------------------------
  filter(autoid == "PIPNAT") %>%
  mutate(
    MATCH.RATIO = as.numeric(MATCH.RATIO),
    correct = ifelse(id == "PNAT", 1, 0)) %>%
  filter(!is.na(MATCH.RATIO))

pnat_min <- pnat_data %>%
  mutate(minute = floor_date(datetime, "minute"))      # use your datetime column

total_min <- n_distinct(pnat_min$site, pnat_min$minute)
true_min  <- pnat_min %>% filter(correct == 1) %>%
  distinct(site, minute) %>% nrow()

minute_stats <- function(c) {
  kept <- pnat_min %>% filter(MATCH.RATIO >= c)
  tibble(
    Minutes_retained         = n_distinct(kept$site, kept$minute),
    Minutes_retained_correct = kept %>% filter(correct == 1) %>%
      distinct(site, minute) %>% nrow()
  )
}

pnat_glm <- glm(
  correct ~ MATCH.RATIO,
  family = binomial,
  data = pnat_data)

loss_table <- data.frame(Probability = target_probs) %>%
  mutate(
    Cutoff_Ratio = pmin(
      sapply(Probability, get_cutoff, model = pnat_glm),
      1),
    Calls_retained = sapply(Cutoff_Ratio, function(c) 
      sum(pnat_data$MATCH.RATIO >= c)),
    Calls_lost = sapply(Cutoff_Ratio, function(c) 
      sum(pnat_data$MATCH.RATIO < c)),
    Percent_loss = round(
      (Calls_lost / nrow(pnat_data)) * 100, 2 ) )

loss_table <- loss_table %>%
  mutate(
    Retained_correct = sapply(Cutoff_Ratio, function(c)
      sum(pnat_data$correct[pnat_data$MATCH.RATIO >= c], na.rm = TRUE)),
    Retained_false   = Calls_retained - Retained_correct,
    Precision        = round(Retained_correct / Calls_retained, 3),
    True_calls_lost  = sum(pnat_data$correct, na.rm = TRUE) - Retained_correct,
    Recall           = round(Retained_correct / sum(pnat_data$correct, na.rm = TRUE), 3),
  )


loss_table_min <- loss_table %>%
  select(Probability, Cutoff_Ratio) %>%
  mutate(stats = lapply(Cutoff_Ratio, minute_stats)) %>%
  unnest(stats) %>%
  mutate(
    Minutes_lost           = total_min - Minutes_retained,
    Percent_loss           = round(Minutes_lost / total_min * 100, 2),
    Minutes_retained_false = Minutes_retained - Minutes_retained_correct,
    Precision              = round(Minutes_retained_correct / Minutes_retained, 3),
    True_minutes_lost      = true_min - Minutes_retained_correct,
    Recall                 = round(Minutes_retained_correct / true_min, 3)
  )

loss_table_min %>% select(Probability, Cutoff_Ratio, Minutes_retained,
                          Minutes_retained_false, Precision,
                          True_minutes_lost, Recall, Percent_loss)

pnat_minlevel <- pnat_min %>%
  group_by(site, minute) %>%
  summarise(MATCH.RATIO = max(MATCH.RATIO), correct = max(correct), .groups = "drop")

pnat_glm_min <- glm(correct ~ MATCH.RATIO, family = binomial, data = pnat_minlevel)
summary(pnat_glm_min)

cutoff_table <- tibble(Cutoff_Ratio = c(0.65, 0.75, 0.86)) %>%
  mutate(stats = lapply(Cutoff_Ratio, minute_stats)) %>%
  unnest(stats) %>%
  mutate(
    Minutes_lost           = total_min - Minutes_retained,
    Percent_loss           = round(Minutes_lost / total_min * 100, 2),
    Minutes_retained_false = Minutes_retained - Minutes_retained_correct,
    Precision              = round(Minutes_retained_correct / Minutes_retained, 3),
    True_minutes_lost      = true_min - Minutes_retained_correct,
    Recall                 = round(Minutes_retained_correct / true_min, 3)
  )

cutoff_table %>%
  select(Cutoff_Ratio, Minutes_retained, Minutes_retained_false, Precision,
         True_minutes_lost, Recall, Percent_loss)


rng <- range(pnat_minlevel$MATCH.RATIO)

loss_table_min <- data.frame(Probability = target_probs) %>%
  mutate(
    Raw_cutoff   = sapply(Probability, get_cutoff, model = pnat_glm_min),
    Cutoff_Ratio = case_when(Raw_cutoff < rng[1] ~ rng[1],
                             Raw_cutoff > rng[2] ~ NA_real_,
                             TRUE ~ Raw_cutoff),
    stats        = lapply(Cutoff_Ratio, minute_stats)
  ) %>%
  unnest(stats) %>%
  mutate(
    Minutes_lost           = total_min - Minutes_retained,
    Percent_loss           = round(Minutes_lost / total_min * 100, 2),
    Minutes_retained_false = Minutes_retained - Minutes_retained_correct,
    Precision              = round(Minutes_retained_correct / Minutes_retained, 3),
    True_minutes_lost      = true_min - Minutes_retained_correct,
    Recall                 = round(Minutes_retained_correct / true_min, 3)
  ) %>%
  select(-Raw_cutoff)
