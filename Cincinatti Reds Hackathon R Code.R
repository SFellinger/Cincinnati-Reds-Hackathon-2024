library(dplyr)
library(ggplot2)
library(tidyverse)
library(readr)
library(purrr)
library(lubridate)
options(scipen = 999)

# Selecting variables needed for analyzing pitch dropoffs
SavantAll = read.csv("savant_pitch_level.csv") %>%
  select(
    game_date,
    player_name,
    role_key,
    pitcher_at_bat_number,
    pitch_type,
    effective_speed,
    release_spin_rate,
    pfx_x,
    pfx_z
  ) %>%
  group_by(
    player_name,
    game_date,
    pitch_type,
    pitcher_at_bat_number,
    role_key
  ) %>%
  summarise(
    avg_effective_speed = mean(effective_speed),
    avg_release_spin_rate = mean(release_spin_rate),
    avg_pfx_x = mean(pfx_x),
    avg_pfx_z = mean(pfx_z),
  )

SavantSummary <- SavantAll %>%
  ungroup() %>%
  group_by(role_key,
           pitch_type)

# Changing the way names are recorded in Baseball Savant data
SavantAll$player_name = gsub(',', '', SavantAll$player_name)
name_components <- str_split(SavantAll$player_name, " ")
SavantAll$player_name <- sapply(name_components, function(x) paste(rev(x), collapse = " "))

# Calculating dropoff for each pitch using stat we determined most important to
# pitch success in Python
Dropoffs <- SavantAll %>%
  group_by(player_name, game_date, pitch_type) %>%
  mutate(dropoff = ifelse(pitch_type %in% c("FF", "SI", "CH"),
                          avg_effective_speed - first(avg_effective_speed),
                          ifelse(pitch_type %in% c("SL", "CS", "FS"),
                                 avg_pfx_x - first(avg_pfx_x),
                                 ifelse(pitch_type %in% c("CU", "KC", "SC"),
                                        avg_pfx_z - first(avg_pfx_z),
                                        ifelse(pitch_type %in% c("FC", "SV", "ST"),
                                               avg_release_spin_rate - first(avg_release_spin_rate), NA))))) %>%
  arrange(game_date, player_name, pitcher_at_bat_number)

# Reading in Python code that pivoted dropoff table from above
DropoffYear <- read.csv("drop_dummy_4.csv")

# Calculating average dropoff per pitch by role (SP vs RP)
DropoffbyRole <- DropoffYear %>%
  group_by(role_key, pitch_type) %>%
  summarise(
    n = n(),
    avg_dropoff_2nd_at_bat = mean(X2, na.rm = TRUE),
    avg_dropoff_3rd_at_bat = mean(X3, na.rm = TRUE),
    avg_dropoff_4th_at_bat = mean(X4, na.rm = TRUE),
    avg_dropoff_5th_at_bat = mean(X5, na.rm = TRUE),
    avg_dropoff_6th_at_bat = mean(X6, na.rm = TRUE),
    avg_dropoff_7th_at_bat = mean(X7, na.rm = TRUE),
    avg_dropoff_8th_at_bat = mean(X8, na.rm = TRUE),
    avg_dropoff_9th_at_bat = mean(X9, na.rm = TRUE),
    avg_dropoff_10th_at_bat = mean(X10, na.rm = TRUE),
    avg_dropoff_11th_at_bat = mean(X11, na.rm = TRUE),
    avg_dropoff_12th_at_bat = mean(X12, na.rm = TRUE),
    avg_dropoff_13th_at_bat = mean(X13, na.rm = TRUE),
    avg_dropoff_14th_at_bat = mean(X14, na.rm = TRUE),
    avg_dropoff_15th_at_bat = mean(X15, na.rm = TRUE),
    avg_dropoff_16th_at_bat = mean(X16, na.rm = TRUE),
    avg_dropoff_17th_at_bat = mean(X17, na.rm = TRUE),
    avg_dropoff_18th_at_bat = mean(X18, na.rm = TRUE),
    avg_dropoff_19th_at_bat = mean(X19, na.rm = TRUE),
    avg_dropoff_20th_at_bat = mean(X20, na.rm = TRUE),
    avg_dropoff_21st_at_bat = mean(X21, na.rm = TRUE),
    avg_dropoff_22nd_at_bat = mean(X22, na.rm = TRUE),
    avg_dropoff_23rd_at_bat = mean(X23, na.rm = TRUE),
    avg_dropoff_24th_at_bat = mean(X24, na.rm = TRUE),
    avg_dropoff_25th_at_bat = mean(X25, na.rm = TRUE),
    avg_dropoff_26th_at_bat = mean(X26, na.rm = TRUE),
    avg_dropoff_27th_at_bat = mean(X27, na.rm = TRUE),
    avg_dropoff_28th_at_bat = mean(X28, na.rm = TRUE),
    avg_dropoff_29th_at_bat = mean(X29, na.rm = TRUE),
    avg_dropoff_30th_at_bat = mean(X30, na.rm = TRUE),
    avg_dropoff_31st_at_bat = mean(X31, na.rm = TRUE),
    avg_dropoff_32nd_at_bat = mean(X32, na.rm = TRUE),
    avg_dropoff_33rd_at_bat = mean(X33, na.rm = TRUE),
    avg_dropoff_34th_at_bat = mean(X34, na.rm = TRUE),
    avg_dropoff_35th_at_bat = mean(X35, na.rm = TRUE)
  ) %>%
  arrange(pitch_type)

# Combining dropoffs for each player
DropoffPlayer <- DropoffYear %>%
  group_by(player_name, pitch_type, role_key) %>%
  mutate(count = n()) %>%
  summarise(
    count = first(count),
    avg_dropoff_2nd_at_bat = mean(X2, na.rm = TRUE),
    avg_dropoff_3rd_at_bat = mean(X3, na.rm = TRUE),
    avg_dropoff_4th_at_bat = mean(X4, na.rm = TRUE),
    avg_dropoff_5th_at_bat = mean(X5, na.rm = TRUE),
    avg_dropoff_6th_at_bat = mean(X6, na.rm = TRUE),
    avg_dropoff_7th_at_bat = mean(X7, na.rm = TRUE),
    avg_dropoff_8th_at_bat = mean(X8, na.rm = TRUE),
    avg_dropoff_9th_at_bat = mean(X9, na.rm = TRUE),
    avg_dropoff_10th_at_bat = mean(X10, na.rm = TRUE),
    avg_dropoff_11th_at_bat = mean(X11, na.rm = TRUE),
    avg_dropoff_12th_at_bat = mean(X12, na.rm = TRUE),
    avg_dropoff_13th_at_bat = mean(X13, na.rm = TRUE),
    avg_dropoff_14th_at_bat = mean(X14, na.rm = TRUE),
    avg_dropoff_15th_at_bat = mean(X15, na.rm = TRUE),
    avg_dropoff_16th_at_bat = mean(X16, na.rm = TRUE),
    avg_dropoff_17th_at_bat = mean(X17, na.rm = TRUE),
    avg_dropoff_18th_at_bat = mean(X18, na.rm = TRUE),
    avg_dropoff_19th_at_bat = mean(X19, na.rm = TRUE),
    avg_dropoff_20th_at_bat = mean(X20, na.rm = TRUE),
    avg_dropoff_21st_at_bat = mean(X21, na.rm = TRUE),
    avg_dropoff_22nd_at_bat = mean(X22, na.rm = TRUE),
    avg_dropoff_23rd_at_bat = mean(X23, na.rm = TRUE),
    avg_dropoff_24th_at_bat = mean(X24, na.rm = TRUE),
    avg_dropoff_25th_at_bat = mean(X25, na.rm = TRUE),
    avg_dropoff_26th_at_bat = mean(X26, na.rm = TRUE),
    avg_dropoff_27th_at_bat = mean(X27, na.rm = TRUE),
    avg_dropoff_28th_at_bat = mean(X28, na.rm = TRUE),
    avg_dropoff_29th_at_bat = mean(X29, na.rm = TRUE),
    avg_dropoff_30th_at_bat = mean(X30, na.rm = TRUE),
    avg_dropoff_31st_at_bat = mean(X31, na.rm = TRUE),
    avg_dropoff_32nd_at_bat = mean(X32, na.rm = TRUE),
    avg_dropoff_33rd_at_bat = mean(X33, na.rm = TRUE),
    avg_dropoff_34th_at_bat = mean(X34, na.rm = TRUE),
    avg_dropoff_35th_at_bat = mean(X35, na.rm = TRUE)
  )

# Adding initial values for each pitch into DropoffbyRole dataframe
Initial <- read.csv("initial_pitch_avg.csv") %>%
  filter(!(pitch_type %in% c("EP", "FA", "KN", "SC")))

DropoffbyRole <- DropoffYearRole %>%
  filter(pitch_type != "SC")

DropoffYearRole$initial <- Initial$selected

# Rearranging columns in DropoffbyRole table
new_column_order <- c("role_key",
                      "pitch_type",
                      "n",
                      "initial",
                      "avg_dropoff_2nd_at_bat",
                      "avg_dropoff_3rd_at_bat",
                      "avg_dropoff_4th_at_bat",
                      "avg_dropoff_5th_at_bat",
                      "avg_dropoff_6th_at_bat",
                      "avg_dropoff_7th_at_bat",
                      "avg_dropoff_8th_at_bat",
                      "avg_dropoff_9th_at_bat",
                      "avg_dropoff_10th_at_bat",
                      "avg_dropoff_11th_at_bat",
                      "avg_dropoff_12th_at_bat",
                      "avg_dropoff_13th_at_bat",
                      "avg_dropoff_14th_at_bat",
                      "avg_dropoff_15th_at_bat",
                      "avg_dropoff_16th_at_bat",
                      "avg_dropoff_17th_at_bat",
                      "avg_dropoff_18th_at_bat",
                      "avg_dropoff_19th_at_bat",
                      "avg_dropoff_20th_at_bat",
                      "avg_dropoff_21st_at_bat",
                      "avg_dropoff_22nd_at_bat",
                      "avg_dropoff_23rd_at_bat",
                      "avg_dropoff_24th_at_bat",
                      "avg_dropoff_25th_at_bat",
                      "avg_dropoff_26th_at_bat",
                      "avg_dropoff_27th_at_bat",
                      "avg_dropoff_28th_at_bat",
                      "avg_dropoff_29th_at_bat",
                      "avg_dropoff_30th_at_bat",
                      "avg_dropoff_31st_at_bat",
                      "avg_dropoff_32nd_at_bat",
                      "avg_dropoff_33rd_at_bat",
                      "avg_dropoff_34th_at_bat",
                      "avg_dropoff_35th_at_bat")
DropoffYearRole <- DropoffYearRole %>%
  select(all_of(new_column_order))

# Determining how many at bats to look at
SavantAll %>%
  group_by(role_key) %>%
  summarise(`Avg # of Batters Faced` = mean(pitcher_at_bat_number))
ggplot(data = SavantAll %>%
  group_by(role_key) %>%
  summarise(`Avg # of Batters Faced` = mean(pitcher_at_bat_number)), aes(x = role_key, y = `Avg # of Batters Faced`, fill= role_key)) +
  geom_col() +
    scale_fill_manual(values = c("SP" = "dodgerblue", "RP" = "indianred1")) +
    labs(x = 'Role', y = 'Avg # of Batters Faced', title = 'Avg # of Batters Faced by Role', fill = 'Role') +
  geom_text(aes(label = round(`Avg # of Batters Faced`, 1)), vjust = -0.25, color = 'black')

# Creating PlayerID as a combination of Name, pitch, and role
DropoffPlayer$NamePitchAndRole <- paste(DropoffPlayer$player_name, DropoffPlayer$pitch_type, DropoffPlayer$role_key, sep = "- ")
ResultData <- data.frame(ID = DropoffPlayer$NamePitchAndRole) 

# Calculating difference between player dropoff and role dropoff for 2nd at bat
ResultData$Result_Value_2nd_at_Bat <- NA

for (i in 1:nrow(DropoffPlayer)) {
  for (j in 1:ncol(DropoffYearRole)) {
    if (!is.na(DropoffYearRole$pitch_type[j]) && !is.na(DropoffPlayer$pitch_type[i]) &&
        DropoffYearRole$pitch_type[j] == DropoffPlayer$pitch_type[i] &&
        DropoffYearRole$role_key[j] != DropoffPlayer$role_key[i]) {
      ResultData$Result_Value_2nd_at_Bat[i] <- DropoffPlayer$avg_dropoff_2nd_at_bat[i] - DropoffYearRole$avg_dropoff_2nd_at_bat[j]
    }
  }
}

# Calculating difference between player dropoff and role dropoff for 3rd at bat
ResultData$Result_Value_3rd_at_Bat <- NA

for (i in 1:nrow(DropoffPlayer)) {
  for (j in 1:ncol(DropoffYearRole)) {
    if (!is.na(DropoffYearRole$pitch_type[j]) && !is.na(DropoffPlayer$pitch_type[i]) &&
        DropoffYearRole$pitch_type[j] == DropoffPlayer$pitch_type[i] &&
        DropoffYearRole$role_key[j] != DropoffPlayer$role_key[i]) {
      ResultData$Result_Value_3rd_at_Bat[i] <- DropoffPlayer$avg_dropoff_3rd_at_bat[i] - DropoffYearRole$avg_dropoff_3rd_at_bat[j]
    }
  }
}

# Calculating difference between player dropoff and role dropoff for 4th at bat
ResultData$Result_Value_4th_at_Bat <- NA

for (i in 1:nrow(DropoffPlayer)) {
  for (j in 1:ncol(DropoffYearRole)) {
    if (!is.na(DropoffYearRole$pitch_type[j]) && !is.na(DropoffPlayer$pitch_type[i]) &&
        DropoffYearRole$pitch_type[j] == DropoffPlayer$pitch_type[i] &&
        DropoffYearRole$role_key[j] != DropoffPlayer$role_key[i]) {
      ResultData$Result_Value_4th_at_Bat[i] <- DropoffPlayer$avg_dropoff_4th_at_bat[i] - DropoffYearRole$avg_dropoff_4th_at_bat[j]
    }
  }
}

# Regression for both groups combined

selectedFanGraphsTest <- FanGraphs_All %>%
  select(Name, ER, ERA, WAR, K_pct_plus, BB_pct_plus, SIERA, WHIP_plus, BABIP_plus, K_per_9_plus, CSW_pct, Stuff_plus,
         Location_plus, LOB_pct, GB_to_FB)

regTest1 <- lm(ER ~ . - ERA - WAR - Name, selectedFanGraphsTest)
regTest2 <- lm(ERA ~ . - ER - WAR - Name, selectedFanGraphsTest)
regTest3 <- lm(WAR ~ . - ER - ERA - Name, selectedFanGraphsTest)

summary(regTest1)
summary(regTest2)
summary(regTest3)

c = cor(selectedFanGraphsTest[,2:15])
corrplot(c, method = 'number')

# Regressions for Starting Pitchers

selectedFanGraphsTestSP <- FanGraphs_All %>%
  filter(Role == 'SP') %>%
  select(Name, ER, ERA, WAR, K_pct_plus, BB_pct_plus, SIERA, WHIP_plus, BABIP_plus, K_per_9_plus, CSW_pct, Stuff_plus,
         Location_plus, LOB_pct, GB_to_FB)


regTest1SP <- lm(ER ~ . - ERA - WAR - Name, selectedFanGraphsTestSP)
regTest2SP <- lm(ERA ~ . - ER - WAR - Name, selectedFanGraphsTestSP)
regTest3SP <- lm(WAR ~ . - ER - ERA - Name, selectedFanGraphsTestSP)

summary(regTest1SP)
summary(regTest2SP)
summary(regTest3SP)

# Regressions for Relief Pitchers

selectedFanGraphsTestRP <- FanGraphs_All %>%
  filter(Role == 'RP') %>%
  select(Name, ER, ERA, WAR, K_pct_plus, BB_pct_plus, SIERA, WHIP_plus, BABIP_plus, K_per_9_plus, CSW_pct, Stuff_plus,
         Location_plus, LOB_pct, GB_to_FB)

regTest1RP <- lm(ER ~ . - ERA - WAR - Name, selectedFanGraphsTestRP)
regTest2RP <- lm(ERA ~ . - ER - WAR - Name, selectedFanGraphsTestRP)
regTest3RP <- lm(WAR ~ . - ER - ERA - Name, selectedFanGraphsTestRP)

summary(regTest1RP)
summary(regTest2RP)
summary(regTest3RP)

# The ERA model was the best one (model 2)

# Look at 3 Selected Pitchers who matched dropoffs for other role better to see 
# if they have stat(s) most indicative to that position's success from regression models above
FanGraphs_All = read.csv('fangraphs_season_level.csv')
FanGraphs_SP_to_RP <- FanGraphs_All %>%
  select(NameASCII, Role, IP, Season, BB_pct_plus, BABIP_plus, CSW_pct, GB_to_FB, BB_pct) %>%
  group_by(NameASCII) %>%
  filter(NameASCII == "Madison Bumgarner" | NameASCII == "Marcus Stroman" | NameASCII == "Caleb Thielbar") %>%
  mutate(BB_pct_all = mean(BB_pct_plus, na.rm = TRUE),
         BABIP_plus_all = mean(BABIP_plus, na.rm = TRUE),
         CSW_pct_all = weighted.mean(CSW_pct, w = IP, na.rm = TRUE),
         GB_to_FB_all = weighted.mean(GB_to_FB, w = IP, na.rm = TRUE)) %>%
  summarise(BB_pct = mean(BB_pct),
            BB_pct_all = first(BB_pct_all),
            BABIP_plus_all = first(BABIP_plus_all),
            SW_pct_all = first(CSW_pct_all),
            GB_to_FB_all = first(GB_to_FB_all))
