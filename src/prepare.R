##
## Remove some subjects and items (deemed necessary by Tijn)
## Save dataframe without some columns
##

here::i_am("src/prepare.R")


library(dplyr)
library(readr)
library(here)

collectedDF <- read_delim(here("data/allACTFiles_vp.csv"), delim = " ")

collectedDF$item <- as.factor(collectedDF$item)

collectedDF <- subset(collectedDF, item != 014 & item != 024 & subjectnr != 004, drop = TRUE)

collectedDF$item <- as.factor(as.numeric(collectedDF$item))

collectedDF <- subset(collectedDF, !(subjectnr == 004 & (item == 3 | item == 4 | item == 6 | item == 7)))
collectedDF <- subset(collectedDF, !(subjectnr == 038 & item == 14))
collectedDF <- subset(collectedDF, !(subjectnr == 040 & item == 19))
collectedDF <- subset(collectedDF, !(subjectnr == 013 & item == 25))
collectedDF <- subset(collectedDF, !(subjectnr == 043 & (item == 3 | item == 6 | item == 7)))
collectedDF <- subset(collectedDF, !(subjectnr == 020 & item == 30))

collectedDF %>%
  filter(subjectnr == 043, item == 3)

collectedDF$cond <- as.character(collectedDF$cond)

collectedDF <- collectedDF %>%
  group_by(cond) %>%
  mutate(
    quan = strsplit(cond[1], "_")[[1]][1],
    subj = strsplit(cond[1], "_")[[1]][2],
    obj = strsplit(cond[1], "_")[[1]][3]
  ) %>%
  ungroup()


collectedDF %>%
  select(-cond, -expname) %>%
  rename(
    subj = subjectnr, subj_cond = subj, obj_cond = obj,
    quan_cond = quan, region = code
  ) %>%
  write_csv(here("results/allACTFiles_vp_clean.csv"))
