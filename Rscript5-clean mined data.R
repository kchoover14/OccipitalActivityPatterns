library(tidyverse)

#read mined data
occ.westcott <- read.csv("data-occ-westcott-raw.csv", stringsAsFactors = TRUE)
occ.williams <- read.csv("data-occ-williams-raw.csv", stringsAsFactors = TRUE)

#create subset for analysis
occ.westcott <- dplyr::select(occ.westcott, Group, Coll, Sex, BCBOC, FMLOC, FMBOC)
occ.williams <- dplyr::select(occ.williams, Group, Site, Sex, BCB, LFM, WFM)

#rename variables to match
occ.westcott <- rename(occ.westcott, Collection=Coll)
occ.williams <- rename(occ.williams, Collection=Site)

occ.westcott <- rename(occ.westcott, BicondylarBreadth=BCBOC)
occ.williams <- rename(occ.williams, BicondylarBreadth=BCB)

occ.westcott <- rename(occ.westcott, ForamenMagnumS=FMLOC)
occ.williams <- rename(occ.williams, ForamenMagnumS=LFM)

occ.westcott <- rename(occ.westcott, ForamenMagnumT=FMBOC)
occ.williams <- rename(occ.williams, ForamenMagnumT=WFM)

#standardize Sex
occ.williams <- rename(occ.williams, SexOld=Sex)
occ.williams <- occ.williams %>% mutate(Sex = case_when(
    SexOld == "F"  ~ "Female",
    SexOld == "M"  ~ "Male"))
occ.williams <- dplyr::select(occ.williams, -SexOld)
occ.williams$Sex <- as.factor(occ.williams$Sex)
occ.williams <- occ.williams %>% relocate(Sex, .after=Collection)

#merge datasets
occ.mined <- rbind(occ.westcott, occ.williams)

#save data
write.csv(occ.mined, "data-occ-mined.csv", quote=FALSE, row.names = FALSE)

#tidy
rm(list = ls())
gc()

