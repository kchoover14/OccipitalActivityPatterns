library(tidyverse)

#read data
occ.wide <- read.csv("data-occ-original-raw-22dec21.csv", stringsAsFactors = TRUE)

#keep only second trial nonmetrics
occ.wide <- dplyr::select(occ.wide, -genform1, -nuchcres1, -nuchline1)
occ.wide <- occ.wide %>%
    rename(NuchalCrest= nuchcres2) %>%
    rename(NuchalLine= nuchline2) %>%
    rename(GeneralForm=genform2)

#take average across three trials and remove replicates
occ.wide$DepthEOP <- rowMeans(occ.wide[ , c(7,8,9)], na.rm=TRUE)
occ.wide$LambdaInion <- rowMeans(occ.wide[ , c(10,11,12)], na.rm=TRUE)
occ.wide$BicondylarBreadth <- rowMeans(occ.wide[ , c(13,14,15)], na.rm=TRUE)
occ.wide$ForamenMagnumS <- rowMeans(occ.wide[ , c(16,17,18)], na.rm=TRUE)
occ.wide$ForamenMagnumT <- rowMeans(occ.wide[ , c(19,20,21)], na.rm=TRUE)

#create final dataset for analysis
occ <- dplyr::select(occ.wide, Group, Site, IID, Age, Sex, Sexn,
                     DepthEOP, LambdaInion, BicondylarBreadth, ForamenMagnumS, ForamenMagnumT,
                     GeneralForm, NuchalCrest, NuchalLine)
rm(occ.wide)

#remove NaNs
occ <- occ %>% mutate(DepthEOP=str_replace(DepthEOP, "NaN", ""))
occ <- occ %>% mutate(LambdaInion=str_replace(LambdaInion, "NaN", ""))
occ <- occ %>% mutate(BicondylarBreadth=str_replace(BicondylarBreadth, "NaN", ""))
occ <- occ %>% mutate(ForamenMagnumS=str_replace(ForamenMagnumS, "NaN", ""))
occ <- occ %>% mutate(ForamenMagnumT=str_replace(ForamenMagnumT, "NaN", ""))

#Create site name data
occ <- occ %>% mutate(Collection = case_when(
    Site == "8BR246"  ~ "Windover",
    Site == "8MT37"  ~ "Hutchinson",
    Site == "8WA108"  ~ "Military",
    Site == "FSUTC"  ~ "FSUTC"))

#Create Subsistence variable name data
occ <- occ %>% mutate(Subsistence = case_when(
    Group == "Archaic"  ~ "Hunter_Gatherer",
    Group == "Historic"  ~ "Agriculture",
    Group == "Study"  ~ "Agriculture"))

occ <- dplyr::select(occ, IID, Subsistence, Collection, Sex, ForamenMagnumS,
                     ForamenMagnumT, BicondylarBreadth,
                     DepthEOP, LambdaInion,
                     GeneralForm, NuchalCrest, NuchalLine)
#save data
write.csv(occ, "data-occ-original.csv", quote=FALSE, row.names = FALSE)

#tidy
rm(list = ls())
gc()

