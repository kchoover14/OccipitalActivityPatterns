library(tidyverse); library(car)

#read clean data from Florida sites
occ.orig <- read.csv("data-occ-original.csv", stringsAsFactors = TRUE)

#create Index for fm area
occ.orig$Index_FM <- pi * (0.5 * occ.orig$ForamenMagnumT) * (0.5 * occ.orig$ForamenMagnumS)
#create index for occipital
occ.orig$Index_Form <- occ.orig$DepthEOP + occ.orig$GeneralForm
#create nonmetric index as sum (larger is more muscled)
occ.orig$Index_Nuchal <- occ.orig$NuchalLine + occ.orig$NuchalCrest

#create analytical dataset and save
occ.orig.index <- dplyr::select(occ.orig, Subsistence, Collection, Sex,
                    BicondylarBreadth, LambdaInion,
                    Index_FM, Index_Form, Index_Nuchal)
write.csv(occ.orig.index, "data-occ-original-index.csv", row.names = F, quote = F)
rm(occ.orig.index, occ.orig)

#read mined data
occ.mined <- read.csv("data-occ-mined.csv", stringsAsFactors = TRUE)

#Create Subsistence variable name data
occ.mined <- occ.mined %>% mutate(Subsistence = case_when(
    Group == "Arikara"  ~ "Horticultural",
    Group == "Study"  ~ "Agriculture"))

#create Index for fm area
occ.mined$Index_FM <- pi * (0.5 * occ.mined$ForamenMagnumT) * (0.5 * occ.mined$ForamenMagnumS)

#save indexed data
occ.mined.index <- dplyr::select(occ.mined, Subsistence, Collection, Sex,
                          BicondylarBreadth, Index_FM)
write.csv(occ.mined.index, "data-occ-mined-index.csv", row.names = F, quote = F)

#tidy
rm(list = ls())
gc()

