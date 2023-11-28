library(tidyverse); library(irr)

occ.wide <- read.csv("data-occ-original-raw-22dec21.csv", stringsAsFactors = TRUE)

#long format data
occ.long <- occ.wide %>%
    gather(key, Value, -Group, -Site, -IID, -Age, -Sex, -Sexn) %>% #gather all var
    extract(key, c("Variable", "Replicate"), "([a-zA-Z]+)(\\d)") #split key into two parts
rm(occ.wide)

#relocate factors together
occ.wide <- occ.long %>% spread(Variable, Value) #spread variables by variables and retain replicate, if needed
rm(occ.long)

occ.wide <- occ.wide %>%
    rename(DepthOP=depthop) %>%
    rename(GeneralForm=genform) %>%
    rename(LambInion=lambinio) %>%
    rename(BicondBreadth=MaxBiCondBrth) %>%
    rename(NuchalCrest=nuchcres) %>%
    rename(NuchalLine=nuchline)

occ.wide <- occ.wide %>%
    relocate(GeneralForm, .after = NuchalLine)

#make factors
occ.wide$Sexn <- as.factor(occ.wide$Sexn)
occ.wide$Replicate <- as.factor(occ.wide$Replicate)
occ.wide$NuchalCrest <- as.factor(occ.wide$NuchalCrest)
occ.wide$NuchalLine <- as.factor(occ.wide$NuchalLine)
occ.wide$GeneralForm <- as.factor(occ.wide$GeneralForm)

#test intraobserver error for factors
crest <- dplyr::select(occ.wide, Replicate, NuchalCrest)
crest <- na.omit(crest)
kappa2(crest) #n 196, 2 raters, kappa and z = 0 p-value = 1

line <- dplyr::select(occ.wide, Replicate, NuchalLine)
line <- na.omit(line)
kappa2(line) #n 196, 2 raters, kappa and z = 0 p-value = 1

form <- dplyr::select(occ.wide, Replicate, GeneralForm)
form <- na.omit(form)
kappa2(form) #n 196, 2 raters, kappa =-value = 1 z=0.277 p-value=0.782

rm(list = ls())
gc()
