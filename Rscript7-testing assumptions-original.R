library(tidyverse); library(EnvStats); library(psych); library(car)

#read indexed original data
occ.orig <- read.csv("data-occ-original-index.csv", stringsAsFactors = TRUE)

#descriptives
substats <- describeBy(occ.orig[4:8], occ.orig$Subsistence)
substats.table <-do.call("rbind",substats)
write.csv(substats.table, file="occ.descstats-SUB-orig.csv", quote = FALSE,
          row.names = FALSE)
rm(substats, substats.table)
sexstats <- describeBy(occ.orig[4:8], occ.orig$Sex)
sexstats.table <-do.call("rbind",sexstats)
write.csv(sexstats.table, file="occ.descstats-SEX-orig.csv", quote = FALSE,
          row.names = FALSE)
rm(sexstats, sexstats.table)

#testing assumptions, normal distribution
png("SF3-normal plot-bcb-original data.png", width = 12, height=8, units="in", pointsize=15,
    res=300, family="sans", type="cairo")
car::qqp(occ.orig$BicondylarBreadth, "norm", grid=FALSE, ylab=paste("Bicondylar Breadth Area"),
         xlab=paste("Normal Distribution Quantiles"))
dev.off()

png("SF4-normal plot-li-original data.png", width = 12, height=8, units="in", pointsize=15,
    res=300, family="sans", type="cairo")
car::qqp(occ.orig$LambdaInion, "norm", grid=FALSE, ylab=paste("Lambda Inion"),
         xlab=paste("Normal Distribution Quantiles"))
dev.off()

png("SF5-normal plot-fm-original data.png", width = 12, height=8, units="in", pointsize=15,
    res=300, family="sans", type="cairo")
car::qqp(occ.orig$Index_FM, "norm", grid=FALSE, ylab=paste("Foramen Magnum Area"),
         xlab=paste("Normal Distribution Quantiles"))
dev.off()

png("SF6-normal plot-form-original data.png", width = 12, height=8, units="in", pointsize=15,
    res=300, family="sans", type="cairo")
car::qqp(occ.orig$Index_Form, "norm", grid=FALSE, ylab=paste("Occipital Morphology"),
         xlab=paste("Normal Distribution Quantiles"))
dev.off()

png("SF7-normal plot-nuchal-original data.png", width = 12, height=8, units="in", pointsize=15,
    res=300, family="sans", type="cairo")
car::qqp(occ.orig$Index_Nuchal, "norm", grid=FALSE, ylab=paste("Nuchal Variation"),
         xlab=paste("Normal Distribution Quantiles"))
dev.off()

#testing assumptions, equality of variances
a <- leveneTest(occ.orig$BicondylarBreadth ~ occ.orig$Subsistence)
b <- leveneTest(occ.orig$BicondylarBreadth ~ occ.orig$Sex)

c <- leveneTest(occ.orig$LambdaInion ~ occ.orig$Subsistence)
d <- leveneTest(occ.orig$LambdaInion ~ occ.orig$Sex)

e <- leveneTest(occ.orig$Index_FM ~ occ.orig$Subsistence)
f <- leveneTest(occ.orig$Index_FM ~ occ.orig$Sex)

g <- leveneTest(occ.orig$Index_Form ~ occ.orig$Subsistence)
h <- leveneTest(occ.orig$Index_Form ~ occ.orig$Sex) #not equal

i <- leveneTest(occ.orig$Index_Nuchal ~ occ.orig$Subsistence)
j <- leveneTest(occ.orig$Index_Nuchal ~ occ.orig$Sex)

#check difference between min and max var for Index_Form
temp.fem <- filter(occ.orig, Sex == "Female", na.rm = TRUE)
temp.male <- filter(occ.orig, Sex == "Male", na.rm = TRUE)
var(temp.fem$Index_Form, na.rm = TRUE) #1.83
var(temp.male$Index_Form, na.rm = TRUE) #6.159
#Female variance for Index FM is 29% the size of male
#could use lm b/c just within the 1/4 cut-off, not quite 4x
#will use white.adjust in Anova
rm(temp.fem, temp.male)

#Levene's Table
levene.desc <- rbind(a, b, c, d, e, f, g, h, i, j)
levene.desc <- data.frame(levene.desc)
rm(a, b, c, d, e, f, g, h, i, j)
head(row.names(levene.desc))
tail(row.names(levene.desc))
row.names(levene.desc)[row.names(levene.desc) == "group"] <- "BicondylarBreadth_Sub"
row.names(levene.desc)[row.names(levene.desc) == ""] <- ""
row.names(levene.desc)[row.names(levene.desc) == "group1"] <- "BicondylarBreadth_Sex"
row.names(levene.desc)[row.names(levene.desc) == "1"] <- ""
row.names(levene.desc)[row.names(levene.desc) == "group2"] <- "LambdaInion_Sub"
row.names(levene.desc)[row.names(levene.desc) == "2"] <- ""
row.names(levene.desc)[row.names(levene.desc) == "group3"] <- "LambdaInion_Sex"
row.names(levene.desc)[row.names(levene.desc) == "3"] <- ""
row.names(levene.desc)[row.names(levene.desc) == "group4"] <- "Index_FM_Sub"
row.names(levene.desc)[row.names(levene.desc) == "4"] <- ""
row.names(levene.desc)[row.names(levene.desc) == "group5"] <- "Index_FM_Sex"
row.names(levene.desc)[row.names(levene.desc) == "5"] <- ""
row.names(levene.desc)[row.names(levene.desc) == "group6"] <- "Index_Form_Sub"
row.names(levene.desc)[row.names(levene.desc) == "6"] <- ""
row.names(levene.desc)[row.names(levene.desc) == "group7"] <- "Index_Form_Sex"
row.names(levene.desc)[row.names(levene.desc) == "7"] <- ""
row.names(levene.desc)[row.names(levene.desc) == "group8"] <- "Index_Nuchal_Sub"
row.names(levene.desc)[row.names(levene.desc) == "8"] <- ""
row.names(levene.desc)[row.names(levene.desc) == "group9"] <- "Index_Nuchal_Sex"
row.names(levene.desc)[row.names(levene.desc) == "9"] <- ""
levene.desc <- round(levene.desc, 4)
write.csv(levene.desc, file="occ.levenes-orig.csv", quote = FALSE,
          row.names = TRUE)

#testing assumptions, outliers
rosnerTest(occ.orig$BicondylarBreadth, k=5, alpha=0.05, warn=TRUE)
rosnerTest(occ.orig$LambdaInion, k=5, alpha=0.05, warn=TRUE)
rosnerTest(occ.orig$Index_FM, k=5, alpha=0.05, warn=TRUE)
rosnerTest(occ.orig$Index_Form, k=5, alpha=0.05, warn=TRUE)
rosnerTest(occ.orig$Index_Nuchal, k=5, alpha=0.05, warn=TRUE)

#tidy
rm(list = ls())
gc()

