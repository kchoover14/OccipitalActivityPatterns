library(tidyverse); library(psych)

#read clean data from Florida sites
occ.orig <- read.csv("data-occ-original.csv", stringsAsFactors = TRUE)

#covariance
png("SF1-splom by subsistence-original data.png", width = 12, height=12, units="in", pointsize=15,
    res=300, family="sans", type="cairo")
pairs.panels(occ.orig[,8:12],lm=TRUE, cor=TRUE,
             method = "pearson",
             bg = c("black", "yellow")[occ.orig$Subsistence],
             pch=21, stars = TRUE)
dev.off()

png("SF2-splom by sex-original data.png", width = 12, height=12, units="in", pointsize=15,
    res=300, family="sans", type="cairo")
pairs.panels(occ.orig[,8:12],lm=TRUE, cor=TRUE,
             method = "pearson",
             bg = c("black", "yellow")[occ.orig$Sex],
             pch=21, stars = TRUE)
dev.off()

#after running once, general form and eop overlap
#rename column to add space to avoid overlap--can't jitter
occ.orig <- dplyr::rename(occ.orig, `                     Depth_EOP` = `DepthEOP`)
occ.orig <- dplyr::rename(occ.orig, `Form &            ` = `GeneralForm`)
occ.orig <- dplyr::rename(occ.orig, Crest = NuchalCrest)
occ.orig <- dplyr::rename(occ.orig, Line = NuchalLine)
#calculate principal components
occ.orig.pca <- prcomp(na.omit(occ.orig[, 8:12]), scale = TRUE)
#reverse the signs
occ.orig.pca$rotation <- -1*occ.orig.pca$rotation
#display principal components
occ.orig.pca$rotation

#variance explained
occ.orig.pca$sdev^2 / sum(occ.orig.pca$sdev^2) * 100


#visualize results
png("F6-pca biplot.png", width = 14, height=14, units="in", pointsize=20,
    res=300, family="sans", type="cairo")
biplot(occ.orig.pca,
       scale = 1,
       col=c("black", "blue"),
       #expand= 1,
       xlim=c(-.3,.37),
       #ylim=c(-2,3),
       xlabs = rep("*",98),
       cex=c(.5,.75))
abline(h=0, col="red", lty=4)
abline(v=0, col="red", lty=4)
dev.off()

#tidy
rm(list = ls())
dev.off()
gc()
