# This script outlines the analysis process we used to
# compare the quality and coverage of NETS v. InfoGroup
# employment data. Because the data is proprietary, it
# will not be publicly available.

# SECTION 1: SUMMATIONS OF EMPLOYMENT COUNTS BY GEOGRAPHIC LEVEL
# SECTION 2: COMPARISON OF NETS and INFOGROUP EMPLOYMENT COUNTS TO LEHD
# SECTION 3: IDENTIFY DUPLICATE RECORDS (NAME AND ADDRESS)
# 3.1: DOES THE BUSINESS EXIST?
# 3.2: ADDRESS QUALITY: IS THE STREET NAMING CONVENTION CONSISTENT?
# 3.3: DUPLICATE ADDRESSES: DO MULTIPLE BUSINESSES SHARE THE SAME ADDRESS AND SUITE?
# SECTION 4: LARGE EMPLOYERS
# SECTION 5: ONE-TO-ONE COMPARISON OF INFOGROUP SAMPLE TO NETS DATA
# 5.1: PREPARE DATA FOR EXPORT AND MANUAL MATCHING
# 5.2: SUMMARIZE DIFFERENCE IN EMPLOYMENT FOR MATCHED SAMPLES
# SECTION 6: COMPOSITION OF ESTABLISHMENTS BY DATA SOURCE AND NUMBER EMPLOYED

# Load necessary packages
pack <- function(pkg){
  newpkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(newpkg)) 
    install.packages(newpkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("foreign", "tidycensus", "tidyverse", "tigris",
              "rgdal", "stringr", "BAMMtools", "sf", "ggplot2",
              "RColorBrewer", "scales")
pack(packages)

# Load data
rm(list=ls())
setwd("D:/alarson/EmploymentDataEval")
ig <- read.csv("InfoGroupFull.csv")
ne <- read.csv("NETSFull.csv")
nets <- readOGR(".", "NETS_Consho")
info <- readOGR(".", "InfoGroup_Consho")

# # Extract small sample dataset for evaluation
# # Do this only once for the sake of consistency
# smheadig <- ig[sample(1:nrow(ig), 143, replace = FALSE),]
# write.csv(smheadig, "infoGroupSampleDuplicates.csv", row.names = FALSE)

# SECTION 1: SUMMATIONS OF EMPLOYMENT COUNTS BY GEOGRAPHIC LEVEL
# Ensure that samples are in same spatial projection
proj4string(nets); proj4string(info)
info <- spTransform(info, proj4string(nets))
# Download block, block group, and tract shapefiles
# For the study area
bl <- blocks(42, 91); bl <- spTransform(bl, proj4string(nets))
bg <- block_groups(42, 91); bg <- spTransform(bg, proj4string(nets))
trct <- tracts(42, 91); trct <- spTransform(trct, proj4string(nets))
# NETS: Point in polygon operations
# Block
netsBl <- over(nets,as(bl, "SpatialPolygons"))
netsBl2 <- over(nets, bl)
netsBl2 <- netsBl2[c("GEOID10")]
# Block group
netsBg <- over(nets,as(bg, "SpatialPolygons"))
netsBg2 <- over(nets, bg)
netsBg2 <- netsBg2[c("GEOID")]
# Tract
netsTrct <- over(nets,as(trct, "SpatialPolygons"))
netsTrct2 <- over(nets, trct)
netsTrct2 <- netsTrct2[c("GEOID")]
# Merge employment counts with GEOIDs
netsBlMerge <- cbind(as.data.frame(nets), netsBl, netsBl2)
netsBgMerge <- cbind(as.data.frame(nets), netsBg, netsBg2)
netsTrctMerge <- cbind(as.data.frame(nets), netsTrct, netsTrct2)
# Aggregate employment counts by GEOID
netsBlTot <- aggregate(netsBlMerge$STUDY_EMP1, list(netsBlMerge$GEOID10), sum)
netsBgTot <- aggregate(netsBgMerge$STUDY_EMP1, list(netsBgMerge$GEOID), sum)
netsTrctTot <- aggregate(netsTrctMerge$STUDY_EMP1, list(netsTrctMerge$GEOID), sum)
# InfoGroup: Point in polygon operations
# Block
infoBl <- over(info,as(bl, "SpatialPolygons"))
infoBl2 <- over(info, bl)
infoBl2 <- infoBl2[c("GEOID10")]
# Block group
infoBg <- over(info,as(bg, "SpatialPolygons"))
infoBg2 <- over(info, bg)
infoBg2 <- infoBg2[c("GEOID")]
# Tract
infoTrct <- over(info,as(trct, "SpatialPolygons"))
infoTrct2 <- over(info, trct)
infoTrct2 <- infoTrct2[c("GEOID")]
# Merge employment counts with GEOIDs
infoBlMerge <- cbind(as.data.frame(info), infoBl, infoBl2)
infoBgMerge <- cbind(as.data.frame(info), infoBg, infoBg2)
infoTrctMerge <- cbind(as.data.frame(info), infoTrct, infoTrct2)
# Aggregate employment counts by GEOID
infoBlTot <- aggregate(infoBlMerge$ACTUAL_LOC, list(infoBlMerge$GEOID10), sum)
infoBgTot <- aggregate(infoBgMerge$ACTUAL_LOC, list(infoBgMerge$GEOID), sum)
infoTrctTot <- aggregate(infoTrctMerge$ACTUAL_LOC, list(infoTrctMerge$GEOID), sum)
# So that they're comparable, match up NETS and InfoGroup block groups
colnames(netsBlTot)[2] <- c("nets")
allBlEmp <- merge(netsBlTot, infoBlTot, by = "Group.1", all = TRUE)
allBlEmp[is.na(allBlEmp)] <- 0
# write.csv(allBlEmp, file = "testBlocks.csv", row.names = FALSE)

# SECTION 2: COMPARISON OF NETS and INFOGROUP EMPLOYMENT COUNTS TO LEHD
# jobsPA <- "https://lehd.ces.census.gov/data/lodes/LODES7/pa/wac/pa_wac_S000_JT00_2015.csv.gz"
# download.file(jobsPA, "pa_wac_S000_JT00_2015.csv.gz")
jobsPAdf<- read.csv(gzfile("D:/alarson/SEPAPedCounting/data/pa_wac_S000_JT00_2015.csv.gz"))
jobsPAdf <- jobsPAdf[c(1:2)]
jobsPAdf$st <- substr(jobsPAdf$w_geocode, 1, 2)
jobsPAdf$cty <- substr(jobsPAdf$w_geocode, 3, 5)
jobsPAdf$stcty <- paste0(jobsPAdf$st, jobsPAdf$cty)
jobsPAdf <- subset(jobsPAdf, stcty == 42091)
blocksCounts <- read.csv("testBlocks.csv") # import just the blocks from NETS and InfoGroup
blocksCounts <- merge(blocksCounts, jobsPAdf,
                      by.x = "Block.GEOID",
                      by.y = "w_geocode",
                      all.x = TRUE)
blocksCounts <- blocksCounts[c(1:4)]
blocksCounts[is.na(blocksCounts)] <- 0
# write.csv(blocksCounts, "testBlocks2.csv", row.names = FALSE)
blocksCounts$bg <- substr(blocksCounts$Block.GEOID, 1, 12)
lehdBgAgg <- aggregate(blocksCounts$C000, list(blocksCounts$bg), FUN = sum)
blocksCounts$trct <- substr(blocksCounts$Block.GEOID, 1, 11)
lehdTrctAgg <- aggregate(blocksCounts$C000, list(blocksCounts$trct), FUN = sum)

# SECTION 3: IDENTIFY DUPLICATE RECORDS (NAME AND ADDRESS)
samp <- read.csv("infoGroupSampleDuplicates.csv")
duplicateNamedBusiness <- subset(samp, OBJECTID %in% c(1,104))
# 3.1: DOES THE BUSINESS EXIST?
# Export data for manual Google search of all businesses.
# We created a field existsAtThisLocation.
# 2 = Definitely, the business has an online presence
# 1 = Maybe, the business might have a Manta listing but nothing else
# 0 = No, the business is permanently closed
# write.csv(as.data.frame(samp$COMPANY_NAME), file = "infoGroupSampleExists.csv", row.names = FALSE)
companyName <- read.csv("infoGroupSampleExists.csv")
print(as.data.frame(table(companyName$existsAtThisLocation)))
# 3.2: ADDRESS QUALITY: IS THE STREET NAMING CONVENTION CONSISTENT?
streetNames <- as.data.frame(gsub("^\\d+", "", samp$PRIMARY_ADDRESS))
# write.csv(streetNames, file = "infoGroupSampleStreetNames.csv", row.names = FALSE)
# 3.3: DUPLICATE ADDRESSES: DO MULTIPLE BUSINESSES SHARE THE SAME ADDRESS AND SUITE? 
# We manually trawled through the sample looking for duplicate addresses AND suite numbers
duplicateAddresses <- subset(samp, PRIMARY_ADDRESS %in%
                               c("200 BARR HARBOR DR # 400", "18 ELIZABETH ST # 400"))
# write.csv(duplicateAddresses, file = "infoGroupDuplicateAddress.csv", row.names = FALSE)

# SECTION 4: LARGE EMPLOYERS
# 4.1: WHO ARE THEY, AND HOW DO THEY DIFFER?
igLarge <- subset(ig, ACTUAL_LOCATION_EMPLOYMENT_SIZE > 50)
neLarge <- subset(ne, STUDY_EMP13 > 50)
igKeepers <- c("COMPANY_NAME", "PRIMARY_ADDRESS", "PRIMARY_CITY",
               "PRIMARY_STATE", "PRIMARY_ZIP_CODE", "LATITUDE",
               "LONGITUDE", "NAICS_CODE", "ACTUAL_LOCATION_EMPLOYMENT_SIZE")
igLarge <- igLarge[igKeepers]
neKeepers <- c("COMPANY", "ADDRESS", "CITY", "STATE",
               "ZIPCODE", "POINT_Y", "POINT_X",  "NAICS_6DIG", "STUDY_EMP13")
neLarge <- neLarge[neKeepers]
colnames(igLarge) <- c("COMPANY", "ADDRESS", "CITY", "STATE",
                       "ZIPCODE", "LATITUDE", "LONGITUDE",  "NAICS", "EMPLOYMENT")
colnames(neLarge) <- c("COMPANY", "ADDRESS", "CITY", "STATE",
                       "ZIPCODE", "LATITUDE", "LONGITUDE",  "NAICS", "EMPLOYMENT")
igLarge$SOURCE <- "InfoGroup"; neLarge$SOURCE <- "NETS"
# write.csv(rbind(igLarge, neLarge), file = "largeEmployers.csv", row.names = FALSE)
# 4.2: WHAT ARE THEIR RELATIVE DISTRIBUTIONS OF EMPLOYEES?
# Density plot
lg <- read.csv("largeEmployers.csv")
lgPlot <- ggplot(lg) + geom_density(aes(x = EMPLOYMENT, fill = SOURCE), color = "gray", alpha = 0.4) +
  labs(x = "Number of Employees Recorded at Establishment",
       y = "Density of Observations",
       title = "Distribution of Employment for Large Employers") +
  theme_minimal() + scale_fill_brewer(palette = "Accent")
# tiff("densityLgEmp.tiff", units = "in", width = 10, height = 6, res = 600, compression = "lzw")
plot(lgPlot)
# dev.off()
# Spiky frequency polygons, ugly but probably less misleading in this case
lgPlot2 <- ggplot(lg) + geom_freqpoly(aes(x = EMPLOYMENT, y = ..density.., color = SOURCE)) +
  labs(x = "Number of Employees Recorded at Establishment",
       y = "Density of Observations",
       title = "Distribution of Employment for Large Employers") +
  theme_minimal() + scale_color_brewer(palette = "Accent")
# tiff("frequencyLgEmp.tiff", units = "in", width = 10, height = 6, res = 600, compression = "lzw")
plot(lgPlot2)
# dev.off()

# SECTION 5: ONE-TO-ONE COMPARISON OF INFOGROUP SAMPLE TO NETS DATA
# Are there systematic differences in employment by establishment?
# 5.1: PREPARE DATA FOR EXPORT AND MANUAL MATCHING
ne$abbrev <- sub(" .*$", "", ne$COMPANY)
samp$abbrev <- sub(" .*$", "", samp$COMPANY_NAME)
ne$newmatch <- NA
for (i in 1:length(samp$OBJECTID)){
  ne$newmatch <- ifelse(ne$abbrev == samp$abbrev[i], samp$OBJECTID[i], ne$newmatch)
}
samp$newmatch <- samp$OBJECTID
neKeepers <- c("COMPANY", "ADDRESS", "CITY",
               "STATE", "ZIPCODE", "POINT_Y",
               "POINT_X",  "NAICS_6DIG", "STUDY_EMP13",
               "newmatch")
neSub <- ne[neKeepers]
igKeepers <- c("COMPANY_NAME", "PRIMARY_ADDRESS", "PRIMARY_CITY",
               "PRIMARY_STATE", "PRIMARY_ZIP_CODE", "LATITUDE",
               "LONGITUDE", "NAICS_CODE", "ACTUAL_LOCATION_EMPLOYMENT_SIZE", "newmatch")
igSub <- samp[igKeepers]
igNames <- c("COMPANY", "ADDRESS", "CITY", "STATE",
             "ZIPCODE", "LATITUDE", "LONGITUDE",  "NAICS", "EMPLOYMENT", "MATCHID")
neNames <- c("COMPANY", "ADDRESS", "CITY", "STATE",
             "ZIPCODE", "LATITUDE", "LONGITUDE",  "NAICS", "EMPLOYMENT", "MATCHID")
igNames <- paste(igNames, "i", sep = "_"); neNames <- paste(neNames, "n", sep = "_")
colnames(igSub) <- igNames
colnames(neSub) <- neNames
# Try to help yourself out by matching the first word of the business name
firstWordMatch <- merge(igSub, neSub, by.x = "MATCHID_i", by.y = "MATCHID_n", all.x = TRUE)
# Will require much manual cleanup
# write.csv(firstWordMatch, file = "DRAFTmatchIGtoNETS.csv", row.names = FALSE)
# Also requires NETS dataset with simplified columns
# write.csv(neSub, file = "NETStenColumn.csv", row.names = FALSE)
# 5.2: SUMMARIZE DIFFERENCE IN EMPLOYMENT FOR MATCHED SAMPLES
# This is checking out the percentage difference in recorded employment for
# A given establishment
matchIGtoNETS <- read.csv("matchIGtoNETS.csv")
matchIGtoNETS$pctDiff <- (abs(matchIGtoNETS$EMPLOYMENT_i - matchIGtoNETS$EMPLOYMENT_n)) / 
  ((matchIGtoNETS$EMPLOYMENT_i + matchIGtoNETS$EMPLOYMENT_n) / 2) * 100
matchIGtoNETS$diff <- matchIGtoNETS$EMPLOYMENT_n - matchIGtoNETS$EMPLOYMENT_i
matchIGtoNETS$diff2 <- NA
matchIGtoNETS$diff2 <- ifelse(matchIGtoNETS$diff > 0, "NETS", matchIGtoNETS$diff2)
matchIGtoNETS$diff2 <- ifelse(matchIGtoNETS$diff < 0, "InfoGroup", matchIGtoNETS$diff2)
matchIGtoNETS$diff2 <- ifelse(matchIGtoNETS$diff == 0, "Same", matchIGtoNETS$diff2)
as.data.frame(table(matchIGtoNETS$diff2))

# SECTION 6: COMPOSITION OF ESTABLISHMENTS BY DATA SOURCE AND NUMBER EMPLOYED
summary(ig$ACTUAL_LOCATION_EMPLOYMENT_SIZE)
# Assume NA values happen when you have a sole proprietor
# So we replace these with ACTUAL_LOCATION_EMPLOYMENT_SIZE = 1
ig$ACTUAL_LOCATION_EMPLOYMENT_SIZE[is.na(ig$ACTUAL_LOCATION_EMPLOYMENT_SIZE)] <- 1
summary(ig$ACTUAL_LOCATION_EMPLOYMENT_SIZE)
summary(ne$STUDY_EMP13)
# Because the sets have such different numbers of employers
# Use a percentage rather than a count graphic
# 0-9, 10-19, 20-29, 30-39. 40-49, 50+
ig$class <- cut(ig$ACTUAL_LOCATION_EMPLOYMENT_SIZE,
                breaks = c(0,10,20,30,40,50,max(ig$ACTUAL_LOCATION_EMPLOYMENT_SIZE)),
                right = FALSE,
                include.lowest = TRUE,
                labels = c("0-9",
                           "10-19",
                           "20-29",
                           "30-39",
                           "40-49",
                           "50+"))
ne$class <- cut(ne$STUDY_EMP13,
                breaks = c(0,10,20,30,40,50,max(ne$STUDY_EMP13)),
                right = FALSE,
                include.lowest = TRUE,
                labels = c("0-9",
                           "10-19",
                           "20-29",
                           "30-39",
                           "40-49",
                           "50+"))
bp1 <- as.data.frame(ig$class)
bp1$counts <- 1
bp1agg <- aggregate(bp1$counts, list(bp1$`ig$class`), FUN = sum)
bp1agg$Type <- "InfoGroup"
bp2 <- as.data.frame(ne$class)
bp2$counts <- 1
bp2agg <- aggregate(bp2$counts, list(bp2$`ne$class`), FUN = sum)
bp2agg$Type <- "NETS"
colnames(bp1agg) <- c("Employment", "Establishments", "Type")
colnames(bp2agg) <- c("Employment", "Establishments", "Type")
bp <- rbind(bp1agg, bp2agg)
bpPlot <- ggplot(bp, aes(x = Type, y = Establishments, fill = Employment)) +
  geom_bar(stat = "identity", position = "fill", alpha = 0.6) + theme_minimal() +
  scale_fill_brewer(palette = "Accent") +
  labs(x = "Data Source", y = "Percent of Establishments",
       title = "Composition of Establishments by Data Source and Number Employed") +
  scale_y_continuous(labels = percent)
# tiff("employmentComposition.tiff", units = "in", width = 8, height = 8, res = 600, compression = "lzw")
plot(bpPlot)
# dev.off()
