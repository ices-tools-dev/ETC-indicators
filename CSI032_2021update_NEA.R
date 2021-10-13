# UPDATE OF THE CSI O32 INDICATOR, for the EEA product
# September 2021
# Authors: Adriana Villamor and David Miller, ICES secretariat

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Clear Workspace
rm(list = ls()) 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(dplyr)
library(tidyr)
library(data.table)
library(icesSAG)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Load workspace with SAG data loaded
# Then can skip lines up to about line 250 
#load("tmpSAG_workspace.RData")   
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# The latest available assessments will be used, although we will only refer to
# year 2019, as official landings are only available until then.

year = 2020

load_sid <- function(year){
        # create url for SID web service
        url <- paste0("http://sd.ices.dk/services/odata4/StockListDWs4?$filter=ActiveYear%20eq%20", year)
        # download json data
        out <- jsonlite::fromJSON(url, simplifyDataFrame = TRUE)$value
        
        unique(out)
}

# We will use the latest available assessments for each stock, from 2016 to 2020
# that way we will make sure to have all the stocks that only provide advice every 5 years
# Load functions to load summary and reference points for those years

load_sag_summary <-  function(year){
        years <- ((year-4):year)
        out <- icesSAG::getSAG(stock = NULL,
                               years,
                               data = "summary",
                               purpose != "Advice",
                               combine = TRUE)
        sid<-load_sid(year)
        sid <-dplyr::filter(sid,!is.na(YearOfLastAssessment))
        # Stocks with Data Category 5 and 6 are not used, as these can't be 
        # considered full assessments (although advice may exist)
        
        #sid <- subset(sid, !(DataCategory %in% c("6.2", "5.2", "6.3", "5.9", "5", "6.9", "6")))
        # sid <- subset(sid, DataCategory <=5)
        #DM : I've changed my mind. I think we should load all SAG stocks. 
        #This would be better for the catch estimation, and should work fine with the new way of getting stock status
        # But Category 5 and 6 stocks should not count as assessed unless they have a non-gray stock status value for F or Stock size
        
        sid <- dplyr::select(sid,StockKeyLabel,
                             YearOfLastAssessment, PreviousStockKeyLabel)
        colnames(sid) <- c("fishstock", "AssessmentYear", "PreviousStockKeyLabel")
        
        
        #Fix a couple of mismatches in sid and sag
        # sid$AssessmentYear[sid$fishstock == "cod.21.1a-e"] <- "2017"
        # sid$AssessmentYear[sid$fishstock == "had.27.5a"] <- "2018"
        # sid$AssessmentYear[sid$fishstock == "her.27.5a"] <- "2018"
        # sid$AssessmentYear[sid$fishstock == "reb.2127.dp"] <- "2016"
        # sid$AssessmentYear[sid$fishstock == "reb.2127.sp"] <- "2016"
        # sid$AssessmentYear[sid$fishstock == "cod.27.5a"] <- "2018"
        # out$fishstock[out$fishstock == "ank.27.78ab"] <- "ank.27.78abd"
        # out$fishstock[out$fishstock == "Pil.27.7"] <- "pil.27.7"
        
        
        #Change old codes for assessmenst before 2017
        old <- dplyr::filter(sid, AssessmentYear < 2017)
        out1 <- merge(out, sid, by = c("fishstock", "AssessmentYear"),all = FALSE)
        out2 <- merge(out, old, by.x = c("fishstock", "AssessmentYear"), by.y = c("PreviousStockKeyLabel", "AssessmentYear"),all = FALSE)
        out2$fishstock <- out2$fishstock.y
        out2 <- subset(out2,select = -fishstock.y)
        out <- merge(out1,out2, all = TRUE)
        out <- subset(out,select = -PreviousStockKeyLabel)
        unique(out)
}

load_sag_refpts <- function(year){
        years <- ((year-4):year)
        out <- icesSAG::getSAG(stock = NULL,
                               years ,
                               purpose != "Advice",
                               data = "refpts",
                               combine = TRUE)
        sid<-load_sid(year)
        sid <-dplyr::filter(sid,!is.na(YearOfLastAssessment))
        
        #sid <- subset(sid, !(DataCategory %in% c("6.2", "5.2", "6.3", "5.9", "5", "6.9", "6")))
        # sid <- subset(sid, DataCategory <=5)
        #DM : I've changed my mind. I think we should load all SAG stocks. 
        #This would be better for the catch estimation, and should work fine with the new way of getting stock status
        # But Category 5 and 6 stocks should not count as assessed unless they have a non-gray stock status value for F or Stock size
        
        sid <- dplyr::select(sid,StockKeyLabel,
                             YearOfLastAssessment, PreviousStockKeyLabel)
        colnames(sid) <- c("StockKeyLabel", "AssessmentYear", "PreviousStockKeyLabel")
        
        # sid$AssessmentYear[sid$StockKeyLabel == "cod.21.1a-e"] <- "2017"
        # sid$AssessmentYear[sid$StockKeyLabel == "had.27.5a"] <- "2018"
        # sid$AssessmentYear[sid$StockKeyLabel == "her.27.5a"] <- "2018"
        # sid$AssessmentYear[sid$StockKeyLabel == "reb.2127.dp"] <- "2016"
        # sid$AssessmentYear[sid$StockKeyLabel == "reb.2127.sp"] <- "2016"
        # sid$AssessmentYear[sid$StockKeyLabel == "cod.27.5a"] <- "2018"
        out$StockKeyLabel[out$StockKeyLabel == "ank.27.78ab"] <- "ank.27.78abd"
        out$StockKeyLabel[out$StockKeyLabel == "Pil.27.7"] <- "pil.27.7"
        #DM: why are these two not commented out, but they are commented out in the function above?
        
        old <- dplyr::filter(sid, AssessmentYear < 2017)
        out1 <- merge(out, sid, by = c("StockKeyLabel", "AssessmentYear"),all = FALSE)
        out2 <- merge(out, old, by.x = c("StockKeyLabel", "AssessmentYear"), by.y = c("PreviousStockKeyLabel", "AssessmentYear"),all = FALSE)
        out2$StockKeyLabel <- out2$StockKeyLabel.y
        out2 <- subset(out2,select = -StockKeyLabel.y)
        out <- merge(out1,out2, all = TRUE)
        out <- subset(out,select = -PreviousStockKeyLabel)
        unique(out)
}



summ <- load_sag_summary(year)

# Filter assessments with Purpose = Advice (not InitAdvice, Benchmarks etc)

summ <- dplyr::filter(summ, Purpose == "Advice")

refpts <- load_sag_refpts(year)

# check stocks in summary and ref points 
a <- unique(summ$fishstock)
b <- unique(refpts$StockKeyLabel)
setdiff(b,a)
# should be character(0)


# Load function to format sag
format_sag <- function(x,y){
        df1 <- dplyr::select(x,Year,
                             StockKeyLabel = fishstock,
                             F,
                             SSB,
                             fishingPressureDescription,
                             stockSizeDescription,
                             landings,
                             catches,
                             discards)
        df2 <- dplyr::select(y,StockKeyLabel,
                             AssessmentYear,
                             Flim = FLim,
                             Fpa,
                             Bpa,
                             Blim,
                             FMSY,
                             MSYBtrigger)
        
        out <- dplyr::left_join(df1,df2)
}

sag_complete <- format_sag(summ, refpts)

unique(sag_complete$StockKeyLabel)

# 257 stocks with assessments between 2018 and 2020, although I asked
# from 2016.

# Load a file with the Ecoregions attributed for this product. In this file, 
# the Ecoregions used in the latest STECF report are also shown, some slight 
# differences exist, but mostly in non-EU waters.

# This file has been checked in 2021, against last years product and also against 
# STECF CFP report

ecoregions <- read.csv("Ecoregions.csv")

# stocks in sag but not in ecoregions doc:
new <- setdiff(sag_complete$StockKeyLabel, ecoregions$StockKeyLabel)
new

# character (0)

# stocks that do not show up in sag but are in the ecoregions file:
out <- setdiff(ecoregions$StockKeyLabel, sag_complete$StockKeyLabel)
out

#in ecoregions file but not in SAG: 

# [1] "ghl.27.1-2"   "sal.neac.all" "sal.wgc.all"  "thr.27.nea"  

# we need to import ghl.27.1-2 from SAG 2019 manually

ghl <- icesSAG::getSAG(stock = "ghl.27.1-2",
                       2019,
                       data = "summary")

ghl_ref <- icesSAG::getSAG(stock = "ghl.27.1-2",
                           2019,
                           data = "refpts")

ghl_tot <- format_sag(ghl, ghl_ref)

sag_complete <- rbind(sag_complete, ghl_tot)

out <- setdiff(ecoregions$StockKeyLabel, sag_complete$StockKeyLabel)
out

#ok now

names(ecoregions)

unique(ecoregions$Ecoregion)

# This year we are not using Oceanic Ecoregion, check again with Dave as it is in the MSFD

sag_complete <- left_join(sag_complete,ecoregions, by = "StockKeyLabel")
names(sag_complete)

## Some stocks in good GES in Iceland do not show up because the reference point
## is HR instead of FMSY
sag_complete$FMSY[which(sag_complete$StockKeyLabel == "aru.27.5a14")] <- 0.171

# sag_complete$FMSY[which(sag_complete$StockKeyLabel == "bli.27.5a14")] <- 1.750 
#DM: No RefPts for this stock's last assessment before 2020

sag_complete$FMSY[which(sag_complete$StockKeyLabel == "cod.27.5a")] <- 0.20 
#DM - cod5a reported both F and HR, but uses HRmsy (0.2). But the time series below is F, not HR:
# sag_complete$F[which(sag_complete$StockKeyLabel == "cod.27.5a")]

sag_complete$FMSY[which(sag_complete$StockKeyLabel == "pok.27.5a")] <- 0.20
#DM: same story as for cod5a (F time series, not HR)
# sag_complete$F[which(sag_complete$StockKeyLabel == "pok.27.5a")]

# sag_complete$FMSY[which(sag_complete$StockKeyLabel == "her.27.5a")] <- 0.15
# DM: stock uses Fmsy. They have HRmgt for their management plan (0.15), but for our indices we should use Fmsy and the F time series (both currently correct)

sag_complete$FMSY[which(sag_complete$StockKeyLabel == "lin.27.5a")] <- 0.24
sag_complete$FMSY[which(sag_complete$StockKeyLabel == "usk.27.5a14")] <- 0.17

sag_complete$FMSY[which(sag_complete$StockKeyLabel == "ank.27.78abd")] <- 1




# 2021 update, not needed as these HR are now in F column
#AV: download HR time series instead of F FIX, for cod.27.5a and pok.27.5a, rest are fine

# assessmentKey <- icesSAG::findAssessmentKey(stock = "cod.27.5a", year = 2019, published = TRUE,
#                   regex = TRUE, full = FALSE)
# cod <- icesSAG::getCustomColumns(assessmentKey)
# unique(cod$customName)
# cod <- cod %>% filter(customName == "HR")
# 
# 
# assessmentKey <- icesSAG::findAssessmentKey(stock = "pok.27.5a", year = 2019, published = TRUE,
#                            regex = TRUE, full = FALSE)
# pok <- icesSAG::getCustomColumns(assessmentKey)
# unique(pok$customName)
# pok <- pok %>% filter(customName == "HR")

#need to get custom column 2 for lez.27.6b SSB

assessmentKey <- icesSAG::findAssessmentKey(stock = "lez.27.6b", year = 2020, published = TRUE,
                                            regex = TRUE, full = FALSE)
lez <- icesSAG::getCustomColumns(assessmentKey)
unique(lez$customName)
lez1 <- lez %>% filter(customName == "B/Bmsy")
lez2 <- lez %>% filter(customName == "F/Fmsy")


# sag_complete <- sag_complete %>% mutate(F=replace(F, StockKeyLabel == "cod.27.5a", cod$customValue)) 
# sag_complete <- sag_complete %>% mutate(F=replace(F, StockKeyLabel == "pok.27.5a", pok$customValue))
sag_complete <- sag_complete %>% mutate(SSB=replace(SSB, StockKeyLabel == "lez.27.6b", lez1$customValue))
sag_complete <- sag_complete %>% mutate(F=replace(F, StockKeyLabel == "lez.27.6b", lez2$customValue))

# sag_complete <- sag_complete %>% mutate(fishingPressureDescription=replace(fishingPressureDescription, StockKeyLabel == "cod.27.5a", "Harvest Rate")) 
# sag_complete <- sag_complete %>% mutate(fishingPressureDescription=replace(fishingPressureDescription, StockKeyLabel == "pok.27.5a", "Harvest Rate"))
sag_complete <- sag_complete %>% mutate(stockSizeDescription=replace(stockSizeDescription, StockKeyLabel == "lez.27.6b", "Biomass index"))
sag_complete$MSYBtrigger[which(sag_complete$StockKeyLabel == "lez.27.6b")] <- 0.5

#We use the latest available assessments but only up to the year 2019

sag_complete2 <- sag_complete %>% filter(Year < year)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Save workspace with SAG data loaded
#save(list = ls(all.names = TRUE), file = "tmpSAG_workspace.RData", envir = .GlobalEnv)

#load("tmpSAG_workspace.RData")   
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


########### Figure1 #############
#######################################

# In figure1 the current status of the stocks (as of 2020) is used

# Load function to extract the current status of a formatted sag df

stockstatus_CLD_current <- function(x) {
        df<- dplyr::select(x,Year,
                           StockKeyLabel,
                           AssessmentYear,
                           F,
                           FMSY,
                           SSB,
                           MSYBtrigger,
                           catches,
                           landings,
                           discards,
                           Ecoregion)
        df$F <- as.numeric(df$F)
        df$SSB <- as.numeric(df$SSB)
        df$FMSY <- as.numeric(df$FMSY)
        df$MSYBtrigger <- as.numeric(df$MSYBtrigger)
        df2 <- dplyr::group_by(df,StockKeyLabel)
        df2 <- dplyr::filter(df2,Year == AssessmentYear - 1)
        df2 <- dplyr::mutate(df2,F_FMSY =  ifelse(!is.na(FMSY),
                                                  F / FMSY,
                                                  NA))
        df2 <- dplyr::select(df2,StockKeyLabel,
                             F_FMSY,
                             catches,
                             landings,
                             discards,
                             FMSY,
                             F, 
                             Ecoregion)
        df3 <- dplyr::group_by(df,StockKeyLabel, AssessmentYear)
        df3 <- dplyr::filter(df3, Year %in% c(AssessmentYear, (AssessmentYear - 1)))
        df3 <- dplyr::mutate(df3, SSB_MSYBtrigger = ifelse(!is.na(MSYBtrigger),
                                                           SSB / MSYBtrigger,
                                                           NA))
        df3 <- dplyr::select(df3, StockKeyLabel,Year,
                             SSB_MSYBtrigger,
                             SSB,
                             MSYBtrigger)
        check <- unique(df3[c("StockKeyLabel", "Year", "MSYBtrigger")])
        check <- check[order(-check$Year),]
        check2 <- check[duplicated(check$StockKeyLabel),]
        df3 <- anti_join(df3,check2)
        df4 <- dplyr::full_join(df2, df3)
        df4
}

current <- stockstatus_CLD_current(sag_complete2)


# In figure 1, GREEN means landings of assessed stocks with info for F and SSB 
# reference points, ORANGE means landings of assessed stocks with info for only
# one of the two reference points, and RED means landings of assessed stocks for
# which no info is available on any reference point. 

current$color_fig1 <- case_when(current$F_FMSY != "NA" & current$SSB_MSYBtrigger != "NA" ~ "GREEN",
                                is.na(current$F_FMSY) & is.na(current$SSB_MSYBtrigger) ~"RED",
                                TRUE ~ "ORANGE")
current <- unique (current)

#If there are no landings but catches, use catches
current <- transform(current, landings2 = ifelse(!is.na(landings), landings, catches))
current$landings2[is.na(current$landings2)] <- "0"
current$landings2 <- as.numeric(current$landings2)


# HERE I will manually add some catches that are either in custom columns in SAG,
# or in the advice sheet.
current$landings2[which(current$StockKeyLabel == "pol.27.67")] <- 2891
current$landings2[which(current$StockKeyLabel == "raj.27.3a47d")] <- 1157.581
current$landings2[which(current$StockKeyLabel == "sal.27.22-31")] <- 1158
current$landings2[which(current$StockKeyLabel == "sal.27.32")] <- 66.9
current$landings2[which(current$StockKeyLabel == "trs.27.22-32")] <- 311
# THIS ONE COMES FROM OFFICIAL LANDINGS
current$landings2[which(current$StockKeyLabel == "ele.2737.nea")] <- 587.496


# HERE I have to remove from current df, stocks in higher categories with no assessment.
sid <- load_sid(year)
sid <-dplyr::filter(sid,!is.na(YearOfLastAssessment))
sid <- subset(sid, !(DataCategory %in% c("6.2", "5.2", "6.3", "5.9", "5", "6.9", "6")))
unique(sid$StockKeyLabel)
# 198 
unique(sid$DataCategory)

current2 <- current %>% filter(StockKeyLabel %in% sid$StockKeyLabel)
unique(current2$StockKeyLabel)
#194 stocks in current2, that seems about right

figure1 <- current2 %>%
  group_by(Ecoregion, color_fig1) %>% 
  summarise(landings = sum(landings2)) %>%
  ungroup() %>%
  spread(color_fig1, landings, fill=0)


# This year total catches will be the sum of:
# SAG catches for ICES stocks (as in nominal catches discards are not taken into account) +
# Nominal catches for all others species and areas

##Load ICES official catches
# still unpublished, so I will use a local copy
 
# catchURL <- "http://ices.dk/data/Documents/CatchStats/OfficialNominalCatches.zip"
# tmpFileCatch <- tempfile(fileext = ".zip")
# download.file(catchURL, destfile = tmpFileCatch, mode = "wb", quiet = TRUE)
# ices_catch_official_raw <- read.csv(unz(tmpFileCatch,
#                                         grep("ICESCatchDataset.*.csv", unzip(tmpFileCatch,
#                                                                              list = TRUE)$Name,
#                                              value = TRUE)),
#                                     stringsAsFactors = FALSE,
#                                     header = TRUE,
#                                     fill = TRUE)


# catch_dat <- ices_catch_official_raw
catch_dat <- read.csv("NominalCatches_2006-2019_01082021.csv")
names(catch_dat)

#Dave, what to do about this now,
# should we do average of previous 3 years? it would be 2 years of data 
# in many cases, right?

# we will approximate the confidential catches with the previous three years average


str(catch_dat)

# catch_dat$X2018 <- as.numeric(catch_dat$X2018)
# catch_dat$X2019 <- as.numeric(catch_dat$X2019)
catch_dat$new <- rowMeans(subset(catch_dat, select = c(X2017,X2016, X2015)))
catch_dat_2019 <- subset(catch_dat, select= c("Species","Area","Country", "X2019", "new"))
# catch_dat_2018 <- subset(catch_dat, select= c("Species","Area","Country", "X2018", "new"))

sub <- catch_dat_2019 %>% filter(X2019 == "c")
sub$X2019 <- sub$new
sub <- sub[,-5]
catch_dat_2019 <- catch_dat_2019 %>% filter(X2019 != "c")
catch_dat_2019 <- catch_dat_2019[,-5]
catch_dat_2019 <- rbind(catch_dat_2019, sub)


# this file enumerates all areas for each stock, so we can filter catch_dat with it.
# Upadted in 2021. It is extracted from RECO with a query every year to account for new stocks.
catch_areas <- read.csv("ICESStockWithArea.csv")
names(catch_areas)

#Extract species code of the stock code
#DM: I am a little worried here that some of our stocks include more than one species code (e.g. sol.27.9 I think is a mix of sole species), or may not match up with official codes (e.g. flounder in the Baltic until this year)
catch_areas$Species <- substr(catch_areas$Code, start = 1, stop = 3)
catch_areas$Species <- toupper(catch_areas$Species)
unique(catch_areas$Species)

catch_areas <- catch_areas[,c(2,4,8)]
colnames(catch_areas) <- c("StockKeyLabel", "Area", "Species")

# only want areas of the stocks we have in sag_complete2
catch_areas <- catch_areas %>% filter(StockKeyLabel %in% sag_complete2$StockKeyLabel)
unique(catch_areas$StockKeyLabel)

#257

#how to deal with _NK catches? will infer the lower _NK for each area, and add them
# to the catch_areas dataframe

catch_areas_nk <- catch_areas 

#remove last characters up to the point included 
catch_areas_nk$Area <- sub(".[^.]+$", "", catch_areas_nk$Area)
# paste _NK in the same place
catch_areas_nk$Area_nk <- paste0(catch_areas_nk$Area, "_NK")

catch_areas_nk <- catch_areas_nk[, -2]
# check <- cbind(catch_areas, catch_areas_nk)
# check <- check[,c(1,2,6)]
#have a look to check
# Looks fine
catch_areas_nk <- catch_areas_nk %>% 
        rename(Area = Area_nk)

# This df has all areas and corresponding _NK in the immediate lower aggregation level
catch_areas <- rbind(catch_areas, catch_areas_nk)
catch_areas <- unique(catch_areas)

# I should remove catches for those areas and species from the 2018 nominal catches.

catch_dat_2019_2 <- anti_join(catch_dat_2019, catch_areas, by=c("Area", "Species")) 

# catch_2 <- left_join(catch_areas, catch_dat_2018)

# catch_2 should include only catches in the relevant areas (_NK included) for the relevant species,
# will double check in the light of the results, or also with Dave´s help.

# catch_2$X2018 <- as.numeric(catch_2$X2018)
# catch_2 <- catch_2 %>% filter(X2018 >0)
# catch_tot <- catch_2 %>% group_by(StockKeyLabel) %>%  summarise(Value=sum(X2018))
# 
# current <- left_join(current, catch_tot)

# Let's have a look to the landings in SAG vs the catches in official data

#DAVE, please have a look to this df
# check <- current[,c(1,3,4,15)]
# write.csv(check, file="D:\\EEA-ETC\\EEA-ETC 2020\\Indicators 2020\\Catch data\\checkCatches.csv")

unique(catch_dat_2019_2$Area)

catch_dat_2019_2 <- catch_dat_2019_2 %>%
        mutate(Ecoregion = case_when(
                .$Area %in% c("27.3.b.23", "27.3.c.22","27.3.d.24", "27.3.d.25", "27.3.d.26","27.3.d.27",
                              "27.3.d.28.1","27.3.d.28.2","27.3.d.30","27.3.d.31","27.3.d.32","27.3_NK") ~ "Baltic Sea",
                .$Area %in% c("27.3.a.20","27.3.a.21", "27.4.a", "27.4.b","27.4.c","27.7.d") ~ "Greater North Sea",
                
                .$Area %in% c("27.8.a", "27.8.b","27.8.c",
                              "27.8.d.2", "27.8.e.2", "27.9.a",
                              "27.9.b.2") ~ "BoBiscay & Iberia",
                .$Area %in% c("27.6.a", "27.6.b.2","27.7.a", "27.7.b", "27.7.c.2",
                              "27.7.f", "27.7.g", "27.7.h","27.7.j.2", "27.7.k.2") ~ "Celtic Seas",
                
                .$Area %in% c("27.5.a.1","27.5.a.2", "27.5.b.1.a","27.5.b.1.b", "27.5.b.2", "27.12.a.2","27.12.a.4", "27.14.b.2", "27.12.a.3", "27.14.a" ) ~ "Iceland, Greenland and Faroes",
                
                .$Area %in% c("27.1.a", "27.1.b", "27.2.a.1", "27.2.a.2", "27.2.b.1", "27.2.b.2") ~ "Arctic Ocean",
                .$Area %in% c("27.10.a.2") ~ "Azores",
                .$Area %in% c("27.10.a.1", "27.10.b", "27.12.c", "27.12.a.1", "27.14.b.1", "27.12.b",
                              "27.6.b.1", "27.7.c.1", "27.7.k.1", "27.8.e.1", "27.8.d.1", "27.9.b.1") ~ "Widely",
                TRUE ~ "OTHER"))




# The shadowed area in Figure 1 represents landings of unassessed stocks

catch_dat_2019_2 <- catch_dat_2019_2 %>% filter(Ecoregion != "OTHER")
catch_dat_2019_2$X2019 <- as.numeric(catch_dat_2019_2$X2019)
catch_dat_2019_2 <- catch_dat_2019_2[complete.cases(catch_dat_2019_2), ]
catch <- catch_dat_2019_2 %>%
        group_by(Ecoregion) %>% 
        summarise(Catch = sum(X2019))


sag_catch <- current %>%
  group_by(Ecoregion) %>% 
  summarise(Catch = sum(landings2))

#Add up both dataframes

catch_figure1 <- bind_rows(catch, sag_catch)%>%group_by(Ecoregion) %>% summarise_all(sum)


unique(figure1$Ecoregion)
unique(catch_figure1$Ecoregion)

figure1 <- merge(figure1, catch_figure1, all = TRUE)

write.csv(figure1, file = "CSI032_figure1NEA_update2021_13oct.csv")


# Figure2 wont be produced in 2021 update

########### Figure 3 #############
##################################

# In Figure 3 is represented the trends on F/ FMSY and SSB/ MSYBtrigger for the
# available time-series.
# A mean accross all ecoregions is shown. We also propose the trends separated
# by ecoregion, as done for the Mediterranean and Black Sea.

# Only use category 1 and 2 stocks
# unique(sid$DataCategory)
sid <- load_sid(year)
cat1234 <- sid %>% filter(DataCategory %in% c("1", "2", "1.2", "1.8", "1.6", "1.7",
                                            "3.2", "3", "3.3", "4.14", "3.9",
                                            "3.14", "4.12", "2.13", "3.8", "4"))
sag_fig3 <- sag_complete2 %>% filter(StockKeyLabel %in% cat1234$StockKeyLabel)

sag_fig3$FMSY <- as.numeric(sag_fig3$FMSY)
sag_fig3$F <- as.numeric(sag_fig3$F)
sag_fig3$MSYBtrigger <- as.numeric(sag_fig3$MSYBtrigger)
sag_fig3$SSB <- as.numeric(sag_fig3$SSB)
df <- dplyr::mutate(sag_fig3,F_FMSY = ifelse(!is.na(FMSY),
                                       F / FMSY, NA),
                    SSB_MSYBtrigger = ifelse(!is.na(MSYBtrigger),
                                             SSB / MSYBtrigger, NA))
df<- dplyr::select(df,Year,
                   StockKeyLabel,
                   Ecoregion,
                   F_FMSY,
                   SSB_MSYBtrigger) 
df2 <-tidyr::gather(df,Metric, Value, -Year, -Ecoregion, -StockKeyLabel) 
df2 <- df2[complete.cases(df2),]
unique(df2$Ecoregion)

# Wont use Arctic Ocean and Iceland, Greenland and Faroes for the mean fo Figure 3        
df2 <- df2 %>% filter(Ecoregion %in% c("BoBiscay & Iberia","Widely","Celtic Seas", "Baltic Sea", "Greater North Sea")) 

df3 <- dplyr::group_by(df2,Metric, Year) %>%
        mutate(Max = max(Value), Min = min(Value))
        
df4 <- dplyr::group_by(df3,Metric, Year, Min, Max)%>%
        summarize(MEAN = mean(Value, na.rm = TRUE))
                
#Put back to short format

fmsy <- df4 %>%filter(Metric == "F_FMSY")
names(fmsy)
fmsy <- fmsy[,-1]
colnames(fmsy) <- c("Year", "Min_F/FMSY", "Max_F/FMSY", "MEAN_F/FMSY")

ssb <- df4 %>%filter(Metric == "SSB_MSYBtrigger")
names(ssb)
ssb <- ssb[,-1]
colnames(ssb) <- c("Year", "Min_SSB/MSYBtrigger", "Max_SSB/MSYBtrigger", "MEAN_SSB/MSYBtrigger")

#Number of assessed stocks by year

DT <- data.table(df3)

stks <- DT[, .(number_of_assessed_stocks = length(unique(StockKeyLabel))), by = Year]
        
figure3 <- ssb %>% left_join(fmsy)        
figure3 <- figure3 %>% left_join(stks)

# Remove the only stock with biomass data from 1905 to 1945, dgs.27.nea
figure3 <- figure3 %>% filter(Year > 1946)
figure3 <- figure3 %>% filter(Year < 2020)

write.csv(figure3, file = "CSI032_figure3NEA_update2021_13oct.csv")


# In last year figure 3 there was "her.27.3031" which is not here this year,
# this year there is no "ank.27.78abd" "hke.27.8c9a"  "lez.27.6b"    "sol.27.7h-k"  "whg.27.6a" 




# Not ready yet
#Figure 3 by Ecoregion, like in the Mediterranean, still have to check it.
##########

df <- dplyr::mutate(sag_fig3,F_FMSY = ifelse(!is.na(FMSY),
                                                 F / FMSY,
                                                 NA),
                    SSB_MSYBtrigger = ifelse(!is.na(MSYBtrigger),
                                             SSB / MSYBtrigger,
                                             NA))
df<- dplyr::select(df,Year,
                   StockKeyLabel,
                   Ecoregion,
                   F_FMSY,
                   SSB_MSYBtrigger) 
df2 <-tidyr::gather(df,Metric, Value, -Ecoregion, -Year,-StockKeyLabel) 
df2 <- df2[complete.cases(df2),]

df3 <- dplyr::group_by(df2,Metric, Year, Ecoregion) %>%
        mutate(Max = max(Value), Min = min(Value))

df4 <- dplyr::group_by(df3,Metric, Year, Ecoregion, Min, Max)%>%
        summarize(MEAN = mean(Value, na.rm = TRUE))

#Put back to short format, 

fmsy <- df4 %>%filter(Metric == "F_FMSY")
names(fmsy)
fmsy <- fmsy[,-1]
colnames(fmsy) <- c("Year", "Ecoregion", "Min_F/FMSY", "Max_F/FMSY", "MEAN_F/FMSY")

ssb <- df4 %>%filter(Metric == "SSB_MSYBtrigger")
names(ssb)
ssb <- ssb[,-1]
colnames(ssb) <- c("Year", "Ecoregion", "Min_SSB/MSYBtrigger", "Max_SSB/MSYBtrigger", "MEAN_SSB/MSYBtrigger")

#Number of assessed stocks by year


DT <- data.table(df3)
stcks <- DT %>% group_by(Ecoregion, Year)%>%summarise(number_of_assessed_stocks = length(unique(StockKeyLabel)))

# stks <- DT[, .(number_of_assessed_stocks = length(unique(StockKeyLabel))), by = Year]

figure3 <- ssb %>% left_join(fmsy)        
figure3 <- merge(figure3,stcks, all = TRUE)
# Remove the only stock with biomass data from 1905 to 1945, dgs.27.nea

figure3 <- figure3 %>% filter(Year > 1945)

write.csv(figure3, file = "CSI032_figure3byecoregionNEA_update2020.csv")


