# UPDATE OF THE CSI O32 INDICATOR, for the EEA product
# Northeastern Atlantic data come mostly from ICES data bases, 
# although the atribution to Ecoregions has been fine-tuned to better fit those 
# in the 2015 indicator, as well as the Ecoregions used by STECF in the latest 
# report on the CFP indicators update (ref)
# June 2020
# Authors: Adriana Villamor and David Miller, ICES secretariat

library(dplyr)
library(tidyr)
library(data.table)

# The latest available assessments will be used, although we will only refer to
# year 2018, as official landings are only available until then.

year = 2019

load_sid <- function(year){
        # create url for SID web service
        url <- paste0("http://sd.ices.dk/services/odata4/StockListDWs4?$filter=ActiveYear%20eq%20", year)
        # download json data
        out <- jsonlite::fromJSON(url, simplifyDataFrame = TRUE)$value
        
        unique(out)
}

# We will use the latest available assessments for each stock, from 2016 to 2019
#DM: we may need to consider going back 5 years, i.e. 2015-2019 (that way we will make sure to have all teh stocks that only provide advcie every 5 years)
#AV: Done, let´s see how it looks like
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
        
        sid <- subset(sid, !(DataCategory %in% c("6.2", "5.2", "6.3", "5.9", "5", "6.9", "6")))
        #DM : is it possible to say DataCategory >=5 instead? Just in case in future somebody uses one of teh other 5.x or 6.x methods
        # sid <- subset(sid, DataCategory <=5)
        #AV: done
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
        sid <- subset(sid, !(DataCategory %in% c("6.2", "5.2", "6.3", "5.9", "5", "6.9", "6")))
        # sid <- subset(sid, DataCategory <=5)
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

#195 when data category <= 5, why?
#197 stocks, without categories 5 and 6

# Load a file with the Ecoregions attributed for this product. In this file, 
# the Ecoregions used in the latest STECF report are also shown, some slight 
# differences exist, but mostly in non-EU waters.

# This file has been checked in 2020, against last years product and also against 
# STECF CFP report

ecoregions <- read.csv("Ecoregions.csv")

# stocks in sag but not in ecoregions doc:
new <- setdiff(sag_complete$StockKeyLabel, ecoregions$StockKeyLabel)
new
# ecoregions file seems complete

# stocks that do not show up in sag but are in the ecoregions file:
out <- setdiff(ecoregions$StockKeyLabel, sag_complete$StockKeyLabel)
out

#Stocks in categories 5 and 6

names(ecoregions)

unique(ecoregions$Ecoregion)

# This year we are not using Oceanic Ecoregion, check again with Dave as it is in the MSFD

sag_complete <- left_join(sag_complete,ecoregions, by = "StockKeyLabel")
names(sag_complete)

##DAVE 
## Check if this is true this year as well. 
## Some stocks in good GES in Iceland do not show up because the reference point
## is HR instead of FMSY
sag_complete$FMSY[which(sag_complete$StockKeyLabel == "aru.27.5a14")] <- 0.171

# sag_complete$FMSY[which(sag_complete$StockKeyLabel == "bli.27.5a14")] <- 1.750 
#DM: No RefPts for this stock

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

#AV: download HR time series instead of F FIX, for cod.27.5a and pok.27.5a, rest are fine

assessmentKey <- icesSAG::findAssessmentKey(stock = "cod.27.5a", year = 2019, published = TRUE,
                  regex = TRUE, full = FALSE)
cod <- icesSAG::getCustomColumns(assessmentKey)
unique(cod$customName)
cod <- cod %>% filter(customName == "HR")


assessmentKey <- icesSAG::findAssessmentKey(stock = "pok.27.5a", year = 2019, published = TRUE,
                           regex = TRUE, full = FALSE)
pok <- icesSAG::getCustomColumns(assessmentKey)
unique(pok$customName)
pok <- pok %>% filter(customName == "HR")

sag_complete <- sag_complete %>% mutate(F=replace(F, StockKeyLabel == "cod.27.5a", cod$customValue)) 
sag_complete <- sag_complete %>% mutate(F=replace(F, StockKeyLabel == "pok.27.5a", pok$customValue))
sag_complete <- sag_complete %>% mutate(fishingPressureDescription=replace(fishingPressureDescription, StockKeyLabel == "cod.27.5a", "Harvest Rate")) 
sag_complete <- sag_complete %>% mutate(fishingPressureDescription=replace(fishingPressureDescription, StockKeyLabel == "pok.27.5a", "Harvest Rate"))


#We use the latest available assessments but only up to the year 2018

sag_complete2 <- sag_complete %>% filter(Year < 2019)


########### Figure1 and 2 #############
#######################################

# In figure1 and 2 only the current status of the stocks (as of 2018) is used

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


#This year we will use catches from nominal catches instead of the landings in SAG.
# To deal with the issue of Ireland and Latvia confidentiality in the nominal catches in eurostat,
# we will approximate them as the average of the last three years.


##Load ICES official catches
 
catchURL <- "http://ices.dk/data/Documents/CatchStats/OfficialNominalCatches.zip"
tmpFileCatch <- tempfile(fileext = ".zip")
download.file(catchURL, destfile = tmpFileCatch, mode = "wb", quiet = TRUE)
ices_catch_official_raw <- read.csv(unz(tmpFileCatch,
                                        grep("ICESCatchDataset.*.csv", unzip(tmpFileCatch,
                                                                             list = TRUE)$Name,
                                             value = TRUE)),
                                    stringsAsFactors = FALSE,
                                    header = TRUE,
                                    fill = TRUE)


catch_dat <- ices_catch_official_raw
names(catch_dat)

# we will aproximate the confidential catches with the previous three years average

catch_dat$new <- rowMeans(subset(catch_dat, select = c(X2017,X2016,X2015)))


catch_dat_2018 <- subset(catch_dat, select= c("Species","Area","Country", "X2018", "new"))

sub <- catch_dat_2018 %>% filter(X2018 == "c")
sub$X2018 <- sub$new
sub <- sub[,-5]
catch_dat_2018 <- catch_dat_2018 %>% filter(X2018 != "c")
catch_dat_2018 <- catch_dat_2018[,-5]
catch_dat_2018 <- rbind(catch_dat_2018, sub)


# this file enumerates all areas for each stock, so we can filter catch_dat with it.
catch_areas <- read.csv("ICESStockWithArea.csv")
names(catch_areas)

#Extract species code of the stock code
catch_areas$Species <- substr(catch_areas$Code, start = 1, stop = 3)
catch_areas$Species <- toupper(catch_areas$Species)
unique(catch_areas$Species)

catch_areas <- catch_areas[,c(2,4,15)]
colnames(catch_areas) <- c("StockKeyLabel", "Area", "Species")

# only want areas of the stocks we have in sag_complete2
catch_areas <- catch_areas %>% filter(StockKeyLabel %in% sag_complete2$StockKeyLabel)
unique(catch_areas$StockKeyLabel)

#197, perfectooo

#how to deal with _NK catches? will infer the lower _NK for each area, and add them
# to the catch_areas dataframe

catch_areas_nk <- catch_areas 

#remove last characters up to the point included 
catch_areas_nk$Area <- sub(".[^.]+$", "", catch_areas_nk$Area)
# paste _NL in the same place
catch_areas_nk$Area_nk <- paste0(catch_areas_nk$Area, "_NK")

catch_areas_nk <- catch_areas_nk[, -2]
check <- cbind(catch_areas, catch_areas_nk)
check <- check[,c(1,2,6)]
#have a look to check
# Looks fine
catch_areas_nk <- catch_areas_nk %>% 
        rename(Area = Area_nk)

# This df has all areas and corresponding _NK in the inmediate lower aggregation level
catch_areas <- rbind(catch_areas, catch_areas_nk)
catch_areas <- unique(catch_areas)
catch_2 <- left_join(catch_areas, catch_dat_2018)

# catch_2 should include only catches in the relevant areas (_NK included) for the relevant species,
# will double check in the light of the results, or also with Dave´s help.

catch_2$X2018 <- as.numeric(catch_2$X2018)
catch_2 <- catch_2 %>% filter(X2018 >0)
catch_tot <- catch_2 %>% group_by(StockKeyLabel) %>%  summarise(Value=sum(X2018))

current <- left_join(current, catch_tot)

# Let's have a look to the landings in SAG vs the catches in official data

#DAVE, please have a look to this df
check <- current[,c(1,3,4,15)]

# Discrepancies between catches and landings and also between official landings
# and SAG landings. Dave.

# COD in 21 is not reported in official catches
# MEG only reported by IE as confidential, and 3 years average is zero

 
figure1 <- current %>%
        group_by(Ecoregion, color_fig1) %>%
        summarise(landings = sum(Value)) %>%
        ungroup() %>%
        spread(color_fig1, landings, fill=0)



#There might be issues with dplyr and operator, dplyr should be loaded the last, so in the search()
#is first
# Attribute landings by Ecoregions used

# DAVE, I guess that also here I am not taking into account _NK catches, at least in the cases 
# where the aggregation level is low. I will think a bit more about it...

catch_dat_2018 <- catch_dat_2018 %>%
        mutate(ECOREGION = case_when(
                .$Area %in% c("27.3.bc", "27.3.d", "27.3_nk") ~ "Baltic Sea",
                .$Area %in% c("27.3.a", "27.4", "27.7.d") ~ "Greater North Sea",
                
                .$Area %in% c("27.8.a", "27.8.b","27.8.c",
                              "27.8.d.2", "27.8.e.2", "27.9.a",
                              "27.9.b.2") ~ "BoBiscay & Iberia",
                .$Area %in% c("27.6.a", "27.6.b.2","27.7.a", "27.7.b", "27.7.c.2",
                              "27.7.f", "27.7.g", "27.7.h","27.7.j.2", "27.7.k.2") ~ "Celtic Seas",
                
                .$Area %in% c("27.5","27.12.a_NK", "27.14.b.2", "27.12.a.3", "27.14.a" ) ~ "Iceland, Greenland and Faroes",
                
                .$Area %in% c("27.1", "27.2") ~ "Arctic Ocean",
                .$Area %in% c("27.10.a.2") ~ "Azores",
                .$Area %in% c("27.10.a.1", "27.10.b", "27.12.c", "27.12.a.1", "27.14.b.1", "27.12.b",
                              "27.6.b.1", "27.7.c.1", "27.7.k.1", "27.8.e.1", "27.8.d.1", "27.9.b.1") ~ "Widely",
                TRUE ~ "OTHER"))

library(operators)
detach("package:dplyr", unload=TRUE)
library(dplyr)

#Atribute stocks in areas to widely, check with DAVE's help
catch_dat_2018 <- catch_dat_2018 %>%
        mutate(ECOREGION2 = case_when(              
                .$Species %in% c("ARU") & .$Area %in% c("27.7", "27.8", "27.9", "27.10", "27.12", "27.6.b") ~ "Widely",
                #included in bli nea, so out to avoid double counting
                #.$Species %in% c("BLI") & .$Area %in% c("27.8", "27.9", "27.10", "27.12", "27.4") ~ "Widely",
                .$Species %in% c("AGN", "ALF","BLI","BSK","BOC", "BSF","CYO","ELE", "DGS", "GAG", "GFB","GUQ", "MAC", 
                                 "ORY","POR","RHG", "RJA","SAL", "SCK", "SDV","THR","TSU", "WHB")
                & .$Area %in% c("27")  ~ "Widely",
                .$Species %in% c("HER") & .$Area %in% c("27.1", "27.2", "27.5.b", "27.14.a") ~ "Widely",
                .$Species %in% c("HKE") & .$Area %in% c("27.4", "27.6", "27.7", "27.8.a","27.8.b","27.8.d") ~ "Widely",
                .$Species %in% c("HOM") & .$Area %in% c("27.8", "27.2.a", "27.4.a", "27.5.b","27.6.a",
                                                        "27.7.a","27.7.b","27.7.c","27.7.e","27.7.f",
                                                        "27.7.g","27.7.h","27.7.i","27.7.j","27.7.k") ~ "Widely",
                .$Species %in% c("LIN") & .$Area %!in% c("27.1", "27.2", "27.5a", "27.5.b") ~ "Widely",
                .$Species %in% c("RNG") & .$Area %in% c("27.6", "27.7", "27.5b", "27.12.b") ~ "Widely",
                #new
                .$Species %in% c("RNG") & .$Area %in% c("27.1", "27.2", "27.4", "27.8", "27.9", "27.14.a", "27.14.b.2", "27.5.a.2") ~ "Widely",
                .$Species %in% c("USK") & .$Area %in% c("27.4", "27.7", "27.8", "27.9","27.3.a","27.5.b",
                                                        "27.6.a", "27.12.b" ) ~ "Widely",
                #new
                .$Species %in% c("GUR") & .$Area %in% c("27.3", "27.4", "27.5", "27.6", "27.7", "27.8") ~ "Widely")) 



catch_dat_2018 <- catch_dat_2018 %>%
        filter(ECOREGION != "OTHER"| ECOREGION2 == "Widely")


out <- catch_dat_2018%>% filter(Species %in% c("AGN", "ALF","BLI","BSK","BOC", "BSF","CYO","ELE", "DGS", "GAG", "GFB","GUQ", "MAC", 
                                               "ORY","POR","RHG", "RJA","SAL", "SCK", "SDV","THR","TSU", "WHB")) 
out <- out %>% filter(Area != "27")
catch_dat_201x <- anti_join(catch_dat_2018, out)

catch_dat_2018 <- catch_dat_201x
catch_dat_2018 <- transform(catch_dat_2018, Final = ifelse(!is.na(ECOREGION2), "Widely", ECOREGION))

catch_dat_2018 <- catch_dat_2018[, -c(4:5)]
colnames(catch_dat_2018) <- c("Species", "Area", "Value", "Ecoregion")
catch_dat_2018 <- catch_dat_2018 %>% filter(Ecoregion != "OTHER")

detach("package:operators", unload=TRUE)


# The shadowed area in Figure 1 represents landings of unassessed stocks

catch <- catch_dat_2018 %>%
        group_by(Ecoregion) %>% 
        summarise(Catch = sum(Value))


# catch_dat_2018 <- transform(catch_dat_2018, Final = ifelse(!is.na(ECOREGION2), "Widely", ECOREGION))
# 
# catch_dat_2018 <- catch_dat_2018[, -c(4:6)]
# colnames(catch_dat_2018) <- c("Species", "Area", "Value", "Ecoregion")
# catch_dat_2018 <- catch_dat_2018 %>% filter(Ecoregion != "OTHER")
# 
# out <- catch_dat_2018%>% filter(Species %in% c("BOC", "BSF","DGS", "GFB", "MAC", "SDV","WHB")) 
# out <- out %>% filter(Area != "27")
# 
# catch_dat_201x <- anti_join(catch_dat_2018, out)
# 
# check <- catch_dat_201x%>% filter(Species %in% c("BOC", "BSF","DGS", "GFB", "MAC", "SDV","WHB")) 
# 
# catch_dat_2018 <- catch_dat_201x
# 
# detach("package:operators", unload=TRUE)
# 
# 
# catch_dat_2018$Value <- as.numeric(catch_dat_2018$Value)
# 
# catch <- catch_dat_2018 %>%
#         group_by(Ecoregion) %>% 
#         summarise(Catch = sum(Value))

#This will be merged with the color counts of figure1

############# ########
###############################################################

unique(figure1$Ecoregion)
unique(catch$Ecoregion)

figure1 <- merge(figure1, catch, all = TRUE)

write.csv(figure1, file = "CSI032_figure1NEA_update2019.csv")


# In Figure2, only assessed stocks are represented.
# GREEN means both reference points in GES, ORANGE means only one ref point 
# in GES, or in case only one reference point is available, this is in GES.
# RED means both reference points not in GES, or if only one reference point is 
# available, it is not in GES.


## DAVE HERE!!

# For the 2020 update, we will try this figure with stockstatus symbols,
# in order to include more stocks

load_sag_status <- function(year) {
        years <- ((year-4):year)
        out <- do.call("rbind", lapply(years,function(x) icesSAG::findAssessmentKey(stock = NULL,
                                                                                    year = x,
                                                                                    full = TRUE)[, c("AssessmentYear",
                                                                                                     "AssessmentKey",
                                                                                                     "StockKeyLabel", "Purpose")]))
        out <- dplyr::filter(out,Purpose =="Advice")
        out <- out[,-4]
        sid<-load_sid(year)
        sid <-dplyr::filter(sid,!is.na(YearOfLastAssessment))
        sid <- dplyr::select(sid,StockKeyLabel,
                             YearOfLastAssessment, PreviousStockKeyLabel, EcoRegion, AdviceCategory)
        colnames(sid) <- c("StockKeyLabel", "AssessmentYear", "PreviousStockKeyLabel", "Ecoregion", "AdviceCategory")
        old <- dplyr::filter(sid, AssessmentYear < 2017)
        out1 <- merge(out, sid, by = c("StockKeyLabel", "AssessmentYear"),all = FALSE)
        out2 <- merge(out, old, by.x = c("StockKeyLabel", "AssessmentYear"), by.y = c("PreviousStockKeyLabel", "AssessmentYear"),all = TRUE)
        out2$StockKeyLabel <- out2$StockKeyLabel.y
        
        out2 <- subset(out2,select = -StockKeyLabel.y)
        out <- merge(out1,out2, all = TRUE)
        out <- subset(out,select = -PreviousStockKeyLabel)
        out <-out[!duplicated(out$StockKeyLabel),]
        
        get_stock_status <- function(assessmentKey) {
                dat <- icesSAG::getStockStatusValues(assessmentKey)[[1]]
                if(is.null(dat)) stop(paste0("NULL value returned for assessmentKey = ", assessmentKey))
                dat
        }
        out <- dplyr::filter(out, !is.na(out$AssessmentKey))
        out2 <- dplyr::mutate(out, stock_status = purrr::map(.x = AssessmentKey, purrr::possibly(get_stock_status, otherwise = NA_real_)))
        out2 <- dplyr::filter(out2, !is.na(stock_status)) 
        out2 <- dplyr::select(out2, -AssessmentKey)
        out2 <- tidyr::unnest(out2, stock_status)
        out2 <- unique(out2)
        # out3 <- subset(out, !(StockKeyLabel %in% out2$StockKeyLabel))
}


format_sag_status <- function(x) {
        df <- x
        df <- dplyr::mutate(df,status = case_when(status == 0 ~ "UNDEFINED",
                                                  status == 1 ~ "GREEN",
                                                  status == 2 ~ "GREEN", 
                                                  status == 3 ~ "ORANGE",
                                                  status == 4 ~ "RED",
                                                  status == 5 ~ "RED", 
                                                  status == 6 ~ "GREY",
                                                  status == 7 ~ "GREY",
                                                  status == 8 ~ "GREY",
                                                  status == 9 ~ "GREY",
                                                  TRUE ~ "OTHER"),
                            fishingPressure = case_when(fishingPressure == "-" &
                                                                type == "Fishing pressure" ~ "FQual",
                                                        TRUE ~ fishingPressure),
                            stockSize = case_when(stockSize == "-" &
                                                          type == "Stock Size" ~ "SSBQual",
                                                  TRUE ~ stockSize),
                            stockSize = gsub("MSY BT*|MSY Bt*|MSYBT|MSYBt", "MSYBt", stockSize),
                            variable = case_when(type == "Fishing pressure" ~ fishingPressure,
                                                 type == "Stock Size" ~ stockSize,
                                                 TRUE ~ type),
                            variable = case_when(lineDescription == "Management plan" &
                                                         type == "Fishing pressure" ~ "FMGT",
                                                 lineDescription == "Management plan" &
                                                         type == "Stock Size" ~ "SSBMGT",
                                                 TRUE ~ variable),
                            variable = case_when(
                                    grepl("Fpa", variable) ~ "FPA",
                                    grepl("Bpa", variable) ~ "BPA",
                                    grepl("^Qual*", variable) ~ "SSBQual",
                                    grepl("-", variable) ~ "FQual",
                                    grepl("^BMGT", variable) ~ "SSBMGT",
                                    grepl("MSYBtrigger", variable) ~ "BMSY",
                                    grepl("FMSY", variable) ~ "FMSY",
                                    TRUE ~ variable
                            )) 
        df <- dplyr::filter(df,variable != "-")
        
        df <- dplyr::filter(df, lineDescription != "Management plan")
        df <- dplyr::filter(df, lineDescription != "Qualitative evaluation")
        df <- dplyr::mutate(df,key = paste(StockKeyLabel, lineDescription, type))
        df<- df[order(-df$year),]
        df <- df[!duplicated(df$key), ]
        df<- subset(df, select = -key)
        df<- subset(df, select = c(StockKeyLabel, AssessmentYear, AdviceCategory, lineDescription, type, status))
        df<- tidyr::spread(df,type, status)
        
        df2<- dplyr::filter(df,lineDescription != "Maximum Sustainable Yield")
        df2<- dplyr::filter(df2,lineDescription != "Maximum sustainable yield")
        
        colnames(df2) <- c("StockKeyLabel","AssessmentYear","AdviceCategory","lineDescription","FishingPressure","StockSize" )
        df2 <-dplyr::mutate(df2, SBL = case_when(FishingPressure == "GREEN" & StockSize == "GREEN" ~ "GREEN",
                                                 FishingPressure == "RED" | StockSize == "RED" ~ "RED",
                                                 FishingPressure == "ORANGE"  |  StockSize == "ORANGE" ~ "RED",
                                                 TRUE ~ "GREY"))
        df2<- subset(df2, select = c(StockKeyLabel, SBL))
        df <- dplyr::left_join(df, df2)
        df$lineDescription <- gsub("Maximum Sustainable Yield", "Maximum sustainable yield", df$lineDescription)
        df$lineDescription <- gsub("Precautionary Approach", "Precautionary approach", df$lineDescription)
        colnames(df) <- c("StockKeyLabel","AssessmentYear","AdviceCategory","lineDescription","FishingPressure","StockSize", "SBL" )
        sid <- load_sid(year)
        sid <- dplyr::filter(sid,!is.na(YearOfLastAssessment))
        sid <- dplyr::select(sid,StockKeyLabel,
                             YearOfLastAssessment, EcoRegion, FisheriesGuild)
        sid$FisheriesGuild <- tolower(sid$FisheriesGuild)
        colnames(sid) <- c("StockKeyLabel", "AssessmentYear", "Ecoregion", "FisheriesGuild")
        df <- merge(df, sid, all = FALSE)
        df
}



sag_status <- load_sag_status(2019)
status_formatted <- format_sag_status(sag_status)

unique(status_formatted$StockKeyLabel)



current <- stockstatus_CLD_current(sag_complete2)

current$color_fig2 <- case_when(current$F_FMSY < 1 & current$SSB_MSYBtrigger > 1 ~ "GREEN",
                                current$F_FMSY < 1 | current$SSB_MSYBtrigger > 1 ~ "ORANGE",
                                current$F_FMSY < 1 & is.na(current$SSB_MSYBtrigger) ~ "ORANGE",
                                is.na(current$F_FMSY) & current$SSB_MSYBtrigger > 1 ~ "ORANGE",
                                current$F_FMSY > 1 & current$SSB_MSYBtrigger < 1 ~ "RED",
                                is.na(current$F_FMSY) & is.na(current$SSB_MSYBtrigger) ~ "GREY",
                                TRUE ~ "RED")
                                
current <- unique (current)

# check <- unique(current[c("color_fig2", "F_FMSY", "SSB_MSYBtrigger")])


figure2 <- current %>%
        dplyr::group_by(Ecoregion, color_fig2) %>% 
        dplyr::summarise(n= dplyr::n()) %>%
        ungroup() %>%
        spread(color_fig2, n, fill=0)

DT <- data.table(current)

# In figure2 the shadowed area represents the total number of assessed stocks.

n <- DT[, .(number_of_assessed_stocks = length(unique(StockKeyLabel))), by = Ecoregion]

figure2 <- left_join(figure2, n)
figure2 <- subset(figure2,select = -GREY)
write.csv(figure2, file = "CSI032_figure2NEA_update2019.csv")


########### Figure 3 #############
##################################

# In Figure 3 is represented the trends on F/ FMSY and SSB/ MSYBtrigger for the
# available time-series.
# A mean accross all ecoregions is shown. We also propose the trends separated
# by ecoregion, as done for the Mediterranean and Black Sea.

df <- dplyr::mutate(sag_complete,F_FMSY = ifelse(!is.na(FMSY),
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
figure3 <- figure3 %>% filter(Year < 2018)

write.csv(figure3, file = "CSI032_figure3NEA_update2019.csv")


#Figure 3 by Ecoregion, like in the Mediterranean, still have to check it.
##########

df <- dplyr::mutate(sag_complete,F_FMSY = ifelse(!is.na(FMSY),
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

write.csv(figure3, file = "CSI032_figure3byecoregionNEA_update2019.csv")


