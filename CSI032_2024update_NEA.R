# UPDATE OF THE CSI O32 INDICATOR, for the EEA product
# August/September 2024
# Authors: Adriana Villamor and David Miller, ICES secretariat

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Clear Workspace
rm(list = ls()) 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(dplyr)
library(tidyr)
library(data.table)
# library(icesSAG)
install.packages('icesSAG', repos = c('https://ices-tools-prod.r-universe.dev', 'https://cloud.r-project.org'))

# The year-1 available assessments will be used, as current year assessments are still not fully available.
# We will only refer to year 2022, as official landings are only available until then.

year = 2023

#loading functions
load_sid <- function(year){
        # create url for SID web service
        url <- paste0("http://sd.ices.dk/services/odata4/StockListDWs4?$filter=ActiveYear%20eq%20", year)
        # download json data
        out <- jsonlite::fromJSON(url, simplifyDataFrame = TRUE)$value
        
        unique(out)
}

# We will use the latest available assessments for each stock,
# Load functions to load summary and reference points for those years

load_sag_summary <-  function(year){
        years <- ((year-4):year)
        out <- icesSAG::getSAG(stock = NULL,
                               years,
                               data = "summary",
                               # purpose != "Advice",
                               combine = TRUE)
        sid<-load_sid(year)
        sid <-dplyr::filter(sid,!is.na(YearOfLastAssessment))
        
        sid <- dplyr::select(sid,StockKeyLabel,
                             YearOfLastAssessment)
        colnames(sid) <- c("FishStock", "AssessmentYear")
        out1 <- merge(out, sid, by = c("FishStock", "AssessmentYear"),all = FALSE)
        unique(out1)
}

load_sag_refpts <- function(year){
        years <- ((year-4):year)
        out <- icesSAG::getSAG(stock = NULL,
                               years ,
                               # purpose != "Advice",
                               data = "refpts",
                               combine = TRUE)
        sid<-load_sid(year)
        sid <-dplyr::filter(sid,!is.na(YearOfLastAssessment))
        
        sid <- dplyr::select(sid,StockKeyLabel,
                             YearOfLastAssessment)
        colnames(sid) <- c("StockKeyLabel", "AssessmentYear")
        
        out$StockKeyLabel[out$StockKeyLabel == "ank.27.78ab"] <- "ank.27.78abd"
        out$StockKeyLabel[out$StockKeyLabel == "Pil.27.7"] <- "pil.27.7"
        
        out1 <- merge(out, sid, by = c("StockKeyLabel", "AssessmentYear"),all = FALSE)
        unique(out1)
}

load_sag_status <- function(year) {
  years <- ((year-3):year)
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
  # out <-out[!duplicated(out$StockKeyLabel),]
  
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



summ <- load_sag_summary(year)

# Filter assessments with Purpose = Advice (not InitAdvice, Benchmarks etc)

summ <- dplyr::filter(summ, Purpose == "Advice")

refpts <- load_sag_refpts(year)

# check stocks in summary and ref points 
a <- unique(summ$FishStock)
b <- unique(refpts$StockKeyLabel)
setdiff(b,a)
setdiff(a,b)
# both ways should be character(0)

# Load function to format sag
format_sag <- function(x,y){
        df1 <- dplyr::select(x,Year,
                             StockKeyLabel = FishStock,
                             AssessmentKey,
                             F,
                             SSB,
                             FishingPressureDescription,
                             StockSizeDescription,
                             Landings,
                             Catches,
                             Discards)
        df2 <- dplyr::select(y,StockKeyLabel,
                             AssessmentYear,
                             Flim= FLim,
                             Fpa,
                             Bpa,
                             Blim,
                             FMSY,
                             MSYBtrigger)
        
        out <- dplyr::left_join(df1,df2)
}


#Will mannually identify the components of North sea cod and remove the general assessment:

assessmentKeys <- icesSAG::findAssessmentKey(stock = "cod.27.46a7d20", year = 2023, published = TRUE,
                                             regex = TRUE, full = FALSE)


summ$FishStock[which(summ$AssessmentKey == "18282")] <- "cod.27.46a7d20V"
summ$FishStock[which(summ$AssessmentKey == "18283")] <- "cod.27.46a7d20NW"
summ$FishStock[which(summ$AssessmentKey == "18284")] <- "cod.27.46a7d20S"

summ <- summ %>% filter(AssessmentKey != "18396")

refpts$StockKeyLabel[which(refpts$AssessmentKey == "18282")] <- "cod.27.46a7d20V"
refpts$StockKeyLabel[which(refpts$AssessmentKey == "18283")] <- "cod.27.46a7d20NW"
refpts$StockKeyLabel[which(refpts$AssessmentKey == "18284")] <- "cod.27.46a7d20S"

refpts <- refpts %>% filter(AssessmentKey != "18396")



sag_complete <- format_sag(summ, refpts)

unique(sag_complete$StockKeyLabel)

# 255 stocks with assessments between 2019 and 2023, but we know there are 3 North sea cod assessments

# Load a file with the Ecoregions attributed for this product. In this file, 
# the Ecoregions used in the latest STECF report are also shown, some slight 
# differences exist, but mostly in non-EU waters.

# This file has been checked in 2024, against last years product and also against 
# STECF CFP report

ecoregions <- read.csv("Ecoregions.csv")

# stocks in sag but not in ecoregions doc:
new <- setdiff(sag_complete$StockKeyLabel, ecoregions$StockKeyLabel)
new

# character(0)


# stocks that do not show up in sag but are in the ecoregions file:
out <- setdiff(ecoregions$StockKeyLabel, sag_complete$StockKeyLabel)
out
# [1] "cod.21.1"        "cod.21.1a-e"     "cod.2127.1f14"   "cod.27.46a7d20"  "cod.27.47d20"    "cod.27.6a"      
# [7] "her.27.3031"     "rjb.27.67a-ce-k" "rjb.27.89a"      "sal.27.22-31"    "sal.27.32"       "sal.neac.all"   
# [13] "sal.wgc.all"     "thr.27.nea"      "trs.27.22-32"
# All checked by DM

names(ecoregions)

unique(ecoregions$Ecoregion)

sag_complete <- left_join(sag_complete,ecoregions, by = "StockKeyLabel")
names(sag_complete)


#Manual changes to custom ref points:
#HRmsy, not Fmsy
sag_complete$FMSY[which(sag_complete$StockKeyLabel == "had.27.5a")] <- 0.35 
sag_complete$FMSY[which(sag_complete$StockKeyLabel == "cod.27.5a")] <- 0.22 
sag_complete$FMSY[which(sag_complete$StockKeyLabel == "dgs.27.nea")] <- 0.043
sag_complete$FMSY[which(sag_complete$StockKeyLabel == "her.27.5a")] <- 0.15
#Lower Management plan triggers
sag_complete$MSYBtrigger[which(sag_complete$StockKeyLabel == "ane.27.8")] <- 24000
sag_complete$MSYBtrigger[which(sag_complete$StockKeyLabel == "cap.27.2a514")] <- 150000

#If there is no MSY btrigger, then use Bpa
sag_complete<- dplyr::mutate(sag_complete, MSYBtrigger = ifelse(is.na(MSYBtrigger),Bpa,MSYBtrigger))


#need to download custom columns for raj.27.3a47d, to get landings

assessmentKey <- icesSAG::findAssessmentKey(stock = "raj.27.3a47d", year = 2023, published = TRUE,
                                                              regex = TRUE, full = FALSE)
raj <- icesSAG::getCustomColumns(assessmentKey)
unique(raj$customName)
raj$customValue <- as.numeric(raj$customValue)
raj$Year <- as.integer(raj$Year)

#sum those two columns and use it as landings.

raj[is.na(raj)] <- 0
temp <- raj%>%select(StockKeyLabel,Year, customValue) 
temp <- temp %>% group_by(Year) %>% summarise(sum(customValue))
temp$StockKeyLabel <- "raj.27.3a47d"
colnames(temp) <- c("Year", "landings", "StockKeyLabel")
sag_complete <- sag_complete %>%
  left_join(temp, by = c('Year', 'StockKeyLabel'))


#2024, need to split the ane.27.9a components manually. Wont be needed next year as there will
# be two assessments in SAG

assessmentKey <- icesSAG::findAssessmentKey(stock = "ane.27.9a", year = 2023, published = TRUE,
                                            regex = TRUE, full =FALSE)
ane <- icesSAG::getCustomColumns(assessmentKey)
unique(ane$customName)

write.csv(sag_complete, file = "sag_complete.csv")

#will try to describe this later on, in the meantime I did it manually in sag_complete

library(readr)
sag_complete <- read_csv("sag_complete2.csv")


#now ane.27.9a is divided in 2 components

#We use the latest available assessments but only up to the year 2022

sag_complete2 <- sag_complete %>% filter(Year < year)
sag_complete2 <- unique(sag_complete2)

########### Figure1 #############
#######################################

# In figure1 the current status of the stocks (as of 2023) is used

# Load function to extract the current status of a formatted sag df

stockstatus_CLD_current <- function(x) {
        df<- dplyr::select(x,Year,
                           StockKeyLabel,
                           AssessmentYear,
                           F,
                           FMSY,
                           SSB,
                           MSYBtrigger,
                           Catches,
                           Landings,
                           Discards,
                           Ecoregion)
        df$F <- as.numeric(df$F)
        df$SSB <- as.numeric(df$SSB)
        df$FMSY <- as.numeric(df$FMSY)
        df$MSYBtrigger <- as.numeric(df$MSYBtrigger)
        df2 <- dplyr::group_by(df,StockKeyLabel)
        df2 <- dplyr::filter(df2,Year == AssessmentYear - 1)
        df2 <- dplyr::mutate(df2,F_FMSY =  ifelse(!is.na(FMSY),     #DM: how does it work with the HRmgt here?
                                                  F / FMSY,
                                                  NA))
        df2 <- dplyr::select(df2,StockKeyLabel,
                             F_FMSY,
                             Catches,
                             Landings,
                             Discards,
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
current <- transform(current, Landings2 = ifelse(!is.na(Landings), Landings, Catches))
current$Landings2[is.na(current$Landings2)] <- "0"
current$Landings2 <- as.numeric(current$Landings2)

# I have to remove from current df, stocks in higher categories with no assessment.
sid <- load_sid(year)
sid <-dplyr::filter(sid,!is.na(YearOfLastAssessment))
unique(sid$DataCategory)
out <- subset(sid,DataCategory %in% c("6.2", "5.2", "5.3", "6.3", "5.9", "5", "6.9", "6"))
unique(out$StockKeyLabel)
#59
unique(sid$StockKeyLabel)
#266, in remain 207

current2 <- subset(current, !(StockKeyLabel %in% out$StockKeyLabel))
unique(current2$StockKeyLabel)
# 200

#I will use Stock status for several stocks:

status_stocks <- c("anf.27.3a46",
                   "bli.27.5a14",
                   "cod.27.5b2",
                   "dab.27.3a4",
                   "dab.27.22-32",
                   "fle.27.3a4",
                   "gug.27.3a47d",
                   "had.27.6b",
                   "her.27.6aN",
                   "her.27.6aS7bc",
                   "hom.27.3a4bc7d",
                   "lem.27.3a47d",
                   "pil.27.7",
                   "ple.27.7e",
                   "ple.27.7fg",
                   "ple.27.7h-k",
                   "rjc.27.6",
                   "rjc.27.7afg",
                   "rjc.27.8abd",
                   "rjc.27.8c",
                   "rjc.27.9a",
                   "rje.27.7fg",
                   "rjh.27.9a",
                   "rjm.27.67bj",
                   "rjm.27.7ae-h",
                   "rjm.27.8",
                   "rjm.27.9a",
                   "rjn.27.8c",
                   "rjn.27.9a",
                   "rng.27.3a",
                   "sol.27.8c9a",
                   "spr.27.7de",
                   "whg.27.3a",
                   "aru.27.123a4",
                   "aru.27.6b7-1012",
                   "ane.27.8" ,
                   "ane.27.9a",
                   "cod.27.46a7d20",
                   "boc.27.6-8",
                   "cod.27.21" ,
                   "cod.27.24-32",
                    "lin.27.1-2",        
"lin.27.346-91214",     "mur.27.3a47d",          "nop.27.3a4",           "pil.27.7",
 "pok.27.1-2" ,  "pol.27.67"  ,         "pol.27.89a",          "raj.27.1012",          "rjn.27.3a4",
 "rjr.27.23a4","san.sa.1r",            "san.sa.2r" ,           "san.sa.3r" ,           "san.sa.4" ,            "sbr.27.10"  ,          "sdv.27.nea"   ,       
"sho.27.67",            "sho.27.89a",           "sol.27.8c9a"  ,        "spr.27.3a4"  ,         "spr.27.7de"   ,        "syc.27.3a47d"    ,    
"syc.27.67a-ce-j" ,     "syc.27.8abd"   ,       "syc.27.8c9a"   ,       "syt.27.67"     ,       "usk.27.1-2"   ,        "usk.27.3a45b6a7-912b",
"whg.27.89a")

       

sag_status <- load_sag_status(2023) 

sag_status <-  sag_status %>% filter(StockKeyLabel %in% status_stocks)

#will edit cod stock names to identify them

sag_status$StockKeyLabel[which(sag_status$AssessmentKey == "18282")] <- "cod.27.46a7d20V"
sag_status$StockKeyLabel[which(sag_status$AssessmentKey == "18283")] <- "cod.27.46a7d20NW"
sag_status$StockKeyLabel[which(sag_status$AssessmentKey == "18284")] <- "cod.27.46a7d20S"

sag_status <- sag_status %>% filter(AssessmentKey != "18396")


#ane.27.9a component again I do it mannually

write.csv(sag_status, file = "sag_status.csv", row.names = FALSE)

sag_status <- read_csv("sag_status2.csv")


format_sag_status <- function(x) {
  df <- x
  df <- dplyr::mutate(df,status = case_when(status == 0 ~ "UNDEFINED",
                                            status == 1 ~ "GREEN",
                                            status == 2 ~ "qual_GREEN", #qualitative green
                                            status == 3 ~ "ORANGE",
                                            status == 4 ~ "RED",
                                            status == 5 ~ "qual_RED", #qualitative red
                                            status == 6 ~ "GREY",
                                            status == 7 ~ "qual_UP",
                                            status == 8 ~ "qual_STEADY",
                                            status == 9 ~ "qual_DOWN",
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

sag_status_frmt <- format_sag_status(sag_status)

sag_status_frmt <- sag_status_frmt %>% filter(lineDescription == "Maximum sustainable yield")

sag_status_frmt$color_bis <- case_when(sag_status_frmt$FishingPressure != "GREY" & sag_status_frmt$StockSize != "GREY" ~ "GREEN",
                                        sag_status_frmt$FishingPressure == "GREY" & sag_status_frmt$StockSize == "GREY" ~ "RED",
                                TRUE ~ "ORANGE")


subset <- sag_status_frmt[,c(1,10)]
current2 <- left_join(current2,subset)
current2 <- mutate(current2, color_fig1 = ifelse(is.na(color_bis) , color_fig1, color_bis))
current2 <- current2[,-16]

figure1 <- current2 %>%
  group_by(Ecoregion, color_fig1) %>% 
  summarise(Landings = sum(Landings2)) %>%
  ungroup() %>%
  spread(color_fig1, Landings, fill=0)


# Total catches will be the sum of:
# SAG catches for ICES stocks (as in nominal catches discards are not taken into account) +
# Nominal catches for all others species and areas

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

# we will approximate the confidential catches with the previous three years average

str(catch_dat)

## I calculate from 2018, the confidential catches as the mean of the previous 3 years
# and move forward until 2022, so the means should be more meaningful, hopefully
catch_dat$new <- rowMeans(subset(catch_dat, select = c(X2017,X2016, X2015)))
sub <- catch_dat %>% filter(X2018 == "0 c")
sub$X2018 <- sub$new
sub <- sub[,-21]
catch_dat <- catch_dat %>% filter(X2018 != "0 c")
catch_dat <- catch_dat[,-21]
catch_dat <- rbind(catch_dat, sub)
catch_dat$X2018 <- as.numeric(catch_dat$X2018)

catch_dat$new <- rowMeans(subset(catch_dat, select = c(X2018,X2017, X2016)))
sub <- catch_dat %>% filter(X2019 == "0 c")
sub$X2019 <- sub$new
sub <- sub[,-21]
catch_dat <- catch_dat %>% filter(X2019 != "0 c")
catch_dat <- catch_dat[,-21]
catch_dat <- rbind(catch_dat, sub)
catch_dat$X2019 <- as.numeric(catch_dat$X2019)

catch_dat$new <- rowMeans(subset(catch_dat, select = c(X2019,X2018, X2017)))
sub <- catch_dat %>% filter(X2020 == "0 c")
sub$X2020 <- sub$new
sub <- sub[,-21]
catch_dat <- catch_dat %>% filter(X2020 != "0 c")
catch_dat <- catch_dat[,-21]
catch_dat <- rbind(catch_dat, sub)
catch_dat$X2020 <- as.numeric(catch_dat$X2020)

catch_dat$new <- rowMeans(subset(catch_dat, select = c(X2020,X2019, X2018)))
sub <- catch_dat %>% filter(X2021 == "0 c")
sub$X2021 <- sub$new
sub <- sub[,-21]
catch_dat <- catch_dat %>% filter(X2021 != "0 c")
catch_dat <- catch_dat[,-21]
catch_dat <- rbind(catch_dat, sub)
catch_dat$X2021 <- as.numeric(catch_dat$X2021)

catch_dat$new <- rowMeans(subset(catch_dat, select = c(X2021,X2020, X2019)))
sub <- catch_dat %>% filter(X2022 == "0 c")
sub$X2022 <- sub$new
sub <- sub[,-21]
catch_dat <- catch_dat %>% filter(X2022 != "0 c")
catch_dat <- catch_dat[,-21]
catch_dat <- rbind(catch_dat, sub)
catch_dat$X2022 <- as.numeric(catch_dat$X2022)


catch_dat_2022 <- subset(catch_dat, select= c("Species","Area","Country", "X2022"))

# This file enumerates all areas for each stock, so we can filter catch_dat with it.
# It is extracted from ICES vocabs every year to account for new stocks.

catch_areas <- read.csv("StocksPerArea2024.csv")
names(catch_areas)

#Extract species code of the stock code
catch_areas$Species <- substr(catch_areas$StockKeyLabel, start = 1, stop = 3)
catch_areas$Species <- toupper(catch_areas$Species)
unique(catch_areas$Species)

# catch_areas <- catch_areas[,c(2,4,8)]
colnames(catch_areas) <- c("StockKeyLabel", "Area", "Species")

# only want areas of the stocks we have in sag_complete2

catch_areas <- catch_areas %>% filter(StockKeyLabel %in% sag_complete2$StockKeyLabel)
# catch_areas <- catch_areas %>% filter(StockKeyLabel %in% sid$StockKeyLabel)
unique(catch_areas$StockKeyLabel)

# 251


#To deal with _NK catches, will infer the lower _NK for each area, and add them
# to the catch_areas dataframe

catch_areas_nk <- catch_areas 

#remove last characters up to the point included 
catch_areas_nk$Area <- sub(".[^.]+$", "", catch_areas_nk$Area)
# paste _NK in the same place
catch_areas_nk$Area_nk <- paste0(catch_areas_nk$Area, "_NK")

catch_areas_nk <- catch_areas_nk[, -2]
catch_areas_nk <- catch_areas_nk %>% 
        rename(Area = Area_nk)

# This df has all areas and corresponding _NK in the immediate lower aggregation level
catch_areas <- rbind(catch_areas, catch_areas_nk)
catch_areas <- unique(catch_areas)

# I should remove catches for those areas and species from the 2022 nominal catches.

catch_dat_2022_2 <- anti_join(catch_dat_2022, catch_areas, by=c("Area", "Species")) 

unique(catch_dat_2022_2$Area)

catch_dat_2022_2 <- catch_dat_2022_2 %>%
        mutate(Ecoregion = case_when(
                .$Area %in% c("27.3.b.23", "27.3.c.22","27.3.d.24", "27.3.d.25", "27.3.d.26","27.3.d.27",
                              "27.3.d.28.1","27.3.d.28.2","27.3.d.30","27.3.d.31","27.3.d.32","27.3_NK",
                              "27.3.d.28_NK","27.3.d_NK" ) ~ "Baltic Sea",
                .$Area %in% c("27.3.a.20","27.3.a.21", "27.4.a", "27.4.b","27.4.c","27.7.d",
                              "27.3.a_NK", "27.4_NK") ~ "Greater North Sea",
                
                .$Area %in% c("27.8.a", "27.8.b","27.8.c",
                              "27.8.d.2", "27.8.e.2", "27.9.a",
                              "27.9.b.2") ~ "BoBiscay & Iberia",
                .$Area %in% c("27.6.a", "27.6.b.2","27.7.a", "27.7.b", "27.7.c.2",
                              "27.7.f", "27.7.g", "27.7.h","27.7.j.2", "27.7.k.2", "27.7.g-k_NK","27.7.bc_NK") ~ "Celtic Seas",
                
                .$Area %in% c("27.5.a.1","27.5.a.2", "27.5.b.1.a","27.5.b.1.b", "27.5.b.2", "27.12.a.2","27.12.a.4", "27.14.b.2", "27.12.a.3", "27.14.a", 
                              "27.5.a_NK","27.5.b_NK" ) ~ "Iceland",
                
                .$Area %in% c("27.1.a", "27.1.b", "27.2.a.1", "27.2.a.2", "27.2.b.1", "27.2.b.2",
                              "27.2.b_NK", "27.2.a_NK") ~ "Arctic Ocean",
                .$Area %in% c("27.10.a.2") ~ "Azores",
                .$Area %in% c("27.10.a.1", "27.10.b", "27.12.c", "27.12.a.1", "27.14.b.1", "27.12.b",
                              "27.6.b.1", "27.7.c.1", "27.7.k.1", "27.8.e.1", "27.8.d.1", "27.9.b.1") ~ "Widely",
                TRUE ~ "OTHER"))


# The shadowed area in Figure 1 represents landings of unassessed stocks

catch_dat_2022_2 <- catch_dat_2022_2 %>% filter(Ecoregion != "OTHER")
catch_dat_2022_2$X2022 <- as.numeric(catch_dat_2022_2$X2022)
catch_dat_2022_2 <- catch_dat_2022_2[complete.cases(catch_dat_2022_2), ]
catch <- catch_dat_2022_2 %>%
        group_by(Ecoregion) %>% 
        summarise(Catch = sum(X2022))


sag_catch <- current2 %>%
  group_by(Ecoregion) %>% 
  summarise(Catch = sum(Landings2))

#Add up both dataframes

catch_figure1 <- bind_rows(catch, sag_catch)%>%group_by(Ecoregion) %>% summarise_all(sum)


unique(figure1$Ecoregion)
unique(catch_figure1$Ecoregion)


figure1 <- merge(figure1, catch_figure1, all = TRUE)

write.csv(figure1, file = "CSI032_figure1NEA_update2024_7nov.csv")

#################FIGURE 2##########################
# In Figure2, we will use the stock status attributed, 
# so we will have some unassessed stocks with colors

# GREEN means both reference points in GES, ORANGE means only one ref point 
# in GES, or in case only one reference point is available, this is in GES.
# RED means both reference points not in GES, or if only one reference point is 
# available, it is not in GES.

sag_status <- load_sag_status(2023) 

#will edit cod stock names to identify them

sag_status$StockKeyLabel[which(sag_status$AssessmentKey == "18282")] <- "cod.27.46a7d20V"
sag_status$StockKeyLabel[which(sag_status$AssessmentKey == "18283")] <- "cod.27.46a7d20NW"
sag_status$StockKeyLabel[which(sag_status$AssessmentKey == "18284")] <- "cod.27.46a7d20S"

sag_status <- sag_status %>% filter(AssessmentKey != "18396")


#ane.27.9a component again I do it manually

# write.csv(sag_status, file = "sag_status.csv", row.names = FALSE)

sag_status <- read_csv("sag_status.csv")

sag_status_frmt <- format_sag_status(sag_status)

unique(sag_status_frmt$StockKeyLabel)

#250

status_formatted <- sag_status_frmt %>% filter(lineDescription == "Maximum sustainable yield")

unique(status_formatted$FishingPressure)
unique(status_formatted$StockSize)

status_formatted$FishingPressure[which(status_formatted$FishingPressure == "qual_GREEN")] <- "GREEN"
status_formatted$FishingPressure[which(status_formatted$FishingPressure == "qual_RED")] <- "RED"
status_formatted$StockSize[which(status_formatted$StockSize == "qual_GREEN")] <- "GREEN"
status_formatted$StockSize[which(status_formatted$StockSize == "qual_RED")] <- "RED"
status_formatted$StockSize[which(status_formatted$StockSize == "UNDEFINED")] <- "GREY"

status_formatted$color_fig2 <- case_when(status_formatted$FishingPressure == "GREEN" & status_formatted$StockSize == "GREEN" ~ "GREEN",
                                         status_formatted$FishingPressure == "GREEN" | status_formatted$StockSize == "GREEN" ~ "ORANGE",
                                         status_formatted$FishingPressure == "GREEN" & status_formatted$StockSize == "GREY" ~ "ORANGE",
                                         status_formatted$FishingPressure == "GREY" & status_formatted$StockSize == "GREEN" ~ "ORANGE",
                                         status_formatted$FishingPressure == "RED" & status_formatted$StockSize == "RED" ~ "RED",
                                         status_formatted$FishingPressure == "GREY" & status_formatted$StockSize == "GREY"  ~ "GREY",
                                         TRUE ~ "RED")

check <- status_formatted %>% filter(color_fig2 == "GREY")

# For the total number of assessed stocks we need to remove those cat 5 and 6 which have both grey.
sid <- load_sid(year)
sid <-dplyr::filter(sid,!is.na(YearOfLastAssessment))
unique(sid$DataCategory)
cat56 <- subset(sid, (DataCategory %in% c("6.2", "5.2", "6.3", "5.9", "5", "6.9", "6", "5.3")))

out <- check %>% filter(check$StockKeyLabel %in% cat56$StockKeyLabel)

status_formatted <- anti_join(status_formatted, out)


status_formatted <- status_formatted[, -(8)]
status_formatted <- left_join(status_formatted, ecoregions)


figure2 <- status_formatted %>%
        dplyr::group_by(Ecoregion, color_fig2) %>% 
        dplyr::summarise(n= dplyr::n()) %>%
        ungroup() %>%
        spread(color_fig2, n, fill=0)


#GREY are assessed stocks with no status assigned

DT <- data.table(ecoregions)
n <- DT[, .(number_of_stocks = length(unique(StockKeyLabel))), by = Ecoregion]

figure2 <- left_join(figure2, n)

write.csv(figure2, file = "CSI032_figure2NEA_update2024_2oct.csv")


########### Figure 3 #############
##################################

# In Figure 3 is represented the trends on F/ FMSY and SSB/ MSYBtrigger for the
# available time-series.
# A mean accross all ecoregions is shown. We also propose the trends separated
# by ecoregion, as done for the Mediterranean and Black Sea.


#I dont think we need this Dave
# check new categories bewteen 1 and 4
# unique(sid$DataCategory)
# sid <- load_sid(year)
# unique(sid$DataCategory)
# cat1234 <- sid %>% filter(DataCategory %in% c("1", "2", "1.2", "1.8", "1.6", "1.7",
#                                             "3.2", "3", "3.3", "4.14", "3.9",
#                                             "3.14", "4.12", "2.13", "3.8", "4", "2.11"))
# sag_fig3 <- sag_complete2 %>% filter(StockKeyLabel %in% cat1234$StockKeyLabel)


sag_fig3 <- sag_complete2
sag_fig3$FMSY <- as.numeric(sag_fig3$FMSY)
sag_fig3$F <- as.numeric(sag_fig3$F)
sag_fig3$MSYBtrigger <- as.numeric(sag_fig3$MSYBtrigger)
sag_fig3$SSB <- as.numeric(sag_fig3$SSB)
#DAVE, still applies?
sag_fig3$MSYBtrigger[which(sag_fig3$StockKeyLabel == "nep.fu.15")] <- 3000000000


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

# Wont use Arctic Ocean and Iceland, Greenland and Faroes for the mean of Figure 3 
#will run figure 3 for NEA without Baltic and then only Baltic

df2 <- df2 %>% filter(Ecoregion %in% c("BoBiscay & Iberia","Widely","Celtic Seas", "Greater North Sea")) 

df3 <-dplyr::group_by(df2,Metric, Year)%>% 
  summarize(percentile_97_5 = quantile(Value, probs = 0.975), percentile_02_5 = quantile(Value, probs = 0.025), MEAN=mean(Value, na.rm = TRUE))


# df3 <- dplyr::group_by(df2,Metric, Year) %>%
#         mutate(Max = max(Value), Min = min(Value))
        
# we have been asked to separate Baltic Sea in this one:
# df3_baltic <- df3 %>% filter(Ecoregion == "Baltic Sea")

# df3 <- df3 %>% filter(Ecoregion %in% c("BoBiscay & Iberia","Widely","Celtic Seas", "Greater North Sea"))

# df4 <- dplyr::group_by(df3,Metric, Year, Min, Max)%>%
#         summarize(MEAN = mean(Value, na.rm = TRUE))
                
#Put back to short format

# fmsy <- df4 %>%filter(Metric == "F_FMSY")
# names(fmsy)
# fmsy <- fmsy[,-1]
# colnames(fmsy) <- c("Year", "Min_F/FMSY", "Max_F/FMSY", "MEAN_F/FMSY")
# 
# ssb <- df4 %>%filter(Metric == "SSB_MSYBtrigger")
# names(ssb)
# ssb <- ssb[,-1]
# colnames(ssb) <- c("Year", "Min_SSB/MSYBtrigger", "Max_SSB/MSYBtrigger", "MEAN_SSB/MSYBtrigger")

#Number of assessed stocks by year

DT <- data.table(df2)

stks <- DT[, .(number_of_assessed_stocks = length(unique(StockKeyLabel))), by = Year]
        
# figure3 <- ssb %>% left_join(fmsy)        
figure3 <- df3 %>% left_join(stks)

# Remove the only stock with biomass data from 1905 to 1945, dgs.27.nea
figure3 <- figure3 %>% filter(Year > 1946)
# figure3 <- figure3 %>% filter(Year < 2022)

write.csv(figure3, file = "CSI032_figure3NEA_NoBalticupdate2024_07nov.csv")

#now same thing with Baltic

df2 <-tidyr::gather(df,Metric, Value, -Year, -Ecoregion, -StockKeyLabel) 
df2 <- df2[complete.cases(df2),]
unique(df2$Ecoregion)

df2 <- df2 %>% filter(Ecoregion %in% c("Baltic Sea")) 

df3 <-dplyr::group_by(df2,Metric, Year)%>% 
  summarize(percentile_97_5 = quantile(Value, probs = 0.975), percentile_02_5 = quantile(Value, probs = 0.025), MEAN=mean(Value, na.rm = TRUE))


# df3 <- dplyr::group_by(df2,Metric, Year) %>%
#   mutate(Max = max(Value), Min = min(Value))
# 
# df4 <- dplyr::group_by(df3,Metric, Year, Min, Max)%>%
#   summarize(MEAN = mean(Value, na.rm = TRUE))

#Put back to short format

# fmsy <- df4 %>%filter(Metric == "F_FMSY")
# names(fmsy)
# fmsy <- fmsy[,-1]
# colnames(fmsy) <- c("Year", "Min_F/FMSY", "Max_F/FMSY", "MEAN_F/FMSY")

# ssb <- df4 %>%filter(Metric == "SSB_MSYBtrigger")
# names(ssb)
# ssb <- ssb[,-1]
# colnames(ssb) <- c("Year", "Min_SSB/MSYBtrigger", "Max_SSB/MSYBtrigger", "MEAN_SSB/MSYBtrigger")

#Number of assessed stocks by year

DT <- data.table(df2)

stks <- DT[, .(number_of_assessed_stocks = length(unique(StockKeyLabel))), by = Year]

# figure3 <- ssb %>% left_join(fmsy)        
figure3 <- df3 %>% left_join(stks)

# Remove the only stock with biomass data from 1905 to 1945, dgs.27.nea
figure3 <- figure3 %>% filter(Year > 1946)

write.csv(figure3, file = "CSI032_figure3BalticSea_update2024_7nov.csv")






#Do we need to do this?

# HERE 2024update
#Baltic, NEA without, Med, BlackSea
#Figure 3 by Ecoregion, like in the Mediterranean, still have to check it.
##########
~
  
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

write.csv(figure3, file = "CSI032_figure3byecoregionNEA_update2024_2oct.csv")


