# UPDATE OF THE CSI O32 INDICATOR, for the EEA product
# Northeastern Atlantic data come mostly from ICES data bases, 
# although the atribution to Ecoregions has been fine-tuned to better fit those 
# in the 2015 indicator, as well as the Ecoregions used by STECF in the latest 
# report on the CFP indicators update (ref)
# April 2019
# Authors: Adriana Villamor and David Miller, ICES secretariat

library(dplyr)
library(tidyr)
library(data.table)
library(operators)

# The latest available assessments will be used, although we will only refer to
# year 2017, as official landings are only available until then.

year = 2018

load_sid <- function(year){
        # create url for SID web service
        url <- paste0("http://sd.ices.dk/services/odata4/StockListDWs4?$filter=ActiveYear%20eq%20", year)
        # download json data
        out <- jsonlite::fromJSON(url, simplifyDataFrame = TRUE)$value
        
        unique(out)
}

# We will use the latest available assessments for each stock, from 2015 to 2018

load_sag_summary <-  function(year){
        years <- ((year-3):year)
        out <- icesSAG::getSAG(stock = NULL,
                               years,
                               data = "summary",
                               purpose != "InitAdvice",
                               combine = TRUE)
        sid<-load_sid(year)
        sid <-dplyr::filter(sid,!is.na(YearOfLastAssessment))
        # Stocks with Data Category 5 and 6 are not used, as these can't be 
        # considered full assessments (although advice may exist)
        
        sid <- subset(sid, !(DataCategory %in% c("6.2", "5.2", "6.3", "5.9", "5", "6.9", "6")))
        sid <- dplyr::select(sid,StockKeyLabel,
                             YearOfLastAssessment, PreviousStockKeyLabel)
        colnames(sid) <- c("fishstock", "AssessmentYear", "PreviousStockKeyLabel")
        
        
        #Fix a couple of mismatches in sid and sag
        sid$AssessmentYear[sid$fishstock == "cod.21.1a-e"] <- "2017"
        sid$AssessmentYear[sid$fishstock == "reb.2127.dp"] <- "2016"
        sid$AssessmentYear[sid$fishstock == "reb.2127.sp"] <- "2016"
        out$fishstock[out$fishstock == "ank.27.78ab"] <- "ank.27.78abd"
        out$fishstock[out$fishstock == "Pil.27.7"] <- "pil.27.7"
        
        
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
        years <- ((year-3):year)
        out <- icesSAG::getSAG(stock = NULL,
                               years ,
                               purpose != "InitAdvice",
                               data = "refpts",
                               combine = TRUE)
        sid<-load_sid(year)
        sid <-dplyr::filter(sid,!is.na(YearOfLastAssessment))
        sid <- subset(sid, !(DataCategory %in% c("6.2", "5.2", "6.3", "5.9", "5", "6.9", "6")))
        sid <- dplyr::select(sid,StockKeyLabel,
                             YearOfLastAssessment, PreviousStockKeyLabel)
        colnames(sid) <- c("StockKeyLabel", "AssessmentYear", "PreviousStockKeyLabel")
        
        sid$AssessmentYear[sid$StockKeyLabel == "cod.21.1a-e"] <- "2017"
        sid$AssessmentYear[sid$StockKeyLabel == "reb.2127.dp"] <- "2016"
        sid$AssessmentYear[sid$StockKeyLabel == "reb.2127.sp"] <- "2016"
        out$StockKeyLabel[out$StockKeyLabel == "ank.27.78ab"] <- "ank.27.78abd"
        out$StockKeyLabel[out$StockKeyLabel == "Pil.27.7"] <- "pil.27.7"
        
        old <- dplyr::filter(sid, AssessmentYear < 2017)
        out1 <- merge(out, sid, by = c("StockKeyLabel", "AssessmentYear"),all = FALSE)
        out2 <- merge(out, old, by.x = c("StockKeyLabel", "AssessmentYear"), by.y = c("PreviousStockKeyLabel", "AssessmentYear"),all = FALSE)
        out2$StockKeyLabel <- out2$StockKeyLabel.y
        out2 <- subset(out2,select = -StockKeyLabel.y)
        out <- merge(out1,out2, all = TRUE)
        out <- subset(out,select = -PreviousStockKeyLabel)
        unique(out)
}


# Get rid of "InitAdvice" assessments, some stocks have 2 assessments per year

summ <- load_sag_summary(year)
out <- dplyr::filter(summ, Purpose == "InitAdvice")
summ <- dplyr::filter(summ, Purpose != "InitAdvice")

refpts <- load_sag_refpts(year)
double <- dplyr::filter(refpts, StockKeyLabel %in% out$fishstock)
double2 <- dplyr::group_by(double, StockKeyLabel)
double2 <- dplyr::filter(double2, AssessmentKey == min(AssessmentKey))
refpts <- anti_join(refpts, double2)


# Format sag
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

#196 stocks, check

# Load a file with the Ecoregions attributed for this product. In this file, 
# the Ecoregions used in the latest STECF report are also shown, some slight 
# differences exist, but mostly in non-EU waters.

ecoregions <- read.csv("Ecoregions.csv")
names(ecoregions)
ecoregions <- ecoregions[,-2]

unique(ecoregions$Ecoregion)

#Stocks in STECF list of stocks, not in the present exercise

noICES<- subset(ecoregions, !(ecoregions$StockKeyLabel %in% sag_complete$StockKeyLabel))
unique(noICES$StockKeyLabel)

# [1] alf.27.nea          bss.27.6a7bj        bss.27.8c9a         cod.27.6b           gag.27.nea         
# [6] gur.27.3-8          hom.27.3a4bc7d      jaa.27.10a2         ldb.27.7b-k8abd     mur.27.67a-ce-k89a 
# [11] nep.27.4outfu       nep.27.6aoutfu      nep.27.7outfu       nop.27.6a           ory.27.nea         
# [16] pil.27.7            ple.27.7bc          ple.27.89a          pol.27.3a4          pol.27.89a         
# [21] pra.27.4a           raj.27.3a47d        raj.27.67a-ce-h     raj.27.89a          rja.27.nea         
# [26] rjb.27.67a-ce-k     rjb.27.89a          rjc.27.7e           rje.27.7de          rjf.27.67          
# [31] rjh.27.4a6          rjh.27.4c7d         rjh.27.7afg         rjh.27.7e           rji.27.67          
# [36] rju.27.7bj          rju.27.8ab          rju.27.8c           rju.27.9a           rng.27.1245a8914ab 
# [41] rng.27.5a10b12ac14b rng.27.5b6712b      san.27.6a           san.sa.6            sbr.27.6-8         
# [46] sol.27.7bc          sol.27.8c9a         spr.27.67a-cf-k     usk.27.12ac         usk.27.6b          
# [51] whg.27.3a           whg.27.6b           whg.27.89a  

#None of these stocks have full assessments. 

sag_complete <- left_join(sag_complete,ecoregions, by = "StockKeyLabel")
names(sag_complete)


########### Figure1 and 2 #############
#######################################

# In figure1 and 2 only the current status of the stocks (as of 2017) is used
# CHECK!

stockstatus_CLD_current <- function(x) {
        df<- dplyr::select(x,Year,
                           StockKeyLabel,
                           Ecoregion,
                           AssessmentYear,
                           F,
                           FMSY,
                           SSB,
                           MSYBtrigger,
                           catches,
                           landings,
                           discards)
        df2<-dplyr::full_join(df %>% dplyr::group_by(StockKeyLabel) %>%
                                      dplyr::filter(Year == 2017) %>%
                                      dplyr::mutate(F_FMSY =  ifelse(!is.na(FMSY),
                                                                     F / FMSY,
                                                                     NA)) %>%
                                      dplyr::select(StockKeyLabel,
                                                    Ecoregion,
                                                    F_FMSY,
                                                    catches,
                                                    landings,
                                                    discards,
                                                    FMSY,
                                                    F),
                              df %>%
                                      dplyr::group_by(StockKeyLabel) %>%
                                      dplyr::filter(Year == 2017) %>%
                                      dplyr::mutate(SSB_MSYBtrigger = ifelse(!is.na(MSYBtrigger),
                                                                             SSB / MSYBtrigger,
                                                                             NA)) %>%
                                      dplyr::select(StockKeyLabel,
                                                    Ecoregion,
                                                    SSB_MSYBtrigger,
                                                    SSB,
                                                    MSYBtrigger))
        df2
}

current <- stockstatus_CLD_current(sag_complete)


# In figure 1, GREEN means landings of assessed stocks with info for F and SSB 
# reference points, ORANGE means landings of assessed stocks with info for only
# one of the two reference points, and RED means landings of assessed stocks for
# which no info is available on any reference point. 

current$color_fig1 <- case_when(current$F_FMSY != "NA" & current$SSB_MSYBtrigger != "NA" ~ "GREEN",
                                is.na(current$F_FMSY) & is.na(current$SSB_MSYBtrigger) ~"RED",
                                TRUE ~ "ORANGE")
current <- unique (current)

# check <- unique(current[c("color_fig1", "F_FMSY", "SSB_MSYBtrigger")])

#If there are no landings but catches, use catches
current <- transform(current, landings2 = ifelse(!is.na(landings), landings, catches))

figure1 <- current %>%
        group_by(Ecoregion, color_fig1) %>% 
        summarise(landings = sum(landings2)) %>%
        ungroup() %>%
        spread(color_fig1, landings, fill=0)


##ICES official catches
# for now I use my local copy, but hopefully when we deliver this will be published.

# catchURL <- "http://ices.dk/marine-data/Documents/CatchStats/OfficialNominalCatches.zip"
# tmpFileCatch <- tempfile(fileext = ".zip")
# download.file(catchURL, destfile = tmpFileCatch, mode = "wb", quiet = TRUE)
# ices_catch_official_raw <- read.csv(unz(tmpFileCatch,
#                                         grep("ICESCatchDataset.*.csv", unzip(tmpFileCatch,
#                                                                              list = TRUE)$Name,
#                                              value = TRUE)),
#                                     stringsAsFactors = FALSE,
#                                     header = TRUE,
#                                     fill = TRUE)

catch_dat <- ICESCatchDataset2006_2017

catch_dat_2017 <- subset(catch_dat, select= c("Species","Area", "2017"))

#There might be issues with dplyr and operator, dplyr should be loaded the last, so in the search()
#is first
# Attribute landings by Ecoregions used

catch_dat_2017 <- catch_dat_2017 %>%
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
                              "27.6.b.1", "27.7.c", "27.7.k.1", "27.8.e.1", "27.8.d.1", "27.9.b.1") ~ "Oceanic",
                TRUE ~ "OTHER"))

catch_dat_2017 <- catch_dat_2017 %>%
        mutate(ECOREGION2 = case_when(              
                .$Species %in% c("ARU") & .$Area %in% c("27.7", "27.8", "27.9", "27.10", "27.12", "27.6.b") ~ "Widely",
                .$Species %in% c("BLI", "BOC", "BSF","DGS", "GFB", "MAC", "SDV","WHB")  ~ "Widely",
                .$Species %in% c("HER") & .$Area %in% c("27.1", "27.2", "27.5", "27.14.a") ~ "Widely",
                .$Species %in% c("HKE") & .$Area %in% c("27.4", "27.6", "27.7", "27.8.a","27.8.b","27.8.d") ~ "Widely",
                .$Species %in% c("HOM") & .$Area %in% c("27.8", "27.2.a", "27.4.a", "27.5.b","27.6.a",
                                                        "27.7.a","27.7.b","27.7.c","27.7.e","27.7.f",
                                                        "27.7.g","27.7.h","27.7.i","27.7.j","27.7.k") ~ "Widely",
                .$Species %in% c("LIN") & .$Area %!in% c("27.1", "27.2", "27.5a", "27.5.b") ~ "Widely",
                .$Species %in% c("RNG") & .$Area %in% c("27.6", "27.7", "27.5b", "27.12.b") ~ "Widely",
                .$Species %in% c("USK") & .$Area %in% c("27.4", "27.7", "27.8", "27.9","27.3.a","27.5.b",
                                                        "27.6.a", "27.12.b" ) ~ "Widely")) 


catch_dat_2017 <- transform(catch_dat_2017, Final = ifelse(!is.na(ECOREGION2), "Widely", ECOREGION))

catch_dat_2017 <- catch_dat_2017[, -c(4:5)]
colnames(catch_dat_2017) <- c("Species", "Area", "Value", "Ecoregion")
catch_dat_2017 <- catch_dat_2017 %>% filter(Ecoregion != "OTHER")




# The shadowed area in Figure 1 represents landings of unassessed stocks

catch <- catch_dat_2017 %>%
        group_by(Ecoregion) %>% 
        summarise(Catch = sum(Value))

#This will be merged with the color counts of figure1


unique(figure1$Ecoregion)
unique(catch$Ecoregion)

figure1 <- merge(figure1, catch, all = TRUE)

write.csv(figure1, file = "CSI032_figure1NEA_update2019.csv")


# In Figure2, only assessed stocks are represented.
# GREEN means both reference points in GES, ORANGE means only one ref point 
# in GES, or in case only one reference point is available, this is in GES.
# RED means both reference points not in GES, or if only one reference point is 
# available, it is not in GES.

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
        dplyr::summarise(n= n()) %>%
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
figure3 <- figure3 %>% filter(Year > 1945)

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


