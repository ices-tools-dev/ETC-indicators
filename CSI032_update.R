## functions and analysis to aggregate sag data for the update of the CSI 032 indicator.
## 
# I am not sure how to attribute stocks to ecoregions, maybe you have clearer ideas.


library(dplyr)

year = 2018


load_sid <- function(year){
        # create url for SID web service
        url <- paste0("http://sd.ices.dk/services/odata4/StockListDWs4?$filter=ActiveYear%20eq%20", year)
        # download json data
        out <- jsonlite::fromJSON(url, simplifyDataFrame = TRUE)$value
        
        unique(out)
}

load_sag_summary <-  function(year){
        years <- ((year-3):year)
        out <- icesSAG::getSAG(stock = NULL,
                               years,
                               data = "summary",
                               purpose = "Advice",
                               combine = TRUE)
        sid<-load_sid(year)
        sid <-dplyr::filter(sid,!is.na(YearOfLastAssessment))
        sid <- dplyr::select(sid,StockKeyLabel,
                             YearOfLastAssessment, PreviousStockKeyLabel)
        colnames(sid) <- c("fishstock", "AssessmentYear", "PreviousStockKeyLabel")
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
                               purpose = "Advice",
                               data = "refpts",
                               combine = TRUE)
        sid<-load_sid(year)
        sid <-dplyr::filter(sid,!is.na(YearOfLastAssessment))
        sid <- dplyr::select(sid,StockKeyLabel,
                             YearOfLastAssessment, PreviousStockKeyLabel)
        colnames(sid) <- c("StockKeyLabel", "AssessmentYear", "PreviousStockKeyLabel")
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
refpts <- load_sag_refpts(year)

format_sag <- function(x,y,year){
        sid <- load_sid(year)
        sid <- dplyr::filter(sid,!is.na(YearOfLastAssessment))
        sid <- dplyr::select(sid,StockKeyLabel,
                             YearOfLastAssessment, EcoRegion, FisheriesGuild)
        colnames(sid) <- c("StockKeyLabel", "AssessmentYear", "Ecoregion", "FisheriesGuild")
        df1 <- dplyr::mutate(x, StockKeyLabel= fishstock)
        df1 <- merge(df1, sid, by = c("StockKeyLabel", "AssessmentYear"), all = FALSE)
        # df1 <- dplyr::filter(df1,(grepl(pattern = ecoregion, Ecoregion)))
        df1 <- dplyr::select(df1,Year,
                             StockKeyLabel,
                             Ecoregion,
                             FisheriesGuild,
                             F,
                             SSB,
                             fishingPressureDescription,
                             stockSizeDescription,
                             landings,
                             catches,
                             discards)
        df2 <- merge(y, sid, by = c("StockKeyLabel", "AssessmentYear"), all = FALSE)
        # df2 <- dplyr::filter(df2,(grepl(pattern = ecoregion, Ecoregion)))
        df2 <- dplyr::select(df2,StockKeyLabel,
                             Ecoregion,
                             AssessmentYear,
                             Flim = FLim,
                             Fpa,
                             Bpa,
                             Blim,
                             FMSY,
                             MSYBtrigger)
        
        out <- dplyr::left_join(df1,df2)
}

sag_complete <- format_sag(summ, refpts,2018)


########### Figure1 and 2 #################

stockstatus_CLD_current <- function(x) {
        df<- dplyr::select(x,Year,
                           StockKeyLabel,
                           FisheriesGuild,
                           Ecoregion,
                           AssessmentYear,
                           F,
                           FMSY,
                           SSB,
                           MSYBtrigger,
                           catches,
                           landings,
                           discards)
        df2<-dplyr::full_join(df %>%
                                      dplyr::group_by(StockKeyLabel) %>%
                                      dplyr::filter(Year == AssessmentYear - 1) %>%
                                      dplyr::mutate(F_FMSY =  ifelse(!is.na(FMSY),
                                                                     F / FMSY,
                                                                     NA)) %>%
                                      dplyr::select(StockKeyLabel,
                                                    FisheriesGuild,
                                                    Ecoregion,
                                                    F_FMSY,
                                                    catches,
                                                    landings,
                                                    discards,
                                                    FMSY,
                                                    F),
                              df %>%
                                      dplyr::group_by(StockKeyLabel) %>%
                                      dplyr::filter(Year == AssessmentYear) %>%
                                      dplyr::mutate(SSB_MSYBtrigger = ifelse(!is.na(MSYBtrigger),
                                                                             SSB / MSYBtrigger,
                                                                             NA)) %>%
                                      dplyr::select(StockKeyLabel,
                                                    FisheriesGuild,
                                                    Ecoregion,
                                                    SSB_MSYBtrigger,
                                                    SSB,
                                                    MSYBtrigger)) %>%
                dplyr::mutate(Status = ifelse(is.na(F_FMSY) | is.na(SSB_MSYBtrigger),
                                              "GREY",
                                              if_else(F_FMSY < 1 & SSB_MSYBtrigger >= 1,
                                                      "GREEN",
                                                      "RED",
                                                      "GREY")))
        df2
}

current <- stockstatus_CLD_current(sag_complete)

#aggregating by ecoregion and according to presence or absence of F/FMSY and SSB/MSYBtrigger,
# we would get data for Figure 1

#Figure 2: Green: F/Fmsy and SSB/MSYBtrigger over 1
#               Orange: one of the two not green
#               Red: both under 1
#
##########################

####### Figure 3 ############


#This function is still not working properly, but you can follow step by step, its only at the end that
#needs tuning for this case


stock_trends <- function(x){
        df <- dplyr::mutate(x,FMEAN = mean(F, na.rm = TRUE),
                            SSBMEAN = mean(SSB, na.rm = TRUE),
                            FMEAN = ifelse(!grepl("F|F(ages 3-6)", fishingPressureDescription),
                                           NA,
                                           FMEAN),
                            SSBMEAN = ifelse(!grepl("SSB", stockSizeDescription),
                                             NA,
                                             SSBMEAN))
        df <- dplyr::mutate(df,F_FMSY = ifelse(!is.na(FMSY),
                                               F / FMSY,
                                               NA),
                            SSB_MSYBtrigger = ifelse(!is.na(MSYBtrigger),
                                                     SSB / MSYBtrigger,
                                                     NA))
        df <- dplyr::mutate(df,F_FMEAN = ifelse(!is.na(FMEAN),
                                                F / FMEAN, 
                                                NA),
                            SSB_SSBMEAN = ifelse(!is.na(SSBMEAN),
                                                 SSB / SSBMEAN,
                                                 NA))
        df<- dplyr::select(df,Year,
                           StockKeyLabel,
                           FisheriesGuild,
                           Ecoregion,
                           F_FMSY,
                           SSB_MSYBtrigger,
                           F_FMEAN,
                           SSB_SSBMEAN) 
        df2 <-tidyr::gather(df,Metric, Value, -Year, -Ecoregion, -StockKeyLabel, -FisheriesGuild) 
        df2 <- dplyr::filter(df2,!is.na(Year))
        
        df3 <- dplyr::group_by(df2,StockKeyLabel, Ecoregion, Metric, Year) %>%
                summarize(Value = mean(Value, na.rm = TRUE)) %>%
                select(StockKeyLabel,
                       Ecoregion,
                       Year,
                       Metric,
                       Value) %>%
                filter(!is.na(Value))
        
        means <- dplyr::group_by(df2,Ecoregion, Metric, Year) %>%
                summarize(Value = mean(Value, na.rm = TRUE),
                          StockKeyLabel = "MEAN") %>%
                select(StockKeyLabel,
                       Ecoregion,
                       Year,
                       Metric,
                       Value) %>%
                filter(!is.na(Value))
        
        df4 <- bind_rows(df3,means) %>%
                distinct(.keep_all = TRUE)
        df4
}

#we should get data for Figure 3
trends <- stock_trends(sag_complete)
