# The data for Figure 3 for the Mediterranean and Black Sea are directly taken
# from the latest STECF report on the CFP indicators update.
# Here we only aggregate the data for Figures 1 and 2, which refer to the 
# current status, as 2016, which is the latest year for which a higher number of
# assessments are available.

library(dplyr)
library(tidyr)
library(data.table)

# Dataframe with STECF and GFCM stock assessments between 2016 and 2018, from STECF report
data1 <- read.csv("STECF_CFP_2020.csv", header = T)
data1$key <- paste0(data1$Stock, data1$Area, "STECF")
data1$ID <- paste0(data1$Stock, data1$Area)

data2 <- read.csv("GFCM_SA_2020.csv", header = T)
data2$key <- paste0(data2$Stock, data2$Area, "GFCM")
data2$ID <- paste0(data2$Stock, data2$Area)

data <- rbind(data1,data2)
names(data)
unique(data$key)
# 44 stocks

df<- dplyr::select(data,Year,
                   key,
                   EcoRegion,
                   asses_year,
                   F_Fref,
                   B_Bref,
                   catch,
                   ID)

# take the terminal year of each stock, looks like last year we only used 2016 as the 
# year with more data, DAVE?

# df2<-dplyr::full_join(df %>% dplyr::group_by(key) %>%
#                               dplyr::filter(Year == 2016)%>%
#                               dplyr::select(key,
#                                             EcoRegion,
#                                             F_Fref,
#                                             catch, 
#                                             ID),
#                       df %>%
#                               dplyr::group_by(key) %>%
#                               dplyr::filter(Year == 2016) %>%
#                               dplyr::select(key,
#                                             EcoRegion,
#                                             B_Bref, 
#                                             ID))
# df2


df2 <- df%>% group_by(ID) %>% top_n(1, Year)
# Only two stocks have ref points of B


df2$color_fig1 <- case_when(df2$F_Fref != "NA" & df2$B_Bref != "NA" ~ "GREEN",
                            is.na(df2$F_Fref) & is.na(df2$B_Bref) ~"RED",
                            TRUE ~ "ORANGE")

df2$catch <- as.numeric(df2$catch)
df2$catch[is.na(df2$catch)] <- 0
#some problem here
# unique(df2$color_fig1)
figure1 <- df2 %>%
        group_by(EcoRegion, color_fig1) %>% 
        summarise(landings = sum(catch)) %>%
        ungroup() %>%
        spread(color_fig1, landings, fill=0)


# This list of stocks all have ref points, but there are other assessments 
# without ref points, I have to include them in figure1

# To be finished, with total number of assessed stocks and with GFCM landings
# http://www.fao.org/gfcm/data/capture-production

# I will use the full list of latest assessments downloaded from 
# https://stecf.jrc.ec.europa.eu/web/stecf/dd/medbs/sambs, the 27th August 2020
# We don´t have the full list of assessments from GFCM.

fullMed <- read.csv("STECF_full_database_27August2020.csv")
names(fullMed)
unique(fullMed$ï..Stockid)
#63
fullMed$Areaw <- gsub("SA ", "", fullMed$Areaw)
fullMed$Areaw <- gsub(" ", "_", fullMed$Areaw)
fullMed$ID <- paste0(fullMed$Stock, fullMed$Areaw)
unique(fullMed$ID)
names(df2)
unique(df2$ID)
new_assessments <- fullMed %>% filter(ID %in% df2$ID)
unique(new_assessments$ID)
# 38 only, there are 6 stocks not in this database

a <- unique(df2$ID)

b <- unique(new_assessments$ID)

c <- setdiff(a,b)
c

# unique(old_assessments$ID)

# they are new, so in total there are 69 stocks, 63 plus 6

old_assessments <- anti_join(fullMed, new_assessments)
unique(old_assessments$Areaw)

old_assessments$EcoRegion <- case_when(old_assessments$Areaw %in% c("01", "05","06", "07" ,"6","5", "9","1",
                                                    "09","01_05_06_07", "10", "11") ~ "Western Med.",
                                       old_assessments$Areaw %in% c("12_13_14_15_16","15_16","17_18",
                                                    "18_19","19","20", "16") ~ "Central Med.",
                                       old_assessments$Areaw %in% c("22_23", "25") ~ "Eastern Med.",
                                       old_assessments$Areaw %in% c("29") ~ "Black Sea")

old_assessments <- unique(old_assessments)

# I take the terminal year of catches for each stock
old_asses <- old_assessments%>% group_by(ID) %>% top_n(1, Year)

old_asses$Landings <- as.numeric(old_asses$Landings)

# assume those landings are about constant in time and put them as red in figure 1
# is there a better proxy of catches of these stocks?

old_asses$Landings <- as.numeric(old_asses$Landings)
old_asses$Landings[is.na(old_asses$Landings)] <- 0
red <-old_asses %>% group_by(EcoRegion) %>% summarise(sum(Landings))
red

figure1$RED <- c(0, 6523, 533, 2126)
gfcm <- read.csv("Med_catches2018_GFCM.csv")
figure1 <- left_join(figure1, gfcm)

write.csv(figure1, file = "CSI032_figure1MED&BS_update2020.csv")


#Note, 23 May 2019: DAVE?
# We can't consider B2003 a valid reference point for GES, so, all landings attributed
# to the GREEN in figure one will be moved to orange in the Mediterranean.




df2$color_fig2 <- case_when(df2$F_Fref < 1 & df2$B_Bref > 1 ~ "GREEN",
                                df2$F_Fref < 1 | df2$B_Bref > 1 ~ "ORANGE",
                                df2$F_Fref < 1 & is.na(df2$B_Bref) ~ "ORANGE",
                                is.na(df2$F_Fref) & df2$B_Bref > 1 ~ "ORANGE",
                                df2$F_Fref > 1 & df2$B_Bref < 1 ~ "RED",
                                is.na(df2$F_Fref) & is.na(df2$B_Bref) ~ "GREY",
                                TRUE ~ "RED")

# check <- unique(df2[c("color_fig2", "F_Fref", "B_Bref")])

figure2 <- df2 %>%
        group_by(EcoRegion, color_fig2) %>% 
        summarise(n= n()) %>%
        ungroup() %>%
        spread(color_fig2, n, fill=0)

DT <- data.table(df2)

n <- DT[, .(number_of_assessed_stocks = length(unique(key))), by = EcoRegion]

figure2 <- left_join(figure2, n)

#Total number of stocks 
datA <- df2 %>% select(c(ID, EcoRegion))
unique(datA)
names(fullMed)
datB <- fullMed %>% select(c(ID, F.Subname))
datB <- unique(datB)
bla <- datA %>% filter(datA$ID %in% datB$ID)
check <- fullMed %>% filter(ï..Stockid != df2$ID)
unique(check$ï..Stockid)
unique(fullMed$ï..Stockid)
tot <- left_join(fullMed, df2)
unique(tot$ï..Stockid)
write.csv(figure2, file = "CSI032_figure2MED&BS_update2020.csv")
