#SportsAnalytics

require(tidyverse)
library(dplyr)
library(lubridate)
library(data.table)
library(rmdformats)

right = function(text, num_char) {
  substr(text, nchar(text) - (num_char-1), nchar(text))
}

left = function(text, num_char) {
  substr(text, 1, num_char)
}

url_pl_1920 <- "https://www.football-data.co.uk/mmz4281/1920/E0.csv"
url_pl_1819 <- "https://www.football-data.co.uk/mmz4281/1819/E0.csv"
url_pl_1718 <- "https://www.football-data.co.uk/mmz4281/1718/E0.csv"
url_pl_1617 <- "https://www.football-data.co.uk/mmz4281/1617/E0.csv"
url_pl_1516 <- "https://www.football-data.co.uk/mmz4281/1516/E0.csv"


url_cs_1920 <- "https://www.football-data.co.uk/mmz4281/1920/E1.csv"
url_cs_1819 <- "https://www.football-data.co.uk/mmz4281/1819/E1.csv"
url_cs_1718 <- "https://www.football-data.co.uk/mmz4281/1718/E1.csv"
url_cs_1617 <- "https://www.football-data.co.uk/mmz4281/1617/E1.csv"
url_cs_1516 <- "https://www.football-data.co.uk/mmz4281/1516/E1.csv"

#HS = Home Team Shots
#AS = Away Team Shots
#HST = Home Team Shots on Target
#AST = Away Team Shots on Target

get_data <- function(url) {
  httpcache::GET(url) %>%
    httr::content()%>%
    mutate(season = left(right(url,11),4))%>%
    mutate(odds_h = 1/((B365H + BWH + IWH + PSH + WHH + VCH + PSCH)/7),
           odds_d = 1/((B365D + BWD + IWD + PSD + WHD + VCD + PSCD)/7),
           odds_a = 1/((B365A + BWA + IWA + PSA + WHA + VCA + PSCA)/7),
           sum_odds_factor = odds_h + odds_d +odds_a,
           odds_h = round(odds_h / sum_odds_factor, digits=2),
           odds_d = round(odds_d / sum_odds_factor,digits=2),
           odds_a = round(odds_a / sum_odds_factor,digits=2))%>%
    select(-c(B365H,B365D,B365A,BWH,BWD,BWA,IWH,IWD,IWA,PSH,PSD,PSA,WHH,WHA,WHD,VCH,VCD,VCA,PSCH,PSCD,PSCA, Referee, sum_odds_factor, HTHG, HTAG, HTR, HY, AY, HR,AR))
  
}


temp_data <- get_data(url_pl_1920)
full_table <- temp_data


temp_data <- get_data(url_pl_1819)
common_cols <- intersect(colnames(full_table), colnames(temp_data))
full_table <- rbind(subset(full_table, select = common_cols), 
                                    subset(temp_data, select = common_cols))

temp_data <- get_data(url_pl_1718)
full_table <- rbind(subset(full_table, select = common_cols), 
                    subset(temp_data, select = common_cols))
temp_data <- get_data(url_pl_1617)
full_table <- rbind(subset(full_table, select = common_cols), 
                    subset(temp_data, select = common_cols))
temp_data <- get_data(url_pl_1516)
full_table <- rbind(subset(full_table, select = common_cols), 
                    subset(temp_data, select = common_cols))
temp_data <- get_data(url_cs_1920)
full_table <- rbind(subset(full_table, select = common_cols), 
                    subset(temp_data, select = common_cols))
temp_data <-get_data(url_cs_1819)
full_table <- rbind(subset(full_table, select = common_cols), 
                    subset(temp_data, select = common_cols))
temp_data <-get_data(url_cs_1718)
full_table <- rbind(subset(full_table, select = common_cols), 
                    subset(temp_data, select = common_cols))
temp_data <-get_data(url_cs_1617)
full_table <- rbind(subset(full_table, select = common_cols), 
                    subset(temp_data, select = common_cols))
temp_data <-get_data(url_cs_1516)
full_table <- rbind(subset(full_table, select = common_cols), 
                    subset(temp_data, select = common_cols))


#Calculate Shots Convertion Rate
full_table <- full_table %>%
  mutate(
    HS_conversion_rate = round(FTHG / HS,2),
    AS_conversion_rate = round(FTAG / AS,2),
    HT_conversion_rate = round(FTHG / HST,2),
    AT_conversion_rate = round(FTAG / AST,2),
    WT_conversion_rate = round(if_else(FTR == "H" & FTR != "D", FTHG / HS, FTAG / AS),2), #Winning team: shots on target conversion rate
    LT_conversion_rate = round(if_else(FTR == "H" & FTR != "D", FTAG / AS, FTHG / HS),2),  #Losing team: shots on target conversion rate
    league = if_else(Div == "E0", "PL", "CS")
    )



#write.table(table_for_report, "...\\Operations\\LikvProg\\likvprog_history.txt", sep="\t")



######
######
#Odds accuracy
######
######

##para ihop odds med tecken
Odds_accuracy <- full_table%>%
  
  mutate(odds_w = if_else(FTR == "H", odds_h, if_else(FTR == "D", odds_d, odds_a)))%>%
  mutate(odds_lost = if_else(FTR =="H", odds_d, odds_h))%>%
  mutate(odds_lost2 = if_else(FTR =="A", odds_d, odds_a))%>%
  mutate(tecken_w = if_else(FTR == "H", "H", if_else(FTR == "D", "D", "A")))%>%
  mutate(tecken_lost = if_else(FTR =="H", "D", "H"))%>%
  mutate(tecken_lost2 = if_else(FTR =="A", "D", "A"))



#Create a dplyr table with correct and incorrect odds
Odds_accuracy_table <- Odds_accuracy%>%
  select(
    odds = odds_w,
    tecken =tecken_w,
    league = league)%>%
  mutate(correct = 1)

#temp table for first batch of incorrect odds
temp_data <- Odds_accuracy%>%
  select(
    odds = odds_lost,
    tecken = tecken_lost,
    league = league)%>%
  mutate(correct = 0)

Odds_accuracy_table <- rbind(Odds_accuracy_table, temp_data)

#temp table for second batch of incorrect odds
temp_data <- Odds_accuracy%>%
  select(
    odds = odds_lost2,
    tecken = tecken_lost2,
    league = league)%>%
  mutate(correct = 0)

Odds_accuracy_table <- rbind(Odds_accuracy_table, temp_data)

Odds_accuracy_table <- Odds_accuracy_table%>%
         mutate(obs= 1)
#Analyse

#League
#Odds_accuracy_PL <- Odds_accuracy_table%>%
#  filter(league =="PL")


#Sum correct per odds, tecken and league
temp_data <- Odds_accuracy_table%>%
  filter(correct == 1)%>%
  group_by(odds, tecken, league, correct) %>%
  summarise(sum_correct = sum(obs))

#Sum all observation per odds, tecken and league
temp_data2 <- Odds_accuracy_table%>%
  group_by(odds, tecken, league) %>%
  summarise(sum_observations = sum(obs))%>%
  filter(sum_observations >4)%>%
  filter(odds>0)


odds_accuracy_check <- left_join(temp_data2, temp_data, by = c("odds", "league", "tecken"), all.x = TRUE)%>%
  select(-correct)

odds_accuracy_check[is.na(odds_accuracy_check)] = 0

#calculate Win percentage
odds_accuracy_check <- odds_accuracy_check %>%
  mutate(
    percent_correct = if_else(sum_correct > 0, round(sum_correct/sum_observations, digits=2), 0),
    diff_vs_odds = if_else(sum_correct > 0, percent_correct - odds,0)
  )

odds_accuracy_check <- odds_accuracy_check[!grepl("Inf", odds_accuracy_check$odds),]



