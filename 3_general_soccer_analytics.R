



######################################################################################################
####################################Odds analysis
######################################################################################################

#test which odds that has performed

#https://www.football-data.co.uk/notes.txt


#Create a table with all. need to take out the common columns first. bind one at the time

###
### Testa lägg ut moving average på digfen för at tydligare se under/ over performance
###


###
#Odds accuracy
###

#analys över ingående odds. choose which league
odds_accuracy_data <- multiple_years_CS
###Choose H, A, D
Which_result <- "A"

if (Which_result == "H"){
  team <- "Hometeam"
}

if (Which_result == "A"){
  team <- "Awayteam"
}



#summera odds
#  test_odds <- test_odds%>%
#  mutate(odds_h = (B365H + BWH + IWH + PSH)/4,
#  odds_d = (B365D + BWD + IWD + PSD)/4,
#  odds_a = (B365A + BWA + IWA + PSA)/4)


##para ihop odds med tecken
odds_accuracy_data <- odds_accuracy_data%>%
  
  mutate(odds_w = if_else(FTR == "H", odds_h, if_else(FTR == "D", odds_d, odds_a)))%>%
  mutate(odds_lost = if_else(FTR =="H", odds_d, odds_h))%>%
  mutate(odds_lost2 = if_else(FTR =="A", odds_d, odds_a))%>%
  mutate(tecken_w = if_else(FTR == "H", "H", if_else(FTR == "D", "D", "A")))%>%
  mutate(tecken_lost = if_else(FTR =="H", "D", "H"))%>%
  mutate(tecken_lost2 = if_else(FTR =="A", "D", "A"))

#skapa tabell med de vann
test_odds_won <- odds_accuracy_data%>%
  select(
    odds = odds_w,
    tecken =tecken_w,
    league = league)%>%
  mutate(won = 1)

#skapa tabell med det odds som inte gick in
test_odds_lost1 <- odds_accuracy_data%>%
  select(
    odds = odds_lost,
    tecken = tecken_lost,
    league = league)%>%
  mutate(won = 0)

#skapa tabell med det odds som inte gick in x2
test_odds_lost2 <- odds_accuracy_data%>%
  select(
    odds = odds_lost2,
    tecken = tecken_lost2,
    league = league)%>%
  mutate(won = 0)

#bind ihop tabellerna
Combine_odds <- rbind(test_odds_won, test_odds_lost1, test_odds_lost2)

#mutera odds
Combine_odds <- Combine_odds%>%
  mutate(odds = round((odds), digits=2),
         obs= 1
  )



#lägg till att kunna välja liga

library(dplyr)

#group by and summarise odds
test_combine <- group_by(Combine_odds, odds, tecken, won) %>%
  summarise(sum_won = sum(obs))

#group by all odds. Min 4 observations
test_combine_2 <- group_by(Combine_odds, odds, tecken) %>%
  summarise(sum_all = sum(obs))%>%
  filter(sum_all >4)

#select home or away team
dt <- test_combine%>%
  filter(won == 1 & tecken == Which_result)%>%
  select(odds, sum_won)

#select home or away team
dt2 <- test_combine_2%>%
  filter(tecken == Which_result)

odds_accuracy_check <- full_join(dt2, dt, by = "odds")
odds_accuracy_check = subset(odds_accuracy_check, select = -c(tecken.x,tecken.y))

#calculate Win percentage
odds_accuracy_check <- odds_accuracy_check %>%
  mutate(
    win_perc = if_else(sum_won > 0, round(sum_won/sum_all, digits=2), 0),
    diff_vs_odds = if_else(sum_won > 0, win_perc - odds,0)
  )
#Drop NA                            
odds_accuracy_check <- odds_accuracy_check %>% drop_na()
#drop line with "Inf"
odds_accuracy_check <- odds_accuracy_check[!grepl("Inf", odds_accuracy_check$odds),]


#create moving average

odds_accuracy_check$MA_diff_vs_odds <- round((odds_accuracy_check$diff_vs_odds + lag(odds_accuracy_check$diff_vs_odds, 1)+ lag(odds_accuracy_check$diff_vs_odds,2))/3, digits=2)
#take away rows with na
odds_accuracy_check <-odds_accuracy_check %>%
  na.omit()		


my_graph_3 <- ggplot(data=odds_accuracy_check, aes(x=odds, y=MA_diff_vs_odds)) +
  geom_bar(stat="identity")+
  theme_classic()+
  scale_y_continuous(limits = c(-0.2, 0.3))+
  labs(title = "Probability (x) vs distribution (y) for ",
       subtitle = team,
       x = "Probablity for victory",
       y = "Difference expected outcome vs actual")+
  scale_x_continuous(breaks = seq(0, 1, by = 0.1))+
  theme(
    panel.grid.major.x = element_line(color = "grey80"),
    panel.grid.major.y = element_line(color = "grey80"),
    
    panel.grid.minor.y = element_line(color = "grey80"),
    panel.grid.minor.x = element_line(color = "grey80"),
    axis.text = element_text(size = 8), 
    axis.title = element_text(size = 8),
    plot.title = element_text(size = 8, face ="bold")
         )
my_graph_3

#http://zevross.com/blog/2014/08/04/beautiful-plotting-in-r-a-ggplot2-cheatsheet-3/





#####################################
##Check for each odds if there are any tilt to if it will be 1,x,2
#####################################

#############Nedan verkar funka, kanske ej väldigt snyggt

dt <- multiple_years_CS

###Drop rows with NA. problems is when 1 or more betting companys dosent put in a odds
dt <-dt %>%
  na.omit()		
#dim(dt)

agg <- matrix(data = 1:11, nrow = 0, ncol = 11) 

#x <- c(1:10)

#funkar ej loopa med decimaler


for(i in c(5:90)){
  
  i <- i / 100
  
  #i = 0.85
  
  #odds_check = i
  
  dt_2 <- dt %>%
    mutate(odds = i)%>%
    mutate(hometeam_odds_result_H = if_else(odds_h == i & FTR == "H", 1,0))%>%
    mutate(hometeam_odds_result_D = if_else(odds_h == i & FTR == "D", 1,0))%>%
    mutate(hometeam_odds_result_A = if_else(odds_h == i & FTR == "A", 1,0))%>%
    mutate(hometeam_odds_total = if_else(odds_h == i, 1,0))%>%
    
    mutate(awayteam_odds_result_H = if_else(odds_a == i & FTR == "H", 1,0))%>%
    mutate(awayteam_odds_result_D = if_else(odds_a == i & FTR == "D", 1,0))%>%
    mutate(awayteam_odds_result_A = if_else(odds_a == i & FTR == "A", 1,0))%>%
    mutate(awayteam_odds_total = if_else(odds_a == i, 1,0))%>%
    mutate(observations = 1)
  
  
  
  
  
  
  
  #dt_sum <- subset(dt, select=c(league, 49:58))
  dt_sum <- subset(dt_2, select=c(27:36))
  
  
  #agg1 <- c(mean(dt_sum$odds),colSums(dt_sum[,-1]))
  
  
  
  agg_test = aggregate(dt_sum,
                       by = list(dt_sum$odds),
                       FUN = sum)
  
  agg_test <- agg_test%>%
    mutate(odds = odds / observations)%>%
    mutate(hometeam_odds_result_H = round(hometeam_odds_result_H / hometeam_odds_total, digits=2))%>%
    mutate(hometeam_odds_result_D = round(hometeam_odds_result_D / hometeam_odds_total, digits=2))%>%
    mutate(hometeam_odds_result_A = round(hometeam_odds_result_A / hometeam_odds_total, digits=2))%>%
    mutate(awayteam_odds_result_H = round(awayteam_odds_result_H / awayteam_odds_total, digits=2))%>%
    mutate(awayteam_odds_result_D = round(awayteam_odds_result_D / awayteam_odds_total, digits=2))%>%
    mutate(awayteam_odds_result_A = round(awayteam_odds_result_A / awayteam_odds_total, digits=2))  
  
  #agg2 <- rbind(agg_test, agg_test)
  agg <- rbind(agg, agg_test)
  
  #agg <- rbind(agg, agg1)
}

###
#create a table with all reults in same table, with a column for 1,x,2
###
#Check odds probability vs distribution for hometeam
sum_agg_hometeam <- agg %>%
  select(odds, hometeam_odds_result_H)%>%
  mutate(result = hometeam_odds_result_H)%>%
  mutate(tecken = 1)%>%
  filter(odds > 0.10 & odds < 0.80)%>%
  select(odds,result, tecken)

sum_agg_hometeam_x <- agg %>%
  select(odds, hometeam_odds_result_D)%>%
  mutate(result = hometeam_odds_result_D)%>%
  mutate(tecken = "x")%>%
  filter(odds > 0.10 & odds < 0.80)%>%
  select(odds,result, tecken)

sum_agg_hometeam_2 <- agg %>%
  select(odds, hometeam_odds_result_A)%>%
  mutate(result = hometeam_odds_result_A)%>%
  mutate(tecken = 2)%>%
  filter(odds > 0.10 & odds < 0.80)%>%
  select(odds,result, tecken)

sum_agg_hometeam <- rbind(sum_agg_hometeam, sum_agg_hometeam_x, sum_agg_hometeam_2)


#http://www.sthda.com/english/wiki/ggplot2-scatter-plots-quick-start-guide-r-software-and-data-visualization
scatter_hometeam = ggplot(sum_agg_hometeam, aes(x=odds, y=result, shape=tecken, color = tecken))+
  geom_point()+
  geom_smooth()+
  theme_classic()+
  labs(title = "Probability (x) vs distribution (y) for hometeam. Expected outcome",
       x = "Probablity for hometeam victory",
       y = "Distribution")+
  theme(
    panel.grid.major.x = element_line(color = "grey80"),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor.x = element_line(color = "grey80"),
    panel.grid.minor.y = element_line(color = "grey80"),
    axis.text = element_text(size = 10), 
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 10, face ="bold")
  )

scatter_hometeam
  
#Check odds probability vs distribution for hometeam




###
#create a table with all reults in same table, with a column for 1,x,2
###
#Check odds probability vs distribution for hometeam
sum_agg_awayteam <- agg %>%
  select(odds, awayteam_odds_result_H)%>%
  mutate(result = awayteam_odds_result_H)%>%
  mutate(tecken = 1)%>%
  filter(odds > 0.10 & odds < 0.80)%>%
  select(odds,result, tecken)

sum_agg_awayteam_x <- agg %>%
  select(odds, awayteam_odds_result_D)%>%
  mutate(result = awayteam_odds_result_D)%>%
  mutate(tecken = "x")%>%
  filter(odds > 0.10 & odds < 0.80)%>%
  select(odds,result, tecken)

sum_agg_awayteam_2 <- agg %>%
  select(odds, awayteam_odds_result_A)%>%
  mutate(result = awayteam_odds_result_A)%>%
  mutate(tecken = 2)%>%
  filter(odds > 0.10 & odds < 0.80)%>%
  select(odds,result, tecken)

sum_agg_awayteam <- rbind(sum_agg_awayteam, sum_agg_awayteam_x, sum_agg_awayteam_2)


#http://www.sthda.com/english/wiki/ggplot2-scatter-plots-quick-start-guide-r-software-and-data-visualization
scatter_awayteam = ggplot(sum_agg_awayteam, aes(x=odds, y=result, shape=tecken, color = tecken))+
  geom_point()+
  geom_smooth()+
  theme_classic()+
  labs(title = "Probability (x) vs distribution (y) for awayteam. Expected outcome",
       subtitle = "testtest, subtitle",
       x = "Probablity for awayteam victory",
       y = "Distribution")+
  theme(
    panel.grid.major.x = element_line(color = "grey80"),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor.x = element_line(color = "grey80"),
    panel.grid.minor.y = element_line(color = "grey80"),
    axis.text = element_text(size = 10), 
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 10, face ="bold")
  )

scatter_awayteam

#Check odds probability vs distribution for hometeam











##TEST PL

#####################################
##Check for each odds if there are any tilt to if it will be 1,x,2
#####################################

#############Nedan verkar funka, kanske ej väldigt snyggt

dt <- multiple_years_PL

###Drop rows with NA. problems is when 1 or more betting companys dosent put in a odds
dt <-dt %>%
  na.omit()		
#dim(dt)

agg <- matrix(data = 1:11, nrow = 0, ncol = 11) 

#x <- c(1:10)

#funkar ej loopa med decimaler


for(i in c(5:90)){
  
  i <- i / 100
  
  #i = 0.85
  
  #odds_check = i
  
  dt_2 <- dt %>%
    mutate(odds = i)%>%
    mutate(hometeam_odds_result_H = if_else(odds_h == i & FTR == "H", 1,0))%>%
    mutate(hometeam_odds_result_D = if_else(odds_h == i & FTR == "D", 1,0))%>%
    mutate(hometeam_odds_result_A = if_else(odds_h == i & FTR == "A", 1,0))%>%
    mutate(hometeam_odds_total = if_else(odds_h == i, 1,0))%>%
    
    mutate(awayteam_odds_result_H = if_else(odds_a == i & FTR == "H", 1,0))%>%
    mutate(awayteam_odds_result_D = if_else(odds_a == i & FTR == "D", 1,0))%>%
    mutate(awayteam_odds_result_A = if_else(odds_a == i & FTR == "A", 1,0))%>%
    mutate(awayteam_odds_total = if_else(odds_a == i, 1,0))%>%
    mutate(observations = 1)
  
  
  
  
  
  
  
  #dt_sum <- subset(dt, select=c(league, 49:58))
  dt_sum <- subset(dt_2, select=c(27:36))
  
  
  #agg1 <- c(mean(dt_sum$odds),colSums(dt_sum[,-1]))
  
  
  
  agg_test = aggregate(dt_sum,
                       by = list(dt_sum$odds),
                       FUN = sum)
  
  agg_test <- agg_test%>%
    mutate(odds = odds / observations)%>%
    mutate(hometeam_odds_result_H = round(hometeam_odds_result_H / hometeam_odds_total, digits=2))%>%
    mutate(hometeam_odds_result_D = round(hometeam_odds_result_D / hometeam_odds_total, digits=2))%>%
    mutate(hometeam_odds_result_A = round(hometeam_odds_result_A / hometeam_odds_total, digits=2))%>%
    mutate(awayteam_odds_result_H = round(awayteam_odds_result_H / awayteam_odds_total, digits=2))%>%
    mutate(awayteam_odds_result_D = round(awayteam_odds_result_D / awayteam_odds_total, digits=2))%>%
    mutate(awayteam_odds_result_A = round(awayteam_odds_result_A / awayteam_odds_total, digits=2))  
  
  #agg2 <- rbind(agg_test, agg_test)
  agg <- rbind(agg, agg_test)
  
  #agg <- rbind(agg, agg1)
}

###
#create a table with all reults in same table, with a column for 1,x,2
###
#Check odds probability vs distribution for hometeam
sum_agg_hometeam <- agg %>%
  select(odds, hometeam_odds_result_H)%>%
  mutate(result = hometeam_odds_result_H)%>%
  mutate(tecken = 1)%>%
  filter(odds > 0.10 & odds < 0.80)%>%
  select(odds,result, tecken)

sum_agg_hometeam_x <- agg %>%
  select(odds, hometeam_odds_result_D)%>%
  mutate(result = hometeam_odds_result_D)%>%
  mutate(tecken = "x")%>%
  filter(odds > 0.10 & odds < 0.80)%>%
  select(odds,result, tecken)

sum_agg_hometeam_2 <- agg %>%
  select(odds, hometeam_odds_result_A)%>%
  mutate(result = hometeam_odds_result_A)%>%
  mutate(tecken = 2)%>%
  filter(odds > 0.10 & odds < 0.80)%>%
  select(odds,result, tecken)

sum_agg_hometeam <- rbind(sum_agg_hometeam, sum_agg_hometeam_x, sum_agg_hometeam_2)


#http://www.sthda.com/english/wiki/ggplot2-scatter-plots-quick-start-guide-r-software-and-data-visualization
scatter_hometeam_PL = ggplot(sum_agg_hometeam, aes(x=odds, y=result, shape=tecken, color = tecken))+
  geom_point()+
  geom_smooth()+
  theme_classic()+
  labs(title = "Probability (x) vs distribution (y) for hometeam. Expected outcome",
       x = "Probablity for hometeam victory",
       y = "Distribution")+
  theme(
    panel.grid.major.x = element_line(color = "grey80"),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor.x = element_line(color = "grey80"),
    panel.grid.minor.y = element_line(color = "grey80"),
    axis.text = element_text(size = 10), 
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 10, face ="bold")
  )

scatter_hometeam_PL

#Check odds probability vs distribution for hometeam




###
#create a table with all reults in same table, with a column for 1,x,2
###
#Check odds probability vs distribution for hometeam
sum_agg_awayteam <- agg %>%
  select(odds, awayteam_odds_result_H)%>%
  mutate(result = awayteam_odds_result_H)%>%
  mutate(tecken = 1)%>%
  filter(odds > 0.10 & odds < 0.80)%>%
  select(odds,result, tecken)

sum_agg_awayteam_x <- agg %>%
  select(odds, awayteam_odds_result_D)%>%
  mutate(result = awayteam_odds_result_D)%>%
  mutate(tecken = "x")%>%
  filter(odds > 0.10 & odds < 0.80)%>%
  select(odds,result, tecken)

sum_agg_awayteam_2 <- agg %>%
  select(odds, awayteam_odds_result_A)%>%
  mutate(result = awayteam_odds_result_A)%>%
  mutate(tecken = 2)%>%
  filter(odds > 0.10 & odds < 0.80)%>%
  select(odds,result, tecken)

sum_agg_awayteam <- rbind(sum_agg_awayteam, sum_agg_awayteam_x, sum_agg_awayteam_2)


#http://www.sthda.com/english/wiki/ggplot2-scatter-plots-quick-start-guide-r-software-and-data-visualization
scatter_awayteam_PL = ggplot(sum_agg_awayteam, aes(x=odds, y=result, shape=tecken, color = tecken))+
  geom_point()+
  geom_smooth()+
  theme_classic()+
  labs(title = "Probability (x) vs distribution (y) for awayteam. Expected outcome",
       x = "Probablity for awayteam victory",
       y = "Distribution")+
  theme(
    panel.grid.major.x = element_line(color = "grey80"),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor.x = element_line(color = "grey80"),
    panel.grid.minor.y = element_line(color = "grey80"),
    axis.text = element_text(size = 10), 
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 10, face ="bold")
  )

scatter_awayteam_PL


scatter_hometeam_PL
scatter_hometeam

scatter_awayteam_PL
scatter_awayteam










  
  ###########
  ## Check when odds over x percentagem what the probability of different results
  ######
  ##
  #dt <- subset(PL_2019, select=c(2:8, B365H, B365D, B365A))
  
  
  dt <- multiple_years_all_leagues
  
  odds_check <- 0.6
  
  dt <- dt %>%
    mutate(hometeam_over_x_h = if_else(odds_h > odds_check & FTR == "H", 1,0))%>%
    mutate(hometeam_over_x_d = if_else(odds_h > odds_check & FTR == "D", 1,0))%>%
    mutate(hometeam_over_x_a = if_else(odds_h > odds_check & FTR == "A", 1,0))%>%
    mutate(hometeam_over_x_total = if_else(odds_h > odds_check, 1,0))%>%
    
    mutate(awayteam_over_x_h = if_else(odds_a > odds_check & FTR == "H", 1,0))%>%
    mutate(awayteam_over_x_d = if_else(odds_a > odds_check & FTR == "D", 1,0))%>%
    mutate(awayteam_over_x_a = if_else(odds_a > odds_check & FTR == "A", 1,0))%>%
    mutate(awayteam_over_x_total = if_else(odds_a > odds_check, 1,0))%>%
    mutate(observations = 1)
  
  
  
  dt_sum <- subset(dt, select=c(league, 49:57))
  
  agg = aggregate(dt_sum, 
                  by = list(dt_sum$league),
                  FUN = sum)
  
  agg <- agg %>%
    mutate(hometeam_over_x_h = round(hometeam_over_x_h / hometeam_over_x_total, digits = 2))%>%
    mutate(hometeam_over_x_d = round(hometeam_over_x_d / hometeam_over_x_total, digits = 2))%>%
    mutate(hometeam_over_x_a = round(hometeam_over_x_a / hometeam_over_x_total, digits = 2))%>%
    mutate(awayteam_over_x_h = round(awayteam_over_x_h / awayteam_over_x_total, digits = 2))%>%
    mutate(awayteam_over_x_d = round(awayteam_over_x_d / awayteam_over_x_total, digits = 2))%>%
    mutate(awayteam_over_x_a = round(awayteam_over_x_a / awayteam_over_x_total, digits = 2))
  
  
  
  #Regel, hemmalag över 60% vinstchans garderas med x. Bortalag med över 60% vinstchans garderas med etta eller helgardering.
  
  
  
  #fdsfsdf
  
  
  ##testa a loopa
  
  
  
  #create matrix with y
  
  
  
  
  
  
  
  
  
  
  
  ###
  #Check expected goals model
  
  check_model <- stats_and_expected_goals%>%
    mutate(exGoals_FTR = if_else(exGoals_home > exGoals_away, "H", if_else(exGoals_home < exGoals_away, "A", "D")))%>%
    mutate(exGoals_LT_FTR = if_else(exGoals_home_LT_avg > exGoals_away_LT_avg, "H", if_else(exGoals_home_LT_avg < exGoals_away_LT_avg, "A", "D")))%>%
    mutate(exGoals_outcome = if_else(FTR == exGoals_FTR, 1, 0))%>%
    mutate(exGoals_LT_outcome = if_else(FTR == exGoals_LT_FTR, 1, 0))
  
  
  
  check_sum_agg <- group_by(check_model, HomeTeam) %>% 
  summarise(exGoals_home = sum(exGoals_outcome))

    check_sum_agg_4 <- group_by(check_model, AwayTeam) %>% 
    summarise(exGoals_away = sum(exGoals_LT_outcome))
    
    check_sum_agg <- cbind(check_sum_agg,check_sum_agg_4)
    
    check_sum_agg <- check_sum_agg%>%
      mutate(exgoals_ = exGoals_home + exGoals_away)%>%
      select(HomeTeam, exgoals_)
    
    
    check_sum_agg_lt <- group_by(check_model, HomeTeam) %>% 
      summarise(exGoals_LT_home = sum(exGoals_LT_outcome))
    
    check_sum_agg_lt_2 <- group_by(check_model, AwayTeam) %>% 
      summarise(exGoals_LT_away = sum(exGoals_LT_outcome))
    
    check_sum_agg_lt <- cbind(check_sum_agg_lt,check_sum_agg_lt_2)
    
    check_sum_agg_lt <- check_sum_agg_lt%>%
      mutate(exgoals_lt = exGoals_LT_home + exGoals_LT_away)%>%
      select(HomeTeam, exgoals_lt)
    
    
    check_sum_agg <- left_join(check_sum_agg, check_sum_agg_lt, by = "HomeTeam")    
    
    
    #Verkar som att LT är bäst att använda, funkar både på topplagen samt övriga. Givet man inte tror att Wolves kommer att fortsätta underprestera framför mål, samt tex chelsea fortsätta överprestera
  
  
  
  

