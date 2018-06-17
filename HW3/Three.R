library(engsoccerdata)
library(dplyr)
library(ggplot2)
library(highcharter)
library(tidyr)
library(kableExtra)
library(knitr)
#########
data(package="engsoccerdata") 
data("spain")
sleag = as.tbl(spain)

#making a team leag
rbind(
  sleag %>% select(Season, team = home, opp = visitor , MG = hgoal , OG = vgoal ,round) , 
  sleag %>% select(Season , team = visitor,opp = home, MG = vgoal , OG = hgoal  , round) 
) -> totalLeag
totalLeag %>% mutate(
  W = ifelse(MG - OG > 0,1,0),
  D = ifelse(MG == OG ,1,0),
  L = ifelse(MG - OG < 0,1,0)
) -> totalLeag
totalLeag %>% group_by(team , Season) %>% 
  summarise(zade = sum(MG) , khorde = sum(OG) , win = sum(MG > OG) , lose = sum(MG < OG) , equal = sum(MG == OG) , gameEachSeason = n() , point = win * 3 + equal) ->
  fullLeag

#one ,  تعداد قهرمانی های تیم ها در تاریخ لالیگا را استخراج کرده و نمودار ستونیآنها را رسم کنید. 
fullLeag %>% group_by(Season) %>%arrange(desc(zade - khorde)) %>%  mutate(rank = rank(-point) %>% as.integer()) -> fullLeag
fullLeag %>% group_by(Season) %>% slice(which.max(point)) -> WinnerEachSeason
WinnerEachSeason %>% group_by(team) %>% summarise(Wins = n()) %>% hchart(hcaes(x = team , y = Wins  ,group = team) , type = "column")
winnerPlot <- ggplot(data = WinnerEachSeason , aes(x = team))
winnerPlot + geom_histogram(stat = "count", aes(fill = team)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#two کسل کننده ترین لیگ و تیم را بیابید. نمودار ده تیم و ده فصل کسل کننده را رسم کنید. 

totalLeag %>% group_by(Season) %>% 
  summarise(goals = sum(MG) , games = n() / 2) -> leag_table
leag_table %>% mutate(mean_goals = round(goals / games, 3)) %>% arrange(mean_goals) %>% slice(1:10) -> boringLeague 
 boringLeague %>%  hchart(hcaes(x = as.character(Season) , y = mean_goals, group = as.character(Season)) , name = "boring leag" , type = "column")
boringLeaguePlot <- ggplot(data = boringLeague , aes(x = as.character(Season) , y = mean_goals))
boringLeaguePlot + geom_bar(stat = "identity", aes(fill = as.character(Season)))


fullLeag %>% group_by(team) %>% 
  summarise(totalGames = sum(gameEachSeason) , Eper = round(sum(equal) * 100/ totalGames )) %>% arrange(desc(Eper)) %>% slice(1:10) -> boringTeam
    boringTeam %>% hchart(hcaes(x = team , y = Eper, group = team), name = "boring teams" , type = "bar")
boringTeamPlot <- ggplot(data = boringTeam , aes(x = team , y = Eper))
boringTeamPlot + geom_bar(stat = "identity", aes(fill = team))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
#three در چند درصد مواد قهرمان نیم فصل در پایان فصل قهرمان شده است؟
totalLeag %>% mutate(
  W = ifelse(MG - OG > 0,1,0),
  D = ifelse(MG == OG ,1,0),
  L = ifelse(MG - OG < 0,1,0)
) -> league


league %>% 
  arrange(Season, team, opp) %>% 
  filter(row_number() %% 2 == 1) %>% 
  group_by(Season, team) %>% 
  summarise(point = sum(W * 3 + D)) %>% 
  group_by(Season) %>% 
  slice(which.max(point)) -> winnerEachHalfSeason

times <- sum(winnerEachHalfSeason$team == WinnerEachSeason$team)
print(round(100 * times / dim(winnerEachHalfSeason)[1], 2))


#four در بین سال های ۲۰۰۱ تا ۲۰۱۰ گربه سیاه تیم های بزرگ چه تیم هایی بوده است؟ 

  fullLeag %>% filter(Season >= 2001 & Season <= 2010) %>% arrange(Season) -> new_fullLeag
  new_fullLeag %>% 
  group_by(Season) %>% 
    mutate(rank = rank(-point) %>% as.integer()) -> new_fullLeag
  new_fullLeag %>% group_by(Season) %>% filter(rank <= 4) -> best_four_each_season
  new_sleag <- sleag[which(sleag$Season >= 2001 & sleag$Season <= 2010) ,]
  totalLeag %>% filter(Season >= 2001 & Season <= 2010) %>% arrange(Season) -> new_totalLeag
  best_four_each_season$blackCat <- "null"
  for(i in 0 : 9) {
    for(j in 1 : 4) {
      best_four_each_season %>% filter(Season == i + 2001) -> temp
      new_totalLeag %>% filter(as.character(team) == as.character(best_four_each_season[4 * i + j, 1]) & MG <= OG & !(as.character(opp) %in% temp$team), Season == i + 2001) -> new
      new %>% group_by(opp) %>% summarise(diff = sum(OG - MG) , number = n())  %>% arrange(desc(diff), desc(number)) -> new
    
      best_four_each_season[4 * i + j , "blackCat"] <- as.character(new[1, 1])
    }
  }
  best_four_each_season %>% select(team, Season, blackCat) -> best_four_each_season
  best_four_each_season %>% spread(team, blackCat) ->best_four_each_season
  best_four_each_season[is.na(best_four_each_season)] = ""
  kable(best_four_each_season, format = "html") %>%
    kable_styling("striped", full_width = F) %>%
    row_spec(0, angle = -45)
  
  
  #five ۵. در تاریخ لالیگا کدام تیم رکورددار زودترین قهرمانی است؟ همچنین کدام تیم مقتدرانه ترین قهرمانی را داشته است؟ 
  fullLeag %>% 
    group_by(Season) %>% 
    mutate(rank = rank(-point, zade - khorde) %>% as.integer()) -> fullLeag
    #moghtaderaneh tarin ghahremani 
    fullLeag %>% group_by(Season) %>% filter(point < max(point)) %>% group_by(Season) %>% 
      summarise(point = max(point)) -> SecondPlace
    WinnerEachSeason$secondplace = SecondPlace$point
    WinnerEachSeason %>% mutate(difference = point - secondplace) %>% arrange(desc(difference)) %>% 
      select(Season, team)
    
    
      
    #zood tarin ghahremani
      
    rbind(
      sleag %>% select(Date,Season, team = home, opp = visitor , MG = hgoal , OG = vgoal ,round) , 
      sleag %>% select(Date,Season , team = visitor,opp = home, MG = vgoal , OG = hgoal  , round) 
    ) -> dateLeague
    dateLeague %>% mutate(
      W = ifelse(MG - OG > 0,1,0),
      D = ifelse(MG == OG ,1,0),
      L = ifelse(MG - OG < 0,1,0)
    ) -> gamesWithDate
    gamesWithDate %>% group_by(Season, team) %>% mutate(gamesLeft = n() - row_number()) -> gamesWithDate
    gamesWithDate %>% select(Season, team , gamesLeft , W, D, L) -> gamesWithDate
    gamesWithDate %>% group_by(team, Season, gamesLeft) %>% summarise(point = 3 * sum(W) + sum(D)) ->gamesWithDate
    gamesWithDate %>% arrange(desc(gamesLeft)) %>%group_by(Season , team)  %>% mutate(point = cumsum(point)) %>% arrange(Season, desc(gamesLeft)) -> gamesWithDate
    gamesWithDate %>% group_by(Season, gamesLeft) %>% mutate(rank = rank(-point,ties.method = 'first' ) %>% as.integer()) -> gamesWithDate
    gamesWithDate %>% filter(rank == 1 | rank == 2) %>% group_by(Season, gamesLeft) %>% filter(max(point) - min(point) > 3 * gamesLeft & gamesLeft >= 1) -> earliestChampoin
      earliestChampoin %>% group_by() %>% filter(gamesLeft == max(gamesLeft)) -> earliestChampoin
  
    
    
    
    
    
    
    
      
    #six ۶. طولانی ترین نوار پیروزی مساوی و شکست مال چه تیم هایی است؟ 
    totalLeag %>% group_by(Season, team) %>% mutate(lastLose = ifelse(W == 1 , -1 , row_number()) , lastWin = ifelse(L == 1 , -1 , row_number()) , lastNotEqual = ifelse(D == 1, -1 , row_number()))  %>% 
      mutate(consecutiveWins = ifelse(W == 0 , 0 , row_number() - cummax(lastLose))
             , consecutiveLoses = ifelse(L == 0 , 0, row_number() - cummax(lastWin)), consecutiveEquals = 
               ifelse(D == 0 , 0 , row_number() - cummax(lastNotEqual)))  %>% group_by() -> longestWLD 
    longestWLD %>% filter(consecutiveWins == max(consecutiveWins)) %>% View()
    longestWLD %>% filter(consecutiveLoses == max(consecutiveLoses)) %>% View()
    longestWLD %>% filter(consecutiveEquals == max(consecutiveEquals)) %>% View()
     
    #seven زودترین سقوط مال کدام تیم بوده است؟ 
    rbind(
      sleag %>% select(Date,Season, team = home, opp = visitor , MG = hgoal , OG = vgoal ,round) , 
      sleag %>% select(Date,Season , team = visitor,opp = home, MG = vgoal , OG = hgoal  , round) 
    ) -> dateLeague
    dateLeague %>% mutate(
      W = ifelse(MG - OG > 0,1,0),
      D = ifelse(MG == OG ,1,0),
      L = ifelse(MG - OG < 0,1,0)
    ) -> gamesWithDate
    gamesWithDate %>% group_by(Season, team) %>% mutate(gamesLeft = n() - row_number()) -> gamesWithDate
    gamesWithDate %>% select(Season, team , gamesLeft , W, D, L) -> gamesWithDate
    gamesWithDate %>% group_by(team, Season, gamesLeft) %>% summarise(point = 3 * sum(W) + sum(D)) ->gamesWithDate
    gamesWithDate %>% group_by(Season , team)  %>%arrange(desc(gamesLeft)) %>% mutate(point = cumsum(point)) %>% arrange(Season, desc(gamesLeft)) -> gamesWithDate
    gamesWithDate %>% group_by(Season, gamesLeft) %>% mutate(rank = rank(-point,ties.method = 'first' ) %>% as.integer()) -> gamesWithDate
    gamesWithDate %>%  arrange(Season, desc(gamesLeft) , desc(rank)) %>% group_by(Season, gamesLeft) %>% mutate(reverseRank = n() - rank + 1) %>% 
      filter(reverseRank == 1 | reverseRank == 4) -> gamesWithDate
    gamesWithDate %>% group_by(Season , gamesLeft) %>% filter(max(point) - min(point) > 3 * gamesLeft & gamesLeft > 0) %>% group_by() %>% 
      filter(gamesLeft == max(gamesLeft)) %>% View()
    
    
    
  #eight مانند شکل بالا تصویری از روند تغییر رتبه تیم ها در طول فصل ۱۹۹۸رسم نمایید. 
    
    
    dateLeague %>% mutate(
      W = ifelse(MG - OG > 0,1,0),
      D = ifelse(MG == OG ,1,0),
      L = ifelse(MG - OG < 0,1,0)
    ) -> gamesWithDate
    gamesWithDate %>% group_by(team,Date, Season) %>% summarise(point = 3 * sum(W) + sum(D)) -> gamesWithDate
    gamesWithDate %>% 
      filter(Season == "1998") %>% 
      select(Date, team, point) %>% 
      spread(team,point) -> gamesWithDate
    gamesWithDate[is.na(gamesWithDate)] <- 0
    gamesWithDate %>% 
    tidyr::gather(colnames(gamesWithDate)[2:dim(gamesWithDate)[2]] , key = "team",value = "point") %>% arrange(Date) -> gamesWithDate
    gamesWithDate %>% group_by(team) %>% mutate(day = row_number()) -> gamesWithDate
    gamesWithDate %>% group_by(team) %>% mutate(pointSum = cumsum(point)) -> gamesWithDate
    gamesWithDate %>% group_by( Date) %>% mutate(rank = rank(-pointSum, ties.method = 'first') %>% as.integer()) -> gamesWithDate
    gamesWithDate %>% group_by(team) %>% mutate(weekend = day %% 7) %>% filter(weekend == 1) %>%  group_by(Date) -> ranking1998
      ranking1998 %>% hchart(hcaes(x = (Date), y = rank, group = team) , type = "line")  %>%
      hc_yAxis(reversed = TRUE)
     ranking1998Plot <- ggplot(data = ranking1998 , aes(x = (Date), y = rank))
     ranking1998Plot + geom_point(aes(color = team), size = 2)  + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + geom_line(aes(group = team, color = team)) + scale_y_continuous(trans = "reverse")
     
     
     
     
     #nine جدولی مشابه بالا برای فصل ۲۰۱۲ از کل نتایج طراحی کنید. 
     totalLeag %>% filter(Season == 2012) %>% select(team, opp, MG, OG) -> totalLeag_2012 
     totalLeag_2012 %>% select(team) %>% distinct() -> teams
     teams <- (sapply(teams, as.character))
     table = data.frame(teams = as.vector(teams))
     for(i in 1 : dim(teams)[1]) {
      
        for(j in 1 : dim(teams)[1]) {
            totalLeag_2012 %>% filter(as.character(team) == as.character(table[i , 1]) & as.character(opp) == as.character(teams[j])) -> match
          if(i < j) {
            
            table[i , j + 1] <- paste(as.character(match[1, 3]) , as.character(match[1, 4]) , sep = "-")
          }
          else if(i > j) {
            table[i , j + 1] <- paste(as.character(match[2, 3]) , as.character(match[2, 4]) , sep = "-")
             
          }
          else {
            table[i , j + 1] <- NA
          }
            
        }   
     }
     colnames(table) <- c("teams", teams)
     
     
     #ten  سه آماره به همراه نمودار فردوسی پسند استخراج کنید. 
      #bishtarin tedad gole zade shode tu in 80 sal
     
     
     #taghire rotbe barcelona
     fullLeag %>% filter(team == "FC Barcelona" | team == "Real Madrid") -> FC
     FC %>% select(Season , rank, team) %>% arrange(Season) -> FC
     FC %>% hchart(hcaes(x = Season , y = rank,group =team), type = "line") %>% hc_yAxis(reversed = TRUE)
     FCPlot <- ggplot(data = FC , aes(x = Season , y=  rank))
     FCPlot + geom_line(aes(color = team), size = 0.75)  + scale_y_continuous(trans = "reverse")
     
     
     #sangin tarin shekast
     totalLeag %>% filter(L == 1) %>% 
       group_by(Season, team,OG, MG) %>% summarise(diff = max(OG - MG)) %>% group_by(Season) %>% 
       slice(which.max(diff)) %>% arrange(desc(diff)) -> loseRecode
     loseRecode %>% hchart(hcaes(x = Season, y= diff, group = team), type = "column")
     
     
     loseRecodPlot = ggplot(data = loseRecode , aes(x = Season , y = diff))
     loseRecodPlot + geom_bar(stat = "identity", aes(fill = team)) + coord_flip()
     
     
     
     #
     sleag %>% select(Season, team = home, opp = visitor , MG = hgoal , OG = vgoal ,round) -> hplay
     sleag %>% select(Season, team = visitor, opp = home , MG = vgoal, OG = hgoal, round) -> vplay 
     hplay %>% group_by(team) %>% summarise(games = n() , hwins = sum(MG > OG)) -> hplay
     vplay %>% group_by(team) %>% summarise(games = n(), vwins = sum(MG < OG)) -> vplay
     full_join(hplay, vplay) -> fullJoint
     fullJoint %>% group_by(team) %>% mutate(hwins = round(hwins * 100 / games) , vwins = round(vwins * 100 / games)) -> fullJoint
     fullJoint %>% hchart(hcaes(x = team , y = hwins, group = team), type = "column")
     
     fulljointPlot <- ggplot(data = fullJoint , aes(x = team , y = hwins))
     fulljointPlot + geom_bar(color = "purple" ,  stat = "identity", aes(fill = team)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
     
     
     
     sleag %>% select(Season, team = home, opp = visitor , MG = hgoal , OG = vgoal ,round) -> hplay
     sleag %>% select(Season, team = visitor, opp = home , MG = vgoal, OG = hgoal, round) -> vplay 
     hplay %>% group_by(team) %>% summarise(games = n() , hwins = sum(MG > OG)) -> hplay
     vplay %>% group_by(team) %>% summarise(games = n(), vwins = sum(MG <= OG)) -> vplay
     full_join(hplay, vplay) -> fullJoint
     fullJoint %>% mutate(games = hwins + vwins) -> fullJoint
     fullJoint %>% group_by(team) %>% mutate(hwins = round(hwins * 100 / games) , vwins = round(vwins * 100 / games)) -> fullJoint
     n <- dim(fullJoint)[1]
     fullJoint %>% hchart(hcaes(x = team , y = hwins, group = team), type = "column")
     fullJoint %>% filter(hwins >= 0.5) -> fullJoint
     round(100 * dim(fullJoint)[1] / n)
     
     library(forcats)
     library(scales)
     sleag %>% filter(Season == 2012) -> test
     test$row = 0
     
     test %>% select(visitor) %>% unique() -> visitors
     
     test %>% mutate(row = which(visitor %in% visitors) %% 2) -> test
     test %>% mutate(result = paste(as.character(hgoal) , as.character(vgoal), sep = "-")) -> test
     test$result[is.na(test$result)] = "null"
     plot <- ggplot(data = test , aes(x = home , y = fct_rev(visitor))) 
     plot + geom_tile(aes(fill = home, width = 0.7 , height = 0.7, size = 10))+ geom_text(aes(label = test$result)) +
       theme(axis.text.x = element_text(angle = 90, hjust = 1)) +scale_y_discrete(expand = c(0, 0), position = "top") + 
       scale_x_discrete(position = "top", expand = c(0,0))  
      
     ggplot(test, aes(home, fct_rev(visitor), fill = as.vector(visitor))) + 
       geom_tile(colour = "gray20", size = 1.5, family = "bold", stat = "identity", height = 1, width = 1) + 
       geom_text(data = test, aes(home, visitor, label = FT), color = "black", size = rel(2)) +
       coord_flip() +
       scale_x_discrete(expand = c(0, 0)) +
       scale_y_discrete(expand = c(0, 0),position = "top") +
       xlab("") + 
       ylab("") +
       theme(
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.border = element_rect(fill = NA,color = "gray20", size = 0.5, linetype = "solid"),
         axis.line = element_blank(),
         axis.ticks = element_blank(), 
         axis.text = element_text(color = "white", size=rel(1)),
         panel.background = element_rect(fill="gray20"),
         plot.background = element_rect(fill="gray20"),
         legend.position = "none",
         axis.text.x  = element_text(angle= 90, vjust= 0.5, hjust=0)        
       ) 
     