rm(list=ls())
library(RCurl)
library(jsonlite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reticulate)
library(psych)
library(stringr)
library(rmarkdown)
library(knitr)
library(tidyverse)
library(dummies)
library(Matrix)
library(gmodels)
library(CatEncoders)
library(PerformanceAnalytics)








#그마,마스터,첼린저 유저들 불러와서 묶기
apikey <- "RGAPI-40d370e1-487f-4d46-86bc-943f44c14921"

master <- paste0("https://kr.api.riotgames.com/lol/league/v4/masterleagues/by-queue/RANKED_SOLO_5x5?api_key=",apikey)
grandmaster <- paste0("https://kr.api.riotgames.com/lol/league/v4/grandmasterleagues/by-queue/RANKED_SOLO_5x5?api_key=",apikey)
challenger <- paste0("https://kr.api.riotgames.com/lol/league/v4/challengerleagues/by-queue/RANKED_SOLO_5x5?api_key=",apikey)


league <- getURL(challenger)
league<-fromJSON(league)
league<-data.frame(league)

league2<- getURL(grandmaster)
league2<-fromJSON(league2)
league2<-data.frame(league2)

q<-rbind(league,league2)

league3<- getURL(master)
league3<-fromJSON(league3)
league3<-data.frame(league3)

q<-rbind(q,league3)
head(q)
str(q)
nrow(q)
#write.csv(league3,"qqqq.csv")




# league.to_csv("LeagueData.csv")
##################################################################################

#summonerid로 accountid찾아서 모으기     
#for(i in nrow(q))
m_league<-read.csv("masterleague.csv")
m_league<-m_league %>% filter(entries.summonerId != "#NAME?")
#q<-m_league$entries.summonerId[110]
accountid<-NULL
#nrow(m_league)373
for(i in c(350:373)) {
  api_url = paste0("https://kr.api.riotgames.com/lol/summoner/v4/summoners/",m_league$entries.summonerId[i],"?api_key=",apikey)
  url = getURL(api_url)
  url<-fromJSON(url)
  url<-data.frame(url)
  accountid<-rbind(accountid,url)
  url<-NULL
}

View(accountid)
str(accountid)
#write.csv(accountid,"master_account.csv")

####################################################################################
#accountid로 게임리스트불러오기
nrow(accountid)
match<-NULL
for(i in 351:373) {
  api_url = paste0("https://kr.api.riotgames.com/lol/match/v4/matchlists/by-account/",accountid$accountId[i],"?queue=420&season=13&api_key=",apikey)
  url = getURL(api_url)
  url<-fromJSON(url)
  url<-data.frame(url)
  match<-rbind(match,url)
}
View(match)
nrow(match)
#write.csv(match,"master_matchid.csv")
##########################################################
#gameid이용해서 게임내용 불러오기
match<-read.csv("master_matchid.csv")
View(match)
str(match)
m<-unique(match$matches.gameId)
length(m)

game<- NULL
p<-1

for(p in c(1:10)){
  ababab<-try(
for(i in 1:21064) {
  api_url=paste0("https://kr.api.riotgames.com/lol/match/v4/matches/",match$matches.gameId[i],"?api_key=",apikey)
  url<- getURL(api_url)
  j_url<-fromJSON(url)
  u_url<-unlist(j_url)
  m_url<-as.matrix(u_url)
  d_url<-data.frame(m_url)
  d_url2<-t(d_url)
  game<-merge(d_url2,game, all=TRUE)
}
,silent = T
)
  if(class(ababab)=="try-error"){
    Sys.sleep(120)
  }
}

View(d_url2)
View(game)


write.csv(rr,"master_gameid.csv",row.names=F)


#############################################


game=merge(game, d_url, all=TRUE)


###############################
#챔프번호 와 이름 매칭시키기

cham_no<-getURL("http://ddragon.leagueoflegends.com/cdn/10.25.1/data/en_US/champion.json")
champ<-fromJSON(cham_no)
champ<-champ[[4]]
View(champ)
c4<-NULL
for(i in 1:153) {
  c1<-champ[[i]][4]
  c2<-champ[[i]][3]
  c3<-cbind(c1,c2)
  c4<-rbind(c3,c4)
}
View(c4)
c5<-as.data.frame(c4)
View(c5)
colnames(c5)<-c("name","number")

write.csv(c5,"champ.csv")
c6<-as.matrix(c5)
write.csv(c6,"champ.csv")

############################################
#스펠json
#힐:key7
#텔포:key12
#정화:key1
#점화:14
#탈진:3
spell<-getURL("http://ddragon.leagueoflegends.com/cdn/11.1.1/data/en_US/summoner.json")
j_spell<-fromJSON(spell)
View(j_spell)




###########################################

y=바텀듀오승률
x=gameVersion 10~이상, (8)
participants(12)->stats(6) ->kda(10~12)
total damagedealt(22)
visionScore(36)
totaldamagetaken(38)
goldEarned(42)
visionwardsbought(52)
pariticipants->timeline ->(5)(6)(8)#자료들마다 칼럼구성이 달라 뺌

#######################################
game<- NULL

for(i in 1:3) {
  api_url=paste0("https://kr.api.riotgames.com/lol/match/v4/matches/",match$matches.gameId[i],"?api_key=",apikey)
  url<- getURL(api_url)
  j_url<-fromJSON(url)
  a0<-j_url[8]
  a1<-j_url[[12]][[6]][10]
  a2<-j_url[[12]][[6]][11]
  a3<-j_url[[12]][[6]][12]
  a4<-j_url[[12]][[6]][22]
  a5<-j_url[[12]][[6]][36]
  a6<-j_url[[12]][[6]][38]
  a7<-j_url[[12]][[6]][42]
  a8<-j_url[[12]][[6]][52]
 
  
  bot<-cbind(a0,a1,a2,a3,a4,a5,a6,a7,a8)
  
  
  m_url<-as.matrix(bot)
  d_url<-data.frame(m_url)
  
  game<-merge(game, d_url, all=TRUE)
}
View(j_url)
View(game)
class(j_url)

####################################################
###################################################


cm<-read.csv("champ.csv")#챔프이름,번호 csv

c1<-read.csv("master_gameid.csv")#gameid있는 csv
View(c1)
str(c1)
c1<-c1 %>% filter(gameDuration >= 1200)
d1<-unique(c1$gameId)
length(d1)
#mat_id<-d1[3]

j=1
k=1
game<-NULL

p<-1

for(p in c(1:10)){
  ababab<-try(
for(k in 1:20){
  mat_id<-d1[k]
  df_lane<-NULL
  bot<-NULL
  
  #게임내용관련데이터 칼럼불러오기
  RoitURL_mat <- paste0("https://kr.api.riotgames.com/lol/match/v4/matches/",mat_id)#,matches.gameId
  url_mat <-paste0(RoitURL_mat, paste0("?api_key=", apikey))
  j_mat<-fromJSON(getURL(url_mat))
  
  # un_mat<-unlist(j_mat)
  # m_mat<-as.matrix(un_mat)
  # df_mat<-as.data.frame(m_mat)
  # df_mat<-t(df_mat)
  # df_mat<-as.data.frame(df_mat)
  
  #좌표관련데이터 칼럼불러오기
  RoitURL_mat_lane <- paste0("https://kr.api.riotgames.com/lol/match/v4/timelines/by-match/",mat_id)#,matches.gameId
  url_mat_lane <-paste0(RoitURL_mat_lane, paste0("?api_key=", apikey))
  j_mat_lane<-fromJSON(getURL(url_mat_lane))
  
  
  a99<-j_mat[["gameId"]]
  a0<-j_mat[["gameVersion"]]
  a1<-j_mat[[12]][[6]][10]
  a2<-j_mat[[12]][[6]][11]
  a3<-j_mat[[12]][[6]][12]
  a4<-j_mat[[12]][[6]][22]
  a5<-j_mat[[12]][[6]][36]
  a6<-j_mat[[12]][[6]][38]
  a7<-j_mat[[12]][[6]][42]
  a8<-j_mat[[12]][[6]][52]
  a9<-j_mat[["participants"]][["stats"]][["win"]]
  a10<-j_mat[["participants"]][["championId"]]
  a12<-j_mat[["participants"]][["stats"]][["firstBloodKill"]]
  a13<-j_mat[["participants"]][["teamId"]]
  a14<-j_mat[["teams"]][["firstDragon"]]
  a15<-j_mat[["participants"]][["stats"]][["firstTowerKill"]]

  a11<-NULL
  for (c in c(1:10)){
    qwer<-cm %>% filter(number == a10[c]) %>% select(name)
    a11<-rbind(a11,qwer)  
    qwer<-NULL
  } #챔프id로 챔프목록csv연결해서 name칼럼 생성
  
 
  
               
  j=1
  for(j in c(1:10)){
    
    a<-j_mat_lane[["frames"]]
    z1<-NULL
    z1$x<-a$participantFrames[,j]$position[,1]
    z1$y<-a$participantFrames[,j]$position[,2]
    z1<-as.data.frame(z1)
    z1<-head(z1,15)
    z1<-z1[-1,]
    
    i=1
    TOP<-0
    JUNGLE<-0
    MID<-0
    BOT_CARRY<-0
    SUP<-0
    lane<-data.frame(TOP,JUNGLE,MID,BOT_CARRY,SUP)
  #   install.packages("png")
  #   library(png)
  #   m<-readPNG("map11.png")
  #   class(m)
  #   dim(m)
  #   m<-as.raster(m[,,1:3])
  #   class(m)
  #   dim(m)
  # plot(-120:15000, type='n')
  # m<-rasterImage(as.raster(m[,,]), -120, -120, 15000, 15000, interpolate=FALSE)
  # plot(m)
  # z1 %>% ggplot(aes(x=x, y=y)) + geom_point()+xlim(-120,14870)+ylim(-120,14980)
    
    if (j_mat[["participants"]][["spell1Id"]][j]==11 |
        j_mat[["participants"]][["spell2Id"]][j]==11 ){
      lane$JUNGLE<-lane$JUNGLE+50 #강타
      lane$BOT_CARRY<-lane$BOT_CARRY-50
      lane$SUP<-lane$SUP-100
    }else if (j_mat[["participants"]][["stats"]][["visionScore"]][j]==max(j_mat[["participants"]][["stats"]][["visionScore"]][c(1:5)]) |
              j_mat[["participants"]][["stats"]][["visionScore"]][j]==max(j_mat[["participants"]][["stats"]][["visionScore"]][c(6:10)])){
      lane$SUP<-lane$SUP+50#시야점수
      lane$BOT_CARRY<-lane$BOT_CARRY-50   
    } 
    if (j_mat[["participants"]][["stats"]][["goldEarned"]][j]==min(j_mat[["participants"]][["stats"]][["goldEarned"]][c(1:5)]) |
             j_mat[["participants"]][["stats"]][["goldEarned"]][j]==min(j_mat[["participants"]][["stats"]][["goldEarned"]][c(6:10)])){
      lane$SUP<-lane$SUP+30#시야점수
      lane$BOT_CARRY<-lane$BOT_CARRY-50   
    }
    
    
    if (j_mat[["participants"]][["spell1Id"]][j]==12 |
        j_mat[["participants"]][["spell2Id"]][j]==12 ){
      lane$BOT_CARRY<-lane$BOT_CARRY-7#텔포
    }
    if (j_mat[["participants"]][["spell1Id"]][j]==7 |
        j_mat[["participants"]][["spell2Id"]][j]==7 ){
      lane$BOT_CARRY<-lane$BOT_CARRY+6#힐
    }
    if (j_mat[["participants"]][["spell1Id"]][j]==1 |
        j_mat[["participants"]][["spell2Id"]][j]==1 ){
      lane$BOT_CARRY<-lane$BOT_CARRY+2#클린즈
    }
    if (j_mat[["participants"]][["spell1Id"]][j]!=4 |
         j_mat[["participants"]][["spell2Id"]][j]!=4 ){
      lane$BOT_CARRY<-lane$BOT_CARRY-10#점멸
    }
    if (j_mat[["participants"]][["spell1Id"]][j]==14 |
        j_mat[["participants"]][["spell2Id"]][j]==14 ){
      lane$BOT_CARRY<-lane$BOT_CARRY-8#점화
    }
    if (j_mat[["participants"]][["spell1Id"]][j]==3 |
        j_mat[["participants"]][["spell2Id"]][j]==3 ){
      lane$BOT_CARRY<-lane$BOT_CARRY-8#탈진
    }
    
    if (j_mat[["participants"]][["championId"]][j] == 145 |
        j_mat[["participants"]][["championId"]][j] == 202 |
        j_mat[["participants"]][["championId"]][j] == 360 |
        j_mat[["participants"]][["championId"]][j] == 21 |
        j_mat[["participants"]][["championId"]][j] == 22 |
        j_mat[["participants"]][["championId"]][j] == 523 |
        j_mat[["participants"]][["championId"]][j] == 81 |
        j_mat[["participants"]][["championId"]][j] == 119 |
        j_mat[["participants"]][["championId"]][j] == 51 |
        j_mat[["participants"]][["championId"]][j] == 429 |
        j_mat[["participants"]][["championId"]][j] == 29 |
        j_mat[["participants"]][["championId"]][j] == 222 |
        j_mat[["participants"]][["championId"]][j] == 498 |
        j_mat[["participants"]][["championId"]][j] == 110 |
        j_mat[["participants"]][["championId"]][j] == 96 ){
      lane$BOT_CARRY<-lane$BOT_CARRY+8#챔프 가산점
    }
    
    if (j_mat[["participants"]][["championId"]][j] == 67 &
        j_mat[["participants"]][["spell1Id"]][j]==7 |
        j_mat[["participants"]][["spell2Id"]][j]==7
        ){
      lane$BOT_CARRY<-lane$BOT_CARRY+6#베인
    }
    
    if (j_mat[["participants"]][["championId"]][j] == 236 &
        j_mat[["participants"]][["spell1Id"]][j]==7 |
        j_mat[["participants"]][["spell2Id"]][j]==7){
      lane$BOT_CARRY<-lane$BOT_CARRY+6#루시안
    }
    
    if (j_mat[["participants"]][["championId"]][j] == 18 &
        j_mat[["participants"]][["spell1Id"]][j]==7 |
        j_mat[["participants"]][["spell2Id"]][j]==7 ){
      lane$BOT_CARRY<-lane$BOT_CARRY+6#트타
    }
    
    if (j_mat[["participants"]][["championId"]][j] == 235 &
        j_mat[["participants"]][["spell1Id"]][j]==7 |
        j_mat[["participants"]][["spell2Id"]][j]==7){
      lane$BOT_CARRY<-lane$BOT_CARRY+6#세나
    }
    
    
    for(i in c(1:11)){
      if (z1$x[i]>=600 & z1$x[i]<=1800 &
          z1$y[i]>=8400 & z1$y[i]<=14100) {
        lane$TOP<-lane$TOP+1
      }
      else if (z1$x[i]>=600 & z1$x[i]<=4500 &
               z1$y[i]>=10200 & z1$y[i]<=14400) {
        lane$TOP<-lane$TOP+3
      }
      else if (z1$x[i]>=900 & z1$x[i]<=6000 &
               z1$y[i]>=13200 & z1$y[i]<=14400) {
        lane$TOP<-lane$TOP+1
      }
      else if (z1$x[i]>=4850 & z1$x[i]<=10000 &
               z1$y[i]>=4850 & z1$y[i]<=12000)  {
        lane$MID<-lane$MID+1
      }
      else if (z1$x[i]>=5600 & z1$x[i]<=8850 &
               z1$y[i]>=6000 & z1$y[i]<=8700) {
        lane$MID<-lane$MID+3
      }
      
      else if (z1$x[i]>=6500 & z1$x[i]<=9600 &
               z1$y[i]>=7000 & z1$y[i]<=9300){
        lane$MID<-lane$MID+1
      }
      else if (z1$x[i]>=3800 & z1$x[i]<=6600 &
               z1$y[i]>=4500 & z1$y[i]<=6700){
        lane$MID<-lane$MID+1
      }
      else if (z1$x[i]>=5000 & z1$x[i]<=10000 &
               z1$y[i]>=200 & z1$y[i]<=15000){
        lane$BOT_CARRY<-lane$BOT_CARRY+1
      }
      else if (z1$x[i]>=10000 & z1$x[i]<=14700&
               z1$y[i]>=600 & z1$y[i]<=5000){
        lane$BOT_CARRY<-lane$BOT_CARRY+1
      }
      else if (z1$x[i]>=5000 & z1$x[i]<=14700&
               z1$y[i]>=1350 & z1$y[i]<=14700) {
        lane$BOT_CARRY<-lane$BOT_CARRY+1
      }
      else{
        lane$MID<-lane$MID+1
      }
      
    } #for (i in 1:15)
   df_lane<-rbind(df_lane,lane)
   
  } #for (j in 1:10)
  
  bot<-cbind(a99,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a15,df_lane)
  
  bot$F_DRG[1:5]<-j_mat[["teams"]][["firstDragon"]][1]
  bot$F_DRG[6:10]<-j_mat[["teams"]][["firstDragon"]][2]
  
  bot_1<-bot[1:5,]
  bot_2<-bot[6:10,]
  bot_1<-bot_1 %>% 
    mutate(AD=bot_1[which.max(bot_1$BOT_CARRY),"name"],SUPP=bot_1[which.max(bot_1$SUP),"name"]) %>% 
    filter(bot_1$name == bot_1[which.max(bot_1$BOT_CARRY),"name"] | bot_1$name == bot_1[which.max(bot_1$SUP),"name"]) %>% 
    arrange(desc(goldEarned)) %>%
    mutate(s_kills=bot_1$kills[2],s_deaths=bot_1$deaths[3],s_assists=bot_1$assists[4]) %>%
    rename(ad_num=a10, ad_kills=kills, ad_deaths=deaths, ad_assists=assists, gameid=a99, result=a9, First_kill=a12, team=a13,First_tkill=a15) %>%
    mutate(
      Damage=(totalDamageDealt[1]+totalDamageDealt[2])/2,
      visionScore_2=(visionScore[1]+visionScore[2])/2,
      DamageTaken=(totalDamageTaken[1]+totalDamageTaken[2])/2,
      goldEarned_2=(goldEarned[1]+goldEarned[2])/2,
      visionWardsBoughtInGame_2=(visionWardsBoughtInGame[1]+visionWardsBoughtInGame[2])/2) %>%
      mutate(sup_num=ad_num[2]) %>%
   select(gameid, team, result, F_DRG, First_kill, First_tkill, ad_num, sup_num, AD, SUPP, ad_kills, ad_deaths, ad_assists, s_kills, s_deaths, s_assists, goldEarned_2, Damage, DamageTaken, visionScore_2, visionWardsBoughtInGame_2) %>%
   head(1)
   
  
  bot_2<-bot_2 %>% 
    mutate(AD=bot_2[which.max(bot_2$BOT_CARRY),"name"],SUPP=bot_2[which.max(bot_2$SUP),"name"]) %>% 
    filter(bot_2$name == bot_2[which.max(bot_2$BOT_CARRY),"name"] | bot_2$name == bot_2[which.max(bot_2$SUP),"name"]) %>% 
    arrange(desc(goldEarned)) %>%
    mutate(s_kills=bot_2$kills[2],s_deaths=bot_2$deaths[3],s_assists=bot_2$assists[4]) %>%
    rename(ad_num=a10, ad_kills=kills, ad_deaths=deaths, ad_assists=assists, gameid=a99, result=a9, First_kill=a12, team=a13,First_tkill=a15) %>%
    mutate(
      Damage=(totalDamageDealt[1]+totalDamageDealt[2])/2,
      visionScore_2=(visionScore[1]+visionScore[2])/2,
      DamageTaken=(totalDamageTaken[1]+totalDamageTaken[2])/2,
      goldEarned_2=(goldEarned[1]+goldEarned[2])/2,
      visionWardsBoughtInGame_2=(visionWardsBoughtInGame[1]+visionWardsBoughtInGame[2])/2) %>%
    mutate(sup_num=ad_num[2]) %>%
    select(gameid, team, result,F_DRG, First_kill, First_tkill, ad_num, sup_num, AD, SUPP, ad_kills, ad_deaths, ad_assists, s_kills, s_deaths, s_assists, goldEarned_2, Damage, DamageTaken, visionScore_2, visionWardsBoughtInGame_2) %>%
    head(1)
  
  
  rr<-df_lane[1:5,] %>% filter(BOT_CARRY == max(BOT_CARRY) | SUP == max(SUP))
  rr_2<-df_lane[6:10,] %>% filter(BOT_CARRY == max(BOT_CARRY) | SUP == max(SUP))
  if(nrow(rr) == 2 & nrow(rr_2) == 2){
    game<-rbind(game,bot_1)
    game<-rbind(game,bot_2)
  }else {
  }
  
  
  rr<-NULL
  rr_2<-NULL
  
} # for (k in 1:10)
,silent = T
)
  if(class(ababab)=="try-error"){
    Sys.sleep(120)
  }
}


##############
View(game) 
str(game)

write.csv(game,"game30.csv",row.names = F)
##############################################################
#

game2<-read.csv("game1.csv")
head(game2,20)
View(game2)
##TRUE,FALSE -> 1,0 으로 교체후 숫자형으로 변환
game2$Win<-str_replace_all(game2$Win,"TRUE","1")
game2$Win<-str_replace_all(game2$Win,"FALSE","0")
game2$Win<-as.factor(game2$Win)

game2$FBK<-str_replace_all(game2$FBK,"TRUE","1")
game2$FBK<-str_replace_all(game2$FBK,"FALSE","0")
game2$FBK<-as.factor(game2$FBK)

game2$DRG<-str_replace_all(game2$DRG,"TRUE","1")
game2$DRG<-str_replace_all(game2$DRG,"FALSE","0")
game2$DRG<-as.factor(game2$DRG)

game2$Team<-as.factor(game2$Team)
str(game2)
game2 <- game2 %>% rename(gameid=a99)


################################
#챔프들 라벨인코딩
factors<-factor(game2$name)
factors<-as.numeric(factors)
game2$label <- factors
game2<-game2 %>% select(-X,-gameid,-ADC,-SUP)
str(game2)
View(game2)
write.csv(game2,"game2.csv")
####################
#수치형만 추출해서 분포 보기
str(game2)
game3<-game2[,6:17]
cor(game3)
pairs.panels(game3)
chart.Correlation(game3, histogram=TRUE, pch=19)
library(corrplot)
corrplot(cor(game3,use="na.or.complete"),method="pie")
CrossTable(acs$sex,acs$obesity,chisq=T,prop.t=F)

#막대그래프 그려보기
#조합별 승률
game2<-read.csv("game2.csv")
game2<-game2 %>% select(-X)
str(game2)
View(game2)
game2$Win<-as.numeric(game2$Win)

View(game2)
#챔프조합별 고승률상위10개
 
  
game2 %>%
  group_by(name) %>%
  count(Win) %>% arrange(name,Win) %>% mutate(sum=sum(n)) %>% filter(Win==1) %>%
  mutate(w=round(n/sum*100,0)) %>%
  arrange(desc(w)) %>% filter(sum >= 100) %>%
  head(10) %>% 
  ggplot(aes(x=name,y=w,fill=factor(name))) +
  geom_text(aes(label=w),
            position=position_stack(1.1),
            size=4) +
  geom_bar(stat="identity",position = "dodge")

#
game2 %>%
  group_by(Win) %>%  
  ggplot(aes(x=Win,y=goldEarned_mean,fill=factor(Win))) + geom_boxplot()





################################################
#머신러닝
game2<-read.csv("game2.csv")

View(game2)
str(game2)
#############################
game2<-game2[,-1]
game3<-game2[,c(-1,-13)]
game4<-game2[,c(-1,-c(7:12))]

str(game2)
str(game3)
str(game4)

library(randomForest)
n<-nrow(game2)
set.seed(1234)
index<- sample(x=n,size=n*0.7,replace = F)
trainSet<- game2 %>% slice(index)
testSet <- game2 %>% slice(-index)
trainSet$Win %>% table() %>% prop.table() %>% round(4) * 100
testSet$Win %>% table() %>% prop.table() %>% round(4) * 100




set.seed(1234)
fit1<-randomForest(formula = Win ~.,
                   data=trainSet,
                   ntree = 1000,
                   mtry = 3,
                   importance = TRUE,
                   do.trace = 50,
                   keep.forest = TRUE)

print(fit1)
print(fit1$err.rate)

#plot(fit1) # OOB어쩌구하는데 뭔지 잘모르겠음

importance(fit1,1)
# varImpPlot(fit1,main='Variable Importance', type=1) #변수들의 중요도

#랜덤 포레스트 분류모형을 적합할 때 사용된 개별 나무모형의 끝마디 수를 히스토그램으로 그림
# fit1 %>%
#   treesize(terminal=T) %>%
#   hist(main='Terminal Nodes')

real<-testSet$Win
pred1<-predict(fit1,testSet,type='response')
table(pred1,real)
prob1<-predict(fit1,testSet,type='vote')[,2]
#혼동행렬 출력
library(caret)
confusionMatrix(pred1,reference = real,positive='1')
#F1점수 출력
library(MLmetrics)
F1_Score(y_true=real,y_pred=pred1,positive = '1')

#ROC곡선 그리기
# library(pROC)
# roc(response=real,predictor = prob1) %>%
#   plot(main='ROC curve', col='red',lty=1)


#그리드 생성
grid<- expand.grid(ntree=c(300,500,700,1000),
                   mtry=c(3,4,5,6,7),
                   error=NA)

print(grid)

#반복문을 사용한 모형 튜닝
n<-nrow(grid)
for(i in c(1:n)) {
  ntree<-grid$ntree[i]
  mtry<-grid$mtry[i]
  disp<-str_glue('현재{i}행 실행 중! [ntree: {ntree}, mtry: {mtry}]')
  cat(disp,'\n\n')
  set.seed(1234)
  fit<-randomForest(formula=Win~.,
                    data=trainSet,
                    ntree=ntree,
                    mtry=mtry)
  grid$error[i]<-tail(fit$err.rate[,1],n=1)
}
#튜닝결과 선그래프 시각화
# plot(x=grid$error,type='b',
#      pch=19,col='gray30',
#      main='Grid Search Result')

#oob 오차의 최소값으로 빨간 수평선을 선그래프에 추가
#abline(h=min(x=grid$error),col='red',lty=2)

#OOB 오차가 최소인 행번호를 확인
loc<- which.min(x=grid$error)
print(loc)

#OOB 오차가 최소일 때의 하이퍼 파라미터 조합을 설정
bestPara<-grid[loc,]
print(bestPara)

#OOB 오차가 최소일 때의 하이퍼 파라미터 조합으로 최적 모형을 적합
set.seed(1234)
best<-randomForest(formula=Win~.,
                   data=trainSet,
                   ntree=bestPara$ntree,
                   mtry=bestPara$mtry,
                   importance=T)
print(best)

#최적 모형 적합 결과를 그래프로 출력
# plot(best,main = 'Best Fit')

#변수의 중요도 테이블을 출력
importance(best,type=1)

#변수의 중요도를 시각화
# varImpPlot(x=best,main='Variable Importance', type=1)

#시험셋으로 목표변수의 추정값을 생성
pred2<- predict(object=best, newdata = testSet, type='response')

#혼동행렬을 출력
confusionMatrix(data=pred2,reference = real, positive = '1')

#F1 점수를 출력
F1_Score(y_true = real,y_pred=pred2, positive = '1')

#시험셋으로 목표변수의 추정확률을 생성
prob2<- predict(best,newdata=testSet,type='vote')[, 2]

#추정확률로ROC곡선을 그림
#1.봇kda,kda_mean(green)
#2.봇kda(blue)
#3.kda_mean(red)
roc(response = real, predictor = prob2) %>% plot(main = 'ROC curve',col='red',lty = 1)

auc(response=real,predictor = prob2)

###################################################
df_full<-read.csv("df_full.csv")

str(df_full)
df_full<-df_full[,-1]
df_full$Team<-as.factor(df_full$Team)
df_full$Win<-as.integer(df_full$Win)
df_full$Win<-as.factor(df_full$Win)
df_full$FBK<-as.integer(df_full$FBK)
df_full$FBK<-as.factor(df_full$FBK)
df_full$DRG<-as.integer(df_full$DRG)
df_full$DRG<-as.factor(df_full$DRG)
n<-nrow(df_full)
set.seed(1234)
index<-sample(n,n*0.7)
train<-df_full %>% slice(index)
test<-df_full %>% slice(-index)
train$Win %>% table() %>% prop.table()*100
test$Win %>% table() %>% prop.table()*100
fit1<-NULL
fit1<-glm(Win ~.,data=train,family=binomial(link='logit'))
#회귀계수
fit1$coefficients %>% exp( ) %>% round(4)
beta.z<-beta(fit1)
beta.z$coefficients[,1] %>% round(4)
real<-test$Win
print(real)
prob1<-predict(fit1,test,'response')
print(prob1)
pred1<-ifelse(prob1>=0.5,1,0) %>% as.factor()
print(pred1)
confusionMatrix(pred1,real)
real<-as.factor(real)
library(MLmetrics)
F1_Score(y_true = real, y_pred = pred1, positive = '1')
library(pROC)
# roc(response = real, predictor = prob1) %>%
#   plot(main = 'ROC Curve', col = 'red', lty = 1)
roc(response = real, predictor = as.numeric(pred1)) %>%
  plot(col = 'blue', lty = 2, lwd = 2, add = TRUE)
class(pred1) as.numeric(pred1)-1
summary(fit1)