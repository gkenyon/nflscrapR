## First

You need to have the roster data and rush-pass play-by-play data saved on your computer.

How to create and save those files is shown [in this file here](https://gist.github.com/guga31bb/9f5c19d298691a726630e46aed6b982e).

Example image:

![Image](https://pbs.twimg.com/media/EBubvx6XYAAGRZn?format=png)

## Notes
There's 3 variables to set:
* ```tm```: the team you want to look at ("SEA", "BAL", or whatever)
* ```c1```: the primary color to use on the chart (for "SEA", I use "blue" for example)
* ```c2```: the secondary color

Also make sure you set the file path for the saving of the chart.

## The code
``` r
library(tidyverse)
library(na.tools)
library(ggimage)

#get roster data down to QBs
rosters <- readRDS("FILENAME/rosters.rds") %>%
  filter(position=="QB") %>% mutate(name=abbr_player_name,posteam=team)%>%
  select(season,name,posteam,position)

#get the play-by-play and join with the QBs
data <- readRDS("FILENAME/pbp_rp.rds") %>%
  select(name,pass,desc,posteam,season,epa,defteam) %>%
  left_join(rosters,by=c("season", "name", "posteam")) %>%
  mutate(qb=ifelse(is.na(position),0,1))

#fix some problem names
data <- data%>%mutate(
  qb=ifelse(name=="N.Peterman" | name=="R.Griffin III" | name=="R.Griffin"| name=="B.Hoyer" |
              name=="R.Mallett" | name=="J.Clausen" | name=="B.Weeden" |
              name=="Jo.Freedman" | name=="J.Freeman" | name=="M.Flynn" |
              name=="Mat.Moore" | name=="Dom.Davis" | name=="Alex Smith" | name=="Ale.Smith" |
              name=="Sh.Hill" | name=="Matt.Moore" | name=="K.Orton" |
              (name=="Ryan" & posteam=="ATL") | (name=="T.Edwards" & posteam=="BUF") |
              name=="Shaun.Hill" | name=="T.Pike (3rd QB)" | name=="B.St. Pierre",1,qb)
  )

#qbs league average
league_qb <- data%>%filter(qb==1)%>%group_by(season)%>%
  summarize(epa=mean(epa))
league_qb$posteam <- "League Average"
league_qb$defteam <- "League Average"

#non-qbs league average
league_nqb <- data%>%filter(qb==0)%>%group_by(season)%>%
  summarize(epa=mean(epa))
league_nqb$posteam <- "League Average"
league_nqb$defteam <- "League Average"

#team qbs
team_qb <- data%>%filter(qb==1)%>%group_by(posteam,season)%>%
  summarize(epa=mean(epa))

#team non-qbs
team_nqb <- data%>%filter(qb==0)%>%group_by(posteam,season)%>%
  summarize(epa=mean(epa))

#def qbs
def_qb <- data%>%filter(qb==1)%>%group_by(defteam,season)%>%
  summarize(epa=mean(epa))

#def non-qbs
def_nqb <- data%>%filter(qb==0)%>%group_by(defteam,season)%>%
  summarize(epa=mean(epa))


##offense chart. SET THESE VARIABLES
tm <- "KC" ##fix
c1 <- "red"
c2 <- "black"

ggplot(data=team_qb, aes(x=season,y=epa,group=posteam)) +
  geom_line(color="lightgray",alpha=.5) +
  geom_line(data=league_qb,aes(x=season,y=epa),color="blue",size=1, linetype = "dashed") +
    geom_line(data=filter(team_qb, posteam == tm),
            aes(x=season,y=epa),color=c1,size=1) +
  geom_point(data=filter(team_qb,posteam == tm),
             aes(x=season,y=epa),color=c2,size=3) +
  geom_hline(yintercept=0,linetype="dashed",color="black") +
  geom_line(data=team_nqb,aes(x=season,y=epa,group=posteam), 
            color="lightgray",alpha=.5) +
  geom_line(data=league_nqb,aes(x=season,y=epa),color="red",size=1, linetype = "dashed") +
  geom_line(data=filter(team_nqb, posteam == tm),
            aes(x=season,y=epa),color=c2,size=1) +
  geom_point(data=filter(team_nqb,posteam == tm),
             aes(x=season,y=epa),color=c1,size=3) +
  ylab("EPA Per Play") + xlab("Season") + scale_x_continuous(breaks=c(2009:2018)) +
  theme(panel.grid.minor = element_blank(),panel.grid.major.x = element_blank(),
      panel.background = element_blank(), panel.border = element_blank(),
      plot.title = element_text(size=20,face = 2,hjust=.5),
      plot.subtitle = element_text(size=16),
      axis.title.x = element_text(size=14),
      axis.title.y = element_text(size=14),
      axis.text.x = element_text(size=12),
      axis.text.y = element_text(size=12)) +
  labs(title=paste(tm,"Offense Timeline"),
      caption = "Data from nflscrapR (graph by @benbbaldwin)") +
  annotate("text",x=2009.5, y= .3, label = "League average\nQB plays", color="blue") +
  annotate("text",x=2009.5, y= -.25, label = "League average\nnon-QB plays", color="red") +
  geom_segment(aes(x=2009.1,xend=2009.1,y=.3,yend=.02),arrow=arrow(length=unit(.2,"cm")),color="blue")+
  geom_segment(aes(x=2009.2,xend=2009.2,y=-.2,yend=-.08),arrow=arrow(length=unit(.2,"cm")),color="red")+
  annotate("text",x=2017, y= .3, label = paste(tm,"QB plays"), color=c1) +
  annotate("text",x=2015, y= -.1, label = paste(tm,"non-\nQB plays"), color=c2) 

ggsave(paste0("PATH/",tm,".png"), dpi=700)

#####################################
#####################################

##defense chart. SET THESE VARIABLES
tm <- "BAL" ##fix
c1 <- "purple"
c2 <- "black"

ggplot(data=def_qb, aes(x=season,y=epa,group=defteam)) +
  geom_line(color="lightgray",alpha=.5) +
  geom_line(data=league_qb,aes(x=season,y=epa),color="blue",size=1, linetype = "dashed") +
  geom_line(data=filter(def_qb, defteam == tm),
            aes(x=season,y=epa),color=c1,size=1) +
  geom_point(data=filter(def_qb,defteam == tm),
             aes(x=season,y=epa),color=c2,size=3) +
  geom_hline(yintercept=0,linetype="dashed",color="black") +
  geom_line(data=def_nqb,aes(x=season,y=epa,group=defteam), 
            color="lightgray",alpha=.5) +
  geom_line(data=league_nqb,aes(x=season,y=epa),color="red",size=1, linetype = "dashed") +
  geom_line(data=filter(def_nqb, defteam == tm),
            aes(x=season,y=epa),color=c2,size=1) +
  geom_point(data=filter(def_nqb,defteam == tm),
             aes(x=season,y=epa),color=c1,size=3) +
  ylab("EPA Per Play") + xlab("Season") + scale_x_continuous(breaks=c(2009:2018)) +
  theme(panel.grid.minor = element_blank(),panel.grid.major.x = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(),
        plot.title = element_text(size=20,face = 2,hjust=.5),
        plot.subtitle = element_text(size=16),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12)) +
  labs(title=paste(tm,"Defense Timeline"),
       caption = "Data from nflscrapR (graph by @benbbaldwin)") +
  annotate("text",x=2009.5, y= .3, label = "League average\nQB plays", color="blue") +
  annotate("text",x=2009.5, y= -.25, label = "League average\nnon-QB plays", color="red") +
  geom_segment(aes(x=2009.1,xend=2009.1,y=.3,yend=.02),arrow=arrow(length=unit(.2,"cm")),color="blue")+
  geom_segment(aes(x=2009.2,xend=2009.2,y=-.2,yend=-.08),arrow=arrow(length=unit(.2,"cm")),color="red")+
  annotate("text",x=2017, y= .3, label = paste(tm,"QB plays"), color=c1) +
  annotate("text",x=2015, y= -.1, label = paste(tm,"non-\nQB plays"), color=c2) 

ggsave(paste0("PATH/def_",tm,".png"), dpi=700)
```