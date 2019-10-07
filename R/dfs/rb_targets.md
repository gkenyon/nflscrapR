## Running back targets

Code used for running back targets piece.

``` r
library(tidyverse)
library(na.tools)
library(ggrepel)
library(ggimage)
library(hrbrthemes)

#get from tutorial
rosters <- readRDS("PATH/rosters.rds") %>%
  filter((position=="RB" | position=="FB" | position == "WR" | position == "TE") & season==2018) %>% 
  mutate(name=abbr_player_name,posteam=team)%>%
  select(season,name,posteam,position)


#get from tutorial
data <- readRDS("PATH/pbp_rp.rds") %>%
  filter(season==2018 & pass==1 & sack==0 & qb_scramble==0)%>%
  select(name,pass,desc,posteam,season,epa,defteam,complete_pass,incomplete_pass,
         air_yards,receiver_player_name, down, success) %>%
  left_join(rosters,by=c("season", "receiver_player_name" = "name", "posteam")) %>%
  mutate(
    qb=ifelse(is.na(position),0,1), rec=receiver_player_name,
    drop = if_else(str_detect(desc, "(sacked)|(scramble)"), 1, 0)
         ) %>% filter(drop==0)

#fix a bunch of problem players
pos <- data %>% mutate(
  position=ifelse(
    rec=="K.Benjamin" | rec=="A.Cooper" | rec=="G.Tate" | rec=="A.Robinson" | rec=="B.Marshall" | 
      rec=="D.Hilliard" | rec=="D.Thompson" | rec=="De.Thomas" | rec=="E.St" | rec=="K.Benjamin" | rec=="K.Bibbs" | 
      rec=="Ty.Williams" | rec=="W.Snead" | rec=="W.Snead IV" | rec=="T.Pryor" | rec=="E.St. Brown" |
      rec=="A.Robinson II" | rec=="J.Gordon" | rec == "D.Carter" | rec=="B.Ellington" |
      rec=="A.Holmes" | rec=="R.Matthews" | rec=="M.Valdes" | rec=="V.Bolden","WR",position
  ),
  position=ifelse(
    rec=="A.Abdullah" | rec=="C.Hyde" | rec == "Dam." | rec == "T.Montgomery" | rec=="A.Ekeler" | rec=="T.Yeldon" | 
      rec=="Dam. Williams" | rec=="Dar.Williams" | rec=="R.Jones II" | rec=="C.Anderson","RB",position
  ),
  position=ifelse(position=="FB","RB",position)
  ) %>%
  filter(!is.na(position), down<=2)


#get means by position
bar <- pos %>%group_by(position) %>%summarize(epa=mean(epa),n=n(), success=mean(success))

#epa by position figure
ggplot(bar, aes(x=position, y=epa, fill=position)) +
  geom_bar(stat="identity")+
  labs(title="Early-down EPA per play by targeted position", subtitle="NFL, 1st & 2nd downs, 2018", caption = "Data from nflscrapR")+
  theme_ipsum_ps()+
  theme(
    legend.position = c(0.1, 0.99),
    legend.justification = c(1, 1) ,
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(hjust = 0.5),
    axis.title.y = element_text(hjust = 0.5)
  ) +  ylab("EPA Per Play") + xlab("Position") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))


ggsave("PATH/g1_bar.png", dpi=500)

#get success rate by position
ggplot(bar, aes(x=position, y=success, fill=position)) +
  geom_bar(stat="identity")+
  labs(title="Early-down EPA success rate by targeted position", subtitle="NFL, 1st & 2nd downs, 2018", caption = "Data from nflscrapR")+
  theme_ipsum_ps()+
  theme(
    legend.position = c(0.1, 1.1),
    legend.justification = c(1, 1) ,
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(hjust = 0.5),
    axis.title.y = element_text(hjust = 0.5)
  ) +  ylab("Success rate (share of positive EPA plays)") + xlab("Position") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
  
ggsave("PATH/g1b_success.png", dpi=500)

#combine TE and WR into one group
pos2 <- pos %>% mutate(
  position = ifelse(position == "WR" | position =="TE", "WR/TE",position)
)

hist <- pos2%>%filter(air_yards> -10 & air_yards<=50)
mu <- hist %>% group_by(position) %>% summarize(grp.mean=mean(air_yards))

#hisogram for piece
ggplot(hist, aes(x=air_yards, color=position, fill=position)) +
  geom_histogram(binwidth=1, aes(y=..density..), alpha=.5, position="dodge") +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=position),
             linetype="dashed")+
  theme_ipsum_ps()+
  theme(
    legend.position = c(.99,.99),
    legend.justification = c(1, 1) ,
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(hjust = 0.5),
    axis.title.y = element_text(hjust = 0.5)
  ) +  ylab("Density") + xlab("Air yards") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  labs(title="Early-down air yards by targeted position", subtitle="NFL, 1st & 2nd downs, 2018", caption = "Data from nflscrapR")
  

ggsave("PATH/g2_adot.png", dpi=500)

#completion % by adot (have to filter to pass attempted bc not counting plays with penalties)
cp1 <- hist %>% filter(complete_pass==1 | incomplete_pass==1) %>%
  group_by(position,air_yards)%>%
  summarize(complete=100*mean(complete_pass))

#epa by adot (now we can count penalties)
cp2 <- hist %>% 
  group_by(position,air_yards)%>%
  summarize(epa=mean(epa))

#not used in piece: plot completion %
cp1 %>% filter(air_yards>= -5 & air_yards <= 20) %>%
  ggplot(aes(x=air_yards, y=complete, color=position, shape=position)) +
  geom_point(size=4)+
  geom_smooth(method=loess, se=FALSE) +
  theme_minimal() +
  theme(
    legend.position = c(0.99, 0.99), 
    legend.justification = c(1, 1) ,
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    panel.grid.minor = element_blank())+ 
  scale_y_continuous(name = "Completion percentage", breaks = scales::pretty_breaks(n = 5))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10), name = "Depth of target")+
  labs(title="Completion percentage by depth of target", caption = "Data from nflscrapR", subtitle="NFL, 1st & 2nd downs, 2018")+
  scale_shape_discrete(name="Position")+scale_color_discrete(name="Position")

ggsave("PATH/g3_cpoe.png", dpi=500)


##above but with EPA
cp2 %>% filter(air_yards>= -6 & air_yards <= 20) %>%
  ggplot(aes(x=air_yards, y=epa, color=position, shape=position)) +
  geom_point(size=4)+
  geom_smooth(method=loess, se=FALSE) +
  theme_minimal() +
  theme(
    legend.position = c(0.99, 0.99), 
    legend.justification = c(1, 1) ,
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    panel.grid.minor = element_blank())+ 
  scale_y_continuous(name = "Expected Points Added", breaks = scales::pretty_breaks(n = 10))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10), name = "Depth of target")+
  labs(title="Expected Points Added by depth of target", caption = "Data from nflscrapR", subtitle="NFL, 1st & 2nd downs, 2018")+
  scale_shape_discrete(name="Position")+scale_color_discrete(name="Position")

ggsave("PATH/g3b_epa.png", dpi=500)


#do the seahawks stuff
sea <- pos %>% filter(posteam=="SEA")

sum <- sea %>% group_by(position)%>%
  summarise(epa=mean(epa))

sea%>% mutate(epa = if_else(epa < -1.5, -1.5, epa), epa = if_else(epa > 3, 3, epa)) %>%
  ggplot(aes(x=position, y=epa, color=position, shape=position)) +
  geom_jitter(aes(x = position, y = epa, fill = position), 
              shape = 21, stroke = 1.25, size = 1, width = 0.33,show.legend=FALSE,alpha=.8)+
  geom_point(data = sum, size = 25, shape=95) + 
  scale_y_continuous(name = "EPA", breaks = scales::pretty_breaks(n = 10))+
  labs(title="Seahawks early-down EPA per play by targeted position", 
       subtitle=paste("2018, 1st & 2nd downs"), caption = "Data from nflscrapR")+
  theme_ipsum_ps()+
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(hjust = 0.5),
    axis.title.y = element_text(hjust = 0.5),
    legend.position = "none"
  ) 

ggsave("PATH/g4_seahawks.png", dpi=500)


#get correlation between rb target share and epa/play
ed <- readRDS("PATH/pbp_rp.rds") %>%
  filter(season==2018 & down<=2)%>%
  select(name,pass,desc,posteam,season,epa,defteam,complete_pass,incomplete_pass,
         air_yards,receiver_player_name, down, success) %>%
  group_by(posteam)%>%summarize(epa=mean(epa))

tgt <- pos %>%
  mutate(
    rb_target=if_else(position=="RB",1,0)
    )%>%
  group_by(posteam)%>%summarise(tgt_share=mean(rb_target)) %>%
  arrange(desc(tgt_share))

test <- inner_join(ed,tgt,by="posteam") %>%arrange(desc(tgt_share))

cor(test$epa,test$tgt_share)

```