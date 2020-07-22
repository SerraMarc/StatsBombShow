library(StatsBombR)
library(SBpitch)
library(tidyverse)
library(ggplot2)
library(grid)

laLiga <- FreeCompetitions() %>%
  filter(competition_name == "La Liga")

matches <- FreeMatches(laLiga)

events <- StatsBombFreeEvents(MatchesDF = matches, Parallel = T) %>%
  allclean()

latestSeason <- events %>%
  filter(season_id == 4)


## XG XGA Barça 2018-19
xGA <- latestSeason %>%
        filter(type.name=="Shot") %>%
          select(shot.key_pass_id, xGA = shot.statsbomb_xg)

shot_assists <- left_join(latestSeason, xGA, by = c("id" = "shot.key_pass_id")) %>% 
                  select(team.name, player.name, player.id, type.name, pass.shot_assist, pass.goal_assist, xGA ) %>%
                    filter(pass.shot_assist==TRUE | pass.goal_assist==TRUE)

player_xGA = shot_assists %>%
                group_by(player.name, player.id, team.name) %>%
                  summarise(xGA = sum(xGA, na.rm = TRUE)) #1

player_xG_xGA <- latestSeason %>%
				            filter(type.name=="Shot" & (shot.type.name!="Penalty" | is.na(shot.type.name))) %>%
					            group_by(player.name, player.id, team.name) %>%
						            summarise(xG = sum(shot.statsbomb_xg, na.rm = TRUE)) %>%
							            merge(player_xGA, all=TRUE) %>%
								            mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
									            mutate(xG_xGA = xG + xGA)

player_minutes <- get.minutesplayed(latestSeason) %>%
                    group_by(player.id) %>%
                      summarise(minutes = sum(MinutesPlayed))

player_xG_xGA_90 <- left_join(player_xG_xGA, player_minutes) %>%
                      mutate(nineties = minutes/90,
                             xG_90 = round(xG/nineties, 2),
                             xGA_90 = round(xGA/nineties,2),
                             xG_xGA_90 = round(xG_xGA/nineties,2))

chart <- player_xG_xGA_90 %>%
            ungroup() %>%
              filter(minutes>=600) %>%
                top_n(n = 15, w = xG_xGA_90) %>% #Not necessary taking only latest season, but useful if I want to use it for all seasons, and it doesn't make anything go wrong.
select(c(player.name, xG_90, xGA_90, xG_xGA_90)) %>%
  pivot_longer(-player.name, names_to = "variable", values_to = "value")

ggplot(chart[chart$variable == "xG_90" | chart$variable == "xGA_90",], 
       aes(x =reorder(player.name, value), y = value, fill=fct_rev(variable))) +
  geom_bar(stat="identity", colour="white") +
  labs(title = "Expected Goal Contribution", subtitle = "F.C. Barcelona, La Liga , 2019-20",
       x="", y="Per 90 mins", 
       caption ="Minimum 600 minutes\nNPxG = Value of shots taken (no penalties)\nxG assisted = Value of shots assisted") +
  theme(axis.text.y = element_text(size=12, color="#333333", family="Source Sans Pro"),
        axis.title = element_text(size=14, color="#333333", family="Source Sans Pro"),
        axis.text.x = element_text(size=12, color="#333333", family="Source Sans Pro"),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white"),
        plot.background = element_rect(fill = "white", colour ="white"),
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
        panel.grid.major.x =  element_line(colour = "light grey"), panel.grid.minor.x = element_line(colour = "light grey"),
        plot.title=element_text(size=24, color="#333333", family="Source Sans Pro" , face="bold"),
        plot.subtitle=element_text(size=18, color="#333333", family="Source Sans Pro", face="bold"),
        plot.caption=element_text(color="#333333", family="Source Sans Pro", size =10),
        text=element_text(family="Source Sans Pro"),
        legend.title=element_blank(),
        legend.text = element_text(size=14, color="#333333", family="Source Sans Pro"),
        legend.position = "bottom") +
  scale_fill_manual(values=c("#3371AC", "#DC2228"), labels = c( "xG Assisted","NPxG")) +
  scale_y_continuous(expand = c(0, 0), limits= c(0,max(chart$value) + 0.3)) +
  coord_flip() +
  guides(fill = guide_legend(reverse = TRUE))





## How do different teams defend against Barcelona 2018-19
heatmap = latestSeason %>%
  filter(type.name=="Pressure" | duel.type.name=="Tackle" | type.name=="Foul Committed" | type.name=="Interception" | type.name=="Block" ) %>%
  filter(team.name!="Barcelona" ) %>%
  mutate(location.x = ifelse(location.x>120, 120, location.x),
         location.y = ifelse(location.y>80, 80, location.y),
         location.x = ifelse(location.x<0, 0, location.x),
         location.y = ifelse(location.y<0, 0, location.y)) %>%
  mutate(xbin = cut(location.x, breaks = seq(from=0, to=120, by = 20),include.lowest=TRUE)) %>%
  mutate(ybin = cut(location.y, breaks = seq(from=0, to=80, by = 20),include.lowest=TRUE))%>%
  group_by(team.name) %>%
  mutate(total_DA = n()) %>%
  group_by(team.name, xbin, ybin) %>%
  summarise(total_DA = max(total_DA),
            bin_DA = n(),
            bin_pct = bin_DA/total_DA,
            location.x = median(location.x),
            location.y = median(location.y)) %>%
  ungroup() %>%
  mutate(league_ave = mean(bin_pct)) %>%
  group_by(team.name, xbin, ybin) %>%
  mutate(diff_vs_ave = bin_pct - league_ave)




defensiveactivitycolors <- c("#dc2429", "#dc2329", "#df272d", "#df3238", "#e14348", "#e44d51",
                             "#e35256", "#e76266", "#e9777b", "#ec8589", "#ec898d", "#ef9195",
                             "#ef9ea1", "#f0a6a9", "#f2abae", "#f4b9bc", "#f8d1d2", "#f9e0e2",
                             "#f7e1e3", "#f5e2e4", "#d4d5d8", "#d1d3d8", "#cdd2d6", "#c8cdd3", 
                             "#c0c7cd", "#b9c0c8", "#b5bcc3", "#909ba5", "#8f9aa5", "#818c98", 
                             "#798590", "#697785", "#526173", "#435367", "#3a4b60", "#2e4257",
                             "#1d3048", "#11263e", "#11273e", "#0d233a", "#020c16")

ggplot(data= heatmap, aes(x = location.x, y = location.y, fill = diff_vs_ave, group =diff_vs_ave)) +
  geom_bin2d(binwidth = c(20, 20), position = "identity", alpha = 0.9) +
  annotate("rect",xmin = 0, xmax = 120, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 0, xmax = 60, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 18, xmax = 0, ymin = 18, ymax = 62, fill = NA, colour = "white", size = 0.6) +
  annotate("rect",xmin = 102, xmax = 120, ymin = 18, ymax = 62, fill = NA, colour = "white", size = 0.6) +
  annotate("rect",xmin = 0, xmax = 6, ymin = 30, ymax = 50, fill = NA, colour = "white", size = 0.6) +
  annotate("rect",xmin = 120, xmax = 114, ymin = 30, ymax = 50, fill = NA, colour = "white", size = 0.6) +
  annotate("rect",xmin = 120, xmax = 120.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 0, xmax = -0.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
  annotate("segment", x = 60, xend = 60, y = -0.5, yend = 80.5, colour = "white", size = 0.6)+
  annotate("segment", x = 0, xend = 0, y = 0, yend = 80, colour = "black", size = 0.6)+
  annotate("segment", x = 120, xend = 120, y = 0, yend = 80, colour = "black", size = 0.6)+
  theme(rect = element_blank(),
        line = element_blank()) +
  annotate("point", x = 12 , y = 40, colour = "white", size = 1.05) +
  # add penalty spot right
  annotate("point", x = 108 , y = 40, colour = "white", size = 1.05) +
  annotate("path", colour = "white", size = 0.6,
           x=60+10*cos(seq(0,2*pi,length.out=2000)),
           y=40+10*sin(seq(0,2*pi,length.out=2000))) +
  # add centre spot
  annotate("point", x = 60 , y = 40, colour = "white", size = 1.05) +
  annotate("path", x=12+10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
           y=40+10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="white") +
  annotate("path", x=108-10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
           y=40-10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="white")  + 
  theme(axis.text.x=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.caption=element_text(size=13,family="Source Sans Pro", hjust=0.5, vjust=0.5),
        plot.subtitle = element_text(size = 18, family="Source Sans Pro", hjust = 0.5),
        axis.text.y=element_blank(),
        legend.title = element_blank(),
        legend.text=element_text(size=22,family="Source Sans Pro"),
        legend.key.size = unit(1.5, "cm"),
        plot.title = element_text(margin = margin(r = 10, b = 10), face="bold",size = 32.5, family="Source Sans Pro", colour = "black", hjust = 0.5),
        legend.direction = "vertical",
        axis.ticks=element_blank(),
        plot.background = element_rect(fill = "white"),
        strip.text.x = element_text(size=13,family="Source Sans Pro")) +
  scale_y_reverse() +
  scale_fill_gradientn(colours = defensiveactivitycolors, trans = "reverse", labels = scales::percent_format(accuracy = 1), limits = c(0.03, -0.03)) + #6
  labs(title = "Where Do Teams Defend vs League Average?", subtitle = "Against Barcelona , 2018/19") + 
  coord_fixed(ratio = 95/100) + 
  annotation_custom(grob = linesGrob(arrow=arrow(type="open", ends="last",
                                                 length=unit(2.55,"mm")), gp=gpar(col="black", fill=NA, lwd=2.2)),
                    xmin=25, xmax = 95, ymin = -83, ymax = -83) + 
  facet_wrap(~team.name)+ 
  guides(fill = guide_legend(reverse = TRUE)) 


## Barcelona's Pass map
heatmap = latestSeason %>%
  filter(type.name=="Pass") %>%
  filter(team.name=="Barcelona" ) %>%
  mutate(pass.end_location.x = ifelse(location.x>120, 120, location.x),
         pass.end_location.y = ifelse(location.y>80, 80, location.y),
         pass.end_location.x = ifelse(location.x<0, 0, location.x),
         pass.end_location.y = ifelse(location.y<0, 0, location.y)) %>%
  mutate(xbin = cut(pass.end_location.x, breaks = seq(from=0, to=120, by = 20),include.lowest=TRUE)) %>%
  mutate(ybin = cut(pass.end_location.y, breaks = seq(from=0, to=80, by = 20),include.lowest=TRUE))%>%
  left_join(matches) %>% 
  mutate(rival_team.name = ifelse(home_team.home_team_name == "Barcelona", away_team.away_team_name, home_team.home_team_name)) %>%
  group_by(rival_team.name) %>%
  mutate(total_Passes = n()) %>%
  group_by(rival_team.name, xbin, ybin) %>%
  summarise(total_Passes = max(total_Passes),
            bin_Passes = n(),
            bin_pct = bin_Passes/total_Passes,
            location.x = median(location.x),
            location.y = median(location.y)) %>%
  ungroup() %>%
  mutate(league_ave = mean(bin_pct)) %>%
  group_by(rival_team.name, xbin, ybin) %>%
  mutate(diff_vs_ave = bin_pct - league_ave)


defensiveactivitycolors <- c("#dc2429", "#dc2329", "#df272d", "#df3238", "#e14348", "#e44d51",
                             "#e35256", "#e76266", "#e9777b", "#ec8589", "#ec898d", "#ef9195",
                             "#ef9ea1", "#f0a6a9", "#f2abae", "#f4b9bc", "#f8d1d2", "#f9e0e2",
                             "#f7e1e3", "#f5e2e4", "#d4d5d8", "#d1d3d8", "#cdd2d6", "#c8cdd3", 
                             "#c0c7cd", "#b9c0c8", "#b5bcc3", "#909ba5", "#8f9aa5", "#818c98", 
                             "#798590", "#697785", "#526173", "#435367", "#3a4b60", "#2e4257",
                             "#1d3048", "#11263e", "#11273e", "#0d233a", "#020c16")

ggplot(data= heatmap, aes(x = location.x, y = location.y, fill = diff_vs_ave, group =diff_vs_ave)) +
  geom_bin2d(binwidth = c(20, 20), position = "identity", alpha = 0.9) +
  annotate("rect",xmin = 0, xmax = 120, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 0, xmax = 60, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 18, xmax = 0, ymin = 18, ymax = 62, fill = NA, colour = "white", size = 0.6) +
  annotate("rect",xmin = 102, xmax = 120, ymin = 18, ymax = 62, fill = NA, colour = "white", size = 0.6) +
  annotate("rect",xmin = 0, xmax = 6, ymin = 30, ymax = 50, fill = NA, colour = "white", size = 0.6) +
  annotate("rect",xmin = 120, xmax = 114, ymin = 30, ymax = 50, fill = NA, colour = "white", size = 0.6) +
  annotate("rect",xmin = 120, xmax = 120.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 0, xmax = -0.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
  annotate("segment", x = 60, xend = 60, y = -0.5, yend = 80.5, colour = "white", size = 0.6)+
  annotate("segment", x = 0, xend = 0, y = 0, yend = 80, colour = "black", size = 0.6)+
  annotate("segment", x = 120, xend = 120, y = 0, yend = 80, colour = "black", size = 0.6)+
  theme(rect = element_blank(),
        line = element_blank()) +
  annotate("point", x = 12 , y = 40, colour = "white", size = 1.05) +
  # add penalty spot right
  annotate("point", x = 108 , y = 40, colour = "white", size = 1.05) +
  annotate("path", colour = "white", size = 0.6,
           x=60+10*cos(seq(0,2*pi,length.out=2000)),
           y=40+10*sin(seq(0,2*pi,length.out=2000))) +
  # add centre spot
  annotate("point", x = 60 , y = 40, colour = "white", size = 1.05) +
  annotate("path", x=12+10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
           y=40+10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="white") +
  annotate("path", x=108-10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
           y=40-10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="white")  + 
  theme(axis.text.x=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.caption=element_text(size=13,family="Source Sans Pro", hjust=0.5, vjust=0.5),
        plot.subtitle = element_text(size = 18, family="Source Sans Pro", hjust = 0.5),
        axis.text.y=element_blank(),
        legend.title = element_blank(),
        legend.text=element_text(size=22,family="Source Sans Pro"),
        legend.key.size = unit(1.5, "cm"),
        plot.title = element_text(margin = margin(r = 10, b = 10), face="bold",size = 32.5, family="Source Sans Pro", colour = "black", hjust = 0.5),
        legend.direction = "vertical",
        axis.ticks=element_blank(),
        plot.background = element_rect(fill = "white"),
        strip.text.x = element_text(size=13,family="Source Sans Pro")) +
  scale_y_reverse() +
  scale_fill_gradientn(colours = defensiveactivitycolors, trans = "reverse", labels = scales::percent_format(accuracy = 1), limits = c(0.03, -0.03)) +
  labs(title = "Bacelona's Pass Map", subtitle = "La Liga, 2018/19") + 
  coord_fixed(ratio = 95/100) + 
  annotation_custom(grob = linesGrob(arrow=arrow(type="open", ends="last",
                                                 length=unit(2.55,"mm")), gp=gpar(col="black", fill=NA, lwd=2.2)),
                    xmin=25, xmax = 95, ymin = -83, ymax = -83) + 
  facet_wrap(~rival_team.name)+ 
  guides(fill = guide_legend(reverse = TRUE)) 


## Density possesion duration against grouped by rival_team.
possessions <- latestSeason %>%
  filter(!is.na(player.id)) %>%
  filter(possession_team.name == "Barcelona") %>%
  select(match_id, possession, possession_team.id, possession_team.name, team.name, TimeInPoss, TimeToPossEnd, player.name) %>%
  distinct(match_id, possession, possession_team.id, .keep_all = TRUE) %>%      
  mutate(PossTime = (TimeToPossEnd + TimeInPoss)) %>% 
  filter(PossTime < 2700) %>% # There are some errors with possessions of over 45 mins.
  left_join(matches) %>%
  mutate(rival_team.name = ifelse(home_team.home_team_name == "Barcelona", away_team.away_team_name, home_team.home_team_name))%>%
  select(rival_team.name, possession, possession_team.name, team.name, PossTime)



ggplot(possessions, 
       aes(x = PossTime, fill = "#dc2429", color = "red", alpha = 0.8)) +
  geom_density(show.legend = FALSE) +
  xlim(0,100) + #percentile 0.98
  theme(axis.text.y = element_blank(),
        axis.title = element_text(size=14, color="#333333", family="Source Sans Pro"),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90,size=8, color="#333333", family="Source Sans Pro", vjust = 0.5, hjust=1),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white"),
        plot.background = element_rect(fill = "white", colour ="white"),
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
        panel.grid.major.x =  element_line(colour = "light grey"), panel.grid.minor.x = element_blank(),
        plot.title=element_text(size=24, color="#333333", family="Source Sans Pro" , face="bold"),
        plot.subtitle=element_text(size=18, color="#333333", family="Source Sans Pro", face="bold"),
        plot.caption=element_text(color="#333333", family="Source Sans Pro", size =10),
        text=element_text(family="Source Sans Pro"),
        legend.title=element_blank(),
        legend.text = element_text(size=14, color="#333333", family="Source Sans Pro"),
        legend.position = "bottom") +
  
  labs(title = "Bacelona's Possession Distribution", subtitle = "Against La Liga Teams, 2018/19", 
       caption = "Limited to 100 seconds posssession max.",
       x="Possession Time(seconds)") + 
  facet_wrap(rival_team.name~.)


## Plot total length with passes along time
passes_length <- events %>%
  filter(type.name == "Pass") %>%
  filter(team.name == "Barcelona") %>%
  filter(is.na(pass.outcome.name)) %>%
  select(player.name, player.id, match_id, pass.length) %>%
  left_join(matches) %>%
  mutate(match_date = as.Date(match_date))%>%
  select(match_date, pass.length, season.season_name) %>%
  group_by(match_date, season.season_name) %>%
  summarise(total_length = sum(pass.length)) %>%
  ungroup() %>%
  group_by(season.season_name) %>%
  summarise(total_length = mean(total_length)) %>%
  ungroup() 

ggplot(passes_length,aes(x = season.season_name, y= total_length, fill = total_length)) +
  geom_bar(stat="identity") +
  labs(title = "Total distance of passes", subtitle = "F.C. Barcelona, La Liga , 2004/05-2018/19",
       x="Whole seasons", y="", 
       caption ="Median of distance passed/match") +
  theme(axis.text.y = element_text(size=12, color="#333333", family="Source Sans Pro"),
        axis.title = element_text(size=14, color="#333333", family="Source Sans Pro"),
        axis.text.x = element_text(angle = 90,size=12, color="#333333", family="Source Sans Pro", vjust = 0.5, hjust=1),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white"),
        plot.background = element_rect(fill = "white", colour ="white"),
        panel.grid.major.y = element_line(colour = "light grey"), panel.grid.minor.y = element_blank(),
        panel.grid.major.x =  element_line(colour = "light grey"), panel.grid.minor.x = element_line(colour = "light grey"),
        plot.title=element_text(size=24, color="#333333", family="Source Sans Pro" , face="bold"),
        plot.subtitle=element_text(size=18, color="#333333", family="Source Sans Pro", face="bold"),
        plot.caption=element_text(color="#333333", family="Source Sans Pro", size =10),
        text=element_text(family="Source Sans Pro"),
        legend.title=element_blank(),
        legend.text = element_text(size=14, color="#333333", family="Source Sans Pro"),
        legend.position = "bottom") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_gradientn(colours = defensiveactivitycolors, trans = "reverse", labels = scales::unit_format(accuracy = 1)) 


## Plot xg through time
xg <- events %>%
  filter(type.name == "Shot") %>%
  select(team.id, team.name, shot.outcome.name, shot.statsbomb_xg, match_id) %>%
  left_join(matches) %>%
  mutate(goal = ifelse(shot.outcome.name == "Goal", TRUE, FALSE), 
         barcelona = ifelse(team.name == "Barcelona", TRUE, FALSE))%>%
  select(match_date, barcelona, shot.statsbomb_xg, goal, season.season_name) %>%
  group_by(match_date, barcelona, season.season_name) %>%
  summarise(xg = sum(shot.statsbomb_xg),
            goals = sum(goal),
            diff = xg-goals) %>%
  ungroup() %>%
  group_by(season.season_name, barcelona) %>%
  summarise(total_xg = sum(xg),
            total_goals = sum(goals),
            total_diff = sum(diff)) %>%
  ungroup() %>%
  mutate(year = as.numeric(substr(season.season_name,1,4)))



ggplot(xg,aes(x = season.season_name, y= total_xg, group=barcelona, fill=barcelona)) +
  geom_area(alpha=0.8) +
  labs(title = "Expected Goal Contribution", subtitle = "F.C. Barcelona, La Liga , 2004/05-2018/19",
       x="", y="Whole SeasonS", 
       caption ="xG = Value of shots taken") +
  theme(axis.text.y = element_text(size=12, color="#333333", family="Source Sans Pro"),
        axis.title = element_text(size=14, color="#333333", family="Source Sans Pro"),
        axis.text.x = element_text(angle = 90,size=12, color="#333333", family="Source Sans Pro", vjust = 0.5, hjust=1),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white"),
        plot.background = element_rect(fill = "white", colour ="white"),
        panel.grid.major.y = element_line(colour = "light grey"), panel.grid.minor.y = element_blank(),
        panel.grid.major.x =  element_line(colour = "light grey"), panel.grid.minor.x = element_line(colour = "light grey"),
        plot.title=element_text(size=24, color="#333333", family="Source Sans Pro" , face="bold"),
        plot.subtitle=element_text(size=18, color="#333333", family="Source Sans Pro", face="bold"),
        plot.caption=element_text(color="#333333", family="Source Sans Pro", size =10),
        text=element_text(family="Source Sans Pro"),
        legend.title=element_blank(),
        legend.text = element_text(size=14, color="#333333", family="Source Sans Pro"),
        legend.position = "bottom") +
  scale_fill_manual(values=c("#3371AC", "#DC2228"), labels = c( "xG Barcelona","xG Against")) +
  guides(fill = guide_legend(reverse = TRUE))