
library(ggplot2)
library(gganimate)
library(tidyverse)
library(janitor)
library(scales)

mil <- read_csv("C:/Users/Alican/Desktop/ie48A/mil.csv",guess_max = 100,
                   col_types = cols(
                  "Tarih" = col_date(format="%Y-%m")
               ))
# cols("TP ODEMGZS ALMANYA"=col_double())

#rename

names(mil) <- substring(names(mil), 11)
names(mil)[1] <- "year"

#keep only related ones


drops <- c(" DAVRUPA"," AVRUPATOP"," BDTTOPLAM"," DAMERIKA"," AMERIKATOP"," DAFRIKA"," AFRIKATOP"," DASYA"," ASYATOP"," GTOPLAM")
new_mil<-mil[1:150,!(names(mil) %in% drops)]

#tidying the data

str(new_mil)

new_mildeneme<-new_mil %>%
  column_to_rownames("year")%>%
  t()  %>%
  as.data.frame
new_mildeneme$ID <- rownames(new_mildeneme)
  
new_mildeneme2<-new_mildeneme  %>%
  gather(year,value,1:150)
write_csv(new_mildeneme2,"C:/Users/Alican/Desktop/ie48A/mil_tidy.csv")



#add rank 

mil_formatted <- new_mildeneme2 %>%
  group_by(year) %>%
# The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = rank(-value),
         Value_rel = value/value[rank==1],
         Value_lbl = paste0(" ",value)) %>%
  group_by(ID) %>% 
  filter(rank <=10) %>%
  ungroup()

#start static plot first

staticplot = ggplot(mil_formatted, aes(rank, group = ID, 
                                       fill = as.factor(ID), color = as.factor(ID))) +
  geom_tile(aes(y = value/2,
                height = value,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(ID, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=value,label = Value_lbl, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm"))

#transition
anim = staticplot + transition_states(year, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'Number of Visitors by Nations(Turkey) : {closest_state}',  
       subtitle  =  "Top 10 Countries between 2008 and 2020",
       caption  = " Data Source: World Bank Data")

# For GIF
animate(anim, 300, fps = 10,  width = 1200, height = 1000,
        renderer = gifski_renderer("gganim.gif"))


