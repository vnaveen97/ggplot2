install.packages("tidyverse")
library(tidyverse)
install.packages("ggmap")
library(ggmap)
install.packages("ggthemes")
library(ggthemes)
college <- read_csv("http://672258.youcanlearnit.net/college.csv")#stored as a tibble
summary(college)
college <- college %>% mutate(state = as.factor(state), gender = as.factor(gender),
                              highest_degree  = as.factor(highest_degree),
                              control = as.factor(control), 
                              region = as.factor(region),
                              loan_default_rate = as.numeric(loan_default_rate))
library(ggplot2)
#Scattterplot
ggplot(data = college) +
  geom_point(mapping = aes(x= tuition, y = sat_avg, col = control),alpha = 0.6)#alpha is for transparency
#lines and smooth
ggplot(data = college,mapping = aes(x= tuition, y = sat_avg, col = control)) +
  geom_point(alpha = 0.1) +
  geom_line(alpha = 0.1) +
  geom_smooth(se= FALSE) 
#bar and col charts
ggplot(data = college)+
  geom_bar(mapping = aes(x=region,fill = control))
#Finding average tuition by region
college %>% group_by(region) %>%
  summarise(avg_tuition=mean(tuition)) %>%
  ggplot() +
  geom_col(mapping = aes(x=region,y=avg_tuition))
#Histograms
ggplot(college)+
  geom_histogram(mapping = aes(x=undergrads),bins = 10,origin=0)
#boxplot
ggplot(college)+
  geom_boxplot(mapping = aes(x=control,y=tuition))
#Themes
ggplot(data = college)+
  geom_bar(mapping = aes(x=region,fill = control))+
  theme(panel.background = element_blank())+
  theme(panel.grid.major.y = element_line(color = "orange") )
#axes
ggplot(data = college)+
  geom_bar(mapping = aes(x=region,fill = control))+
  theme(panel.background = element_blank())+
  theme(panel.grid.major.y = element_line(color = "orange") )+
  xlab("Region")+ylab("Number of Schools")+
  ylim(0,500)
#Scaling
ggplot(data = college)+
  geom_bar(mapping = aes(x=region,fill = control))+
  theme(panel.background = element_blank())+
  theme(panel.grid.major.y = element_line(color = "orange") )+
  scale_x_discrete(name="Region")+
  scale_y_continuous(name="Number of Schools",limits = c(0,500))+
  scale_fill_manual(values = c("Red","Black"))
#legends
ggplot(data = college)+
  geom_bar(mapping = aes(x=region,fill = control))+
  theme(panel.background = element_blank())+
  theme(panel.grid.major.y = element_line(color = "orange") )+
  scale_x_discrete(name="Region")+
  scale_y_continuous(name="Number of Schools",limits = c(0,500))+
  scale_fill_manual(values = c("Red","Black"),
                    guide = guide_legend(title = "Institution type",nrow = 1,
                                         label.position = "bottom",keywidth = 2.5))
#Annotations
ggplot(data = college) +
  geom_point(mapping = aes(x= tuition, y = sat_avg, col = control,size=undergrads),alpha = 0.6) +
  geom_hline(yintercept = mean(college$sat_avg))+
  geom_vline(xintercept = mean(college$tuition))+
  scale_size_continuous(name = "Undergraduates")+
  scale_color_discrete(name="Institution type")+
  scale_x_continuous(name = "Tuition")+
  scale_y_continuous(name="Average SAT Score")+
  annotate("text",label="Mean SAT",x=47500,y=mean(college$sat_avg)-15)+
  annotate("text",label="Mean Tuition",y=700,x=mean(college$tuition)+7500)+
  annotate("text",label="ELITE Schools",x=47500,y=1450)+
  ggtitle("SAT Scores vs Tuition")
#Maps
install.packages("devtools")
devtools::install_github("dkahle/ggmap")
library(ggmap)
register_google(key = "Paste your API key here")
qmap(location = "coimbatore",zoom=11)
cbe<- get_map(location = "coimbatore",zoom=10)
ggmap(cbe)
ggmap(get_map("College Station, TX",zoom=15))
nyc<- geocode("New York, NY")
usa<- geocode("United States")
ggmap(get_map(nyc))+
  geom_point(mapping=aes(x=lon,y=lat),data=nyc,color="red")
placenames<- c("New York, NY","College Station, TX",
               "California","Houston, TX","Mt. Rushmore")
locations<- geocode(placenames)
locations
tib<- tibble(name=placenames,lat=locations$lat,lon=locations$lon)
ggmap(get_map(usa,zoom = 4,maptype = "watercolor"))+
  geom_point(mapping = aes(x=lon,y=lat),data=tib,color="green")+
  geom_text(mapping=aes(x=lon,y=lat,label=name),data = tib,nudge_y = 1)
#Creating maps manually
install.packages("maps")
library(maps)
install.packages("mapproj")
library(mapproj)
states<- map_data("state")
ggplot(data=states,mapping = aes(x=long,y = lat,group=group))+
  geom_polygon()+
  coord_map()+
  theme(axis.ticks = element_blank(),axis.title = element_blank(),
        axis.text = element_blank(),panel.background = element_blank())
college_summary<- college %>% 
  group_by(state)%>%
  summarise(schools=n())
college_summary <- college_summary%>%
  mutate(region=as.character(setNames(str_to_lower(state.name),
                                      state.abb)[as.character(state)]))
college_summary<- college_summary %>% 
  mutate(region=ifelse(as.character(state)=="DC","district of columbia",region))
View(college_summary)
mapdata<- merge(college_summary,states,by="region")
View(mapdata)
ggplot(data = mapdata)+
  geom_polygon(mapping = aes(x=long,y=lat,group=group,fill=schools))+
  coord_map()+
  theme(axis.title = element_blank(),axis.text = element_blank(),
        plot.background = element_blank(),panel.background = element_blank(),
        axis.ticks = element_blank())+
  scale_fill_gradient(low="beige",high="red")
#case study\
college<- college %>% 
  filter(state!="AK"&state!="HI")
states<- map_data("state")
california<- map_data(map="county",region="california")
college<- college%>% filter(state=="CA")
city_names<- c("San Francisco","San Jose","Sacramento","Los Angeles",
               "San Diego","Fresno")
cities<- geocode(city_names)
city<- tibble(name=city_names,lat=cities$lat,lon=cities$lon)
ggplot(california)+
  geom_polygon(mapping = aes(x=long,y = lat,group=group))+
  coord_map()+
  theme(axis.ticks = element_blank(),axis.title = element_blank(),
        axis.text = element_blank(),panel.background = element_blank())+
  geom_point(mapping = aes(x=lon,y=lat,color=control,size=undergrads),
             data = college,alpha=0.4)+
  geom_text(mapping = aes(x=lon,y=lat,label=name),data = city)+
  scale_size_continuous(name="Undergrad Population")+
  scale_color_discrete(name="Institution type")+
  theme(legend.key=element_blank())+
  ggtitle("Most California Colleges in Popular Cities",
          subtitle = "Source:U.S. Department of Education")
