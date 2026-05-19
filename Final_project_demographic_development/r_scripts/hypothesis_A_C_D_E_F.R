#load packages
library(tidyverse)
library(readr)
library(here)
library(dplyr)
library(gganimate)
library(gifski)
library(av)
library(ggplot2)

#read CSV files
total_1801 <- read_csv2("data/1801_total.csv", na="NULL")
total_1834 <- read_csv2("data/1834_total.csv", na="NULL")
total_1860 <- read_csv2("data/1860_total.csv", na="NULL")

#create objects with selected rows
#add column for shire in 1801 and 1834
clean_1801 <- total_1801 %>%
  filter(amt=="Århus") %>% 
  select(amt, sogn, koen, alder, civilstand) %>% 
  mutate(
    herred = case_when(
      sogn %in% c("Lyngå", "Hadsten", "Vitten", "Haldum","Foldby", "Lading", "Sabro", "Fårup") ~ "Sabro",
      sogn %in% c("Spørring","Grundfør","Trige","Søften","Ølsted","Elev","Elsted","Lisbjerg") ~ "Vester Lisbjerg",
      sogn %in% c("Skivholme","Borum","Sjelle","Skørring","Galten","Skovby","Framlev","Storring","Harlev","Stjær") ~ "Framlev",
      sogn %in% c("Kasted","Skejby","Vejlby","Tilst","Lyngby","Årslev","Brabrand","Hasle","Åby","Århus Købstad") ~ "Hasle",
      sogn %in% c("Ormslev","Kolt","Viby","Holme","Tranbjerg","Tiset","Mårslet","Beder","Malling","Astrup","Tulstrup","Tunø") ~ "Ning",
      sogn %in% c("Odder","Hvilsted","Torrild","Nølev","Saksild","Bjerager","Randlev","Hundslund","Falling","Ørting","Halling","Gosmer","Gylling","Alrø") ~ "Hads",
      TRUE ~ "Ukendt"
    )
  )

clean_1834 <- total_1834 %>%
  filter(amt=="Århus") %>% 
  select(amt, sogn, koen, alder, civilstand) %>% 
  mutate(
    herred = case_when(
      sogn %in% c("Lyngå", "Hadsten", "Vitten", "Haldum","Foldby", "Lading", "Sabro", "Fårup") ~ "Sabro",
      sogn %in% c("Spørring","Grundfør","Trige","Søften","Ølsted","Elev","Elsted","Lisbjerg") ~ "Vester Lisbjerg",
      sogn %in% c("Skivholme","Borum","Sjelle","Skørring","Galten","Skovby","Framlev","Storring","Harlev","Stjær") ~ "Framlev",
      sogn %in% c("Kasted","Skejby","Vejlby","Tilst","Lyngby","Årslev","Brabrand","Hasle","Åby","Århus Købstad") ~ "Hasle",
      sogn %in% c("Ormslev","Kolt","Viby","Holme","Tranbjerg","Tiset","Mårslet","Beder","Malling","Astrup","Tulstrup","Tunø") ~ "Ning",
      sogn %in% c("Odder","Hvilsted","Torrild","Nølev","Saksild","Bjerager","Randlev","Hundslund","Falling","Ørting","Halling","Gosmer","Gylling","Alrø") ~ "Hads",
      TRUE ~ "Ukendt"
    )
  )

clean_1860 <- total_1860 %>%
  filter(amt=="Århus") %>% 
  select(amt, herred, sogn, koen, alder, civilstand)

#combining datasets
#add column for year
all_census <- bind_rows(
  mutate(clean_1801, år = 1801),
  mutate(clean_1834, år = 1834),
  mutate(clean_1860, år = 1860)
)


  ## Hypothesis A ##

#create object, count inhabitants in each shire each year
inhab_shire <- all_census %>% 
  count(herred,år)

#plot
ggplot(inhab_shire,
       aes(x=år,y=n,color=herred))+
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  theme_bw()+
  theme(
    axis.text.x = element_text(size = 8),
    panel.spacing = unit(1, "lines"),
    text = element_text(size = 13))+
  labs(title = "Population growth: Aarhus county",
       x = "Year",
       y = "Population",
       color = "Shire")

ggsave("pop_grow_county.png")

#Same but only Hasle shire 
inhab_hasle <- all_census %>% 
  filter(herred=="Hasle") %>% 
  count(sogn,år)

#plot
ggplot(inhab_hasle,
       aes(x=år,y=n,color=sogn))+
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  theme_bw()+
  theme(
    axis.text.x = element_text(size = 8),
    panel.spacing = unit(1, "lines"),
    text = element_text(size = 13))+
  labs(title = "Population growth: Hasle shire",
       x = "Year",
       y = "Population",
       color = "Parish")

ggsave("pop_grow_hasle.png")


  ## Hypothesis C ##

#create object, calculate average age in each shire
inhab_age <- all_census %>%
  filter(år %in% c(1801, 1834, 1860)) %>%
  group_by(år,herred) %>%
  summarise(
    average_age = mean(alder, na.rm = TRUE)
  )

#plot it
ggplot(inhab_age,
       aes(x = år,
           y = average_age,
           group = herred,
           color = herred)) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  theme_bw()+
  theme(
    axis.text.x = element_text(size = 8),
    panel.spacing = unit(1, "lines"),
    text = element_text(size = 13))+
  labs(
    title = "Average age over time in Aarhus county",
    x = "Year",
    y = "Average age",
    color = "District"
  )

ggsave("age_aarhus.png")

#Calculate average age in Hasle shire 
inhab_age_hasle <- all_census %>%
  filter(herred=="Hasle") %>% 
  filter(år %in% c(1801, 1834, 1860)) %>%
  group_by(år,sogn) %>%
  summarise(
    average_age = mean(alder, na.rm = TRUE)
  )

#plot it
ggplot(inhab_age_hasle,
       aes(x = år,
           y = average_age,
           group = sogn,
           color = sogn)) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  theme_bw()+
  theme(
    axis.text.x = element_text(size = 8),
    panel.spacing = unit(1, "lines"),
    text = element_text(size = 13))+
  labs(
    title = "Average age over time in Hasle shire",
    x = "Year",
    y = "Average age",
    color = "Parish"
  )

ggsave("age_hasle.png")


  ## Hypothesis D ##

#check witch labels are included in the dataset
unique(all_census$civilstand)
unique(all_census$koen)

#categorize labels as married 
all_census <- all_census %>%
  mutate(
    civilstand = case_when(
      civilstand %in% c("enke", "separeret", "enkemand", "skilt", "forlovet") ~ "gift",
      TRUE ~ civilstand
    )
  )

#create object, count number of married and unmarried in Aarhus county
  #calculate average age for both groups 
mean_marriage <- all_census %>%
  filter(
    alder >= 15,
    alder <= 50,
    civilstand %in% c("gift", "ugift"), 
    civilstand != "",
    koen != "") %>%
  group_by(år, herred, civilstand) %>%
  summarise(antal=n(),
    gennemsnitsalder = mean(alder, na.rm = TRUE))

#plot it
ggplot(mean_marriage,
       aes(x = år,
           y = gennemsnitsalder,
           color = civilstand,
           group=civilstand)) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  facet_wrap(~herred) +
  labs(
    title = "Average age of marriage: Aarhus county",
    x = "Year",
    y = "Average age",
    color = "Marital status")+
  scale_color_discrete(labels = c(
        "Married",
        "Unmarried"))+
  theme_bw()+
  theme(
    axis.text.x = element_text(size = 8),
    legend.position = "bottom",
    panel.spacing = unit(1, "lines"),
    text = element_text(size = 15))

ggsave("mean_marry_aarhus.png",width = 8, height = 6)

#Do the same for Hasle shire only 
mean_marriage_hasle <- all_census %>%
  filter(
    alder >= 15,
    alder <= 50,
    civilstand %in% c("gift", "ugift"),
    herred=="Hasle",
    civilstand != "",
    koen != "") %>%
  group_by(år, sogn, civilstand) %>%
  summarise(antal=n(),
            gennemsnitsalder = mean(alder, na.rm = TRUE))

#plot it
ggplot(mean_marriage_hasle,
       aes(x = år,
           y = gennemsnitsalder,
           color = civilstand,
           group=civilstand)) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  facet_wrap(~sogn, ncol=4) +
  labs(
    title = "Average age of marriage: Hasle shire",
    x = "Year",
    y = "Average age",
    color = "Marital status")+
  scale_color_discrete(labels = c(
    "Married",
    "Unmarried"))+
  theme_bw()+
  theme(
    axis.text.x = element_text(size = 8),
    legend.position = "bottom",
    panel.spacing = unit(1, "lines"),
    text = element_text(size = 15))

ggsave("mean_marriage_hasle.png",width = 8, height = 6)


  ## Hypothesis E ##

#object, calculate share of elders in Aarhus county
plot_old_aarhus <- all_census %>%
  filter(alder != "") %>%
  group_by(år, herred) %>%
  summarise(
    total_befolkning = n(),
    over_60 = sum(alder > 60),
    procent_over_60 = over_60 / total_befolkning * 100
  )

#plot it
ggplot(plot_old_aarhus,
       aes(x = år,
           y = procent_over_60,
           group=herred,
           color = herred)) +
  geom_line(size = 0.5) +
  geom_point(size = 1)+
  labs(
    title = "Elder population: Aarhus county",
    x = "Year",
    y = "Percentage share of elders (>60)",
    color = "Shire:")+
  theme_bw()+
  theme(
    axis.text.x = element_text(size = 8),
    legend.position = "bottom",
    panel.spacing = unit(1, "lines"),
    text = element_text(size = 17)) 

ggsave("above_60_aarhus.png", width = 8, height = 8)

#Same for Hasle shire only
plot_old_hasle <- all_census %>%
  filter(herred=="Hasle",
         alder != "") %>%
  group_by(år, sogn) %>%
  summarise(
    total_befolkning = n(),
    over_60 = sum(alder > 60),
    procent_over_60 = over_60 / total_befolkning * 100
  )

#plot it
ggplot(plot_old_hasle,
       aes(x = år,
           y = procent_over_60,
           group=sogn,
           color = sogn)) +
  geom_line(size = 0.5) +
  geom_point(size = 1)+
  labs(
    title = "Elder population: Hasle shire",
    x = "Year",
    y = "Percentage share of elders (>60)",
    color = "Parish: ")+
  theme_bw()+
  theme(
    axis.text.x = element_text(size = 8),
    legend.position = "bottom",
    panel.spacing = unit(1, "lines"),
    text = element_text(size = 17))

ggsave("above_60_hasle.png", width = 8, height = 8)


  ## Hypothesis F ##

#Calculate share of children below 15 in Aarhus county
plot_young_aarhus <- all_census %>%
  filter(alder != "") %>%
  group_by(år, herred) %>%
  summarise(
    total_befolkning = n(),
    under_15 = sum(alder < 15),
    procent_under_15 = under_15 / total_befolkning * 100
  )

#plot it
ggplot(plot_young_aarhus,
       aes(x = år,
           y = procent_under_15,
           group=herred,
           color = herred)) +
  geom_line(size = 0.5) +
  geom_point(size = 1)+
  labs(
    title = "Young population: Aarhus county",
    x = "Year",
    y = "Percentage share of children (<15)",
    color = "Shire")+
  theme_bw()+
  theme(
    axis.text.x = element_text(size = 8),
    legend.position = "bottom",
    panel.spacing = unit(1, "lines"),
    text = element_text(size = 18))

ggsave("children_aarhus.png",width = 8, height = 6)

#Do the same for Hasle shire only
plot_young_hasle <- all_census %>%
  filter(,
    herred=="Hasle",
    alder != "") %>%
  group_by(år, sogn) %>%
  summarise(
    total_befolkning = n(),
    under_15 = sum(alder < 15),
    procent_under_15 = under_15 / total_befolkning * 100
  )

#plot it
ggplot(plot_young_hasle,
       aes(x = år,
           y = procent_under_15,
           group=sogn,
           color = sogn)) +
  geom_line(size = 0.5) +
  geom_point(size = 1)+
  labs(
    title = "Young population: Hasle shire",
    x = "Year",
    y = "Percentage share of children (<15)",
    color = "Parish")+
  theme_bw()+
  theme(
    axis.text.x = element_text(size = 8),
    panel.spacing = unit(1, "lines"),
    text = element_text(size = 18))

ggsave("children_hasle.png", width = 8, height = 6)
  
