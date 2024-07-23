# load packages-----
library(tidyverse)
library(plotly)
library(glue)

# load data ----

## Dateinamen erzeugen
date <- date <- '2024-07-22'
my_in_file<-glue('bip_verwendung_{date}.rds')

### Daten importieren
load(xfun::from_root("data","VGR",my_in_file))


# Daten aufbereiten -----

## Außenbeitrag ermitteln -----
bip_tmp <- bip_verwendung %>%
  group_by(Jahr) %>%
  mutate(AB = EX-IM) %>%
  ungroup
#check
#bip_tmp$C_priv+bip_tmp$C_gov+bip_tmp$Inv_br+bip_tmp$AB

## Anteile berechnen ----
verw_shares <- bip_tmp %>%
  pivot_longer(!Jahr,names_to = "Aggregat") %>%
  group_by(Jahr) %>%
  filter(Aggregat%in%c("C_priv","C_gov","Inv_br","AB")) %>%
  mutate(Aggregat=factor(Aggregat,levels=c("C_priv","C_gov","Inv_br","AB"),
                         labels=c("private Konsumausgaben",
                                  "Konsumausgaben des Staates",
                                  "Bruttoinvestitionen",
                                  "Aussenbeitrag")
  ),
  share=value/sum(value),
  Anteil=sprintf("%.2f%%",share*100)
  )

# Dataviz -----
year_list <- c(1991,2001,2011,2016,2021,2023)
library(RColorBrewer)
p <- verw_shares %>%
  filter(Jahr%in%year_list) %>%
  ggplot(aes(fill=Aggregat,
             x=share, y=as.factor(Jahr),label=Anteil),
         text = paste("", Aggregat, "", Anteil)) +
  geom_bar(stat="identity") +
  labs(x="Anteil in Prozent",y="Jahr") +
  scale_x_continuous(labels = scales::percent) +
  scale_fill_manual(values =c("violetred4","violetred3",
                              "turquoise4","turquoise2"))
#p
#p

#my_img <- glue("bip_verwendung_{date}.svg")
#ggsave(xfun::from_root("images","unit01",my_img),
#       width=1250,height = 750,units="px")

p <- plotly::ggplotly(p,tooltip = c("Aggregat", "Anteil"))

