librarian::shelf(tidyverse, STATcubeR, janitor, ggstream, ggtext, colorspace, MetBrewer, msthemes)

# Load latest data from API
database <- "str:database:devpi15"
db_schema <- sc_schema_db(database)
einfluss <- db_schema$Influence$`Einfluss Vorjahresmonat`$id
coicop2 <- db_schema$`Mandatory Fields`$`Coicop 5-Steller`$`Coicop 2-Steller`$id
energie <- db_schema$`Mandatory Fields`$`Coicop 5-Steller`$`Coicop 3-Steller`$id
time <- db_schema$`Mandatory Fields`$Time$Time$id

inflation <- sc_table_custom(database, dimensions = c(time,coicop2), measures = einfluss, 
                             language = "de")
energie <- sc_table_custom(database, dimensions = c(time,energie), measures = einfluss, 
                           language = "de")

fin <- bind_rows(inflation$tabulate(), 
                 energie$tabulate() |> filter(str_starts(`Coicop 5-Steller`, "04.5"))) |> 
  as.data.frame()


plotdat <- fin |>
  drop_na() |> 
  rename(Datum=`Zeitraum der Erhebung`) |> 
  mutate(Datum = str_replace_all(Datum, "Jän", "Jan"),
         Datum = dmy(paste("01", Datum), locale = "de_DE")) |> 
  pivot_wider(names_from = "Coicop 5-Steller", values_from = "Einfluss Vorjahresmonat") |> 
  clean_names() |> 
  mutate(`Wohnen & Haushalt` = x04_wohnung_wasser_energie - x04_5_aufwand_fur_energie +
           x05_hausrat_instandhaltung_des_hauses,
         Sonstige = x02_alkoholische_getranke_und_tabak + x03_bekleidung_und_schuhe +
           x06_gesundheitspflege + x08_nachrichtenubermittlung + 
           x10_erziehung_und_unterricht + x12_verschiedene_waren_dienstleistungen) |> 
  select(Datum = datum,
         Nahrungsmittel = x01_nahrungsmittel_alkoholfr_getranke,
         `Wohnen & Haushalt`,
         Energie = x04_5_aufwand_fur_energie,
         Verkehr = x07_verkehr,
         `Freizeit & Kultur` = x09_freizeit_und_kultur,    
         `Restaurants & Hotels` = x11_restaurants_und_hotels,              
         Sonstige)  |> 
  mutate(Inflation = rowSums(across(where(is.numeric)))) |> 
  pivot_longer(cols = -Datum, names_to = "Coicop", values_to = "Beitrag") |> 
  mutate(Coicop = factor(Coicop, levels = c("Sonstige", "Restaurants & Hotels","Freizeit & Kultur","Nahrungsmittel","Energie", "Wohnen & Haushalt", "Verkehr", "Inflation"))) |> 
  filter(Datum >= "2019-01-01")

pal <- met.brewer("Lakota")[c(2,1,5,3)]
cols <- c(rbind(lighten(pal, 0.1), darken(pal, 0.2)))

plotdat |>  filter(!Coicop == "Inflation") |> 
  ggplot(aes(x = Datum, y = Beitrag, group = Coicop, fill = Coicop)) +
  geom_stream(type = "mirror") +
  scale_fill_manual(values = cols, name = "") +
  labs(x=NULL, y=NULL) +
  coord_cartesian(expand = F) +
  theme_ms() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        axis.text = element_blank(),
        panel.background = element_rect(fill='transparent', color = NA),
        plot.background = element_rect(fill='transparent', color=NA),
        legend.background = element_rect(fill='transparent',color = NA))

ggsave("bgimage2.png", dpi = 320, width = 8, height = 2.3, bg = "transparent")
