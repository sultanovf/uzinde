# ==============================================================================
# R Script for Quarto Shiny file (uzbinde.qmd).                                |
# by Fazliddin Sultonov                                                        |
# ==============================================================================



# load variables ===============================================================================

source("src/r_var.R")

# load data =====================================================================================
data_raw <- read.csv2(file = "data/usbinde.csv", header = TRUE, sep = ";")

# prapare data
data <-
  data_raw |> 
  replace(data_raw == "-", 0) |>
  #select(-c(1:3)) |> 
  #mutate_if(is.character,as.numeric) |>
  mutate_at(-c(1:3), as.numeric) |> 
  #mutate(across(everything(), na_if, 0)) |> 
  separate(col = Stichtag, into = c("D", "M", "Year")) |> 
  select(-c("D","M")) 
  #head()


# Check if there is na ------------------------------------------------
# any(is.na(data))
# colSums(is.na(data))
# rowSums(is.na(data))

# row & col with NAs
# which(is.na(data), arr.ind=TRUE)

# hier letzte row leer, cols haben na, deswegen letzte row löschen
data <- na.omit(data)                      # löscht alle rows mit NA
# data <- data[complete.cases(data), ]       # gibt df zurück ohne rows mit NA
# data <- data[rowSums(is.na(data)) == 0, ]  # gibt df zurück mit rows ohne NA

# rename colnames with customised names
data <- rename(data, any_of(lookup))

# Data Frame for Server ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# test <-
#   data |> 
#   mutate_at("Year", as.integer) |> 
#   rowwise() |> 
#   mutate(Cnt = sum(c_across(4:length(colnames(data)))))
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Cisco daten from Germany für Mapplot
ger <- gisco_get_nuts(
  year = 2024,
  epsg = 4326,
  nuts_level = 1,
  resolution =  "01",
  country = "Germany"
)

# df for map
last_y <- max(as.integer(unique(data$Year)))
df_de <-
  data |> 
  mutate_at("Year", as.integer) |> 
  filter(Year==last_y) |> 
  rowwise() |> 
  mutate(Cnt = sum(c_across(4:length(colnames(data))))) |>
  select(c(BL, Geschlecht, Cnt)) |> 
  group_by(BL) |> 
  summarise(Total = sum(Cnt))

# Merge df_de wit cisco data ger
# create col BL by mapping bl and Nuts Name

ger |> 
  left_join(df_de, by = c("NUTS_NAME" = "BL")) |> 
  mutate(BL = bl[ger$NUTS_NAME]) |> 
  mutate(lblName = paste0(BL, "-", NUTS_NAME)) |> 
  select(NUTS_NAME, Total, BL, lblName) -> df_map

# df_map$BL <- bl[df_map$NUTS_NAME]

# create mapplot
#map_plt <-
ggplot(df_map) +
  geom_sf(aes(fill = lblName))+  
  geom_sf_label(aes(label = paste(BL, ":", Total)), fill = "aliceblue", col = "dodgerblue4", colour = "aliceblue") +
  geom_text(aes(y = 47.1, x = 6.9, label = paste("Gesamt:", sum(Total))),stat = "unique",
            size = 3.9, size.unit = "mm", col = "dodgerblue4", fontface = "bold")+
  annotation_raster(img_dop, xmin = 6, xmax = 8, ymin = 54, ymax = 55.5, interpolate = FALSE) +
  theme_minimal()+
  labs(
    title = glue::glue("**Usbeken in Deutschland | Zahlen für 2024**"),
    subtitle = "**Data Source:** *©Statistisches Bundesamt (Destatis), 2025 | Stand: 31.07.2025*",
    caption = "*Plot by:* ***Fazliddin Sultonov***"
  ) + 
  guides(fill = guide_legend(title="Bundesland")) +
  theme(plot.title = ggtext::element_markdown(size = 13, color = "steelblue4"),
        plot.subtitle = element_markdown(),
        plot.caption = element_markdown(size = 11), # element_textbox_simple()
        plot.caption.position = "plot",             #plot, margin, panel,
        legend.title = element_text(color = "darkgrey", size = 12, face = "bold"),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()
  )

# Funnel chart ============================================
df_stat<-
  data |> 
  # mutate_at("Year", as.character()) |>
  # filter(Year==2022) |> 
  pivot_longer(!c(Year, BL, Geschlecht), names_to = "Status", values_to = "Cnt") |> 
  mutate(
    Cnt = case_when(
      Geschlecht == "männlich" ~ Cnt * -1,
      TRUE ~ Cnt) 
  ) #|> ungroup()

brks <- c(seq(-4000, 4000, by = 500))
lbls = c(seq(4, 0, -0.5), seq(0.5, 4, 0.5))
input <- "2024"
df_stat |> 
  filter(Year==input) |> 
  ggplot(aes(x = reorder(Status, abs(Cnt)), y= Cnt, group = Geschlecht))+
  geom_col(aes(fill = Geschlecht), position="stack")+
  #geom_col(stat = "identity", width = .6, position = "dodge") +
  #geom_bar(fill = Geschlecht)+
  scale_y_continuous(breaks = brks, labels = function(x) ifelse(x == 0, paste0(lbls), paste0(lbls, "k"))) +
  #scale_x_continuous(expand = expansion(c(0.01, 0.05)))
  scale_fill_manual(
    name = "Geschlecht: ",
    values=c(männlich = "#39bfc2", weiblich = "tomato3")) +
  labs(
    title = glue("**Anzahl nach Aufenthaltsstatus {input}**"),
    subtitle = "*Data Source:* ©Statistisches Bundesamt (Destatis), 2025 | Stand: 31.07.2025",
    caption = "*Plot by:* ***Fazliddin Sultonov***",
    x = "Aufenthaltsstatus",
    y = "k = Tausend"
  ) +
  coord_flip() +
  theme_minimal()+
  theme(
    plot.title = element_markdown(size = 14, color = "steelblue4", hjust = 0.5),
    plot.subtitle = element_markdown(size = 11, hjust = 0.5),
    plot.caption = element_markdown(size = 12, color = "steelblue4", hjust = 0.5),
    legend.position = "inside", legend.position.inside = c(0.76, 0.07),
    legend.direction = "horizontal",
    axis.text.y = element_text(face = "bold", size = 11),
    axis.text.x = element_text(face = "italic", size = 10),
    panel.background = element_rect(fill = "gray100"),
    plot.background = element_rect(fill = "gray95")

  )

# Barplot grouped bei Geschlecht
df_gen <- 
  data |> 
  # mutate_at("Year", as.integer) |> 
  #filter(Year==input) |> 
  rowwise() |> 
  mutate(Cnt = sum(c_across(4:length(colnames(data))))) |>
  select(c(Year, BL, Geschlecht, Cnt)) |> 
  pivot_wider(names_from = Geschlecht, values_from = Cnt)  |> 
  rowwise() |> 
  mutate(Total = sum(männlich, weiblich)) |> 
  pivot_longer(!c(Year, BL, Total), names_to = "Geschlecht", values_to = "Cnt")

# geom_col
df_gen |> 
  filter(Year==input) |>
  ggplot(aes(x = reorder(BL, Cnt), y = Cnt, group = Geschlecht))+
  geom_col(aes(fill = Geschlecht), position = "dodge") +
  geom_text(aes(label = Cnt, y = Cnt - 35),
    position = position_dodge(0.9),
    hjust = 0.75, size=3.5, color = "white") +
  geom_col(aes(y = Total/2), width = 0.95, fill = "gray", alpha = 0.3)+
  geom_text(aes( y = Total, label=paste("Σ:", Total)), hjust = 0.3, color="gray37", size=3.5) +
  scale_y_continuous(
    labels = function(y) ifelse(y == 0, paste0(y), paste0(y, "k")),
    expand = expansion(c(0.01, 0.3))) +
  scale_fill_manual(
    name = "Geschlecht: ",
    values=c(männlich = "#39bfc2", weiblich = "tomato3"))+
  labs(
    title = "**Anzahl in Bundesländern bei Geschlecht**",
    subtitle = "*Data Source:* ©Statistisches Bundesamt (Destatis), 2025 | Stand: 31.07.2025",
    caption = "*Plot by:* ***Fazliddin Sultonov***",
    x = NULL,y = NULL) +
  coord_flip() +
  theme_minimal() +
  theme(
    legend.position = "inside", legend.position.inside = c(0.76, 0.07),
    legend.direction = "horizontal",
    #legend.title = element_blank(),
    axis.text.y = element_text(face = "bold", size = 10),
    axis.text.x = element_text(face = "italic", size = 9),
    plot.title = element_markdown(size = 14, color = "steelblue4", hjust = 0.5),
    plot.subtitle = element_markdown(size = 11, hjust = 0.5),
    plot.caption = ggtext::element_markdown(size=11, hjust = 0.5),
    panel.background = element_rect(fill = "ghostwhite"),
    plot.background = element_rect(fill = "aliceblue")
  )



## Data for Line Plot ===================================================================================================
data |> 
  mutate_at("Year", as.integer) |> 
  rowwise() |> 
  mutate(Cnt = sum(c_across(4:length(colnames(data))))) |> 
  select(c(Year, BL, Geschlecht, Cnt)) |> 
  group_by(Year, Geschlecht) |> 
  summarise(Total = sum(Cnt)) -> df_time

f_date <- as.integer(first(unique(df_time$Year)))
l_date <- as.integer(last(unique(df_time$Year)))
# pimg <- "deuz.png"
# imgp <- readPNG("uzgirl_2.png")


df_time |> 
  ggplot(aes(x = Year, y = Total, group = Geschlecht, color = Geschlecht))+
  geom_line() +
  geom_point() +
  scale_y_continuous(
    labels = function(x) paste(x/1000, "k")) +
  scale_x_continuous(
    limits = c(f_date,l_date),
    breaks = seq(f_date,l_date, 2),
    expand = expansion(c(0.01, 0.02))) + 
  labs(
    title = "Anzahl bei Geschlecht über Jahren",
    subtitle = "source",
    caption = "Fazi",
    x = NULL, y = "k = Tausend")+
  theme_minimal()+
  my_theme +  # standard verwendbar in anderen plots/ in r_var.R
  theme(
    axis.ticks.x = element_line(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.background = element_rect(fill = "ghostwhite"),
    panel.background = element_rect(fill = "gray100"),
    
    axis.text.x = element_text(size = 11, face = "bold"),
    axis.text.y = element_text(size = 10, face = "bold.italic"))

