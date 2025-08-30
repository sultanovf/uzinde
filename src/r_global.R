# ==============================================================================
# R Script for Quarto Shiny file (uzbinde.qmd).                                |
# by Fazliddin Sultonov                                                        |
# ==============================================================================



# load variables ===============================================================================

source("src/r_var.R")

# load data =====================================================================================
data_raw <- read.csv2(file = "data/usbinde.csv", header = TRUE, sep = ";")

# prapare data
# for Page data ===========================================================
dt <-
  data_raw |> 
  replace(data_raw == "-", 0) |>
  mutate_at(-c(1:3), as.numeric) 
  # separate(col = Stichtag, into = c("D", "M", "Year")) |> 
  # select(-c("D","M")) 


# hier letzte row leer, cols haben na, deswegen letzte row löschen
dt <- na.omit(dt)                            # löscht alle rows mit NA
# data <- data[complete.cases(data), ]       # gibt df zurück ohne rows mit NA
# data <- data[rowSums(is.na(data)) == 0, ]  # gibt df zurück mit rows ohne NA

# for furter analyste/Server ====================================================


# rename colnames with customised names
data <- 
  dt |> 
  separate(col = Stichtag, into = c("D", "M", "Year")) |> 
  select(-c("D","M")) |> 
  rename(any_of(lookup))

# Data Frame for Server ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# test <-
#   data |> 
#   mutate_at("Year", as.integer) |> 
#   rowwise() |> 
#   mutate(Cnt = sum(c_across(4:length(colnames(data)))))
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# Funnel chart ==================================================================================
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



## Data for Line Plot ===================================================================================================
# f_date <- as.integer(first(unique(data$Year)))
# l_date <- as.integer(last(unique(data$Year)))
df_time <-
  data |> 
  mutate_at("Year", as.integer) |> 
  rowwise() |> 
  mutate(Cnt = sum(c_across(4:length(colnames(data))))) |> 
  select(c(Year, BL, Geschlecht, Cnt)) |> 
  group_by(Year, Geschlecht) |> 
  summarise(Total = sum(Cnt))


# deploy
#library(rsconnect)
#rsconnect::writeManifest()
