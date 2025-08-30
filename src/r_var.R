# ==============================================================================
# R Script for Quarto Shiny file (r_global.R.qmd).                                |
# by Fazliddin Sultonov                                                        |
# ==============================================================================


# Load libraries ===============================================================
# package for install (if not) and load packages
if (!require(pacman)) {
  install.packages("pacman")
  require("pacman")
}
#
pacman::p_load(
  #odbc,         # for db connect
  tidyverse,    # for data manupulation, dplyr, ggplot, ...
  lubridate,    # for data time
  DT,           # for data table
  glue,         # string
  ggpol,        # für facet_share
  ggpubr,       # ggdortchart, ggtexttable
  gridExtra,    # Multiple plots and tables together
  gt,           # Table
  gtExtras,     # Support gt
  svglite,      # for gt Tables
  giscoR,       # for map
  png,          # read png
  ggimage,      # pictures to the plot
  ggtext,       # markdown
  ggpol         # facet
)

# Paths ==================================================================================
# images --------------------
img_dop <- readPNG("pics/dop.png")

# Variables ==============================================================================
lookup <- c(
  BL = "Bundesland", 
  AE_FreiG_EU = "Aufenthaltsrecht.nach.FreizügG.EU",
  AE_Befreit = "Vom.Erfordernis.eines.Aufenthaltstitels.befreit", 
  NE ="Unbefristete.Niederlassungserlaubnis",
  AE_Befristet = "Befristete.Aufenthaltserlaubnis",
  AE_Ausbildung = "Befristete.Aufenthaltserlaubnis..Ausbildung",
  AE_Arbeit = "Befristete.Aufenthaltserlaubnis..Erwerbstätigkeit",
  AE_Human = "Befristete.AE..völkerrechtl...human...pol..Gründe",
  AE_Family = "Befristete.Aufenthaltserlaubnis..familiäre.Gründe",
  National_Visa = "Befristete.AE..besondere.Gründe.und.nationale.Visa",
  AE_Antrag = "Antrag.auf.einen.Aufenthaltstitel.gestellt",
  AE_Gestattung = "Aufenthaltsgestattung",
  AE_Ohne = "Ohne.Aufenthaltstitel..Duldung.oder.Gestattung"
)

# Abkürzung Bundesländer
bl <- c("Schleswig-Holstein" = "SH", "Mecklenburg-Vorpommern"="MV", "Thüringen" = "TH", 
"Niedersachsen" = "NI", "Baden-Württemberg" = "BW", "Nordrhein-Westfalen" = "NW", 
"Rheinland-Pfalz" = "RP", "Saarland" = "SL", "Bayern" ="BY", "Berlin" ="BE", 
"Sachsen" ="SN", "Brandenburg" ="BB", "Bremen" ="HB", "Hamburg" ="HH", 
"Sachsen-Anhalt" = "ST", "Hessen" ="HE")

# Plot ===================================================================================
subt <- "*Data Source:* ©Statistisches Bundesamt (Destatis), 2025 | Stand: 31.07.2025"
cap <- "*Plot by:* ***Fazliddin Sultonov***"

# Theme standard
my_theme <- theme(
  plot.title = element_markdown(size = 14, color = "steelblue4", hjust = 0.5),
  plot.subtitle = element_markdown(size = 12, hjust = 0.5),
  plot.caption = element_markdown(size = 12, color = "steelblue4", hjust = 0.5),
  legend.position = "inside", legend.position.inside = c(0.76, 0.07),
  legend.direction = "horizontal",
)


