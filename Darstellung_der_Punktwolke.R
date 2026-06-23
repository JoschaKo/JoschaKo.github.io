# Punktwolkendarstellung für BPM 10 Postervorstellung                 01.06.2026
# von Joscha Koch @ joscha.koch1@stud.hawk.de
# überarbeitet am 23.06.2026
# ------------------------------------------------------------------------------

# Pakete------------------------------------------------------------------------

library(lidR)
library(rgl)
library(terra)
library(sf)
library(htmltools)
library(qrcode)

# Vorbereiten der Punktwolke----------------------------------------------------

## Datenimport

grenze <- sf::st_read('./Daten/Grenzen/Grenze.shp') # Grenzen zum Zuschneiden der LAS
coords <- data.frame(sf::st_coordinates(grenze)) # Koordinaten der Grenzen

las_dtm <- readLAS("./Daten/BPM10_MLS_Flaeche_2_georef.laz") # las import
st_crs(las_dtm) <- 25832 # crs festlegen
las <- lidR::clip_polygon(las_dtm, xpoly = coords$X, ypoly = coords$Y) # Zuschneiden auf Grenzen
las <- filter_duplicates(las) # Duplikate entfernen

segments <- readLAS("./Daten/Fitted sections.laz") # import der Sektionen
st_crs(segments) <- 25832 # crs festlegen
segments <- lidR::clip_polygon(segments, xpoly = coords$X, ypoly = coords$Y) # Zuschneiden auf Grenzen
segments <- filter_duplicates(segments) # Duplikate entfernen

## Normalisierung

las_dtm <- classify_ground(las_dtm, ptd(20)) # Bodenpunkte klassifizieren
dtm <- rasterize_terrain(las_dtm, res = 1, algorithm = knnidw()) # DTM aus der vollständigen Punktwolke berechnen
crs(dtm) <- "EPSG:25832" # crs festlegen
rm(las_dtm) # speicher sparen

nlas <- normalize_height(las, dtm) # Stammfüße auf Null, Gesamtpunktwolke

segments <- normalize_height(segments, dtm) # Sektionen Nomalisieren

writeLAS(nlas, "./Output/nlas.laz") # hochladen in https://3dtrees.earth/

# ACHTUNG-----------------------------------------------------------------------
# nlas in https://3dtrees.earth/ hochladen und segmentieren lassen!
# ACHTUNG-----------------------------------------------------------------------

# Darstellung der segmentierten Punktwolke--------------------------------------

## Import von https://3dtrees.earth/

las <- readLAS("./Daten/3dtree_2013_7867_segmentation.laz") # import des segmentierten laz @ https://3dtrees.earth/
st_crs(las) <- 25832 # crs festlegen

## Punkte reduzieren

### Bodenpunkte entfernen

las <- filter_poi(las, Z > 0.05)
segments <- filter_poi(segments, Z > 0.005)

### Gesamtpunkte

n <- as.numeric(las@header$`Number of point records`)
las <- decimate_points(las, random(n*0.00018))

## Punktwolken zusammenführen

segments@data$PredInstance <- 4444 # Segmente einfärben

common <- intersect(names(las@data), names(segments@data))

las_2 <- las
segments2 <- segments

las_2@data <- las_2@data[, common, with = FALSE]
segments2@data <- segments2@data[, common, with = FALSE]

las_merged <- rbind(las_2, segments2) # Punktwolken verbinden

## Bounding Box anpassen

idm <- which.min(las_merged@data$Z)
las_merged@data$Z[idm] <- las_merged@data$Z[idm] - 10

las_merged@data$PredInstance[idm] <- NA

# HTML Export (WebGL)-----------------------------------------------------------

## Farbpalette anpassen

farbwerte <- c(
  "1"  = "darkgreen",
  "2"  = "#ff7f0e",
  "3"  = "#2c402c",
  "4"  = "#d62728",
  "5"  = "#9467bd",
  "6"  = "#8c564b",
  "7"  = "#e377c2",
  "8"  = "#7f7f7f",
  "9"  = "#bcbd22",
  "10" = "#17becf",
  "11" = "#393b79",
  "12" = "#637939",
  "13" = "#8c6d31",
  "14" = "#843c39",
  "15" = "#7b4173",
  "16" = "#3182bd",
  "17" = "#316354",
  "18" = "#756bb1",
  "19" = "#636363",
  "20" = "#e6550d",
  "21" = "#6baed6",
  "22" = "#74c476",
  "23" = "#9e9ac8",
  "24" = "green")

## Plotten
plot(las_merged, bg = 'black', color = 'PredInstance', pal = farbwerte, NAcol = '#00000000')

## rgl-Widget erzeugen
widget <- rglwidget(elementId = "Punktwolkenausschnitt")

## HTML-Seite erstellen
seite <- tagList(
  
  ### Mobile Optimierung
  tags$head(
    tags$meta(
      name = "viewport",
      content = "width=device-width, initial-scale=1"
    )
  ),
  
  #### Header
  tags$div(
    style = "
      background:#2E6F40;
      color:white;
      padding:15px;
      font-size:24px;
      font-weight:bold;
      text-align:center;
      margin-bottom:15px;",
    "Ausschnitt der Punktwolke inklusive Segmentierung der Einzelbaumdurchmesser"
  ),
  
  ### Widget-Container
  tags$div(
    style = "
    width:95%;
    max-width:1800px;
    margin:0 auto;
    display:flex;
    justify-content:center;",
    widget
  ),
  
  ### Button-Container
  tags$div(
    style = "
      display:flex;
      flex-wrap:wrap;
      justify-content:center;
      gap:15px;
      margin-top:20px;
      margin-bottom:20px;",
    
    tags$a( # Button 1
      href = "https://joschako.github.io/Hausarbeit.pdf",
      target = "_blank",
      style = "
        display:inline-block;
        padding:10px 20px;
        background:#2E6F40;
        color:white;
        text-decoration:none;
        border-radius:5px;",
      "Hausarbeit"
    ),
    
    tags$a( # Button 2
      href = "https://joschako.github.io/Darstellung_der_Punktwolke.R",
      target = "_blank",
      style = "
        display:inline-block;
        padding:10px 20px;
        background:#2E6F40;
        color:white;
        text-decoration:none;
        border-radius:5px;",
      "R-Skript"
    ),
    
    tags$a( # Button 3
      href = "https://3dtrees.earth/datasets/2013",
      target = "_blank",
      style = "
        display:inline-block;
        padding:10px 20px;
        background:#2E6F40;
        color:white;
        text-decoration:none;
        border-radius:5px;",
      "Punktwolke"
    )
  )
)

### HTML speichern
save_html(seite, "punktwolke.html")

# QR-Code für das Poster erstellen----------------------------------------------

qr <- qr_code("https://joschako.github.io/punktwolke.html")
png("qrcode.png", width = 500, height = 500)
plot(qr)

# Ende--------------------------------------------------------------------------