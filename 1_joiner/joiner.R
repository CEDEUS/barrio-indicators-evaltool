library(tidyverse)

barrios_m <- readRDS("Data/barrios_merged.RDS")
barrios_p <- readRDS("Data/barrios_pil.RDS")

manzbarr <- readRDS("Data/Manzanasbarriospiloto.RDS")

vars <- data.frame(
  bigname = c(
    "% Empleados en la misma comuna",
    "% Viviendas de buena calidad",
    "% Mujeres trabajando",
    "% Acceso a tecnologias de la información",
    "Cercanía a Areas Verdes",
    "Caminabilidad a servicios urbanos - 10 min - 1.25 m/s",
    "Caminabilidad a servicios urbanos - 15 min - 1.38 m/s",
    "Mediana de años de construcción (Año mínimo)",
    "Mediana de años de construcción (Año máximo)",
    "Presencia de vehiculos",
    "Avaluo",
    "Personas"
  ),
  codename = c(
    "empl",
    "matr",
    "mujt",
    "acte",
    "aav",
    "acc",
    "acc2",
    "acntrmin",
    "acntrmax",
    "vehiculos",
    "avaluo",
    "personas"
  ),
  path = c(
    "Data/empleabilidadmismacomuna.RDS",
    "Data/materialidad.RDS",
    "Data/mujerestrabajando.RDS",
    "Data/accesotecnologia.RDS",
    "Data/aav.RDS",
    "Data/accesibility_score_final.RDS",
    "Data/accesibility_score_final_15.RDS",
    "Data/anoconstruccion_min.RDS",
    "Data/anoconstruccion_max.RDS",
    "Data/vehiculos.RDS",
    "Data/valuesii.RDS",
    "Data/numerodepersonas.RDS"
  )
)


dataFunc <- function(a, b, c) {
  if(b=="acc" | b=="acc2") {
    t <- readRDS(as.character(a))[,c("ID_W","value","d")]
    t$codename <- rep(as.character(b), nrow(t))
    t$bigname <- rep(as.character(c), nrow(t))
    return(t)
  } else {
    t <- readRDS(as.character(a))[,c("ID_W","value")]
    t <- filter(t, t$ID_W %in% as.character(barrios_m@data$MANZENT))
    t$d <- barrios_m@data[match(t$ID_W,as.character(barrios_m@data$MANZENT)),]$BARRIO
    t$codename <- rep(as.character(b), nrow(t))
    t$bigname <- rep(as.character(c), nrow(t))
    return(t)
  }
}

data <- apply(vars, 1, function(x) dataFunc(x['path'],x['codename'],x['bigname']))
