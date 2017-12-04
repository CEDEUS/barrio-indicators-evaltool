library(tidyverse)

barrios_m <- readRDS("Data/barrios_merged.RDS")
manzbarr <- readRDS("Data/Manzanasbarriospiloto.RDS")

barrios <- read_delim("Id,BARRIO,CIUDAD,COMUNA
                      0,Barrio Amanecer,Temuco - Padre Las Casas,Temuco
                      0,Barrio Estación,Temuco - Padre Las Casas,Temuco
                      0,Camilo Henriquez,Gran Concepción,Concepción
                      0,Fundo El Carmen,Temuco - Padre Las Casas,Temuco
                      0,Lomas de San Sebastian,Gran Concepción,Concepción
                      0,Villa Los Fundadores,Valdivia,Valdivia
                      0,Parque Krahmer,Valdivia,Valdivia
                      0,Sector Regional,Valdivia,Valdivia
                      0,Barrio Brasil,Gran Santiago,Santiago
                      0,Villa Codelco,Copiapó,Copiapó
                      0,Tierra Viva Oriente,Copiapó,Copiapó
                      0,El Llano,Gran Coquimbo,Coquimbo
                      0,Villa Vista Hermosa,Gran Coquimbo,La Serena
                      0,Barrio San Miguel,Gran Santiago,San Miguel
                      0,Barrio Las Lilas,Gran Santiago,Providencia
                      0,Barrio Jardin del Este,Gran Santiago,Vitacura
                      0,Barrio Plaza de Maipú,Gran Santiago,Maipú
                      0,Barrios Bajos,Valdivia,Valdivia
                      0,Guayacán,Gran Coquimbo,Coquimbo
                      0,Villa Esperanza,Copiapó,Copiapó
                      0,El Faro Parte Alta,Gran Coquimbo,Coquimbo
                      0,Juan XXIII,Gran Coquimbo,La Serena
                      0,Chorrillos,Gran Santiago,Independencia
                      0,Brasilia,Gran Santiago,San Miguel
                      0,El Mariscal,Gran Santiago,Puente Alto
                      0,U.V. 35 José María Caro,Gran Santiago,Lo Espejo
                      0,Pucará de Lasana,Gran Santiago,Quilicura
                      0,Juan González Huerta,Gran Concepción,Talcahuano
                      0,Cerro Verde Alto,Gran Concepción,Penco
                      0,Leonera 2,Gran Concepción,Chiguayante
                      0,Padre Hurtado (CCSS),Temuco - Padre Las Casas,Temuco
                      0,Las Quilas,Temuco - Padre Las Casas,Temuco", delim = ",")

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
  if (b == "acc" | b == "acc2") {
    t <- readRDS(as.character(a))[, c("ID_W", "value", "d")]
    t <- left_join(t, barrios, by = c("d" = "BARRIO"))
    t$codename <- rep(as.character(b), nrow(t))
    t$bigname <- rep(as.character(c), nrow(t))
    t$manzb <- (t$ID_W %in% as.character(manzbarr$MANZENT))
    return(t)
  } else {
    t <- readRDS(as.character(a))[, c("ID_W", "value")]
    t <- right_join(t,barrios_m@data, by = c("ID_W" = "MANZENT"))
    t <- right_join(t,barrios, by = "BARRIO") %>% mutate(d=BARRIO)
    t$codename <- rep(as.character(b), nrow(t))
    t$bigname <- rep(as.character(c), nrow(t))
    t$manzb <- (t$ID_W %in% as.character(manzbarr$MANZENT))
    return(t)
  }
}

data <- apply(vars, 1, function(x)
  dataFunc(x["path"], x["codename"], x["bigname"]))

data <- bind_rows(data)
