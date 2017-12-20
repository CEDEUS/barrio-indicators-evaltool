library(tidyverse)

barrios_m <- readRDS("Data/barrios_merged.RDS")
manzbarr <- readRDS("Data/Manzanasbarriospiloto.RDS")

barrios <- read_delim("ID,BARRIO,CIUDAD,COMUNA,LOCATION
0,Barrio Amanecer,Temuco - Padre Las Casas,Temuco,Periferico
                      1,Barrio Estación,Temuco - Padre Las Casas,Temuco,Central
                      2,Camilo Henriquez,Gran Concepción,Concepción,Central
                      3,Fundo El Carmen,Temuco - Padre Las Casas,Temuco,Periferico
                      4,Lomas de San Sebastian,Gran Concepción,Concepción,Periferico
                      5,Villa Los Fundadores,Valdivia,Valdivia,Periferico
                      6,Parque Krahmer,Valdivia,Valdivia,Peri-central
                      7,Sector Regional,Valdivia,Valdivia,Peri-central
                      8,Barrio Brasil,Gran Santiago,Santiago,Central
                      9,Villa Codelco,Copiapó,Copiapó,Peri-central
                      10,Tierra Viva Oriente,Copiapó,Copiapó,Periferico
                      11,El Llano,Gran Coquimbo,Coquimbo,Peri-central
                      12,Villa Vista Hermosa,Gran Coquimbo,La Serena,Peri-central
                      13,Barrio San Miguel,Gran Santiago,San Miguel,Peri-central
                      14,Barrio Las Lilas,Gran Santiago,Providencia,Peri-central
                      15,Barrio Jardin del Este,Gran Santiago,Vitacura,Peri-central
                      16,Barrio Plaza de Maipú,Gran Santiago,Maipú,Central
                      17,Barrios Bajos,Valdivia,Valdivia,Peri-central
                      18,Guayacán,Gran Coquimbo,Coquimbo,Peri-central
                      19,Villa Esperanza,Copiapó,Copiapó,Periferico
                      20,El Faro Parte Alta,Gran Coquimbo,Coquimbo,Peri-central
                      21,Juan XXIII,Gran Coquimbo,La Serena,Periferico
                      22,Chorrillos,Gran Santiago,Independencia,Peri-central
                      23,Brasilia,Gran Santiago,San Miguel,Peri-central
                      24,El Mariscal,Gran Santiago,Puente Alto,Periferico
                      25,U.V. 35 José María Caro,Gran Santiago,Lo Espejo,Peri-central
                      26,Pucará de Lasana,Gran Santiago,Quilicura,Periferico
                      27,Juan González Huerta,Gran Concepción,Talcahuano,Central
                      28,Cerro Verde Alto,Gran Concepción,Penco,Peri-central
                      29,Leonera 2,Gran Concepción,Chiguayante,Periferico
                      30,Padre Hurtado (CCSS),Temuco - Padre Las Casas,Temuco,Periferico
                      31,Las Quilas,Temuco - Padre Las Casas,Temuco,Peri-central", delim = ",")

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
    t <- readRDS(as.character(a))[, c("ID_W", "listo", "d")]
    t$ID_W <- as.numeric(as.character(t$ID_W))
    t <- left_join(t, barrios, by = c("d" = "BARRIO"))
    t$codename <- rep(as.character(b), nrow(t))
    t$bigname <- rep(as.character(c), nrow(t))
    t$manzb <- (t$ID_W %in% as.character(manzbarr$MANZENT))
    return(t)
  } else {
    t <- readRDS(as.character(a))[, c("ID_W", "listo")]
    t$ID_W <- as.numeric(as.character(t$ID_W))
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

write.csv(data,"data.csv")
