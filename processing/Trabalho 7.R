### Práctico 7       ###
### Joshe Norambuena ###
########################

#### 1. Cargar librerías ####
pacman::p_load(tidyverse, #Conjunto de paquetes, sobre todo dplyr y ggplot2
               car, #Para recodificar
               haven,
               summarytools, #Para descriptivos
               sjmisc,
               psych     # para Alfa de Chronbach
)

## 1.1 Desactivar notación cientifica
options(scipen = 999) # para desactivar notacion cientifica

## 1.2 Limpiar el espacio de trabajo
rm(list = ls()) # para limpiar el entorno de trabajo

#### 2. Cargar base de datos ####
load(url("https://github.com/cursos-metodos-facso/investigacion-cuantitativa/raw/main/files/data/casen2022.RData")) #Cargar base de datos

#### 3. Descripción de variables ####
view(dfSummary(casen2022, headings=FALSE, graph.col = FALSE))

#### 4. Crear indicadores de pobreza multidimensional con cuatro indicadores ####
## 4.1 Seleccionar indicadores
indicadores2014 <- casen2022 %>% select(asistencia, 
                                        rezago, 
                                        escolaridad, 
                                        malnutricion, 
                                        sist_salud, 
                                        atencion, 
                                        ocupacion, 
                                        seg_social, 
                                        jubilacion, 
                                        hacinamiento, 
                                        estado_vivienda=vivienda, 
                                        serv_basicos)  %>% 
  na.omit() %>% # Eliminar Na's
  mutate_all(~(as.numeric(.))) # Convertimos todas las variables a numéricas

## 4.1 Crear una variable para cada dimensión
indicadores2014 = indicadores2014 %>% 
  rowwise() %>%
  mutate(educ = mean(c(asistencia, rezago, escolaridad)),
         salud = mean(c(malnutricion, sist_salud, atencion)),
         trabajo= mean(c(ocupacion, seg_social, jubilacion)),
         vivienda= mean(c(hacinamiento, estado_vivienda, serv_basicos))) %>% 
  ungroup()

## 4.2 Promedio de las cuatro dimensiones
indicadores2014 = indicadores2014 %>% 
  rowwise() %>%
  mutate(pobreza = mean(c(educ, salud, trabajo, vivienda))) %>% 
  ungroup()

indicadores2014 %>% select(pobreza) %>% head(10) # Primeros 10 casos

summary(indicadores2014$pobreza) # Resumen

## 4.3 Conocer el porcentaje total de pobreza multidimensional del país
indicadores2014 <- indicadores2014 %>% mutate(pobreza = case_when(pobreza>=0.25~"si",
                                                                  pobreza<0.25~"no")
)
prop.table(table(indicadores2014$pobreza))*100

#### 4. Crear indicadores de pobreza multidimensional con cinco indicadores ####
## 4.1 Crear una variable para cada dimensión 
indicadores2016 <- casen2022 %>% select(asistencia, 
                                        rezago, 
                                        escolaridad, 
                                        malnutricion, 
                                        sist_salud, 
                                        atencion, 
                                        ocupacion, 
                                        seg_social, 
                                        jubilacion, 
                                        habitabilidad, 
                                        serv_basicos,
                                        entorno,
                                        ap_part_social,
                                        trato,
                                        seguridad,
                                        area,
                                        region) %>% 
  na.omit() %>% # Eliminar Na's
  mutate_all(~(as.numeric(.))) # Convertimos todas las variables a numéricas

## 4.2 Promedio de las cinco dimensiones 
indicadores2016 = indicadores2016 %>% 
  rowwise() %>%
  mutate(educ = mean(c(asistencia, rezago, escolaridad)),
         salud = mean(c(malnutricion, sist_salud, atencion)),
         trabajo= mean(c(ocupacion, seg_social, jubilacion)),
         vivienda= mean(c(habitabilidad, serv_basicos, entorno)),
         redes_cohesion= mean(c(ap_part_social, trato, seguridad))) %>% 
  ungroup()

## 4.3 Calcular indice ponderado
indicadores2016 = indicadores2016 %>% 
  rowwise() %>%
  mutate(pobreza_pond = (educ*22.5) + (salud*22.5) + (trabajo*22.5) + (vivienda*22.5) + (redes_cohesion*10)) %>%  
  ungroup()






