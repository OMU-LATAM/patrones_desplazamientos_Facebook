library(sf)
library(tidyverse)
library(readr)

table <- read.csv("data/cities_table.csv") ## Se carga la tabla básica de ciudades
geo <- st_read("data/geo_shp.shp") ## Se cargan las geometrías de las ciudades, previamente procesadas

## Se crean listas vacías que contentrán cada uno de los dataframes como elementos.

distancias_medias <- list()
viajes_adm_final <- list()
viajes <- list()
distancias <- list()

for (j in 1:nrow(table)){    ## El loop recorrerá cada fila de la tabla
  
  ## Se buscan los nombres de los archivos de los dos tipos de datasets utilizados.
  adm_files <- grep("0800", list.files(paste0("data/", table[j,]$abrev, "/adm"), full.names = T), value = T)
  tiles_files <- grep("0800", list.files(paste0("data/", table[j,]$abrev, "/tiles"), full.names = T), value = T)
  
  ## Se arma un boundary box (BBOX) con los límites geográficos del dataset de Tiles.
  
  municipalities <- geo %>% filter(city == table[j, ]$city) ## Se filtra la ciudad j de la tabla
  
  bbox2 <- municipalities %>% 
    st_as_sfc() %>% st_bbox()
  
  pop_d <- paste0("data/pop/", table[j, ]$country, "_pop.csv") ## Se busca el archivo de High Resolution Density Maps
  
  ## Se lee el archivo de High Resolution Density Maps y se transforma a SF
  pop_hd <- read_csv(pop_d) %>%                                        # Lee el archivo de población
    filter(longitude > bbox2[1], latitude > bbox2[2],                  # Filtra según el bbox de lat y lon para que no ocupe mucho procesamiento (son archivos enormes)
           longitude < bbox2[3], latitude < bbox2[4]) %>% 
    st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(4326))  #se convierte a SF con CRS 4326
  
  if (ncol(pop_hd)==3){pop_hd <- pop_hd %>% select(c(2,3))} ## Algunos datasets están estructurados con dos proyecciones (2015 y 2020), para el estudio nos quedamos siempre con las de 2020.
  
  colnames(pop_hd)[1] <-  "population"
  ## Se intersectan cada uno de los puntos de population con las geometrías administrativas.
  ## Luego se quitan las geometrías para que el dataset sea más liviano, ya que no sirven más.
  ## Se realiza el mismo procedimiento para los otros dos datasets de population (menores de 5 y mayores de 60)
  intersect_pop <- st_join(municipalities, pop_hd) %>% st_drop_geometry()
  
  rm(list=c("pop_hd")) ## Borrado de dataframes innecesarios
  
  pop_5_d <- paste0("data/pop/", table[j, ]$country, "_5.csv")
  
  pop_5 <- read_csv(pop_5_d) %>% 
    filter(longitude > bbox2[1], latitude > bbox2[2],
           longitude < bbox2[3], latitude < bbox2[4]) %>% 
    st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(4326))
  
  colnames(pop_5)[1] <-  "population"
  
  intersect_pop_5 <- st_join(municipalities, pop_5) %>% st_drop_geometry()
  
  rm(list=c("pop_5")) ## Borrado de dataframes innecesarios
  
  pop_60_d <- paste0("data/pop/", table[j, ]$country, "_60.csv")
  
  pop_60 <- read_csv(pop_60_d) %>% 
    filter(longitude > bbox2[1], latitude > bbox2[2],
           longitude < bbox2[3], latitude < bbox2[4]) %>% 
    st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(4326)) 
  
  colnames(pop_60)[1] <-  "population"
  
  intersect_pop_60 <- st_join(municipalities, pop_60) %>% st_drop_geometry()
  
  rm(list=c("pop_60")) ## Borrado de dataframes innecesarios
  
  ## Para saber efectivamente cuánta population hay en cada región administrativa,
  ## se efectúa un group_by y un summarize de la población
  
  intersect_pop <- intersect_pop %>%
    group_by(start_polygon_name) %>% 
    summarise(pobl = sum(as.numeric(population))) 
  
  intersect_pop_60 <- intersect_pop_60 %>%
    group_by(start_polygon_name) %>% 
    summarise(pobl_60 = sum(population))
  
  intersect_pop_5 <- intersect_pop_5 %>%
    group_by(start_polygon_name) %>% 
    summarise(pobl_5 = sum(population)) 
  
  ## Para cumplimentar requistos metodológicos, se procede a quitar de la population
  ## a los niños menores a 5 años y a los mayores de 60 años. 
  
  intersect_pop <- intersect_pop %>%
    left_join(intersect_pop_5) %>% 
    left_join(intersect_pop_60) %>% 
    mutate(pobl = pobl - pobl_5 - pobl_60) %>% 
    dplyr::select(start_polygon_name, pobl)
  
  rm(list=c("intersect_pop_5", "intersect_pop_60")) ## Borrado de dataframes innecesarios
  
  
  ## Aquí comienza el loop que generará el procesamiento de cada dataset para cada fecha.
  
  for (i in 1:length(adm_files)){
    
    ## Se lee el archivo i de Movement Between Administrative Regions
    ## Es fundamental que a la fecha i de un dataset le corresponda la fecha i del otro.
    
    mov_adm <- read.csv(adm_files[i])
    
    ## Se genera el primer factor de expansión, el cual compara la cantidad de personas
    ## reales con la cantidad de viajes que surgen de cada partido.
    
    expansion <- intersect_pop %>% 
      left_join(mov_adm %>% group_by(start_polygon_name) %>% 
                  summarise(n_crisis = sum(n_crisis, na.rm = T))) %>% ## la columna 'n_crisis' es la que representa los viajes
      mutate(fact_expansion = pobl/n_crisis)
    
    ## Se lee el archivo i en formato Tiles. 
    mov_tiles <- read.csv(tiles_files[i]) 
    
    ## Se obtiene el segundo factor de expansión, que es el que representa la cantidad
    ## de viajes "cortos" dentro del mismo partido o municipio
    expansion_2 <- mov_tiles %>%                                ## Se parte del dataset de tiles
      filter(start_polygon_name == end_polygon_name) %>%        ## Se filtra a los viajes que comienzan y terminan en el mismo partido (pero no en el mismo tile)
      group_by(start_polygon_name) %>%                          ## Se agrupa según el polígono de origen
      summarise(tiles = sum(n_crisis, na.rm=T)) %>%             ## Se suma la cantidad de viajes. Esto da la cantidad de viajes intrapartido según el dataset de Tiles.
      left_join(mov_adm %>%                                            ## Se joinea con el dataset de MBAR
                  filter(start_polygon_name == end_polygon_name) %>%   ## Se filtra, de la misma manera, a los viajes que comienzan y terminan en el mismo partido.
                  group_by(start_polygon_name) %>%                     ## Se agrupa según el polígono de origen
                  summarise(mbar = sum(n_crisis, na.rm=T))) %>%        ## Se suma la cantidad de viajes. Esto da la cantidad de viajes intrapartido según el dataset de MBAR.
      mutate(fact_expansion_2 = mbar/tiles,                                                        ## Se calcula el factor de expansión 2 propiamente dicho
             fact_expansion_2 = if_else(is.nan(fact_expansion_2), 1, fact_expansion_2),            ## Si da NaN, le otorgo factor de expansión 1 (no cambia el resultado)
             fact_expansion_2 = if_else(is.infinite(fact_expansion_2), 1, fact_expansion_2)) %>%   ## Si da infinito, le otorgo 1 (no cambia el resultado)
      select(start_polygon_name, fact_expansion_2)                                                 ## Se seleccionan las columnas relevantes.
    
    viajes_cortos <- mov_tiles %>%                                     ## Se calculan los viajes cortos dentro del mismo partido expandidos, para esto:
      filter(start_quadkey != end_quadkey,                             ## Se quitan los viajes que tengan distancia 0, es decir, que comienzan y terminan en el mismo tile
             start_polygon_name == end_polygon_name) %>%               ## Se mantienen los viajes en el mismo partido
      mutate(n_crisis = if_else(is.na(n_crisis), 0, n_crisis)) %>%     ## Se rellenan con 0 los n_crisis que son NA.
      group_by(start_polygon_name, end_polygon_name, date_time) %>%    ## Se agrupa según origen, destino y fecha
      summarise(viajes_cortos = sum(n_crisis, na.rm=T),                ## Los viajes cortos van a ser la suma de los n_crisis
                dist_media_intra = weighted.mean(length_km, n_crisis, na.rm=T)) %>%  ## La distancia media intrapartido va a ser la media ponderada de la distancia de cada una de las filas por su n_crisis.
      left_join(expansion_2) %>%                                             ## Se joinea con el segundo factor de expansión
      mutate(viajes_cortos = viajes_cortos*fact_expansion_2)  %>%            ## Se multiplica a los viajes cortos según este factor
      rename(viajes = viajes_cortos, length_km=dist_media_intra) %>%         ## Se cambian los nombres de las columnas por conveniencia
      left_join(expansion %>% dplyr::select(start_polygon_name,              ## Se joinea con el primer factor de expansión poblacional
                                            fact_expansion)) %>%               
      mutate(viajes = viajes*fact_expansion)                                 ## Se multiplica por el primer factor de expansión, lo que ya estaba expandido anteriormente.
    
    viajes_adm_final[[i]] <- mov_adm %>%                            ## El elemento de la lista de viajes va a ser, el dataset de movimientos entre regiones administrativas
      filter(start_polygon_name!=end_polygon_name) %>%              ## Sólo con los viajes que comienzan y terminan en distintos polígonos
      left_join(expansion %>% dplyr::select(start_polygon_name, 
                                            fact_expansion)) %>%  
      mutate(viajes = n_crisis*fact_expansion) %>%                  ## Se expande el dato, como anteriormente
      bind_rows(viajes_cortos) %>%                                  ## Y se suman las filas de los viajes cortos, que sí o sí van a ser con el mismo polígono de origen y de destino
      mutate(date = as.Date(date_time))                             ## Se formaliza la fecha
    
    distancias_medias[[i]]  <- viajes_adm_final[[i]] %>%             ## Se crea el dataset de distancias medias
      mutate(viajes = data.table::nafill(viajes, fill = 0)) %>%     ## Se llenan los viajes NA con 0
      group_by(start_polygon_name, date) %>%                        ## Se agrupa según el polígono de origen
      summarise(distancia_media = stats::weighted.mean(length_km, viajes)) %>%  ## Se calculan las distancias medias ponderadas
      filter(!is.nan(distancia_media))                              ## Se quitan las que hayan resultado NA
    print(i)
  }
  
  viajes[[j]] <- bind_rows(viajes_adm_final) %>% ## Se bindean todos los elementos para dar lugar a un sólo dataset
    mutate(city = table[j, ]$city) ## Se nombra a la ciudad en cuestión 
  
  distancias[[j]] <- bind_rows(distancias_medias) %>% ## Se bindean todos los elementos para dar lugar a un sólo dataset
    mutate(city = table[j, ]$city) ## Se nombra a la ciudad en cuestión 
  
  print(paste0("DONE! ", table[j, ]$city)) ## Pequeño mensaje para mostrar el progreso del loop
}

viajes <- bind_rows(viajes) %>% rename(geometry=GEOMETRY) %>%         ## se convierten a data.frames las listas con j elementos
  dplyr::select(geometry, start_polygon_name, end_polygon_name,       ## se seleccionan las variables de interés
                fact_expansion, viajes, length_km, date, city) %>% 
  filter(viajes>0)                                                    ## se filtran regiones donde existan viajes 0 (esta acción tambien filtra los NA)

distancias_medias <- bind_rows(distancias) %>% 
  filter(distancia_media>=0)

## Se borran data.frames innecesarios

rm(list=c("distancias", "expansion", "expansion_2", 
          "intersect_pop", "mov_adm", "mov_tiles", "municipalities", "viajes_adm_final",
          "viajes_cortos", "adm_files", "bbox2", "i", "j", 
          "pop_5_d", "pop_60_d", "pop_d", "tiles_files"))

