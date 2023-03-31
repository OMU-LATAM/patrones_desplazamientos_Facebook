##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                      FACEBOOK MOVEMENT DATA PROCESSING                   ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##----------------- JUAN IGNACIO FULPONI & CRISTIAN MOLERES---------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                  Banco Interamericano de Desarrollo - 2022               ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#...............................................................................
#                                                                              .
#  Dependencias                                                                .
#                                                                              .
#...............................................................................

# Lista de paquetes requeridos

required_packages <- c("sf", "tidyverse", "furrr", "data.table")

# Instalar y cargar paquetes
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

# Seteo de paralelización con FURRR
plan(multiprocess)


#...............................................................................
#                                                                              .
#  Carga básica                                                                .
#                                                                              .
#...............................................................................


table <- read.csv("data/cities_table.csv") 
geo <- st_read("data/geo_shp.shp") 

distancias_medias <- list()
viajes_adm_final <- list()
viajes <- list()
distancias <- list()

#...............................................................................
#                                                                              .
#  Funciones específicas                                                       .
#                                                                              .
#...............................................................................

get_files <- function(folder, j) {
  grep("0800", list.files(paste0("data/", table[j,]$abrev, "/",
                                 folder), full.names = T), value = T)
}

calculate_expansion_2 <- function(mov_tiles, mov_adm) {
  mov_tiles %>%
    filter(start_polygon_name == end_polygon_name) %>%
    group_by(start_polygon_name) %>%
    summarise(tiles = sum(n_crisis, na.rm=T)) %>%
    left_join(mov_adm %>%
                filter(start_polygon_name == end_polygon_name) %>%
                group_by(start_polygon_name) %>%
                summarise(mbar = sum(n_crisis, na.rm=T))) %>%
    mutate(fact_expansion_2 = mbar/tiles,
           fact_expansion_2 = if_else(is.nan(fact_expansion_2), 1, 
                                      fact_expansion_2),
           fact_expansion_2 = if_else(is.infinite(fact_expansion_2), 1, 
                                      fact_expansion_2)) %>%
    select(start_polygon_name, fact_expansion_2)
}

calculate_viajes_cortos <- function(mov_tiles, expansion_2, expansion) {
  mov_tiles %>%
    filter(start_quadkey != end_quadkey,
           start_polygon_name == end_polygon_name) %>%
    mutate(n_crisis = if_else(is.na(n_crisis), 0, n_crisis)) %>%
    group_by(start_polygon_name, end_polygon_name, date_time) %>%
    summarise(viajes_cortos = sum(n_crisis, na.rm=T),
              dist_media_intra = weighted.mean(length_km, 
                                               n_crisis, na.rm=T)) %>%
    left_join(expansion_2) %>%
    mutate(viajes_cortos = viajes_cortos*fact_expansion_2) %>%
    rename(viajes = viajes_cortos, length_km=dist_media_intra) %>%
    left_join(expansion %>% dplyr::select(start_polygon_name,
                                          fact_expansion)) %>%
    mutate(viajes = viajes*fact_expansion)
}

calculate_viajes_adm_final <- function(mov_adm, viajes_cortos, expansion) {
  mov_adm %>%
    filter(start_polygon_name != end_polygon_name) %>%
    left_join(expansion %>% dplyr::select(start_polygon_name,
                                          fact_expansion)) %>%
    mutate(viajes = n_crisis*fact_expansion) %>%
    bind_rows(viajes_cortos) %>%
    mutate(date = as.Date(date_time))
}

calculate_distancias_medias <- function(viajes_adm_final) {
  viajes_adm_final %>%
    mutate(viajes = nafill(viajes, fill = 0)) %>%
    group_by(start_polygon_name, date) %>%
    summarise(distancia_media = weighted.mean(length_km, viajes)) %>%
    filter(!is.nan(distancia_media))
}

process_pop <- function(file_suffix) {
  pop_d <- paste0("data/pop/", table[j,]$country, file_suffix)
  colname <- paste0("population", gsub("_", "", file_suffix))
  
  read_csv(pop_d) %>%
    filter_bbox() %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(4326)) %>%
    {if (file_suffix != "_pop.csv") {colnames(.)[1] <- "population"}} %>%
    st_join(municipalities, .) %>% st_drop_geometry() %>%
    group_by(start_polygon_name) %>% 
    summarise(!!colname := sum(as.numeric(population)))
}

process_files <- function(i) {
  mov_adm <- read.csv(adm_files[i])
  expansion <- intersect_pop %>% 
    left_join(mov_adm %>% group_by(start_polygon_name) %>% 
                summarise(n_crisis = sum(n_crisis, na.rm = T))) %>%
    mutate(fact_expansion = pobl/n_crisis)
  
  mov_tiles <- read.csv(tiles_files[i]) 
  expansion_2 <- calculate_expansion_2(mov_tiles, mov_adm)
  
  viajes_cortos <- calculate_viajes_cortos(mov_tiles, expansion_2, 
                                           expansion)
  viajes_adm_final <- calculate_viajes_adm_final(mov_adm, viajes_cortos, 
                                                 expansion)
  distancias_medias <- calculate_distancias_medias(viajes_adm_final)
  
  list(viajes_adm_final = viajes_adm_final, 
       distancias_medias = distancias_medias)
  
}

#...............................................................................
#                                                                              .
#  Loop para cada una de las ciudades                                          .
#                                                                              .
#...............................................................................


for (j in 1:nrow(table)){    ## El loop recorrerá cada fila de la tabla
  
  adm_files <- get_files("adm", j)
  tiles_files <- get_files("tiles", j)
  
  municipalities <- geo %>% filter(city == table[j,]$city)
  bbox2 <- st_bbox(st_as_sfc(municipalities))
  
  pop_d <- paste0("data/pop/", table[j]$country, "_pop.csv")
  
  filter_bbox <- function(data) {
    data %>% filter(longitude > bbox2[1], latitude > bbox2[2],
                    longitude < bbox2[3], latitude < bbox2[4])
  }
  
  pop_hd <- read_csv(pop_d) %>%
    filter_bbox() %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(4326))
  
  if (ncol(pop_hd) == 3) {pop_hd <- pop_hd %>% select(2, 3)}
  
  colnames(pop_hd)[1] <- "population"

  suffixes <- c("_pop.csv", "_5.csv", "_60.csv")
  pop_results <- future_map_dfr(suffixes, process_pop)
  
  pop_final <- pop_results %>%
    pivot_wider(names_from = "suffix", 
                values_from = starts_with("population")) %>%
    mutate(pobl = population_pop - population_5 - population_60) %>%
    dplyr::select(start_polygon_name, pobl)
  
  results <- future_map(1:length(adm_files), process_files)
  
  viajes[[j]] <- bind_rows(lapply(results, "[[", "viajes_adm_final")) %>% 
    mutate(city = table[j,]$city)
  
  distancias[[j]] <- bind_rows(lapply(results, "[[", "distancias_medias")) %>% 
    mutate(city = table[j,]$city)
  
}

#...............................................................................
#                                                                              .
#  Bind de resultados                                                          .
#                                                                              .
#...............................................................................

viajes <- bind_rows(viajes) %>%
  rename(geometry = GEOMETRY) %>%
  dplyr::select(geometry, start_polygon_name, 
                end_polygon_name, fact_expansion, 
                viajes, length_km, date, city) %>%
  filter(viajes > 0)

distancias_medias <- bind_rows(distancias) %>%
  filter(distancia_media >= 0)

#..............................     .............................
#..............................     .............................

