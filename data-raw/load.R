library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

# join comarcas e fortified map -----------------------------------------------
data(d_map_sp, package = 'shapefiles')

muda_nomes <- function(d, v, valor1, valor2) {
  v <- deparse(substitute(v))
  for(i in seq_along(valor1)) d[[v]][d[[v]] == valor1[i]] <- valor2[i]
  d
}

d_comarcas <- 'data-raw/COMARCAS_MUNICÍPIOS_RAJ_CJ.xlsx' %>%
  readxl::read_excel(skip = 2) %>%
  stj::arrumar_nomes() %>%
  setNames(gsub('/', '_', names(.))) %>%
  filter(!is.na(municipio)) %>%
  distinct(municipio) %>%
  mutate(municipio = gsub("'", '', municipio)) %>%
  muda_nomes(municipio,
             c('SEVERINEA', "SANTO ANTONIO DA POSSE", "SANTA BARBARA D OESTE",
               "PARIQUERA ACU","PALMEIRA D OESTE", "IPAUCU", "FLORINEA",
               "ESTRELA D OESTE", "BRODOSQUI", "BIRITIBA MIRIM"),
             c('SEVERINIA', "SANTO ANTONIO DE POSSE", "SANTA BARBARA DOESTE",
               "PARIQUERA-ACU", "PALMEIRA DOESTE", "IPAUSSU", "FLORINIA",
               "ESTRELA DOESTE", "BRODOWSKI","BIRITIBA-MIRIM"))

## Bras Cubas é um DISTRITO de Mogi das Cruzes.
d_map_sp %>%
  distinct(id) %>%
  mutate(municipio = tjsp::desacentuar(NM_MUNICIP)) %>%
  {anti_join(d_comarcas, ., 'municipio')}

d_map_comarcas_sp <- d_map_sp %>%
  mutate(municipio = tjsp::desacentuar(NM_MUNICIP)) %>%
  inner_join(d_comarcas, 'municipio')

# devtools::use_data(d_map_comarcas_sp, overwrite = TRUE)

# merge do shapefile para comarcas --------------------------------------------
library(rgdal)
library(rgeos)
library(maptools)
data(mapa_sp, package = 'shapefiles')

tab <- mapa_sp@data %>%
  mutate(municipio = tjsp::desacentuar(NM_MUNICIP)) %>%
  inner_join(d_comarcas, 'municipio') %>%
  mutate_each(funs(as.character))

tab_coma <- tab %>%
  distinct(comarca_foro_distrital_sede) %>% {
    row.names(.) <- .$comarca_foro_distrital_sede
    .
  }

mapa_sp <- mapa_sp %>%
  unionSpatialPolygons(tab$comarca_foro_distrital_sede) %>%
  SpatialPolygonsDataFrame(tab_coma)

d_map_comarcas_sp <- mapa_sp %>%
  fortify(region = 'CD_GEOCMU') %>%
  tbl_df() %>%
  left_join(tab, c('id' = 'CD_GEOCMU'))

d_map_comarcas_sp %>% {
  ggplot() +
    geom_map(map = d_map_sp, aes(x = long, y = lat, map_id = id),
             fill = 'transparent', colour = 'gray90',
             data = d_map_sp) +
    geom_map(map = ., aes(x = long, y = lat, map_id = id),
             fill = 'transparent', colour = 'black',
             data = select(., -municipio)) +
    coord_equal() +
    theme_minimal() +
    xlab('') +
    ylab('') +
    theme(axis.text = element_blank())
}

# comarcas conectadas ---------------------------------------------------------

connected_comarcas <- d_map_comarcas_sp %>%
  distinct(comarca_foro_distrital_sede) %>%
  arrange(comarca_foro_distrital_sede) %>%
  add_rownames() %>% {
    l_latlon <- d_map_comarcas_sp %>%
      filter(!hole) %>%
      group_by(id) %>%
      mutate(n_group = n_distinct(group)) %>%
      arrange(desc(n_group)) %>%
      filter(group == first(group)) %>%
      ungroup() %>%
      mutate(latlon = paste(lat, long)) %>%
      distinct(latlon, comarca_foro_distrital_sede) %>%
      select(comarca_foro_distrital_sede, latlon) %>%
      plyr::dlply('comarca_foro_distrital_sede', function(x) x$latlon)
    test_connected <- function(x) {
      anyDuplicated(unlist(.subset(l_latlon, x))) > 0
    }
    ind <- mutate(., comarca_foro_distrital_sede = as.integer(rowname)) %>%
      transmute(coma1 = comarca_foro_distrital_sede,
                coma2 = comarca_foro_distrital_sede) %>%
      expand.grid() %>%
      data.frame() %>%
      tbl_df() %>%
      filter(coma1 != coma2) %>%
      arrange(coma1, coma2) %>%
      as.matrix() %>%
      plyr::aaply(1, test_connected, .progress = 'text')
    d <- transmute(., coma1 = comarca_foro_distrital_sede,
                   coma2 = comarca_foro_distrital_sede) %>%
      expand.grid() %>%
      data.frame() %>%
      tbl_df() %>%
      filter(coma1 != coma2) %>%
      mutate(connected = ind)
    d
  } %>%
  mutate_each(funs(as.character), coma1, coma2) %>%
  filter(connected) %>%
  arrange(coma1, coma2)

devtools::use_data(connected_comarcas, overwrite = TRUE)

# municipios conectados -------------------------------------------------------

d_map_comarcas_sp_limpo <- d_map_comarcas_sp %>%
  filter(!hole) %>%
  group_by(id) %>%
  mutate(n_group = n_distinct(group)) %>%
  arrange(desc(n_group)) %>%
  filter(group == first(group)) %>%
  ungroup()
d_connected <- d_map_comarcas_sp %>%
  distinct(municipio) %>%
  arrange(municipio) %>%
  add_rownames() %>% {
    ind <- mutate(., municipio = as.integer(rowname)) %>%
      transmute(municipio1 = municipio, municipio2 = municipio) %>%
      expand.grid() %>%
      data.frame() %>%
      tbl_df() %>%
      filter(municipio1 != municipio2) %>%
      arrange(municipio1, municipio2) %>%
      as.matrix() %>%
      plyr::aaply(1, test_connected, .progress = 'text')
    l_latlon <- d_map_comarcas_sp_limpo %>%
      mutate(latlon = paste(lat, long)) %>%
      distinct(latlon, municipio) %>%
      select(municipio, latlon) %>%
      plyr::dlply('municipio', function(x) x$latlon)
    test_connected <- function(x) {
      anyDuplicated(unlist(.subset(l_latlon, x))) > 0
    }
    d <- transmute(., municipio1 = municipio, municipio2 = municipio) %>%
      expand.grid() %>%
      data.frame() %>%
      tbl_df() %>%
      filter(municipio1 != municipio2) %>%
      mutate(connected = ind)
    d
  } %>%
  mutate_each(funs(as.character), municipio1, municipio2) %>%
  filter(connected) %>%
  arrange(municipio1, municipio2)

# devtools::use_data(d_connected, overwrite = TRUE)

# desenhando vizinhanca -----------------------------------------------------

desenhar <- function() {
  d_connected %>%
    filter(municipio1 == 'PIRACICABA') %>% {
      cat(first(.$municipio1), 'n =', nrow(.), '\n')
      filter(d_map_comarcas_sp,
             municipio %in% c(.$municipio2, first(.$municipio1)))
    } %>% {
      ggplot() +
        geom_map(map = ., aes(x = long, y = lat, map_id = id),
                 fill = 'transparent', colour = 'black',
                 data = select(., -municipio)) +
        geom_map(map = ., aes(x = long, y = lat, map_id = id,
                              fill = municipio),
                 data = ., alpha = .5) +
        facet_wrap(~municipio) +
        coord_equal() +
        theme_minimal() +
        xlab('') +
        ylab('') +
        theme(axis.text = element_blank())
    }
}
desenhar()

# vizinhanca das comarcas -----------------------------------------------------

desenhar <- function(coma = '') {
  if(coma == '') {
    coma <- sample(unique(connected_comarcas$coma1), 1)
  }
  connected_comarcas %>%
    filter(coma1 == coma) %>% {
      x <- filter(d_map_comarcas_sp,
                  comarca_foro_distrital_sede %in% c(.$coma2, first(.$coma1)))
      x <- mutate(x, coma1 = first(.$coma1))
      x
    } %>% {
      ggplot() +
        geom_map(map = ., aes(x = long, y = lat, map_id = id),
                 fill = 'transparent', colour = 'black',
                 data = select(., -comarca_foro_distrital_sede)) +
        geom_map(map = ., aes(x = long, y = lat, map_id = id,
                              fill = comarca_foro_distrital_sede),
                 data = ., alpha = .5) +
        facet_wrap(~comarca_foro_distrital_sede) +
        coord_equal() +
        theme_minimal() +
        xlab('') +
        ylab('') +
        ggtitle(first(.$coma1)) +
        theme(axis.text = element_blank())
    }
}
desenhar()

# base final ------------------------------------------------------------------

data(cempre, package = 'rais')
cempre_comarcas <- cempre %>%
  separate(municipio, c('municipio', 'uf'), sep = ' - ') %>%
  filter(uf == 'SP') %>%
  muda_nomes(municipio, c('MOJI MIRIM'), c('MOGI MIRIM')) %>%
  group_by(municipio) %>%
  summarise(n = sum(n)) %>%
  inner_join(d_comarcas, 'municipio') %>%
  group_by(comarca_foro_distrital_sede) %>%
  summarise(n_empresas = sum(n)) %>%
  rename(comarca = comarca_foro_distrital_sede) %>%
  mutate(comarca = paste0('coma_', comarca))

dados <- connected_comarcas %>%
  mutate(connected = as.integer(connected)) %>%
  mutate_each(funs(paste('coma', ., sep = '_')), coma1, coma2) %>%
  spread(coma2, connected, fill = 0) %>%
  rename(comarca = coma1) %>%
  inner_join(cempre_comarcas, 'comarca') %>%
  select(comarca, n_empresas, starts_with('coma_'))









