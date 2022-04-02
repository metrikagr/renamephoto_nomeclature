link_db_catalogo_variedades <- "https://docs.google.com/spreadsheets/u/1/d/1QW-w6ZMHnV59VbzLr6XN_RPvPcHTX-bMZoPWfFDAkGE/edit?usp=drive_web&ouid=105557885030805715232"

library(googledrive)
library(tidyverse)

#listado de carpetas
dt_lista_chupataz <- list.files(path = "D:/omar/METRIKA-GROUP/Github/renamephoto_nomenclature/imagenes/Fotos-Pataz/") %>% 
                      as.data.frame() %>% 
                      dplyr::filter(!str_detect(., ".psd")) %>% 
                      arrange(.)
names(dt_lista_chupataz) <- "name"

#vector con los nombres unicos de los folderes y variedades
folder_chupataz <- unique(dt_lista_chupataz$name)

#listado incluye hojas
dt_lista_chupataz_subfolder <- vector(mode="list", length = length(folder_chupataz))
sub_folder <- vector(mode="character", length = length(folder_chupataz))

for(i in seq.int(folder_chupataz)){
  
  print(folder_chupataz[i])
  
  dt_lista_chupataz_subfolder[[i]] <- list.files(full.names = TRUE, path = file.path("D:/omar/METRIKA-GROUP/Github/renamephoto_nomenclature/imagenes/Fotos-Pataz/", folder_chupataz[i]))
  
  dt_lista_chupataz_subfolder[[i]] <- dt_lista_chupataz_subfolder[[i]] %>% 
                                      as.data.frame() %>% 
                                      dplyr::filter(!str_detect(., ".psd")) %>% 
                                      dplyr::filter(tools::file_ext(.)!="") %>% 
                                      dplyr::filter(str_detect(., ".JPG|.jpg|.png|.jpg|.JPEG|.tif")) %>% 
                                      dplyr::filter(!str_detect(., "ICONOS time coccion")) %>% 
                                      arrange(.) 
  names(dt_lista_chupataz_subfolder[[i]]) <- "id" 
  
  
  dt_lista_chupataz_subfolder[[i]] <- dt_lista_chupataz_subfolder[[i]] %>% 
                                          mutate(sub_folder = folder_chupataz[i]) %>% 
                                          mutate(nombre_variedad = str_replace_all(dt_lista_chupataz$name[i], pattern = "P\\d+ ", replacement = "")
                                                 %>% toupper() %>% 
                                                   str_trim(side="both") 
                                                 )
                                          
}

## Juntar todas las tablas de fotos
dt_total_listado_chupataz <- data.table::rbindlist(dt_lista_chupataz_subfolder) %>% as.data.frame()
dt_total_listado_chupataz <- dt_total_listado_chupataz %>% 
                                      mutate(raw_photoname = basename(id)) %>%
                                      mutate(nombre_variedad =
                                               gsub(sub_folder, pattern = "P\\d+ ", replacement = "") %>% toupper()    
                                      ) %>% 
                                      mutate(
                                        crop = "potato", crop_abbrev= "pt",
                                        country = "Peru", country_abbrev = "pe",
                                        site = "La Libertad", site_abbrev = "libchupataz",
                                        year = "2016", group_abbrev="", plant_part = ""
                                      ) %>% 
                                      mutate(
                                        plant_part = case_when(
                                          str_detect(raw_photoname, "QUIPU") ~ "molecular",
                                          str_detect(raw_photoname, "PAPAS") ~ "tuberculo",
                                          str_detect(raw_photoname, "PAPA")  ~ "tuberculo",
                                          str_detect(raw_photoname, "BROTE") ~ "brote",
                                          str_detect(raw_photoname, "FLOR")   ~ "flor",
                                          str_detect(raw_photoname, "PLANTA") ~ "planta",
                                                                        TRUE ~ plant_part
                                                              )
                                          )  %>% 
                                      mutate(
                                              nomenclature = paste0(
                                                crop_abbrev, group_abbrev,
                                                country_abbrev, "_", year,
                                                site_abbrev,"",
                                                nombre_variedad, "_",plant_part
                                              )
                                            ) %>% 
                                      mutate(
                                              nomenclature =
                                                str_replace_all(nomenclature, pattern = "[[:space:]]",
                                                                replacement = "-")
                                            )
                                       
  



# ##Inventario previo - no incluye imagenes de hojas
# inventario_photos_catalogo <- readxl::read_xlsx("invetario_photos_catalogo_papa.xlsx") %>% 
#                                       filter(folder == "Fotos_Chugay-Pataz")  %>% 
#                                       mutate(
#                                         crop = "potato", crop_abbrev= "pt",
#                                         country = "Peru", country_abbrev = "pe",
#                                         site = "La Libertad", site_abbrev = "libchupataz",
#                                         year = "2016",group_abbrev=""
#                                       ) %>% 
#                                       mutate(nombre_variedad =
#                                                gsub(sub_folder, pattern = "P\\d+ ", replacement = "") %>% toupper()    
#                                       )%>% 
#                                       mutate(
#                                         plant_part = case_when(
#                                           plant_part == "quipu" ~ "molecular",
#                                           plant_part == "papas" ~ "tuberculo",
#                                           plant_part == "papa" ~ "tuberculo",
#                                           TRUE ~ plant_part
#                                         )
#                                       )  %>% 
#                                       mutate(
#                                         nomenclature = paste0(
#                                           crop_abbrev, group_abbrev,
#                                           country_abbrev, "_", year,
#                                           site_abbrev,"",
#                                           nombre_variedad, "_",plant_part
#                                         )
#                                       ) %>% 
#                                       mutate(
#                                         nomenclature =
#                                           str_replace_all(nomenclature, pattern = "[[:space:]]",
#                                                           replacement = "-")
#                                       )


#JOIN YA NO ES NECESARIO.
join_chupataz16 <- dt_total_listado_chupataz #left_join(dt_total_listado_chupataz,inventario_photos_catalogo)
#names(join_chupataz16)[3] <- "Nombre"#cambiar nombre_variedad --> nombre 
join_chupataz16 <- join_chupataz16 %>% 
                                   mutate(nombre_variedad = 
                                          case_when(
                                            nombre_variedad == "NUEVA VARIEDAD BRETANA" ~ "BRETAÑA",
                                            nombre_variedad == "CARGANAKA  BLANCA" ~ "CARGANAKA BLANCA",
                                            nombre_variedad == "MURU AMAPOLA" ~ "AMAPOLA",
                                            nombre_variedad == "MURU CARGANAKA" ~ "CARGANAKA",
                                            nombre_variedad == "PANZA DE OBEJA" ~ "PANZA DE OVEJA",
                                            nombre_variedad == "PURPURA NEGRA" ~ "PURPURA",
                                            TRUE ~ nombre_variedad)
                                          ) %>%
                                          mutate(
                                            nombre_utf8 =  str_replace_all(nombre_variedad,"[[:punct:]]", "") %>% 
                                              stringi::stri_trans_general(., "latin-ascii") %>% 
                                              gsub("´", replacement = "",.) %>% toupper()
                                            
                                          ) %>% 
                                          arrange(nombre_variedad)


## Juntar con datos de la base de datos de variedades catalogo de variedades
link_db_catalogo_variedades <- "https://docs.google.com/spreadsheets/u/1/d/1QW-w6ZMHnV59VbzLr6XN_RPvPcHTX-bMZoPWfFDAkGE/edit?usp=drive_web&ouid=105557885030805715232"
db_catalogo_variedades <- googlesheets4::read_sheet(link_db_catalogo_variedades)
db_catalogo_variedades <- db_catalogo_variedades %>% filter(Departamento == "La Libertad", Distrito == "Chugay")
db_catalogo_variedades <- db_catalogo_variedades %>% 
                            mutate(
                              nombre_utf8 =  str_replace_all(Nombre,"[[:punct:]]", "") %>% 
                                stringi::stri_trans_general(., "latin-ascii") %>% 
                                gsub("´", replacement = "",.) %>% toupper()
                              
                            ) %>% 
                            arrange(Nombre)


# %>% 
#                             mutate(Nombre=chartr(old = "áéíóúÁÉÍÓÚ", new = "aeiouAEIOU", Nombre))       

## 
join_dbvar_listado <- left_join(join_chupataz16, db_catalogo_variedades, by= "nombre_utf8") %>% 
                      dplyr::distinct(.) %>% 
                      mutate( nomenclature = paste0(nomenclature,"_",Orden)) %>% 
                      arrange(nombre_utf8) %>% 
                      filter(!is.na(plant_part))  

# Leer tabla --------------------------------------------------------------

#join_dbvar_listado <- readxl::read_excel("inventarios/inv_ren_chupataz_2016.xlsx") %>% 
                              


variedad <- unique(join_dbvar_listado$nombre_utf8)
variety_photos <- vector(mode = "list", length = length(variedad))



for(i in 1:length(variedad)){

  dir.create(path =
               file.path("D:/omar/METRIKA-GROUP/Github/renamephoto_nomenclature/imagenes/Fotos-Pataz/Rename_Fotos_ChugayPataz2016/", variedad[i])
             )
  
  variety_photos[[i]] <- join_dbvar_listado %>% filter(nombre_utf8 == variedad[i])
  
  old_name <- variety_photos[[i]]$id
  new_name <- file.path("imagenes/Fotos-Pataz/Rename_Fotos_ChugayPataz2016/", variedad[i], variety_photos[[i]]$nomenclature )
  new_name <- paste0(new_name, ".jpg")
  #from <- paste0(file.path("D:/omar/METRIKA-GROUP/Github/renamephoto_nomenclature/imagenes/Fotos_Chugay-Pataz-20220226T185737Z-001/Rename_ChugayPataz_2016/", subfolder[i]), ), 
  
  file.copy(
            from =old_name,
            to =  new_name,
            overwrite = TRUE
            )
}


join_dbvar_listado <- join_dbvar_listado %>% 
                          select(-id, -nombre_utf8)  

#Guardar 
writexl::write_xlsx(join_dbvar_listado, path = "inventarios/inv_ren_chupataz_2016.xlsx")

