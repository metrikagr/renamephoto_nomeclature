link_db_catalogo_variedades <- "https://docs.google.com/spreadsheets/u/1/d/1QW-w6ZMHnV59VbzLr6XN_RPvPcHTX-bMZoPWfFDAkGE/edit?usp=drive_web&ouid=105557885030805715232"

library(googledrive)
library(tidyverse)

#listado de carpetas
dt_lista_bolivia <- list.files(path = "D:/omar/METRIKA-GROUP/Github/renamephoto_nomenclature/imagenes/Fotos-Bolivia/Bolivia/") %>% 
                          as.data.frame() %>% 
                          dplyr::filter(!str_detect(., ".psd")) %>% 
                          arrange(.)
names(dt_lista_bolivia) <- "name"


#vector con los nombres unicos de los folderes y variedades
folder_bolivia <- unique(dt_lista_bolivia$name)

#listado incluye hojas
dt_lista_bolivia_subfolder <- vector(mode="list", length = length(folder_bolivia))
sub_folder <- vector(mode="character", length = length(folder_bolivia))

for(i in seq.int(folder_bolivia)){
  
  print(folder_bolivia[i])
  
  dt_lista_bolivia_subfolder[[i]] <- list.files(full.names = TRUE, path = file.path("D:/omar/METRIKA-GROUP/Github/renamephoto_nomenclature/imagenes/Fotos-Bolivia/Bolivia/", folder_bolivia[i]))
  
  dt_lista_bolivia_subfolder[[i]] <- dt_lista_bolivia_subfolder[[i]] %>% 
                                        as.data.frame() %>% 
                                        dplyr::filter(!str_detect(., ".psd")) %>% 
                                        dplyr::filter(tools::file_ext(.)!="") %>% 
                                        dplyr::filter(str_detect(., ".JPG|.jpg|.png|.jpg|.JPEG|.tif")) %>% 
                                        dplyr::filter(!str_detect(., "ICONOS time coccion")) %>% 
                                        arrange(.) 
  names(dt_lista_bolivia_subfolder[[i]]) <- "id" 
  
  
  dt_lista_bolivia_subfolder[[i]] <- dt_lista_bolivia_subfolder[[i]] %>% 
                                      mutate(sub_folder = folder_bolivia[i]) %>% 
                                      mutate(nombre_variedad = str_replace_all(dt_lista_bolivia$name[i], pattern = "P\\d+ ", replacement = "")
                                             %>% toupper() %>% 
                                               str_trim(side="both") 
                                      )
  
}
dt_total_listado_bolivia <- data.table::rbindlist(dt_lista_bolivia_subfolder) %>% 
                                        as.data.frame() %>% 
                                        arrange(nombre_variedad)

#Fuentes para lidiar con caracteres especiales
#https://stackoverflow.com/questions/9934856/removing-non-ascii-characters-from-data-files
#https://stackoverflow.com/questions/11972203/special-characters-in-r

dt_total_listado_bolivia <- dt_total_listado_bolivia %>% 
                                  mutate(
                                    crop = "potato", crop_abbrev= "pt",
                                    country = "Bolivia", country_abbrev = "bol",
                                    site = "Bolivia", site_abbrev = "bol",
                                    year = "20",group_abbrev=""
                                  ) %>% 
                                  mutate(
                                    raw_photoname = basename(id)
                                  ) %>% 
                                  mutate(
                                    plant_part = 
                                      case_when(
                                        str_detect(raw_photoname,"-T") ~ "tuberculo",
                                        str_detect(raw_photoname,"-B") ~ "brote",
                                        str_detect(raw_photoname,"-H") ~ "hoja",
                                        str_detect(raw_photoname,"-F") ~ "flor",
                                        str_detect(raw_photoname,"-P") ~ "planta",
                                        str_detect(raw_photoname,"PAPAS") ~ "tuberculo"
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
                                        nombre_utf8 = str_replace_all(nombre_variedad,
                                                "[[:punct:]]", "") %>% 
                                                stringi::stri_trans_general(., "latin-ascii") %>% 
                                                gsub("´", replacement = "",.) %>% toupper()
                                ) %>% 
                                arrange(nombre_variedad)



db_catalago_variedades <- googlesheets4::read_sheet(link_db_catalogo_variedades) %>% 
                                         filter(Pais == "Bolivia") %>% 
                                         mutate(
                                                nombre_utf8 =  str_replace_all(Nombre,"[[:punct:]]", "") %>% 
                                                               stringi::stri_trans_general(., "latin-ascii") %>% 
                                                               gsub("´", replacement = "",.) %>% toupper()
                                                  
                                               ) %>% 
                                          arrange(Nombre)
                     

join_bolivia <- dplyr::left_join(dt_total_listado_bolivia, db_catalago_variedades) %>% 
                        mutate(
                          nomenclature = paste0(
                            crop_abbrev, group_abbrev,
                            country_abbrev, "_", year,
                            site_abbrev,"",
                            nombre_utf8, "_",plant_part
                          )
                        )  %>% 
                        mutate(
                          nomenclature =
                            str_replace_all(nomenclature, pattern = "[[:space:]]",
                                            replacement = "-")
                        ) %>% 
                        mutate(
                          nomenclature = paste0(nomenclature, "_", Orden)
                        ) %>% 
                        filter(!str_detect(id,pattern = "PAG Izquierda PAR")) %>% 
                        filter(!str_detect(raw_photoname,pattern ="LAPAC016-F3 8cm.jpg")) 



variedad <- unique(join_bolivia$nombre_utf8)
variety_photos <- vector(mode = "list", length = length(variedad))



for(i in 2:length(variedad)){
  
  dir.create(path =
               file.path("D:/omar/METRIKA-GROUP/Github/renamephoto_nomenclature/imagenes/Fotos-Bolivia/Rename_bolivia_2021/", variedad[i])
  )
  
  variety_photos[[i]] <- join_bolivia %>% filter(nombre_utf8 == variedad[i])
  
  old_name <- variety_photos[[i]]$id
  new_name <- file.path("imagenes/Fotos-Bolivia/Rename_bolivia_2021/", variedad[i], variety_photos[[i]]$nomenclature )
  new_name <- paste0(new_name, ".jpg")
  #from <- paste0(file.path("D:/omar/METRIKA-GROUP/Github/renamephoto_nomenclature/imagenes/Fotos_Chugay-Pataz-20220226T185737Z-001/Rename_ChugayPataz_2016/", subfolder[i]), ), 
  
  file.copy(
    from =old_name,
    to =  new_name,
    overwrite = TRUE
  )
  
  
}

join_bolivia <- join_bolivia %>% 
                    select(-id, -nombre_utf8)    

writexl::write_xlsx(join_bolivia, path = "inventarios/inv_ren_bolivia_2016.xlsx")


