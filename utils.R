library(tidyverse)
library(data.table)

#' @param x character vector 
#' @param sep character separador para realizar la division (split) de caracteres

get_photo_parts <- function(x, sep){
  
  dt_split_names <- str_split(x, sep)
  dt_split_names <- as.data.frame(do.call(rbind, dt_split_names))
  names(dt_split_names) <- c("Nombre", "plant_part")
  dt_split_names[,1] <- str_trim(dt_split_names[,1],side = "both")
  dt_split_names[,2] <- str_trim(dt_split_names[,2],side = "both")
  
  return(dt_split_names)
}

#'@param from character ruta de la carpeta inicial
#'@param to character ruta de la carpeta final
#'@param pho_new character formato nuevo de la foto
#'
copy_photos_drive <- function(from,  to, pho_new){

  ### Listar archivo de una carpeta o folder
  dt_folder_fotos_lista <- drive_ls(path = from)
  
  ### quitar de la lista elementos que no son fotos
  dt_folder_fotos_lista <- dt_folder_fotos_lista %>% 
                            dplyr::filter(tools::file_ext(name)!="") 
  
  dt_folder_fotos_lista <- dt_folder_fotos_lista %>% 
                            dplyr::filter(stringr::str_detect(name, ".JPG|.jpg|.png|.jpg|.JPEG|.tif"))
  
  ### numero de elementos                                          
  nfilas <- nrow(dt_folder_fotos_lista)
  
  ### Renombrar imagenes con extension .JPG con .jpg
  renombre_fotos_lista <- stringr::str_replace_all(dt_folder_fotos_lista$name, 
                                                   ".JPG|.jpg|.png|.jpg|.JPEG|.tif", pho_new)
  
  #copiar fotos en las carpetas
  for(i in 1:nfilas){
    drive_cp(
      file = dt_folder_fotos_lista$id[i],
      path = to,
      name = renombre_fotos_lista[i],
      overwrite = TRUE
    )
  }
  
}
#ejemplo
# copy_photos_drive(from="papas AZULADO 88-123", 
#to="papas AZULADO 88-123/papas AZULADO 88-123_copia",
#                   ".jpg")


#'@param path ruta del archivo
#'@param dirname nombre del directorio

create_dir <- function(path, dirname){
  
  #folder <- drive_ls(type = "folder", pattern = dirname)
  #if(nrow(folder)==0){
    path_dir <- file.path(path, dirname)
    drive_mkdir(path_dir)
    print("se creo el folder con exito")
  #}else{
  # print("no se creo el folder") 
  #}
}





