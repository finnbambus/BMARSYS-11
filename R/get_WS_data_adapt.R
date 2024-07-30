
get_WS_data_adapt <- function(file) {
  #file = files[1]
  
  dat <- readLines(file)
  
  #Variablennamen
  dat_head <- dat[63]
  dat_head
  
  
  #Daten
  dat_data <- dat[-(1:63)]
  dat_data <- dat_data[-15]
  
  #Aufbereitung Variablennamen
  dat_head <- str_split(dat_head, "\t", simplify = TRUE)
  
  dat_head <- dat_head %>%
    str_to_lower() %>%
    str_replace_all("[^[:alnum:]]", " ") %>%
    str_trim() %>%
    str_replace(" ", "_") %>%
    str_remove_all(" ") %>%
    str_remove("_hhmmss")
  
  dat_head <- as.vector(t(dat_head))
  
  
  #Aufbereitung Daten
  dat_data <- str_trim(dat_data)
  dat_data <- str_split(dat_data, "\t", simplify = TRUE)
  
  
  # create tibble 'tob_tbl'
  ws_tbl <- as.data.frame(dat_data)
  
  names(ws_tbl) <- dat_head
  
  #Filename ergänzen
  ws_tbl$filename <- file
  
  
  #nicht benötigte Spalten löschen
  ws_tbl <- ws_tbl %>%
    select(-bottle, -bottom_alarm, -comments_index)
  
  
  #Uhrzeit formatieren
  ws_tbl$time <- hms(ws_tbl$time)
  
  #Datum aus Dateiname ergänzen
  
  ws_tbl <- ws_tbl %>%
    separate(filename, into = c("cruise", "date", "station", "substation", "hol"), sep = "_")
  
  ws_tbl$hol <- ws_tbl$hol %>%
    str_remove_all("[^0-9]")
  
  ws_tbl$station <- ws_tbl$station %>%
    str_remove_all("[^0-9]") 
  
  ws_tbl$date <- ymd(ws_tbl$date)
  
  
  #alle anderen Variablen numerisch
  ws_tbl[, c(2:12, 15:17)] <- lapply(ws_tbl[, c(2:12, 15:17)], as.numeric)
  
  #nur Daten vom Absenken, nicht die vom aufsteigen 
  ws_tbl <- ws_tbl %>%
    arrange(time) %>%
    filter(cumsum(pressure_dbar == max(pressure_dbar)) <= 1)
  
  out <- as_tibble(ws_tbl)
  return(out)
}
