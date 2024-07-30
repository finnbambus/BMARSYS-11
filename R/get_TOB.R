get_TOB <- function(file) {
  #file = files[1]
  
  dat <- readLines(file)
  
  #Variablennamen
  dat_head <- dat[39]
  dat_head
  
  
  #Daten
  dat_data <- dat[-(1:41)]
  dat_data
  
  #Aufbereitung Variablennamen
  dat_head <- str_to_lower(dat_head) %>%
  str_remove_all("_") 
  
  dat_head <- str_split(dat_head, " +", simplify = TRUE)
  dat_head <- dat_head[,3:17]
  dat_head <- as.vector(t(dat_head))
  
  #Aufbereitung Daten
  dat_data <- str_trim(dat_data)
  dat_data <- str_split(dat_data, " +", simplify = TRUE)
  dat_data <- dat_data[,2:16]
  
  
  # create tibble 'tob_tbl'
  tob_tbl <- as.data.frame(dat_data)
  names(tob_tbl) <- dat_head
  
  #Filename ergänzen
  tob_tbl$filename <- file
  
  #Rename Column 1 - date
  which_names <- which(names(tob_tbl) == "datim")
  names(tob_tbl)[which_names[1]] <- "date"
  
  #Datum formatieren
  tob_tbl$date <- dmy(tob_tbl$date)
  tob_tbl$datim <- hms(tob_tbl$datim)
  
  #Cruise, Station extrahieren
  if (anyDuplicated(colnames(tob_tbl)) > 0) {
    colnames(tob_tbl) <- make.unique(colnames(tob_tbl))
  }
  
  tob_tbl <- tob_tbl %>%
    separate(filename, into = c("cruise", "datum", "station", "substation", "hol"), sep = "_")
  
  tob_tbl$hol <- tob_tbl$hol %>%
    str_remove_all("[^0-9]")
  
  tob_tbl$station <- tob_tbl$station %>%
    str_remove_all("[^0-9]")
  
  
  #Doppelte  Spalten löschen
  tob_tbl <- tob_tbl[,c(-15, -17)]

  
  #alle anderen Variablen numerisch
  tob_tbl[, c(3:14, 16:18)] <- lapply(tob_tbl[, c(3:14, 16:18)], as.numeric)
  
  #nur Daten vom Absenken, nicht die vom aufsteigen 
  tob_tbl <- tob_tbl %>%
    arrange(datim) %>%
    filter(cumsum(press == max(press)) <= 1)
  
  out <- as_tibble(tob_tbl)
  return(out)
}
