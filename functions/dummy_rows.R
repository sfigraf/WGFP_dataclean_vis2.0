# stationary1 <- Stationary
# biomark1 <- Biomark
# release1 <- Release
add_dummy_rows <- function(stationary1, biomark1, release1) {
  Stationary <- stationary1 %>%
    add_row(Code = "S",
            DTY = "2022-12-01",
            ARR = "07:58:58.900",
            TRF = "G",
            DUR = "00:00.9",
            TTY = "A",
            TAG = "900_230000999999",
            SCD = "CD1",
            ANT = "A1",
            NCD = "30",
            EFA = .6) %>%
    add_row(Code = "S",
            DTY = "2022-12-02",
            ARR = "07:58:58.900",
            TRF = "G",
            DUR = "00:00.9",
            TTY = "A",
            TAG = "900_230000999999",
            SCD = "CD2",
            ANT = "A2",
            NCD = "30",
            EFA = .6) %>%
    add_row(Code = "S",
            DTY = "2022-12-03",
            ARR = "07:58:58.900",
            TRF = "G",
            DUR = "00:00.9",
            TTY = "A",
            TAG = "900_230000999999",
            SCD = "CU1",
            ANT = "A3",
            NCD = "30",
            EFA = .6) %>%
    add_row(Code = "S",
            DTY = "2022-12-04",
            ARR = "07:58:58.900",
            TRF = "G",
            DUR = "00:00.9",
            TTY = "A",
            TAG = "900_230000999999",
            SCD = "CU2",
            ANT = "A4",
            NCD = "30",
            EFA = .6) %>%
    add_row(Code = "S",
            DTY = "2022-12-05",
            ARR = "07:58:58.900",
            TRF = "G",
            DUR = "00:00.9",
            TTY = "A",
            TAG = "900_230000999999",
            SCD = "CS1",
            ANT = "A1",
            NCD = "30",
            EFA = .6) %>%
    add_row(Code = "S",
            DTY = "2022-12-06",
            ARR = "07:58:58.900",
            TRF = "G",
            DUR = "00:00.9",
            TTY = "A",
            TAG = "900_230000999999",
            SCD = "CS2",
            ANT = "A2",
            NCD = "30",
            EFA = .6)
  
  
  ### Biomark
  
  Biomark <- biomark1 %>%
    add_row(Scan.Date = "2022-12-07",Scan.Time = "12:52:10.810", Download.Date = "9/16/2022",Download.Time = "11:30:41",Reader.ID = "A3",Antenna.ID = 1,HEX.Tag.ID = "384.358D14F739",
            DEC.Tag.ID = "900.230000999999",Temperature.C = NA,Signal.mV= NA,Is.Duplicate= "Yes",Latitude = NA,Longitude= NA,File.Name= NA) %>%
    add_row(Scan.Date = "2022-12-08",Scan.Time = "12:52:10.810", Download.Date = "9/16/2022",Download.Time = "11:30:41",Reader.ID = "A4",Antenna.ID = 1,HEX.Tag.ID = "384.358D14F739",
            DEC.Tag.ID = "900.230000999999",Temperature.C = NA,Signal.mV= NA,Is.Duplicate= "Yes",Latitude = NA,Longitude= NA,File.Name= NA) %>%
    add_row(Scan.Date = "2022-12-09",Scan.Time = "12:52:10.810", Download.Date = "9/16/2022",Download.Time = "11:30:41",Reader.ID = "B4",Antenna.ID = 1,HEX.Tag.ID = "384.358D14F739",
            DEC.Tag.ID = "900.230000999999",Temperature.C = NA,Signal.mV= NA,Is.Duplicate= "Yes",Latitude = NA,Longitude= NA,File.Name= NA)
  
  release1 <- release1 %>%
    add_row(TagID = "230000999999", River = "Fraser River", ReleaseSite = "Fraser River Ranch", Date = "9/1/2020", Time = "12:00:00", UTM_X = "417231",UTM_Y = "4438604", Species = "LOC", Length = 566, Weight = 600, Event = "Release")
  
  ghost_dummy <- data.frame(TAG = c("230000228444"), Ghost_date = as.Date(c("2021-06-03")))
  
  
  dummy_list <- list("Stationary" = Stationary, "Biomark" = Biomark, "Release" = release1, "Ghost_tags" = ghost_dummy)
  return(dummy_list)
}

