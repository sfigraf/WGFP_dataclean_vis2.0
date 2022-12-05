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
            SCD = "CD7",
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
            SCD = "CD8",
            ANT = "A1",
            NCD = "30",
            EFA = .6) %>%
    add_row(Code = "S",
            DTY = "2022-12-03",
            ARR = "07:58:58.900",
            TRF = "G",
            DUR = "00:00.9",
            TTY = "A",
            TAG = "900_230000999999",
            SCD = "CD9",
            ANT = "A1",
            NCD = "30",
            EFA = .6) %>%
    add_row(Code = "S",
            DTY = "2022-12-04",
            ARR = "07:58:58.900",
            TRF = "G",
            DUR = "00:00.9",
            TTY = "A",
            TAG = "900_230000999999",
            SCD = "CD10",
            ANT = "A1",
            NCD = "30",
            EFA = .6) %>%
    add_row(Code = "S",
            DTY = "2022-12-05",
            ARR = "07:58:58.900",
            TRF = "G",
            DUR = "00:00.9",
            TTY = "A",
            TAG = "900_230000999999",
            SCD = "CU11",
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
            SCD = "CU12",
            ANT = "A1",
            NCD = "30",
            EFA = .6)
  
  
  ### Biomark
  
  Biomark <- biomark1 %>%
    add_row(Scan.Date = "2022-12-07",Scan.Time = "59:59.9", Download.Date = "9/16/2022",Download.Time = "11:30:41",Reader.ID = "A3",Antenna.ID = 1,HEX.Tag.ID = "384.358D14F739",
            DEC.Tag.ID = "900.230000999999",Temperature.C = NA,Signal.mV= NA,Is.Duplicate= "Yes",Latitude = NA,Longitude= NA,File.Name= NA) %>%
    add_row(Scan.Date = "2022-12-08",Scan.Time = "59:59.9", Download.Date = "9/16/2022",Download.Time = "11:30:41",Reader.ID = "A4",Antenna.ID = 1,HEX.Tag.ID = "384.358D14F739",
            DEC.Tag.ID = "900.230000999999",Temperature.C = NA,Signal.mV= NA,Is.Duplicate= "Yes",Latitude = NA,Longitude= NA,File.Name= NA)
  
  release1 <- release1 %>%
    add_row(TagID = "230000999999", River = "Fraser River", ReleaseSite = "Fraser River Ranch", Date = "9/1/2020", Time = "12:00:00", UTM_X = "417231",UTM_Y = "4438604", Species = "LOC", Length = 566, Weight = 600, Event = "Release")
  
  dummy_list <- list("Stationary" = Stationary, "Biomark" = Biomark, "Release" = release1)
  return(dummy_list)
}

