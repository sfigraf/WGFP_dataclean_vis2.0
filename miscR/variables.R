# variables
# name of multiplexor: CD7 with A1-A4
#dam stationung: 8330 in states function as well
# Fraser River Confluence: 10120 , used in get_movements
#test tags: c("900230000102751","900226001581072","900230000004000")
#utm sites for each entenna: in allEventsCombined
#biomark tags : "999000000007585", "999000000007601", "999000000007602"
#rivers/antennas: (Event %in% c("RB1", "RB2")) ~ "Colorado River", # there is no is.na here because RB UTM
# (Event %in% c("HP3", "HP4")) ~ "Colorado River",
# (Event %in% c("CF5", "CF6")) ~ "Colorado River",
# (Event %in% c("CD7", "CD8", "CD9", "CD10", "CU11", "CU12")) ~ "Connectivity Channel",
# (Event %in% c("B3", "B5")) ~ "Colorado River",
# (Event %in% c("B4", "B6")) ~ "Fraser River",