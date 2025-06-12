Title = "Batch9_2Days"
# ----------------------------------------------------------------------------
info <- data.table::data.table(

  file = rep(c("Monitor22.txt", "Monitor7.txt",
               "Monitor14.txt", "Monitor3.txt",
               "Monitor15.txt", "Monitor39.txt",
               "Monitor53.txt", "Monitor27.txt",
               "Monitor52.txt", "Monitor17.txt",
               "Monitor21.txt", "Monitor50.txt"), each = 32),
  region_id = 1:32,
  status = "OK",
  start_datetime = "2019-08-17 10:00:00",
  stop_datetime = "2019-08-19 10:20:00",
  Sex = "M",
  Genotype = rep(c("SIP-L2-2", "SIP-L2-2",
                   "SIP-S2-8", "SIP-S2-8",
                   "SIP-S2-1", "SIP-S2-1",
                   "CS", "CS",
                   "SIP-S2-10", "SIP-S2-10",
                   "SIP-L2-1", "SIP-L2-1"), each = 32),
  Temperature = "21.5C",
  Treatment = rep(c("Iso","Grp",
                    "Iso","Grp",
                    "Iso","Grp",
                    "Iso","Grp",
                    "Iso","Grp",
                    "Iso","Grp"),each = 32),
  Environment = "2D",
  Light = "12:12"
)
# ----------------------------------------------------------------------------

# Change status manually to exclude cuvettes from analysis,
# info <- SetStatus(info, regionID=4, monitor="M33")

