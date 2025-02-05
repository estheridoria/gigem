# Set a title for the data analysis (this can be the batch name or other descriptor)
Title = "Batch9_5Days"

# ----------------------------------------------------------------------------
# Create a data table 'info' to store details of the monitoring setup

info <- data.table::data.table(
  
  # Filename associated with each monitor data
  # "each" reflects the number of sensors for the array used; 
  # 32 for DAM monitors holding 32 flies
  file = rep(c("Monitor39.txt","Monitor3.txt",
               "Monitor50.txt","Monitor17.txt",
               "Monitor15.txt","Monitor14.txt",
               "Monitor7.txt","Monitor52.txt",
               "Monitor21.txt","Monitor27.txt",
               "Monitor53.txt","Monitor22.txt",
               "Monitor26.txt","Monitor38.txt"), each = 32),
  
  # Monitor identifier
  monitor = rep(c("M39","M3",
                  "M50","M17",
                  "M15","M14",
                  "M7","M52",
                  "M21","M27",
                  "M53","M22",
                  "M26","M38"), each = 32 ),
  
  # Unique identifier for each region, same as number for each
  region_id = 1:32,
  
  # Status of the data (e.g., "OK")
  status = "OK",
  
  # Start datetime for monitoring period
  start_datetime = "2019-08-20 10:01:00",
  
  # Stop datetime for monitoring period
  stop_datetime = "2019-08-23 10:20:00",
  
  # Sex of the subject being monitored
  Sex = rep(c("M","M",
              "M","M",
              "M","M",
              "M","M",
              "M","M",
              "M","M",
              "M","M"), each = 32),
  
  # all genotypes of the subject being monitored
  Genotype = rep(c("SIP-S2-1","SIP-S2-1",
                   "SIP-L2-1","SIP-L2-1",
                   "SIP-S2-8","SIP-S2-8",
                   "SIP-L1-2","SIP-L1-2",
                   "CS","CS",
                   "SIP-S2-10","SIP-S2-10",
                   "SIP-L2-2","SIP-L2-2"), each = 32),
  
  # temperature during monitoring
  Temperature = rep(c("21.5C", "21.5C",
               "21.5C", "21.5C",
               "21.5C", "21.5C",
               "21.5C", "21.5C",
               "21.5C", "21.5C",
               "21.5C", "21.5C",
               "21.5C", "21.5C"), each = 32),

  # Treatments applied during monitoring period
  Treatment = rep(c("Iso","Grp",
                    "Iso","Grp",
                    "Iso","Grp",
                    "Iso","Grp",
                    "Iso","Grp",
                    "Iso","Grp",
                    "Iso","Grp"),each = 32),

  # Environmental factors
  Environment = rep(c("5D","5D",
                      "5D","5D",
                      "5D","5D",
                      "5D","5D",
                      "5D","5D",
                      "5D","5D",
                      "5D","5D"), each = 32),
  
  # Light.Dark phases
  Light = rep(c("12.12", "12.12",
                "12.12", "12.12",
                "12.12", "12.12",
                "12.12", "12.12",
                "12.12", "12.12",
                "12.12", "12.12",
                "12.12", "12.12"), each = 32)
)
# ----------------------------------------------------------------------------

# Change status manually to exclude cuvettes from analysis,
# info <- SetStatus(info, regionID=4, monitor="M33")

