# Create a data table 'info' to store details of the monitoring setup

info <- data.table::data.table(
  
  # Set a title for the data analysis (this can be the batch name or other descriptor)
  Batch = "Batch9_2Days",
  
  # Filename associated with each monitor's data
  # "each" reflects the number of sensors per monitor in the array used;
  # In this case, there are 32 sensors per DAM monitor, each holding data for 32 flies
  file = rep(c("Monitor22.txt", "Monitor7.txt",
               "Monitor14.txt", "Monitor3.txt",
               "Monitor15.txt", "Monitor39.txt",
               "Monitor53.txt", "Monitor27.txt",
               "Monitor52.txt", "Monitor17.txt",
               "Monitor21.txt", "Monitor50.txt"), each = 32),
  
  # Unique identifier for each sensor region (range from 1 to 32)
  region_id = 1:32,
  
  # Data status (e.g., whether the data collection was successful, marked "OK")
  status = "OK",
  
  # Start date and time of the monitoring period
  start_datetime = "2019-08-17 10:00:00",
  
  # Stop date and time of the monitoring period
  stop_datetime = "2019-08-19 10:20:00",
  
  # Sex of the subjects being monitored (M = Male, F = Female)
  Sex = "M",
  
  # Genotype of the subjects being monitored (this is replicated for each group of flies)
  Genotype = rep(c("SIP-L2-2", "SIP-L2-2",
                   "SIP-S2-8", "SIP-S2-8",
                   "SIP-S2-1", "SIP-S2-1",
                   "CS", "CS",
                   "SIP-S2-10", "SIP-S2-10",
                   "SIP-L2-1", "SIP-L2-1"), each = 32),
  
  # Temperature during the monitoring period
  Temperature = "21.5C",
  
  # Treatment applied during monitoring period
  Treatment = rep(c("Iso","Grp",
                    "Iso","Grp",
                    "Iso","Grp",
                    "Iso","Grp",
                    "Iso","Grp",
                    "Iso","Grp"),each = 32),
  
  # Environment condition during the experiment (e.g., NA if not applicable)
  Environment = rep(c("2D", "2D",
                      "2D", "2D",
                      "2D", "2D",
                      "2D", "2D",
                      "2D", "2D",
                      "2D", "2D"), each = 32),
  
  # Light cycle during monitoring (e.g., light-dark cycle in hours)
  Light = rep(c("12:12", "12:12",
                "12:12", "12:12",
                "12:12", "12:12",
                "12:12", "12:12",
                "12:12", "12:12",
                "12:12", "12:12"), each = 32)
)
# ----------------------------------------------------------------------------

# Change status manually to exclude cuvettes from analysis,
# info <- SetStatus(info, regionID=4, monitor="M33")

