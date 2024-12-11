# Set a title for the data analysis
Title = "Batch9_2Days"

# ----------------------------------------------------------------------------
# Create a data table 'info' to store details of the monitoring setup

info <- data.table::data.table(

  # Filename associated with each monitor's data
  # "each" reflects the number of sensors per monitor in the array used;
  # In this case, there are 32 sensors per DAM monitor, each holding data for 32 flies
  file = rep(c("Monitor22.txt", "Monitor7.txt",
               "Monitor14.txt", "Monitor3.txt",
               "Monitor15.txt", "Monitor39.txt",
               "Monitor53.txt", "Monitor27.txt",
               "Monitor52.txt", "Monitor17.txt",
               "Monitor21.txt", "Monitor50.txt"), each = 32),

  # Monitor identifier (this is a shorthand for each monitor in the experiment)
  monitor = rep(c("M22", "M7",
                  "M14", "M3",
                  "M15", "M39",
                  "M53", "M27",
                  "M52", "M17",
                  "M21", "M50"), each = 32),

  # Unique identifier for each sensor region (range from 1 to 32)
  region_id = 1:32,

  # Data status (e.g., whether the data collection was successful, marked "OK")
  status = "OK",

  # Start date and time of the monitoring period
  start_datetime = "2019-08-17 10:05:00",

  # Stop date and time of the monitoring period
  stop_datetime = "2019-08-19 10:20:00",

  # Sex of the subjects being monitored (M = Male, F = Female)
  sex = "M",

  # Genotype of the subjects being monitored (this is replicated for each group of flies)
  genotype = rep(c("SIP-L2-2", "SIP-L2-2",
                   "SIP-S2-8", "SIP-S2-8",
                   "SIP-S2-1", "SIP-S2-1",
                   "CS", "CS",
                   "SIP-S2-10", "SIP-S2-10",
                   "SIP-L2-1", "SIP-L2-1"), each = 32),

  # Temperature during the monitoring period
  temp = "21.5C",

  # Treatment applied during the monitoring period (e.g., Isolation or Group treatment)
  treatment = rep(c("Iso_2D", "Grp_2D",
                    "Iso_2D", "Grp_2D",
                    "Iso_2D", "Grp_2D",
                    "Iso_2D", "Grp_2D",
                    "Iso_2D", "Grp_2D",
                    "Iso_2D", "Grp_2D"), each = 32),

  # Environment condition during the experiment (e.g., NA if not applicable)
  environment = rep(c("NA", "NA",
                      "NA", "NA",
                      "NA", "NA",
                      "NA", "NA",
                      "NA", "NA",
                      "NA", "NA"), each = 32),

  # Light cycle during monitoring (e.g., light-dark cycle in hours)
  light = rep(c("12.12", "12.12",
                "12.12", "12.12",
                "12.12", "12.12",
                "12.12", "12.12",
                "12.12", "12.12",
                "12.12", "12.12"), each = 32)
)
# ----------------------------------------------------------------------------

# Change status manually to exclude cuvettes from analysis,
# info <- SetStatus(info, regionID=4, monitor="M33")

