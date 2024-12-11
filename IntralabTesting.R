#download gigem-----------------------------------------------------------------
  install.packages("devtools")  # or install.packages("remotes")
  library(devtools)             # or library(remotes)
  
  Sys.setenv(GITHUB_PAT = "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")
  
  install_github('Wanhelilab/gigem')
  library(gigem)

# Download template files-------------------------------------------------------
  token <- "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"
  
  auth_url <- paste0("https://", token, "@raw.githubusercontent.com/WanheLiLab/gigem/main/HitRun.R")
  download.file(auth_url, destfile = "HitRun.R", method = "libcurl")
  
  auth_url <- paste0("https://", token, "@raw.githubusercontent.com/WanheLiLab/gigem/main/Main.R")
  download.file(auth_url, destfile = "Main.R", method = "libcurl")
  
  auth_url <- paste0("https://", token, "@raw.githubusercontent.com/WanheLiLab/gigem/main/README.md")
  download.file(auth_url, destfile = "gigemREADME.md", method = "libcurl")
# # Example data -----------------------------------------------------------------
#   dest_dir <- paste0(getwd(),"/ExampleAnalysis")  # Folder to clone into
# 
#   # Construct the URL with the token for authentication
#   auth_url <- paste0("https://", "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", 
#                      "@github.com/WanheLiLab/gigemExample.git")
#   
#   # Run the git clone command
#   system(paste("git clone", auth_url, dest_dir))
