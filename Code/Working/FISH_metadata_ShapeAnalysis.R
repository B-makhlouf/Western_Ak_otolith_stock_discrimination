# This script is to automatically populate the metadata for otolith shape analysis 
MasterDirectory<- "/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/ShapeAnalysis/Original"

# list the directories within 
directories<- list.dirs(MasterDirectory, full.names = FALSE, recursive = FALSE)


# Create an empty data frame with the desired structure
FSH_dataframe <- data.frame(
  Watershed = character(), 
  folder = character(), 
  picname = character(), 
  cal = numeric(), 
  stringsAsFactors = FALSE
)

# Loop through each directory
for (i in 1:length(directories)) {
  # List all files in the current directory
  files <- list.files(paste0(MasterDirectory, "/", directories[i]), full.names = TRUE)
  
  # Determine the Watershed and folder based on the directory name
  if (directories[i] == "NK") {
    Watershed <- "Nushagak"
    folder <- "NK"
  } else if (directories[i] == "KK") {
    Watershed <- "Kuskokwim"
    folder <- "KK"
  } else if (directories[i] == "YK") {
    Watershed <- "Yukon"
    folder <- "YK"
  } else {
    # Optional: Handle unknown directories
    stop(paste("Unknown directory:", directories[i]))
  }
  
  # Get the base names of the files (excluding the full path)
  picnames <- basename(files)
  
  # Remove the ".jpg" extension from file names
  picnames <- gsub(".jpg$", "", picnames)
  
  # Add the data to the FSH_dataframe
  FSH_dataframe <- rbind(
    FSH_dataframe, 
    data.frame(
      Watershed = Watershed, 
      folder = folder, 
      picname = picnames, 
      cal = rep(200, length(picnames)),  # Setting cal to 20 for all entries
      stringsAsFactors = FALSE
    )
  )
}



# Save the metadata file if needed
write.csv(FSH_dataframe, "ShapeAnalysis/FISH.csv", row.names = FALSE)
