library(Microsoft365R)
# Create sharepoint site object for the Data Science Team/Sharepoint
site <- get_sharepoint_site("Data Science")
# Create drive object for the documents
datasci_sp <- site$get_drive("Documents")
# List all the files/folders in the R folder for the Data Science team
datasci_sp$list_files("R")
# Download a file
datasci_sp$download_file("R/packages - profile use by R group.xlsx")

