# ============================================================================
# Load required packages
# ============================================================================
pckgList <- c(
  "shiny", 
  "shinyjs",
  "shinyBS", 
  "shinydashboard",
  "shinycssloaders", 
  "DT", 
  "heatmaply", 
  "RSQLite",
  "reshape", 
  "igraph", 
  "visNetwork",
  "htmlwidgets", 
  "openxlsx", 
  "rjson",
  "rintrojs",
  "httr"
)
pckgMissing <- pckgList[!(pckgList %in% installed.packages()[,"Package"])]
if(length(pckgMissing)) install.packages(pckgMissing)

for (i in pckgList) {
  library(i, character.only= TRUE)
}

# ============================================================================
# Load dataset stars
# ============================================================================
starsHsa <- read.delim("./www/decorationTables/gseHsa.txt")			
starsMmu <- read.delim("./www/decorationTables/gseMmu.txt")			
starsII <- read.delim("./www/decorationTables/ipf_vs_ctrl_lung_coding.txt")
starsIII <- read.delim("./www/decorationTables/bleoD14_vs_ctrl_lung_coding.txt")
starsIV <- read.delim("./www/decorationTables/gseNonCoding.txt")
stars <- rbind(starsHsa, starsMmu, starsIV)

# ============================================================================
# Connect to DB
# ============================================================================
db_path <- "."
fibromine_db <- dbConnect(RSQLite::SQLite(), 
  file.path(db_path, "FibromineDB.sqlite"))

# ============================================================================
# Source custom functions
# ============================================================================
curDir <- getwd()
source(file.path(curDir, "utils.R"))

# ============================================================================
# techChoices
# ============================================================================
techChoices <- c("RNA expression profiling", 
	"Non-coding RNA expression profiling")

# ============================================================================
# gcnHsaChoices
# ============================================================================
gcnHsaChoices <- as.character(dbGetQuery(conn = fibromine_db,
  statement = '
    SELECT 
      Name
    FROM 
      GCNdrivers
    WHERE 
      Comparison = "IPF_vs_Ctrl"
  ;'
)$Name)

# ============================================================================
# gcnMmuChoices
# ============================================================================
gcnMmuChoices <- as.character(dbGetQuery(conn = fibromine_db,
  statement = '
    SELECT 
      Name
    FROM 
      GCNdrivers
    WHERE 
      Comparison = "BleomycinD14_vs_Ctrl"
  ;'
)$Name)

# ============================================================================
# scGeneChoices
# ============================================================================
scGeneChoices <- unique(as.character(dbGetQuery(conn = fibromine_db,
  statement = '
    SELECT 
      Gene
    FROM 
      scDEA
  ;'
)$Gene))

# ============================================================================
# DEmiRNAChoices
# ============================================================================
DEmiRNAChoices <- as.character(dbGetQuery(conn = fibromine_db,
  statement = '
    SELECT 
      prodID
    FROM 
      nonCodingDEShort
  ;'
)$prodID)

# ============================================================================
# Datasets lists to use in download data tab
# ============================================================================
transDatasets <- dbGetQuery(
	fibromine_db,
	'SELECT DISTINCT
		DatasetID 
	FROM 
		DatasetsDescription
	;'
)
transDatasets <- as.character(transDatasets[,1])
transDatasets <- transDatasets[grep("^GSE", transDatasets)]

protDatasets <- dbGetQuery(
	fibromine_db,
	'SELECT DISTINCT
		DatasetID 
	FROM 
		DatasetsDescription
	;'
)
protDatasets <- as.character(protDatasets[,1])
protDatasets <- protDatasets[-grep("^GSE", protDatasets)]
