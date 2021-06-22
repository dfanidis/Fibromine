# Fibromine "server.R" for Shiny Server
starsHsa <- read.delim("./www/decorationTables/gseHsa.txt")			
starsMmu <- read.delim("./www/decorationTables/gseMmu.txt")			
starsII <- read.delim("./www/decorationTables/ipf_vs_ctrl_lung_coding.txt")
starsIII <- read.delim("./www/decorationTables/bleoD14_vs_ctrl_lung_coding.txt")
starsIV <- read.delim("./www/decorationTables/gseNonCoding.txt")
stars <- rbind(starsHsa, starsMmu, starsIV)

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
  "httr",
  "enrichR"
)
pckgMissing <- pckgList[!(pckgList %in% installed.packages()[,"Package"])]
if(length(pckgMissing)) install.packages(pckgMissing)

for (i in pckgList) {
  library(i, character.only= TRUE)
}

# ============================================================================
# shinyServer()
# ============================================================================

shinyServer(function(input, output, session) {

	# ============================================================================
	# Connect to DB
	# ============================================================================
	db_path <- "."
	fibromine_db <- dbConnect(RSQLite::SQLite(), 
	file.path(db_path, "FibromineDB.sqlite"))

	curDir <- getwd()
	source(file.path(curDir, "utils.R"))

	# ============================================================================
	# "Home" tab Items
	# ============================================================================

	# Introductory tour

	## Set the tour
	steps <- reactive(
        data.frame(
            element= paste0(".step_", 1:10),
            intro= c(
                 paste("<b>Dataset explorer</b> provides a quick way to explore",
                	"transcriptomic and proteomic datasets in an independent",
                	"or an integrated fashion."
                ),
                paste("<b>Gene explorer</b> enables the user to assess the expression",
                	"pattern of specific gene(-s) of interest via single or batch search.",
                	"Wealthy gene metadata, as well as the report of any differentially",
                	"expressed proteins coded by the searched genes are also available."
                ),
                paste("Via <b>Protein explorer</b> the user can discover the differenitally",
                	"expressed proteins in the lung of IPF patients compared to the respective",
                	"healthy controls.", "In addition, condition-specific protein-protein networks",
                	"can be plotted."
                ),
                paste("<b>Datasets benchmarking</b> tab summarizes  the results of a <b>custom</b> datasets",
                	"benchmarking process implemented into Fibromine: the <b>more stars</b> a dataset",
                	'has been decorated with, the <b>more "trustworthy" it is.</b><br/>'
                ),
                paste("<b>Single cell data</b> tab summarizes all single cell datasets published",
                	"thus far in the context of IPF pathology."
                ),
                paste("For anyone that wants to perform his/her own analysis, <b>Download Data</b>",
                	"feature enables the acquisition of normalized gene expression data per dataset.",
                	"Proteomics data per dataset are also available for download."
                ),
                paste("Detailed info about Fibromine explorers and their back-end processing can",
                	"be found in the <b>Docs</b> section"
                ),
				paste("<b>How to</b> tab holds a visual guide for almost all Fibromine's features and analysis",
                	"results. Check it out!"
                ),
                paste("Designed and implemented by <b>Dionysios Fanidis</b>.<br />",
                	"Supervised by <b>Panagiotis Moulos</b> & <b>Vassilis Aidinis</b>"
                ),
                paste("<b>Feedback makes us better!</b> Please, report any issue or new dataset",
                	"not already included into Fibromine on our GitHub repository.")
            ),
            position= rep("auto", 10)
        )
    )

	## Take the tour
	observeEvent(input$introHome,
	introjs(session, 
		options = list(
			steps= steps(),
			"nextLabel" = "Next",
			"prevLabel" = "Previous", 
			"doneLabel" = "Let's begin!"
			)
		)
	)

	## Render external image hyperlinks
	output$gene <- renderImage({
		width<- "160px"
		height<- "70px"
		list(
			src = "./www/logos/ncbiLogo.jpeg",
			contentType = "image/jpeg",
			width = width,
			height = height,
			style="display: block; margin-left: auto; margin-right: auto;"
		)
	}, deleteFile= FALSE)

	output$ensembl <- renderImage({
		width<- "160px"
		height<- "70px"
		list(
			src = "./www/logos/ensemblLogo.png",
			contentType = "image/png",
			width = width,
			height = height,
			style="display: block; margin-left: auto; margin-right: auto;"
		)
	}, deleteFile= FALSE)

	output$uniprot <- renderImage({
		width<- "160px"
		height<- "70px"
		list(
			src = "./www/logos/uniprotLogo.png",
			contentType = "image/png",
			width = width,
			height = height,
			style="display: block; margin-left: auto; margin-right: auto;"
		)
	}, deleteFile= FALSE)

	output$string <- renderImage({
		width<- "160px"
		height<- "70px"
		list(
			src = "./www/logos/stringLogo.png",
			contentType = "image/png",
			width = width,
			height = height,
			style="display: block; margin-left: auto; margin-right: auto;"
		)
	}, deleteFile= FALSE)

	output$go <- renderImage({
		width<- "160px"
		height<- "70px"
		list(
			src = "./www/logos/GOLogo.png",
			contentType = "image/png",
			width = width,
			height = height,
			style="display: block; margin-left: auto; margin-right: auto;"
		)
	}, deleteFile= FALSE)
	
	output$lungAgingAtlas <- renderImage({
		width<- "160px"
		height<- "70px"
		list(
			src = "./www/logos/lungAgingAtlasLogoOfMine.png",
			contentType = "image/gif",
			width = width,
			height = height,
			style="display: block; margin-left: auto; margin-right: auto;"
		)
	}, deleteFile= FALSE)
	
	output$emouse <- renderImage({
		width<- "160px"
		height<- "70px"
		list(
			src = "./www/logos/emouseLogo.gif",
			contentType = "image/gif",
			width = width,
			height = height,
			style="display: block; margin-left: auto; margin-right: auto;"
		)
	}, deleteFile= FALSE)

	output$pulmon <- renderImage({
		width<- "160px"
		height<- "70px"
		list(
			src = "./www/logos/pulmondbLogo.png",
			contentType = "image/png",
			width = width,
			height = height,
			style="display: block; margin-left: auto; margin-right: auto;"
		)
	}, deleteFile= FALSE)

	output$nupulmon <- renderImage({
		width<- "160px"
		height<- "70px"
		list(
			src = "./www/logos/nupulmonaryLogoOfMine.png",
			contentType = "image/png",
			width = width,
			height = height,
			style="display: block; margin-left: auto; margin-right: auto;"
		)
	}, deleteFile= FALSE)

	output$ipfatlas <- renderImage({
		width<- "160px"
		height<- "70px"
		list(
			src = "./www/logos/ipfatlasLogoOfMine.png",
			contentType = "image/png",
			width = width,
			height = height,
			style="display: block; margin-left: auto; margin-right: auto;"
		)
	}, deleteFile= FALSE)
	
	# ============================================================================
	# "Dataset explorer" tab Items
	# ============================================================================
	datasetVals <- reactiveValues(mainTable=NULL, protTable= NULL, pval=0.05, 
		fc=1.2, hidePA = NULL, stat=NULL, plotHeat= NULL, plotVolc= NULL)

	## Shape Datasets explorer main table=========================================
	datasetVals$mainTable <- merge(
		dbGetQuery(conn= fibromine_db,
			statement='
				SELECT 
					DatasetsDescription.DatasetID, Reference, ReferenceURL, 
					Datasets.Tech, Platform, GPL, Datasets.Species, Tissue,
					DescrContrast, nExpCtrlPostCuration 
				FROM 
					DatasetsDescription 
				JOIN 
					Datasets 
				ON 
					DatasetsDescription.DatasetID = Datasets.DatasetID
				WHERE 
					Datasets.Tech != "Proteome profiling techniques"
			;'
		),
		stars,
		by= c("DatasetID", "DescrContrast"), all.x= TRUE
	)

	output$datasetsTable <- DT::renderDataTable({
		
		out <- datasetVals$mainTable
		url_prefix <- "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc="

		## Transform GSE in url
		out$DatasetID <- paste0("<a href='", url_prefix, out$DatasetID, 
		  "' rel='noopener noreferrer' target='_blank'>",          
		out$DatasetID, "</a>")

		## Set PMID as a url
		out$Reference <- gsub("^https.*pubmed\\/", "", out$ReferenceURL)
		out$Reference <- gsub("^https.*gov\\/", "", out$Reference)

		out$Reference <- paste0("<a href='", out$ReferenceURL,
			"' rel='noopener noreferrer' target='_blank'>", 
			out$Reference, "</a>")
		out[out$Reference == "<a href='-' rel='noopener noreferrer' target='_blank'>-</a>",
			"Reference"] <- "-"
		out[out$Reference == "<a href='https://www.biorxiv.org/content/biorxiv/early/2019/03/23/580498.full.pdf' rel='noopener noreferrer' target='_blank'>https://www.biorxiv.org/content/biorxiv/early/2019/03/23/580498.full.pdf</a>",
			"Reference"] <- "<a href='https://www.biorxiv.org/content/biorxiv/early/2019/03/23/580498.full.pdf' rel='noopener noreferrer' target='_blank'>biorxiv link</a>"

		## Transform Platform GPL code into hyperlink directing to the GPL page 
		out$Platform <- paste0("<a href='", url_prefix, out$GPL,
			"' rel='noopener noreferrer' target='_blank'>",
			out$GPL, "</a>")

		## Split nExpCtrlPostCuration so as to be properly sortable on the client side
		nSamples <- strsplit(out$nExpCtrlPostCuration, split= "/")
		nSamples <- do.call("rbind", nSamples)
		out$Exp <- as.integer(nSamples[,1])
		out$Ctrl <- suppressWarnings(as.integer(nSamples[,2]))
		# Set the "pooled" Ctrl samples of a specific dataset into 1 (as if they were 1 sample)
		out$Ctrl[is.na(out$Ctrl)] <- 1

		out <- subset(out, select= -c(ReferenceURL, GPL, nExpCtrlPostCuration))
		out <- out[,c(1,3,2,8,4:7,9,10)]
		colnames(out) <- c("GEO accession", "PMID", "Comparison", 
			"Stars count", "Technology", "Platform",
			"Species", "Tissue", "#Experimental", "#Control")

		## Convert to factors for easier client-side filtering of the table
		out$Technology <- as.factor(out$Technology)
		out$Species <- as.factor(out$Species)
		out$Tissue <- as.factor(out$Tissue)
		out$Comparison <- as.factor(out$Comparison)

		## Color stars column
		starBrks <- 0:7
		starClrsPal <- colorRampPalette(c("blue", "red"))
		starClrs <- starClrsPal(length(starBrks)+1)

		datatable(out, selection = "multiple", 
			rownames= FALSE,
			escape= FALSE, 
			filter= "top",
			class= "compact",
			options= list(
				sDom= '<"top">lrt<"bottom">ip', # to enable search bar use "flrt" instead (f for Filtering)
			    columnDefs= list(
					list(
						className= "dt-center", 
						targets= "_all"
					)
				)
			),  
			extensions= "Responsive",
		) %>% formatStyle(
			1:ncol(out), cursor = 'pointer'
		) %>% formatStyle("Stars count",
			color= styleInterval(starBrks, starClrs)
		)

	})
	datasetsTableProxy <- dataTableProxy("datasetsTable")

	## Shape Datasets explorer proteomics table===========================================================
	datasetVals$protTable <- dbGetQuery(conn= fibromine_db,
		statement='
			SELECT 
				DatasetsDescription.DatasetID, Reference, ReferenceURL, 
				Datasets.Tech, Datasets.Species, Tissue, DescrContrast, 
				nExpCtrl 
			FROM 
				DatasetsDescription 
			JOIN 
				Datasets 
			ON 
				DatasetsDescription.DatasetID = Datasets.DatasetID
			WHERE
				Datasets.Tech = "Proteome profiling techniques"
		;'
	)

	output$proteomicsTable <- DT::renderDataTable({
		out <- datasetVals$protTable
			
		## Set PMID as a url
		out$Reference <- gsub("^https.*gov\\/", "", out$ReferenceURL)

		out$Reference <- paste0("<a href='", out$ReferenceURL,
			"' rel='noopener noreferrer' target='_blank'>", 
			out$Reference, "</a>")
		out[out$Reference == "<a href='-' rel='noopener noreferrer' target='_blank'>-</a>",
			"Reference"] <- "-"
		
		## Split nExpCtrl so as to be properly sortable on the client side
		nSamples <- strsplit(out$nExpCtrl, split= "/")
		nSamples <- do.call("rbind", nSamples)
		out$Exp <- as.integer(nSamples[,1])
		out$Ctrl <- suppressWarnings(as.integer(nSamples[,2]))

		out <- subset(out, select= -c(ReferenceURL, nExpCtrl))
		colnames(out) <- c("Dataset", "PMID", 
			"Technology", "Species", "Tissue", "Comparison", 
			"#Experimental", "#Control"
		)

		## Convert to factors for easier client-side filtering of the table
		out$Technology <- as.factor(out$Technology)
		out$Species <- as.factor(out$Species)
		out$Tissue <- as.factor(out$Tissue)
		out$Comparison <- as.factor(out$Comparison)

		datatable(out, selection = "multiple", 
			rownames= FALSE,
			escape= FALSE, 
			filter= "top", 
			class= "compact",
			options= list(
				sDom= '<"top">lrt<"bottom">ip', # to enable search bar use "flrt" instead (f for Filtering)
				columnDefs= list(
					list(
						className= "dt-center", 
						targets= "_all"
					)
				)
			),	
	  		extensions= "Responsive",
	  	) %>% formatStyle(1:ncol(out), cursor = 'pointer')

	})
	proteomicsTableProxy <- dataTableProxy("proteomicsTable")

	# ============================================
	# "Transcriptomic datasets" tour
	# ============================================

	stepsTrans <- reactive(
	    data.frame(
	        element= c(".step_1", ".step1_trans", ".step2_trans", ".step3_trans"),
	        intro= c(
	            paste("<b>Dataset explorer</b> provides a quick way to explore",
	            	"transcriptomic and proteomic datasets in an independent",
	            	"or an integrated fashion."
	            ),
				paste("All Fibromine <b>transcriptomic datasets</b> can be viewed",
					"in the following table. To explore any of them, click on", 
					"the respective <b>table row(-s)</b> and ..."
				),
				paste("then press <b>this</b> button.<br>",
					"Dataset(-s) exploration <b>results</b> are presented in the <b>second tab</b>.",
					"Selection of a <b>single</b> dataset will lead to its simple <b>exploration</b>,",
					"while selection of <b>multiple</b> datasets to the integration of their data."
				),
	            paste("When a <b>new search</b> is required, it is <b>advised</b>",
	            	"to <b>un-check</b> the previously selected datasets using",
	            	"<b>this button. Manually</b> de-selecting each dataset is", 
	            	"possible, but according to our experience is also an",
	            	"<b>error prone</b> process."
	            )
	        ),
	        position= c("top", "bottom-left-aligned", rep("auto", 2))
	    )
	)

	## Take the tour
	observeEvent(input$introTrans,
	introjs(session, 
		options = list(
			steps= stepsTrans(),
			"nextLabel" = "Next",
			"prevLabel" = "Previous", 
			"doneLabel" = "Let's begin!"
			)
		)
	)

	# ============================================
	# "Transcriptomic datasets" tab
	# ============================================

	## Retrieve transcriptomic datasets data==============================================================
	
	## Retrieve selection
	transSamplesSelected <- eventReactive(input$transDtstsSearch, {
		datasetVals$mainTable[input$datasetsTable_rows_selected, 
			c("DatasetID", "DescrContrast", "Tech", "Tissue", "Species")]
	})

	## Automatically remove any previous pathway analysis table
	## once a new search has been initiated
	observeEvent(input$transDtstsSearch, {
		datasetVals$hidePA <- TRUE
	})

	## Check if datasets from multiple species have been simultaneously selected
	## - if yes warn and reset selection
	## - if no redirect to the "DEA statistics" panel to show analysis
	observeEvent({transSamplesSelected()
		datasetVals$pval
		datasetVals$fc
		}, {
			if (length(unique(transSamplesSelected()$Species)) > 1) {
				modal <- modalDialog(												
					title= "Attention!",
					paste(	
						"Currently Fibromine cannot integrate datasets from multiple species.",
						"Please, choose another combination of datasets to integrate."
					),
					footer= tagList(
						actionButton("okSpecies","OK"),
						modalButton("Cancel")
					)
				)
				showModal(modal)
			} else {
				# Redirect to "DEA statistics" panel
				updateTabsetPanel(session, "transDatasetsBox",
					selected= "DEA statistics"
				)
			}
	})

	observeEvent(input$okSpecies, {
		removeModal()

		## Deselect table rows
		datasetsTableProxy %>% selectRows(NULL)

		## Reset p-value and fc thresholds
		updateNumericInput(
			session= session,
			inputId= "pvalCommonIn",
			label= "Type a p-value",
			value= 0.05,
			step= 0.01
		)

		updateNumericInput(
			session= session,
			inputId= "fcCommonIn",
			label= "Type a Fold change",
			value= 1.2,
			step= 0.01
		)

		datasetVals$pval <- 0.05
		datasetVals$fc <- 1.2

	})

	## Choose all rows
	observeEvent(input$selectTransAll, {
		datasetsTableProxy %>% selectRows(input$datasetsTable_rows_all)
	})

	## Clear results
	observeEvent(input$clearTransDtstsSearch, {
		modal <- modalDialog(												
			title= "Attention!",
			paste0(	
				"Datasets choosen will be dropped along with the respective data. ",
				"Analysis parameters will be also reseted."
			),
			footer= tagList(
				actionButton("okDatasets","OK"),
				modalButton("Cancel")
			)
		)
		showModal(modal)
	})

	observeEvent(input$okDatasets, {
		removeModal()

		## Deselect table rows
		datasetsTableProxy %>% selectRows(NULL)

		## Reset p-value and fc thresholds
		updateNumericInput(
			session= session,
			inputId= "pvalCommonIn",
			label= "Type a p-value",
			value= 0.05,
			step= 0.01
		)

		updateNumericInput(
			session= session,
			inputId= "fcCommonIn",
			label= "Type a Fold change",
			value= 1.2,
			step= 0.01
		)

		datasetVals$pval <- 0.05
		datasetVals$fc <- 1.2

	})

	## Retrieve statistics
	observeEvent(input$filterCommon, {
		datasetVals$pval <- input$pvalCommonIn
		datasetVals$fc <- input$fcCommonIn
	})

	samplesStat <- eventReactive({transSamplesSelected()
		datasetVals$pval
		datasetVals$fc
		}, {

		req(datasetVals$pval)
		req(datasetVals$fc)
		fcTemp <- log2(datasetVals$fc)

		stats <- vector("list", length= nrow(transSamplesSelected()))
		names(stats) <- transSamplesSelected()$DatasetID 

		for (i in 1:length(stats)) {

			if (transSamplesSelected()[i,"Tech"] == "Non-coding RNA profiling by array") {
					
				## Get statistics
				## All genes must be fetched in order to properly compute FDR.
				## DEGs will be selected downstream.
				stat_temp <- dbGetQuery(																
					conn=fibromine_db,
					statement= '
						SELECT 
							prodAC, GSE, log2FC, Pval, StatContrastNonCoding
						FROM 
							StatComparisonsNonCoding 
						WHERE 
							GSE = :x AND StatContrastNonCoding = :y
					;',
					params= list(
						x= transSamplesSelected()[i,"DatasetID"], 
						y= transSamplesSelected()[i,"DescrContrast"]
					)
				)

				# Compute FDR and place it next to nominal p-value
				stat_temp$FDR <- p.adjust(stat_temp$Pval, method= "fdr")

				# Keep genes based on user-defined pval and fc thresholds
				stat_temp <- stat_temp[
					which(stat_temp$log2FC < -fcTemp | stat_temp$log2FC > fcTemp),
				] 
				stat_temp <- stat_temp[
					which(stat_temp$Pval < datasetVals$pval),
				]

				## Get prodAC2prodID map
				## miRNA names are fetched separately
				## to be in consistency with the coding datasets
				## (see below)
				prodAC2prodID <- dbGetQuery(															
					conn= fibromine_db,
					statement= '
						SELECT 
							prodAC, prodID 
						FROM 
							miRBase
						WHERE
							prodAC = :x
					;',
					params= list(
						x= stat_temp$prodAC
					)
				)

				## Merge
				stat_temp <- merge(stat_temp, prodAC2prodID, by="prodAC", all.x=TRUE, sort=FALSE)		
				stat_temp[which(is.na(stat_temp$prodID)),"prodID"] <- "-"
				stat_temp <- stat_temp[, c(ncol(stat_temp), 1: (ncol(stat_temp)-1))]

				## Format table
				stat_temp <- stat_temp[ ,c(1,2,4,5,7,3,6)]
				colnames(stat_temp)[c(1,2,7)] <- c("Name", "Code", "Comparison")

				stats[[i]] <- stat_temp

			} else {

				## Get statistics
				## All genes must be fetched in order to properly compute FDR.
				## DEGs will be selected downstream.
				stat_temp <- dbGetQuery(																
					conn=fibromine_db,
					statement= '
						SELECT 
							ENSGid, GSE, log2FC, Pval, StatContrast
						FROM 
							StatComparisonsCoding 
						WHERE 
							GSE = :x AND StatContrast = :y
					;',
					params= list(
						x= transSamplesSelected()[i,"DatasetID"], 
						y= transSamplesSelected()[i,"DescrContrast"]
					)
				)

				# Compute FDR and place it next to nominal p-value
				stat_temp$FDR <- p.adjust(stat_temp$Pval, method= "fdr")

				# Keep genes based on user-defined pval and fc thresholds
				stat_temp <- stat_temp[
					which(stat_temp$log2FC < -fcTemp | stat_temp$log2FC > fcTemp),
				] 
				stat_temp <- stat_temp[
					which(stat_temp$Pval < datasetVals$pval),
				]

				## Get ENSGid2Symbol map
				## Symbols are fected separately
				## so as to be able to catch ensgid that
				## do not have a matching gene symbol 
				## (especially for RNA-seq datasets.) 
				ENSGid2Symbol <- dbGetQuery(															
					conn=fibromine_db,
					statement='
						SELECT 
							ENSGid, Symbol 
						FROM 
							GeneAnnotation
						WHERE 
							ENSGid = :x
					;',
					params= list(
						x= stat_temp$ENSGid
					)
				)

				## Merge
				stat_temp <- merge(stat_temp, ENSGid2Symbol, by="ENSGid", all.x=TRUE, sort=FALSE)
				stat_temp[which(is.na(stat_temp$Symbol)),"Symbol"] <- "-"
				stat_temp <- stat_temp[, c(ncol(stat_temp), 1: (ncol(stat_temp)-1))]

				## Format table
				stat_temp <- stat_temp[ ,c(1,2,4,5,7,3,6)]
				colnames(stat_temp)[c(1,2,7)] <- c("Name", "Code", "Comparison")

				stats[[i]] <- stat_temp
			}

		}
		stats <- do.call("rbind", stats)
		rownames(stats) <- NULL

		## Keep DEGs common in at least half the selected datasets
		freq <- table(stats$Name)
		common <- names(which(freq >= length(unique(stats$GSE))*.5))
		out <- stats[which(stats$Name %in% common),]
		return(out)
	})

	statSum <- eventReactive(samplesStat(), {

		nDatasets <- length(unique(samplesStat()$GSE))
		statList <- split(samplesStat(), f= samplesStat()$Name)

		## Isolate any genes without Symbol and separate them
		if (any(names(statList) == "-")) {
			
			# Isolate
			noSymbol <- statList[["-"]]
			statList <- statList[which(names(statList) != "-")]

			# Genes without Symbol may be expressed by less than 
			# half of the datasets. Check again for DEGs common 
			# in at least half the selected datasets
			freq <- table(noSymbol$Code)
			common <- names(which(freq >= length(unique(samplesStat()$GSE))*.5))

			# If any gene left
			if (length(common)) {
				noSymbol <- noSymbol[which(noSymbol$Code %in% common),]
				# Separate them by Code
				noSymbol <- split(noSymbol, f= noSymbol$Code)
			} else {
				noSymbol <- common		
			}
		} else {
			noSymbol <- NULL
		}

		## Summarize statistics for genes with UNavailable Symbol
		## that endured the statistics filtering above.
		## If not left assign NULL
		if (length(noSymbol)) {
			statSumTempNo <- lapply(noSymbol, function(x) {
				## Orientation
				orient0 <- sign(x$log2FC)
				orient <- table(orient0)

				if (length(orient) > 1 && orient["-1"] == orient["1"]) {
					out <- NA
				} else {
					orient <- orient[which(orient == max(orient))]
					orient <- as.numeric(names(orient))

					if (orient == 1) {
						orientFinal <- "Upregulated"
					} else if (orient == -1) {
						orientFinal <- "Downregulated"
					} else {
						orientFinal <- "-"	# Useless, but ...
					}

					## Keep proper orientation datasets
					keep <- which(orient0 == orient)
					nDatasetsFinal <- length(keep)

					## Calculate ave log2FC
					log2FcAve <- mean(x[keep, "log2FC"])

					## Get p-value threshold
					pvalThres <- datasetVals$pval
					# pvalThres <- 0.05

					## Out
					out <- data.frame(
						Name= unique(x$Name),
						Code= unique(x$Code),
						n= nDatasetsFinal,
						log2FcAve= log2FcAve,
						pvalThres= pvalThres,
						stringsAsFactors= TRUE
					)
				}
				return(out)	
			})
			if (any(is.na(statSumTempNo))) {
				drop <- which(is.na(statSumTempNo))
				statSumTempNo <- statSumTempNo[-drop]
			} 
			statSumTempNo <- do.call("rbind", statSumTempNo)
		} else {
			statSumTempNo <- NULL
		}
		
		## Summarize statistics for genes with available Symbol
		statSumTemp <- lapply(statList, function(x) {

			## Orientation
			orient0 <- sign(x$log2FC)
			orient <- table(orient0)

			if (length(orient) > 1 && orient["-1"] == orient["1"]) {
				out <- NA
			} else {
				orient <- orient[which(orient == max(orient))]
				orient <- as.numeric(names(orient))

				if (orient == 1) {
					orientFinal <- "Upregulated"
				} else if (orient == -1) {
					orientFinal <- "Downregulated"
				} else {
					orientFinal <- "-"	# Useless, but ...
				}

				## Keep proper orientation datasets
				keep <- which(orient0 == orient)
				nDatasetsFinal <- length(keep)

				## Calculate ave log2FC
				log2FcAve <- mean(x[keep, "log2FC"])

				## Get p-value threshold
				pvalThres <- datasetVals$pval
				# pvalThres <- 0.05

				## Out
				out <- data.frame(
					Name= unique(x$Name),
					Code= unique(x$Code),
					n= nDatasetsFinal,
					log2FcAve= log2FcAve,
					pvalThres= pvalThres,
					stringsAsFactors= TRUE
				)
			}
			return(out)
		})
		if (any(is.na(statSumTemp))) {
			drop <- which(is.na(statSumTemp))
			statSumTemp <- statSumTemp[-drop]
		} 
		statSumTemp <- do.call("rbind", statSumTemp)

		## Bind statSumTempNo (if any) and statSumTemp
		statSumTemp <- rbind(statSumTempNo, statSumTemp)
		rm(statSumTempNo)

		## Query gene annotation
		annot <- dbGetQuery(
			conn= fibromine_db,
			statement='
				SELECT 
					ENSGid, Chromosome, StartPosition,
					EndPosition, Biotype
				FROM
					GeneAnnotation
				WHERE
					ENSGid = :x
			;',
			params= list(x= statSumTemp$Code)
		)

		## Add annotation
		out <- merge(statSumTemp, annot, by.x= "Code", 
			by.y= "ENSGid", all.x=TRUE, sort= FALSE
		)
		out[which(is.na(out$Chromosome)),
			c("Chromosome","StartPosition", "EndPosition", "Biotype")] <- "-"
		out <- out[,c(2,1,6,7,8,9,3,4,5)]

		colnames(out) <- c("Name", "Code",
			"Chromosome", "Start", "Stop", "Biotype",
			paste0("Out of ", nDatasets, " Datasets"),
			"log2FcAve", "pvalThres"
		)
		
		return(out)
	})

	## Get DEP data
	depSum <- eventReactive(statSum(), {
		
		out <- dbGetQuery(
			conn= fibromine_db,
			statement= '
				SELECT
					GeneAnnotation.Symbol, GeneAnnotation.ENSGid, 
					DEP.DatasetID, ExpressionDirection, DEP.Contrast,
					DatasetsDescription.Tissue	 
				FROM 
					GeneAnnotation JOIN ENSGid2UniP JOIN DEP JOIN DatasetsDescription
				ON
					GeneAnnotation.ENSGid = ENSGid2UniP.ENSGid
				AND 
					ENSGid2UniP.UniprotAC = DEP.UniprotAC
				AND 
					DEP.DatasetID = DatasetsDescription.DatasetID 
				WHERE
					GeneAnnotation.ENSGid = :x
			;',
			params= list(x= statSum()$Code)
		)
		out <- out[out$Contrast %in% transSamplesSelected()[,"DescrContrast"], ]
		out <- out[out$Tissue %in% transSamplesSelected()[,"Tissue"], ]
		colnames(out)[1:2] <- c("Name", "Code")
		return(out)
	})	

	## Display help messages=============================================================================

	## Conditional help text on "Concerning dataset(-s)" tab
	output$datasetsConserningHelp <- renderUI({
		if (is.null(input$datasetsTable_rows_selected))
			helpText("Choose one or more datasets from the 'Datasets' tab and then press 'Search'.")
	})

	## Conditional help text on "DEA statistics" tab
	output$deaStatHelp <- renderUI({
		if (is.null(input$datasetsTable_rows_selected)) {
			helpText("Choose one or more datasets from the 'Datasets' tab and then press 'Search'.")
		} else {
			helpText(
				"To easily view the expression values for a given gene across all selected",
				"datasets, filter the table by 'Name' or 'Code' using the textboxes below 
				column names.",
				"Note: for a batch gene search across datasets use the batch search of", 
				"the 'Gene explorer.'"
			)
		}

	})

	## Transcriptomic datasets -> Datasets statistics tour===============================================
	stepsTransStats <- reactive(
	    data.frame(
	        element= c(".step1_transStats", ".step2_transStats", ".step3_transStats"),
	        intro= c(
	            paste("The <b>results</b> of datasets exploration/integration are presented here.<br>",
	            	"<b>Transcriptomics summary</b> tab displays those differentially expressed genes",
	            	"found in <b>all</b> user selected datasets (same expression direction).<br>",
	            	"<b>Proteomics summary</b> tab lists the differentially expressed proteins, if any,",
	            	"which are coded by the genes presented in the first tab.<br>",
	            	"<b>Trancriptomics analytically</b> presents the differentially expressed genes",
	            	"found in <b>at least half</b> the selected datasets (same expression direction",
	            	"is <b>not</b> required)."
	            ),
	            paste("For a gene to be called <b>differentially expressed</b>, the thresholds of",
	            	"<b>0.05 and 1.2 for the p-value and |FC|</b> respectively must be fulfilled.",
	            	"The user can <b>choose</b> his/her own thresholds from the boxes above and then",
	            	"press this button to filter the results."
	            ),
	            paste("Exploit enrichR to perform pathway analysis using as input the consensus differentially",
	            	"expressed genes reported and the most popular Enrich databases."
	            )
	        ),
	        position= rep("auto", 3)
	    )
	)

	## Take the tour
	observeEvent(input$introTransStats,
	introjs(session, 
		options = list(
			steps= stepsTransStats(),
			"nextLabel" = "Next",
			"prevLabel" = "Previous", 
			"doneLabel" = "Let's begin!"
			)
		)
	)

	## Display fetched expression info===================================================================

	## Transcriptomics summary
	output$degStatsSum <- DT::renderDataTable({
		req(input$datasetsTable_rows_selected) ## "Hide" table if no rows are selected 
		out <- statSum()

		## Color up- and down- regulated genes red and green
		brks <- quantile(out$log2FcAve, probs= seq(0, 1, 0.1), na.rm= TRUE)
		clrsPal <- colorRampPalette(c("green","white","red"))
		clrs <- clrsPal(length(brks)+1)

		## Combine start end columns to get more space
		out$Coordinates <- paste(out$Start, out$Stop, sep= ":")

		## Wrap biotypes to get more space
		out$Biotype <- gsub("_", " ", out$Biotype)

		## Convert to factors for easier client-side filtering of the table
		out$Name <- as.factor(out$Name)
		out$Biotype <- as.factor(out$Biotype)

		## Order columns
		out <- out[,c(1,2,3,10,6:9)]

		dtable <- datatable(data= out, 
			selection= "none", 
			rownames= FALSE,
			filter= "top",
			class= "compact",
			options= list(
				rowsGroup = list(0), 
				order = list(list(0, "asc")),
				sDom  = '<"top">lrt<"bottom">ip',
				columnDefs= list(
					list(
						className= "dt-center", 
						targets= "_all"
					)
				)
			)#,
	  		#extensions= "Responsive"
		) %>% formatStyle(1:ncol(out), 
			cursor = 'pointer'
		) %>% formatStyle("log2FcAve",
				backgroundColor= styleInterval(brks, clrs)
		) %>% formatRound(c("log2FcAve"),
			digits= 5
		)

	})

	## Download the whole table
	output$degSumAll <- downloadHandler(
		filename= function() {
			paste0("SummaryStatistics_",
				"pval", input$pvalCommonIn, 
				"_fc", input$fcCommonIn,"_",
				Sys.Date(), ".xlsx"
			)
		},
		content= function(file) {
			out <- statSum()
			out$log2FcAve <- round(out$log2FcAve, 5)
			write.xlsx(out, file)
		}
	)

	## Download filtered table
	output$degSumFiltered <- downloadHandler(
		filename= function() {
			paste0("SummaryStatistics_filtered_",
				"pval", input$pvalCommonIn, 
				"_fc", input$fcCommonIn,"_",
				Sys.Date(), ".xlsx"
			)
		},
		content= function(file) {
			out <- statSum()[input$degStatsSum_rows_all,]
			out$log2FcAve <- round(out$log2FcAve, 5)
			write.xlsx(out, file)
		}
	)

	## Proteomics summary
	output$depSum <- DT::renderDataTable({
		req(input$datasetsTable_rows_selected)

		out <- depSum()
		
		## To factors for easier client-side filtering
		out$Name <- as.factor(out$Name)
		out$DatasetID <- as.factor(out$DatasetID)
		out <- out[, c(1,2,4,3,5,6)]
		
		dtable <- datatable(data= out, 
			selection= "none", 
			rownames= FALSE,
			filter= "top",
			class= "compact",
			options= list(
				rowsGroup = list(0), 
				order = list(list(0, "asc")),
				sDom  = '<"top">lrt<"bottom">ip',
				columnDefs= list(
					list(
						className= "dt-center", 
						targets= "_all"
					)
				)
			),
	  		extensions= "Responsive"
		) %>% formatStyle("ExpressionDirection", "ExpressionDirection",
			backgroundColor= styleEqual(c("Up","Down"), c("#FF8E8E","#7AFF59")))

		path <- "./www"
		depend <- htmltools::htmlDependency("RowsGroup", "2.0.0",
			path, script= "dataTables.rowsGroup.js")
		dtable$dependencies <- c(dtable$dependencies, list(depend))
		return(dtable)
	})

	## Download the whole table
	output$depSumDw <- downloadHandler(
		filename= function() {
			paste0("ProteomicSummaryStatistics_",
				Sys.Date(), ".xlsx"
			)
		},
		content= function(file) {
			out <- depSum()
			out <- out[, c(1,2,4,3,5,6)]
			write.xlsx(out, file)
		}
	)

	
	## Expression statistics analytically
	output$degStats <- DT::renderDataTable({
		req(input$datasetsTable_rows_selected) ## "Hide" table if no rows are selected

		## Display genes with significant P-value only
		deg <- samplesStat()

		## Color log2FC column
		brks <- quantile(deg$log2FC, probs= seq(0, 1, 0.1), na.rm= TRUE)
		clrsPal <- colorRampPalette(c("green","white","red"))
		clrs <- clrsPal(length(brks)+1)
		
		## Color significant FDR red
		deg$fdrColor <- ifelse(deg$FDR < 0.05, 1, 0)

		## Convert to factors for easier client-side filtering of the table
		deg$Name <- as.factor(deg$Name)
		deg$Code <- as.factor(deg$Code)
		deg$GSE <- as.factor(deg$GSE)
		deg$Comparison <- as.factor(deg$Comparison)

		dtable <- datatable(data=deg, 
			selection="multiple", 
			# autoWidth= TRUE,	## To fix column width when filtering
			rownames= FALSE,
			filter= "top",
			class= "compact",
			options= list(
				rowsGroup = list(0), 
				order = list(list(0, "asc")),
				sDom  = '<"top">lrt<"bottom">ip',
				columnDefs= list(
					list(
						targets= 7, 
						visible = FALSE
					),
					list(
						className= "dt-center", 
						targets= "_all"
					)
				)
			),
	  		extensions= "Responsive"
		) %>% formatStyle(1:ncol(deg), 
			cursor = 'pointer'
		) %>% formatStyle("log2FC",
				backgroundColor= styleInterval(brks, clrs)
		) %>% formatStyle("FDR", "fdrColor",
			backgroundColor= styleEqual(1, "#ff8e8e")
		) %>% formatRound(c("log2FC","Pval","FDR"),
			digits= 5
		)

		path <- "./www"
		depend <- htmltools::htmlDependency("RowsGroup", "2.0.0",
			path, script= "dataTables.rowsGroup.js")
		dtable$dependencies <- c(dtable$dependencies, list(depend))
		
		return(dtable)

	})

	## Download the whole table
	output$degStatAll <- downloadHandler(
		filename= function() {
			paste0("DEG_stats_all_",
				"pval", input$pvalCommonIn, 
				"_fc", input$fcCommonIn,"_",
				Sys.Date(), ".xlsx"
			)
		},
		content= function(file) {
			write.xlsx(samplesStat(), file)
		}
	)

	## Download filtered table
	output$degStatFiltered <- downloadHandler(
		filename= function() {
			paste0("DEG_stats_filtered_",
				"pval", input$pvalCommonIn, 
				"_fc", input$fcCommonIn,"_",
				Sys.Date(), ".xlsx"
			)
		},
		content= function(file) {
			write.xlsx(samplesStat()[input$degStats_rows_all,], file)
		}
	)

	## Download the selected rows only
	output$degStatSel <- downloadHandler(
		filename= function() {
			paste0("DEG_stats_selected_",
				"pval", input$pvalCommonIn, 
				"_fc", input$fcCommonIn,"_",
				Sys.Date(), ".xlsx"
			)
		},
		content= function(file) {
			write.xlsx(samplesStat()[input$degStats_rows_selected, ], file)
		}
	)

	## Pathway analysis ===================================================================================

	## Enable the display of the new pathway analysis results
	observeEvent(input$paRun,
		datasetVals$hidePA <- FALSE
	)

	## Perform the analysis
	paRes <- eventReactive({input$paRun
		datasetVals$hidePA
		}, {
		req(statSum())

		## Create a progress object
		progress <- shiny::Progress$new()
		on.exit(progress$close())
		progress$set(message = "Fetching data", value = 0)

		## Remove the table if display is disabled
		## (e.g. during a new datasets integration) 
		if (isTRUE(datasetVals$hidePA))  {
			return(NULL)
		} else {
			## Data
			data <- statSum()

			## Choose enricr site and dbs
			setEnrichrSite("Enrichr")
			ifelse(
				any(grep("^ENSG", data$Code)),
				dbs <- c(
					"GO_Biological_Process_2018", 
					"GO_Cellular_Component_2018", 
					"GO_Molecular_Function_2018",
					"KEGG_2021_Human",
					"COVID-19_Related_Gene_Sets"
				),
				dbs <- c(
					"GO_Biological_Process_2018", 
					"GO_Cellular_Component_2018", 
					"GO_Molecular_Function_2018",
					"KEGG_2019_Mouse"
				)
			)

			progress$inc(0.15)			

			## Check if there are no protein_coding genes
			if (any(grep("^ENS", data$Code))) {

				## Perform the analysis according to the chosen PA method?
				paChoice <- input$paChoice
				paRes <- switch(paChoice,
					"ora" = {
						# Separate up from down regulated genes
						up <- as.character(data[data$log2FcAve > 0, "Name"])
						dw <- as.character(data[data$log2FcAve < 0, "Name"])

						# Remove any entries not having a gene symbol
						up <- up[which(up != "-")]
						dw <- dw[which(dw != "-")]

						# Perform the analysis
						progress$set(message = "Analysis step 1/2", value = 0.20)
						enrUp <- enrichr(genes = up, databases = dbs)
						progress$inc(0.2)			

						progress$set(message = "Analysis step 2/2", value = 0.55)
						enrDw <- enrichr(genes = dw, databases = dbs)
						progress$set(message = "Formating results", value = 0.75)

						# Keep the significantly enriched terms
						enrUp <- lapply(enrUp, function(x){
							out <- x[x$Adjusted.P.value < 0.05,]
						})
						enrDw <- lapply(enrDw, function(x){
							out <- x[x$Adjusted.P.value < 0.05,]
						})

						# As a data.frame
						enrUp <- do.call("rbind", enrUp)
						enrDw <- do.call("rbind", enrDw)

						enrUp$Database <- rownames(enrUp)
						enrDw$Database <- rownames(enrDw)

						enrUp$Database <- gsub("\\..*$", "", enrUp$Database)
						enrDw$Database <- gsub("\\..*$", "", enrDw$Database)

						enrUp$inputList <- "Up DEGs"
						enrDw$inputList <- "Down DEGs"

						out <- as.data.frame(rbind(enrUp, enrDw))

						for (i in 1:length(out$Database)) {
							if(out$Database[i] == "GO_Biological_Process_2018") {			# GO categories
								out$Database[i] <- "BP"
							} else if (out$Database[i] == "GO_Cellular_Component_2018") {
								out$Database[i] <- "CC"
							} else if (out$Database[i] == "GO_Molecular_Function_2018") {
								out$Database[i] <- "MF"
							} else if (out$Database[i] == "KEGG_2021_Human") {				# KEGG categories
								out$Database[i] <- "KEGG"
							} else if (out$Database[i] == "KEGG_2019_Mouse") {	
								out$Database[i] <- "KEGG"
							} else if (out$Database[i] == "COVID-19_Related_Gene_Sets") {	# COVID-19 gene set
								out$Database[i] <- "CoV-19"
							}
						}

						# enrichR capitalizes Mmu gene names as if they were human genes
						# If input gene list is of murine origin, format properly gene names in the output
						if (any(grep("^ENSMUS", data$Code))) {
							out$Genes <- stringr::str_to_title(out$Genes)
						}

						# Return the results
						out
					}#,
					# "preRnk" = {
					# 	# Return the results
					# 	"lalakis"
					# }
				)
				progress$inc(0.25)			
			} else {
				paRes <- data.frame(
					Database = as.character(),
					Term = as.character(),
					Overlap = as.character(),
					"Adjusted P value" = as.character(),
					"Odds Ratio" = as.character(),
					"Combined Score" = as.character()
				)
			}
			return(paRes)
		}
	})

	output$paResTable <- DT::renderDataTable({
		req(paRes())
		req(input$datasetsTable_rows_selected) ## "Hide" table if no rows are selected 

		## Non-protein coding genes only?
		validate(
			need(!nrow(paRes()) == 0,
				"Currently Fibromine cannot perform pathway analysis using only non-protein coding genes."
			)
		)

		out <- paRes()

		# Format the table
		out <- select(out, select = -c("P.value", "Old.P.value", "Old.Adjusted.P.value",
			"Genes"))
		colnames(out) <- gsub("\\.", " ", colnames(out))
		out$Database <- as.factor(out$Database)
		out <- out[,c("Database", "Term", "Overlap", "Adjusted P value", "Odds Ratio",
			"Combined Score", "inputList")]

		datatable(
			data= out, 
			selection= "none", 
			filter = "top",
			rownames= FALSE,
			escape= FALSE,
			class= "compact", 
			options= list(
				sDom= '<"top">lrt<"bottom">ip',
				columnDefs= list(
					list(
						targets = 6, 
						visible = FALSE
					),
					list(
						className = "dt-center",
						targets = "_all"
					)
				),
				lengthChange = FALSE
			)		
		) %>% formatStyle("Term", "inputList",
			backgroundColor= styleEqual("Up DEGs", "#ff8e8e")
		) %>% formatStyle("Term", "inputList",
			backgroundColor= styleEqual("Down DEGs", "#8eff8e")
		) %>% formatRound(c("Adjusted P value", "Odds Ratio", "Combined Score"),
			digits = 3
		)
	})

	## Download handler
	output$paResDown <- downloadHandler(
		filename= function() {
			paste0(input$paChoice, "_", Sys.Date(), ".xlsx")
		},
		content= function(file) {
			write.xlsx(paRes(), file)
		}
	)

	## "Concerning datasets" tour==========================================================================
	stepsTransConc <- reactive(
	    data.frame(
	        element= ".step1_transConc",
	        intro= c(
	            paste("This tab provides extra info about the datasets selected via the <b>'Datasets'</b> tab,",
	            	"in the form of a table.",
	            	"In addition, a <b>heatmap</b> and <b>volcano plot</b> for each one of the queried",
	            	"datasets <b>can be plotted in 2 steps</b> <b>after the process of the 'Datasets' tab has been</b>",
	            	"<b>completed</b>:<br>",
	            	"(1) <b>choose</b> any dataset of the table which will appear below<br>",
	            	"(2) press the <b>'Plot'</b> button in the respective box."
	            )
	        ),
	        position= "below-middle-aligned"
	    )
	)

	## Take the tour
	observeEvent(input$introTransConc,
	introjs(session, 
		options = list(
			steps= stepsTransConc(),
			"nextLabel" = "Next",
			"prevLabel" = "Previous", 
			"doneLabel" = "Let's begin!"
			)
		)
	)

	## Display "Concerning datasets" info================================================================

	datasetsConserning <- eventReactive(transSamplesSelected(), {
		query <- dbGetQuery(conn= fibromine_db,
			statement= '
				SELECT 
					Datasets.DatasetID, Species,
					DatasetsDescription.DescrContrast, Tech 
				FROM 
					Datasets JOIN DatasetsDescription
				ON 
					Datasets.DatasetID = DatasetsDescription.DatasetID
				WHERE 
					Datasets.DatasetID = :x AND 
					DatasetsDescription.DescrContrast = :y 
			;',
			params= list(
				x= transSamplesSelected()$DatasetID,
				y= transSamplesSelected()$DescrContrast
			)
		)
		colnames(query)[c(3,4)] <- c("Comparison", "Technology") 
		return(query)
	})

	output$datasetsConserning <- DT::renderDataTable({
		req(datasetsConserning())
		req(input$datasetsTable_rows_selected)

		out <- datasetsConserning()
		out$DatasetID <- as.factor(out$DatasetID)

		datatable( 
			data= out, 
			selection= "single", 
			rownames= FALSE,
			filter= "top",
			class= "compact",
			options= list(
				sDom= '<"top">lrt<"bottom">ip',
				columnDefs= list(
					list(
						className= "dt-center",
						targets= "_all"
					)
				)
			),
			extensions= "Responsive"		
		)%>% formatStyle(1:ncol(datasetsConserning()), cursor = 'pointer')
	})

	## Return heatmap plot upon request==================================================================
	output$heatmapQCHelp <- renderUI({
		if (is.null(input$datasetsConserning_rows_selected))
			helpText("Choose a dataset from the table above and then press 'Plot heatmap'")
	})

	## Use a 2nd level observeEvent to remove plot once the tab is changed
	observeEvent(input$plotHeatmap, {
		datasetVals$plotHeat <- input$plotHeatmap 
	})

	observeEvent(input$datasetResultsTab, {
		datasetVals$plotHeat <- FALSE
	})

	## Retrieve data
	heatmapQCIn <- eventReactive(datasetVals$plotHeat, {
		
		## Remove plot if tab is changed
		if (isFALSE(datasetVals$plotHeat)) return()

		req(input$datasetsConserning_rows_selected)

		progress <- shiny::Progress$new()
		on.exit(progress$close())
		progress$set(message= "Retrieving data", value=0)
		progress$inc(0.25)

		normExprValues <- dbGetQuery(
			conn=fibromine_db,
			statement= '
				SELECT 
					Symbol, NormValues, ExpCondition, GSMcode 
				FROM 
					NormExprValues 
				WHERE 
					GSE = :x
			;',
			params= list(x= datasetsConserning()[input$datasetsConserning_rows_selected, 1])
		)
		progress$inc(0.25)

		## Reconstruct expression table
		expr_temp <- split(normExprValues, f= normExprValues$GSMcode)
		expr_temp <- lapply(expr_temp, function(x) {
			out <- data.frame(x$NormValues, stringsAsFactors=FALSE)
		})
		expr <- data.frame(matrix(unlist(expr_temp), ncol=length(expr_temp),
			byrow=FALSE), stringsAsFactors= FALSE)
		expr$Name <- normExprValues$Symbol[1:nrow(expr)]
		colnames(expr) <- c(names(expr_temp),"Name")

		## Find the most variable 1000 genes (FDR) and then keep the DE ones
		stat_temp <- subset(samplesStat(), 
			GSE == datasetsConserning()[input$datasetsConserning_rows_selected, "DatasetID"])
		
		## Are there any genes with FDR < 0.05 (e.g. GSE72073)?
		if (!any(stat_temp$FDR < 0.05)) {
			topGenes <- stat_temp[order(stat_temp$Pval, decreasing= FALSE), ]
			topGenes <- topGenes[1:1000,]
			topGenes <- as.character(subset(topGenes, Pval < 0.05, Name)[,1])
			expr <- expr[which(expr$Name %in% topGenes), ]
			adjpval <- FALSE  
		} else {
			topGenes <- stat_temp[order(stat_temp$FDR, decreasing= FALSE), ]
			topGenes <- topGenes[1:1000,]
			topGenes <- as.character(subset(topGenes, FDR < 0.05, Name)[,1])
			expr <- expr[which(expr$Name %in% topGenes), ]
			adjpval <- TRUE
		}			
		progress$inc(0.25)

		## Retrieve experimental conditions
		tbl <- table(normExprValues$GSMcode,normExprValues$ExpCondition)
		cond <- vector("character", length= nrow(tbl))
		names(cond) <- rownames(tbl) 
		for(i in names(cond)) {
			temp <- tbl[i,]
			cond[i] <- names(temp[temp!=0])
		}

		progress$inc(0.25)
		out <- list(expr= expr,cond= cond, adjpval= adjpval)
		return(out)

	})

	## Plot
	output$heatmapQC <- plotly::renderPlotly({
		req(heatmapQCIn())
		plotHeatmapQC(heatmapQCIn()$expr, colorPallete, heatmapQCIn()$cond)
	})

	## Give a warning if raw p-values are used
	output$heatmapQCstatWarning <- renderUI({
		req(input$datasetsConserning_rows_selected)
		req(heatmapQCIn())

		if (isTRUE(heatmapQCIn()$adjpval)) {
			return(NULL)
		} else {
			helpText("Warning!!! Raw p-values are used for the clustering
				as the were no genes with FDR < 0.05")
		}

	})

	## Return volcano plot (upon request)===============================================================
	## using the top 1000 most variable genes
	output$volcanoQCHelp <- renderUI({
		if (is.null(input$datasetsConserning_rows_selected))
			helpText("Choose a dataset from the table above 
				and then press 'Plot volcano'")
	})

	## Use a 2nd level observeEvent to remove plot once the tab is changed
	observeEvent(input$plotVolcano, {
		datasetVals$plotVolc <- input$plotVolcano 
	})

	observeEvent(input$datasetResultsTab, {
		datasetVals$plotVolc <- FALSE
	})

	## Retrieve data
	volcanoQCIn <- eventReactive(datasetVals$plotVolc, {
		
		## Remove plot if tab is changed
		if (isFALSE(datasetVals$plotVolc)) return()

		progress <- shiny::Progress$new()
		on.exit(progress$close())
		progress$set(message= "Retrieving data", value=0)
		progress$inc(0.5)

		species <- datasetsConserning()[input$datasetsConserning_rows_selected, 
			"Species"]
		tech <- datasetsConserning()[input$datasetsConserning_rows_selected, 
			"Technology"]

		if (tech == "Non-coding RNA profiling by array") {

			stat <- dbGetQuery(
				conn= fibromine_db,
				statement= '
					SELECT 
							StatComparisonsNonCoding.prodAC, GSE, log2FC,
							Pval, miRBase.prodID 
						FROM 
							StatComparisonsNonCoding JOIN miRBase
						ON 
							StatComparisonsNonCoding.prodAC = miRBase.prodAC
						WHERE 
							GSE = :x AND StatContrastNonCoding = :y
					;',
				params= list(
					x= datasetsConserning()[input$datasetsConserning_rows_selected,
						"DatasetID"], 
					y= datasetsConserning()[input$datasetsConserning_rows_selected,
						"Comparison"]
				)
			)
			stat[which(is.na(stat$prodID)),"prodID"] <- "-"
			stat <- stat[, c(ncol(stat), 1: (ncol(stat)-1))]

			## Format table
			stat <- stat[ ,c(1,2,4,5,3)]
			colnames(stat)[c(1,2)] <- c("Name", "Code")

		} else {

			stat <- dbGetQuery(																
				conn=fibromine_db,
				statement= '
					SELECT 
						StatComparisonsCoding.ENSGid, GSE, log2FC, Pval,
						GeneAnnotation.Symbol
					FROM 
						StatComparisonsCoding JOIN GeneAnnotation
					ON
						StatComparisonsCoding.ENSGid = GeneAnnotation.ENSGid 
					WHERE 
						GSE = :x AND StatContrast = :y
				;',
				params= list(
					x= datasetsConserning()[input$datasetsConserning_rows_selected,
						"DatasetID"], 
					y= datasetsConserning()[input$datasetsConserning_rows_selected,
						"Comparison"]
				)
			)
			stat[which(is.na(stat$Symbol)),"Symbol"] <- "-"
			stat <- stat[, c(ncol(stat), 1: (ncol(stat)-1))]

			## Format table
			stat <- stat[ ,c(1,2,4,5,3)]
			colnames(stat)[c(1,2)] <- c("Name", "Code")
		}
		
		out <- list(stat= stat, species= species, tech= tech)
		progress$inc(0.5)
		return(out)
	})

	## Plot
	output$volcanoQC <- plotly::renderPlotly({
		req(volcanoQCIn())
		plotVolcanoQC(
			volcanoQCIn()$stat, 
			volcanoQCIn()$species, 
			volcanoQCIn()$tech
		)
	})

	# ============================================
	# "Proteomic datasets" tours
	# ============================================

	## Summary page tour
	stepsProtDat <- reactive(
	    data.frame(
	        element= c(".step1_protDat", ".step2_protDat"),
	        intro= c(
	            paste("All Fibromine <b>proteomic datasets</b> can be viewed",
	            	"in the following table. To explore any of them, click on", 
	            	"the respective <b>table row(-s)</b> and then press the <b>'Search'</b>", 
	            	"button bellow.<br>",
	            	"Dataset(-s) exploration results are presented in the <b>second tab</b>.",
	            	"Selection of a <b>single</b> dataset will lead to its simple <b>exploration</b>,",
	            	"while selection of <b>multiple</b> datasets to the integration of their data."
	            ),
	            paste("When a <b>new search</b> is required, it is <b>advised</b>",
	            	"to <b>un-check</b> the previously selected datasets using",
	            	"<b>this button. Manually</b> de-selecting each dataset is", 
	            	"possible, but according to our experience is also an",
	            	"<b>error prone</b> process."
	            )
	        ),
	        position= c("top", "bottom-left-aligned")
	    )
	)

	observeEvent(input$introProtDat,
	introjs(session, 
		options = list(
			steps= stepsProtDat(),
			"nextLabel" = "Next",
			"prevLabel" = "Previous", 
			"doneLabel" = "Let's begin!"
			)
		)
	)
	
	## Analytical statistics tour
	stepsProtStats <- reactive(
	    data.frame(
	        element= ".step1_protStats",
	        intro= paste("The <b>results</b> of datasets exploration/integration are presented here.<br>",
	        	"<b>Proteomics summary</b> tab displays those differentially expressed proteins",
	        	"found in <b>all</b> user selected datasets (same expression direction).<br>",
	        	"<b>Proteomics analytically</b> presents the differentially expressed proteins",
	        	"found in <b>at least half</b> the selected datasets (same expression direction",
	        	"is <b>not</b> required)."
	        ),
	        position= "auto"
	    )
	)

	observeEvent(input$introProtStats,
	introjs(session, 
		options = list(
			steps= stepsProtStats(),
			"nextLabel" = "Next",
			"prevLabel" = "Previous", 
			"doneLabel" = "Let's begin!"
			)
		)
	)

	# ====================================================================================================
	# "Proteomic Datasets" tab 
	# ====================================================================================================

	## Retrieve proteomic datasets data==============================================================

	## Retrieve selection
	protSamplesSelected <- eventReactive(input$protDtstsSearch, {
		datasetVals$protTable[input$proteomicsTable_rows_selected, 
			c("DatasetID", "DescrContrast", "Tech", "Tissue")]
	})

	## Automatically redirect to the results tab
	observeEvent(input$protDtstsSearch, {
		updateTabsetPanel(session, "protDatasetsBox",
				selected= "DEA data")
	})

	## Choose all rows
	observeEvent(input$selectProtAll, {
		proteomicsTableProxy %>% selectRows(input$proteomicsTable_rows_all)
	})

	## Clear results
	observeEvent(input$clearProtDtstsSearch, {
		modal <- modalDialog(												
			title= "Attention!",
			paste0(	
				"Datasets choosen will be dropped along with the respective data. ",
				"Analysis parameters will be also reseted."
			),
			footer= tagList(
				actionButton("okProtDatasets","OK"),
				modalButton("Cancel")
			)
		)
		showModal(modal)
	})

	observeEvent(input$okProtDatasets, {
		removeModal()
		## Deselect table rows
		proteomicsTableProxy %>% selectRows(NULL)
	})

	## Retrieve DE data
	protSamplesStat <- eventReactive(protSamplesSelected(), {

		protStats <- vector("list", length= nrow(protSamplesSelected()))
		names(protStats) <- protSamplesSelected()$DatasetID 

		for (i in 1:length(protStats)) {

			## Get DEA results
			protStat_temp <- dbGetQuery(
				conn=fibromine_db,
				statement= '
					SELECT 
						*
					FROM 
						DEP 
					WHERE 
						DEP.DatasetID = :x AND DEP.Contrast = :y
				;',
				params= list(
					x= protSamplesSelected()[i,"DatasetID"], 
					y= protSamplesSelected()[i,"DescrContrast"]
				)
			)

			# For some reason there are some datasets with duplicate
			# proteins (UniprotAC). I must look it out. Until then...
			protStat_temp <- protStat_temp[!duplicated(protStat_temp$UniprotAC), ]

			## Query and process protein annotation
			protAnnot <- dbGetQuery(
				conn= fibromine_db,
				statement= '
					SELECT 
						Uniprot.UniprotAC, Length, Names,
						GeneAnnotation.Symbol, GeneAnnotation.ENSGid
					FROM 
						Uniprot JOIN ENSGid2UniP JOIN GeneAnnotation
						ON
						Uniprot.UniprotAC = ENSGid2UniP.UniprotAC 
							AND 
						ENSGid2UniP.ENSGid = GeneAnnotation.ENSGid
					WHERE
						Uniprot.UniprotAC = :x
				;',
				params= list(
					x= protStat_temp$UniprotAC
				)
			)
			protAnnot <- split(protAnnot, f= protAnnot$"UniprotAC")

			protAnnot <- lapply(protAnnot, function(x){
				# Keep and rectify the primary protein name given by Uniprot
				temp <- strsplit(x$Names, split= "|||",
					fixed= TRUE)
				x$Names <- temp[[1]][1]
				x$Names <- gsub("\\{.*\\}", "", x$Names)
				
				# In the case of a one:many Uniprot:Symbol relationship
				if (nrow(x) > 1) {
					x <- data.frame(
						UniprotAC= unique(x$UniprotAC),
						Length= unique(x$Length),
						Names= unique(x$Names),
						Symbol= paste(x$Symbol, collapse= "|||"),
						ENSGid= paste(x$ENSGid, collapse= "|||")
					)
				}
				return(x)
			})
			protAnnot <- do.call("rbind", protAnnot)

			## Add annotation
			out <- merge(protStat_temp, protAnnot,
				by= "UniprotAC"
			)
			out <- out[,c("Names", "UniprotAC", "ExpressionDirection",
				"DatasetID", "Contrast", "Length", "Symbol", "ENSGid")] 
			colnames(out)[c(1,5,7:8)] <- c("Protein Name", "Comparison", 
				"Gene symbol", "Gene code")

			protStats[[i]] <- out
		}
		protStats <- do.call("rbind", protStats)
		rownames(protStats) <- NULL

		## Keep DEPs common in at least half the selected datasets
		protFreq <- table(protStats$"Protein Name")
		protCommon <- names(which(protFreq >= length(unique(protStats$DatasetID))*.5))
		out <- protStats[which(protStats$"Protein Name" %in% protCommon),]
		return(out)
	})

	protStatSum <- eventReactive(protSamplesStat(), {

		nProtDatasets <- length(unique(protSamplesStat()$DatasetID))
		protStatList <- split(protSamplesStat(), f= protSamplesStat()$UniprotAC)

		## Summarize statistics
		protStatSumTemp <- lapply(protStatList, function(x) {

			## Orientation
			orient <- table(x$ExpressionDirection)

			if (length(orient) > 1 && orient["Down"] == orient["Up"]) {
				out <- NA
			} else {
				orient <- orient[which(orient == max(orient))]
				orient <- names(orient)

				if (orient == "Up") {
					orientFinal <- "Upregulated"
				} else if (orient == "Down") {
					orientFinal <- "Downregulated"
				} else {
					orientFinal <- "-"	# Useless, but ...
				}

				## Keep proper orientation datasets
				keep <- which(x$ExpressionDirection == orient)
				nDatasetsFinal <- length(keep)

				## Out
				out <- data.frame(
					"Protein Name"= unique(x$"Protein Name"),
					UniprotAC= unique(x$UniprotAC),
					n= nDatasetsFinal,
					ExpressionDirection= orient,
					Length= unique(x$Length),
					"Gene symbol"= unique(x$"Gene symbol"),
					"Gene code"= unique(x$"Gene code"),
					stringsAsFactors= TRUE 
				)
			}
			return(out)
		})
		protStatSumTemp[is.na(protStatSumTemp)] <- NULL
		protStatSumTemp <- do.call("rbind", protStatSumTemp)
		rownames(protStatSumTemp) <- NULL
		colnames(protStatSumTemp)[3] <- paste0("Out of ", nProtDatasets,
			" Datasets")
		return(protStatSumTemp)
	})

	## Display fetched expression info===================================================================

	## Proteomics summary
	output$depStatsSum <- DT::renderDataTable({
		req(input$proteomicsTable_rows_selected) ## "Hide" table if no rows are selected 
		out <- protStatSum()

		## Convert to factors for easier client-side filtering of the table
		out$"Protein.Name" <- as.factor(out$"Protein.Name")
		out$UniprotAC <- as.factor(out$UniprotAC)
		out$ExpressionDirection <- as.factor(out$ExpressionDirection)
		out$Gene.symbol <- as.factor(out$Gene.symbol)
		out$Gene.code <- as.factor(out$Gene.code)

		colnames(out)[c(1,6:7)] <- c("Protein name", "Gene symbol", "Gene code")

		dtable <- datatable(data= out, 
			selection="multiple", 
			rownames= FALSE,
			filter= "top",
			class= "compact",
			options= list(
				rowsGroup = list(0), 
				order = list(list(0, "asc")),
				sDom  = '<"top">lrt<"bottom">ip',
				columnDefs= list(
					list(
						className= "dt-center", 
						targets= "_all"
					)
				)
			),
	  		extensions= "Responsive"
		) %>% formatStyle(1:ncol(out), 
			cursor = 'pointer'
		) %>% formatStyle("ExpressionDirection", "ExpressionDirection",
			backgroundColor= styleEqual(c("Up","Down"), c("#FF8E8E","#7AFF59")))
	})

	## Download the whole table
	output$depStatsSumAll <- downloadHandler(
		filename= function() {
			paste0("DEP_statsSum_all_", Sys.Date(), ".xlsx")
		},
		content= function(file) {
			write.xlsx(protStatSum(), file)
		}
	)

	## Download filtered table
	output$depStatsSumFiltered <- downloadHandler(
		filename= function() {
			paste0("DEP_statsSum_filtered_", Sys.Date(), ".xlsx")
		},
		content= function(file) {
			write.xlsx(protStatSum()[input$depStatsSum_rows_all,], file)
		}
	)


	## DEA data analytically
	output$depStats <- DT::renderDataTable({
		req(input$proteomicsTable_rows_selected) ## "Hide" table if no rows are selected

		## Display genes with significant P-value only
		dep <- protSamplesStat()

		## Convert to factors for easier client-side filtering of the table
		dep$"Protein Name" <- as.factor(dep$"Protein Name")
		dep$ExpressionDirection <- as.factor(dep$ExpressionDirection)
		dep$UniprotAC <- as.factor(dep$UniprotAC)
		dep$DatasetID <- as.factor(dep$DatasetID)
		dep$Comparison <- as.factor(dep$Comparison)
		dep$"Gene symbol" <- as.factor(dep$"Gene symbol")
		dep$"Gene code" <- as.factor(dep$"Gene code")

		dtable <- datatable(data=dep, 
			selection="multiple", 
			rownames= FALSE,
			filter= "top",
			class= "compact",
			options= list(
				rowsGroup = list(0), 
				order = list(list(0, "asc")),
				sDom  = '<"top">lrt<"bottom">ip',
				columnDefs= list(
					list(
						className= "dt-center", 
						targets= "_all"
					)
				)
			),
	  		extensions= "Responsive"
		) %>% formatStyle(1:ncol(dep), 
			cursor = 'pointer'
		) %>% formatStyle("ExpressionDirection", "ExpressionDirection",
			backgroundColor= styleEqual(c("Up","Down"), c("#FF8E8E","#7AFF59")))

		path <- "./www"
		depend <- htmltools::htmlDependency("RowsGroup", "2.0.0",
			path, script= "dataTables.rowsGroup.js")
		dtable$dependencies <- c(dtable$dependencies, list(depend))
		
		return(dtable)
	})
	
	## Download the whole table
	output$depStatAll <- downloadHandler(
		filename= function() {
			paste0("DEP_stats_all_", Sys.Date(), ".xlsx")
		},
		content= function(file) {
			write.xlsx(protSamplesStat(), file)
		}
	)

	## Download filtered table
	output$depStatFiltered <- downloadHandler(
		filename= function() {
			paste0("DEP_stats_filtered_", Sys.Date(), ".xlsx")
		},
		content= function(file) {
			write.xlsx(protSamplesStat()[input$depStats_rows_all,], file)
		}
	)

	## Download the selected rows only
	output$depStatSel <- downloadHandler(
		filename= function() {
			paste0("DEP_stats_selected_", Sys.Date(), ".xlsx")
		},
		content= function(file) {
			write.xlsx(protSamplesStat()[input$depStats_rows_selected, ], file)
		}
	)

	# ============================================================================
	# "Gene explorer" tab Items
	# ============================================================================
	geneVals <- reactiveValues(geneData= NULL, geneNames= NULL)
	data_geneNameSearch <- eventReactive(eventExpr=input$geneNameSearch, 
		valueExpr=c(input$geneName, input$speciesChoice))
	data_example <- eventReactive(eventExpr=input$geneNameSearchExample, valueExpr= "MAP3K8")

	## Gene explorer's tour================================================================== 
	stepsGene <- reactive(
        data.frame(
            element= c(".step_2", "#geneName", "#speciesChoice", "#geneInfo",
            	".step1_geneExplorer"),
            intro= c(
                paste("<b>Gene explorer</b> enables the user to assess the expression",
                	"pattern of specific gene(-s) of interest via single or batch search.",
                	"Wealthy gene metadata, as well as the report of any differentially",
                	"expressed proteins coded by the searched genes are also available."
                ),
                paste("To <b>search</b> for one or multiple genes, <b>first</b>, type the gene's symbol or ",
                	"Ensembl accession number in the box. For detailed instructions <b>hover</b> ",
                	"the cursor over the search box."
                ),
                paste("Then choose in which <b>species</b> you want to search and press the ", 
                	"<b>'Search'</b> button above."
                ),
                paste("<b>General</b> information about the gene(-s) queried will be presented here."
                ),
                paste("<b>Gene differential expression statistics</b> will be presented it the first tab. ",
                	"Differential expression of any <b>protein</b> coded by the searched gene ",
                	"will be also presented below, if any.<br>",
                	"Any <b>Gene Ontology</b> categories in which the queried gene(-s) belong will be ",
                	"presented in the homonym tab.<br>",
                	"Lastly, <b>Refseq</b> data and potential interactors non-coding/coding interactors ",
                	"as included into <b>mirDB</b> will be presented at the third tab."
                )
            ),
            position= c(rep("right", 4), "top")
        )
    )

	## Take the tour
	observeEvent(input$introGene,
	introjs(session, 
		options = list(
			steps= stepsGene(),
			"nextLabel" = "Next",
			"prevLabel" = "Previous", 
			"doneLabel" = "Let's begin!"
			)
		)
	)

	## Example case============================================================================ 
	observeEvent(data_example(), {

		## Update input textbox to display the example gene
		updateTextInput(session, inputId= "geneName", label= "Type a number of genes", 
			value= "MAP3K8")

		## Update radio buttons to display "Human" selected
		updateRadioButtons(session, inputId= "speciesChoice", selected= "Human")

		## Create a process object
		progress <- shiny::Progress$new()					## Create a progress object
		on.exit(progress$close())
		progress$set(message= "Fetching data", value=0)
		progress$inc(0.25)				

		batch <- "MAP3K8"
		geneNameList <- vector("list", length(batch))
		names(geneNameList) <- batch

		## Transform HGNC symbol given to ensembl gene ID
		for (i in names(geneNameList)) {
			geneNameList[[i]] <- dbGetQuery(
				conn= fibromine_db,
				statement= '
					SELECT 
						ENSGid, Symbol
					FROM 
						GeneAnnotation 
					WHERE 
						Symbol = :x
				;',
				params= list(x= i)
			)$ENSGid
		}
		progress$inc(0.25)	

		## Retrieve gene related data
		suppressWarnings(
		geneVals$geneData <- lapply(geneNameList, function(gene) {			
			## Retrieve annotation----------------------------------------------------------------
			annot_temp <- dbGetQuery(
				conn= fibromine_db,
				statement= '
					SELECT 
						*
					FROM 
						GeneAnnotation 
					WHERE 
						ENSGid = :x
				;',
				params= list(x= gene)
			)

			## annot for display
			annot <- subset(annot_temp, select= c(Symbol, Aliases, ENSGid, Chromosome, 
				StartPosition, EndPosition, Biotype, IsTF))
			annot$Aliases <- gsub("|||", ", ", annot$Aliases, fixed= TRUE)
			annot$IsTF <- ifelse(annot$IsTF == 0, "No", "Yes")
			colnames(annot)[1:3] <- c("Name", "Aliases", "Code")

			## Retrieve statistics and compute FDR-------------------------------------------------
			stat <- dbGetQuery(
				conn=fibromine_db,
				statement='
					SELECT 
						ENSGid, log2FC, Pval, StatContrast, GSE 
					FROM 
						StatComparisonsCoding
				;'
			)
			stat <- split(stat, f=stat$GSE)

			## Compute FDR and keep the gene of interest
			stat <- lapply(stat, function(x) {
				x$FDR <- p.adjust(x$Pval, method= "fdr")
				out <- x[x$ENSGid== gene, ]
				return(out)
			})
			stat <- do.call("rbind", stat)
			rownames(stat) <- NULL

			## Retrieve dataset-related info to add to statistics table
			samples <- dbGetQuery(	
				conn= fibromine_db,
				statement= '
					SELECT 
						DatasetsDescription.DatasetID, nExpCtrl, nExpCtrlPostCuration, 
						Species, Tech, Tissue, DescrContrast
					FROM 
						DatasetsDescription JOIN Datasets 
					ON 
						DatasetsDescription.DatasetID = Datasets.DatasetID
					WHERE 
						DatasetsDescription.DatasetID = :x AND DescrContrast = :y
				;',
				params= list(
					x= stat$GSE, 
					y= stat$StatContrast
				)
			)

			samples <- merge(samples, stars, 
	           by= c("DatasetID", "DescrContrast"), 
	           all.x= TRUE
			)

			## Format table
			stat$Species <- samples$Species
			stat$Tissue <- samples$Tissue
			stat$"#Exp/\n#Ctrl" <- samples$nExpCtrlPostCuration
			stat$Tech <- samples$Tech
			stat$"Stars count" <- samples$Stars.count
			stat$Name <- annot_temp$Symbol

			## Abbreviate to gain some space
			stat$Species <- gsub("Homo sapiens", "Hsa", stat$Species)
			stat$Species <- gsub("Mus musculus", "Mmu", stat$Species)

			## Wrap comparisons to gain more space
			stat$StatContrast <- gsub("_vs_", "\nvs\n", stat$StatContrast)

			stat <- stat[, c("Name", "Species", "Tissue",
		       "GSE", "StatContrast", "Stars count", "#Exp/\n#Ctrl",
		       "Tech", "log2FC", "Pval", "FDR")]
			colnames(stat)[c(4:5)] <- c("DatasetID","Comparison")

			## Shape RefSeq annotation--------------------------------------------------------------
			refseq <- annot_temp[,c("RefSeqmRNA","RefSeqncRNA","RefSeqPeptide")]
			refseq <- apply(refseq, 2, function(x){
				strsplit(x, split="|||", fixed=TRUE)[[1]]}
			)
			if (!is.matrix(refseq) & !is.data.frame(refseq)) {
				refseq <- as.data.frame(t(refseq), stringsAsFactors=FALSE)
			} else {
				refseq <- as.data.frame(refseq, stringsAsFactors=FALSE)
			}
			colnames(refseq) <- c("mRNA","ncRNA","Peptide")

			## miRNA - targets info-----------------------------------------------------------------
			miRDB <- dbGetQuery(
				conn=fibromine_db,
				statement= '
					SELECT 
						* 
					FROM 
						miRDB 
					WHERE 
						RefSeqmRNA = :x
				;',
				params= list(x= refseq$mRNA)
			)
			miRDB <- subset(miRDB, select= -miRDBid)

			## Error:
			## 	There may be genes whose refseq sequences are NOT INCLUDED
			## 	into miRDB. This results into an empty miRDB data.frame which
			## 	if not treated properly returns an error, which in turn
			## 	disconnects the session from the server.
			## 	Such a gene is e.g. MMP23 when organism is mouse
			## Solution: 
			## 	Check data.frame
			if (nrow(miRDB)) {
				miRDB$targetGene <- annot_temp$Symbol

				## Render table
				miRDB <- miRDB[, c(4,1,2,3)]
				colnames(miRDB) <- c("Gene_name", "Target", "miRNA", "Score") 
			} else {
				miRDB <- data.frame(
					Gene_name= "-",
					Target= "-",
					miRNA= "-",
					Score= "-"
				)
			}
				
			## Retrieve GO annotation---------------------------------------------------------------
			go <- dbGetQuery(
				conn= fibromine_db,
				statement= '
					SELECT 
						GO.GOid, GOname, GOnamespace 
					FROM 
						GO JOIN ENSGid2GOid 
					ON 
						GO.GOid = ENSGid2GOid.GOid 
					WHERE 
						ENSGid2GOid.ENSGid = :x
				;',
				params= list(x= annot_temp$ENSGid)
			)

			## DEP data-------------------------------------------------------------------------
			dep <- dbGetQuery(
				conn= fibromine_db,
				statement= '
					SELECT 
						DEP.*, DatasetsDescription.Tissue
					FROM 
						ENSGid2UniP JOIN DEP JOIN DatasetsDescription
					ON
						ENSGid2UniP.UniprotAC = DEP.UniprotAC
					AND
						DEP.DatasetID = DatasetsDescription.DatasetID
					WHERE
						ENSGid2UniP.ENSGid = :x
				;',
				params= list(x= annot_temp$ENSGid)
			)

			## Shape output-------------------------------------------------------------------------
			out <- list(annot, stat, dep, refseq, go, miRDB)
			names(out) <- c("Annotation", "Statistics", "DEP",
				"RefSeq", "GO", "miRDB")
			return(out)
		}))
		progress$inc(0.25)

		## Store gene names to use for the names of downloadable .xlsx files
		annot <- lapply(geneVals$geneData, function(gene) {
			return(gene$Annotation)
		})
		annot <- do.call("rbind", annot)
		geneVals$geneNames <- paste(unique(annot$Name))
		progress$inc(0.25)
	})

	## Fetch gene-related info============================================================================ 
	observeEvent(data_geneNameSearch(), {
		
		progress <- shiny::Progress$new()					## Create a progress object
		on.exit(progress$close())
		progress$set(message= "Fetching data", value=0)
		progress$inc(0.25)						

		## Format input
		batch <- input$geneName
		batch <- gsub("[^A-Za-z0-9 -]"," ",batch)
		batch <- strsplit(batch, split="\\s")[[1]]
		batch <- batch[batch!=""]

		geneNameList <- vector("list", length(batch))
		names(geneNameList) <- batch

		## Transform names given. If ensembl OK. If HGNC symcol transform to ensembl gene ID
		for (i in names(geneNameList)) {
			if (length(grep("^ENSG|^ENSMUSG", i))) {
				geneNameList[[i]] <- i
			} else if (length(grep("^MIM", i))) {
				geneNameList[[i]] <- i
			} else if (length(grep("^hsa|mmu|ebv|hcmv|hiv1|hsv1|kshv", i))) {
				geneNameList[[i]] <- dbGetQuery(
					conn= fibromine_db,
					statement= '
						SELECT 
							* 
						FROM 
							miRBase 
						WHERE 
							prodID = :x
					;',
					params= list(x= i)
				)$prodAC
			} else {
				geneNameList[[i]] <- dbGetQuery(
					conn= fibromine_db,
					statement= '
						SELECT 
							ENSGid, Symbol 
						FROM 
							GeneAnnotation 
						WHERE 
							Symbol = :x
						COLLATE 
							NOCASE
					;',
					params= list(x= i)
				)

				# Keep the user selected species (by default both)
				if (nrow(geneNameList[[i]])) {
					speciesChoice <- input$speciesChoice
					selectedSps <- switch(speciesChoice,
						"Mouse"= "ENSMUS",
						"Human"= "ENSG",
						"Both"= "both"
					)

					# Were both species selected?
					if(selectedSps == "both"){
						geneNameList[[i]] <- geneNameList[[i]]
					} else{
						geneNameList[[i]] <- geneNameList[[i]][
							grepl(selectedSps, geneNameList[[i]]$ENSGid), ]
					}
					out <- as.list(geneNameList[[i]]$ENSGid)
					names(out) <- geneNameList[[i]]$Symbol

					geneNameList[[i]] <- NULL
					geneNameList <- c(geneNameList, out)

				} else {
					geneNameList[[i]] <- geneNameList[[i]]$ENSGid 
				}
			}
		}

		## Check names validity
		geneNameList <- lapply(geneNameList, function(x) {
			if (length(x)) {
				return(unique(x))
			} else {
				return(NA)
			}
		})

		## Remove invalid gene names
		geneNameList <- geneNameList[!is.na(geneNameList)]
		progress$inc(0.25)						

		## Retrieve gene related data
		if (length(geneNameList)) {	## If any valid gene names	

			suppressWarnings(
			geneVals$geneData <- lapply(geneNameList, function(gene) {			
				if (length(grep("^ENSG|^ENSMUSG", gene))) {	## If coding gene
					
					## Retrieve annotation----------------------------------------------------------------
					annot_temp <- dbGetQuery(
						conn= fibromine_db,
						statement= '
							SELECT 
								*
							FROM 
								GeneAnnotation 
							WHERE 
								ENSGid = :x
						;',
						params= list(x= gene)
					)

					## annot for display
					annot <- subset(annot_temp, select= c(Symbol, Aliases, ENSGid, Chromosome, 
						StartPosition, EndPosition, Biotype, IsTF))
					annot$Aliases <- gsub("|||", ", ", annot$Aliases, fixed= TRUE)
					annot$IsTF <- ifelse(annot$IsTF == 0, "No", "Yes")
					colnames(annot)[1:3] <- c("Name", "Aliases", "Code")
					
					## Retrieve statistics and compute FDR-------------------------------------------------
					stat <- dbGetQuery(
						conn=fibromine_db,
						statement='
							SELECT 
								ENSGid, log2FC, Pval, StatContrast, GSE 
							FROM 
								StatComparisonsCoding
						;'
					)
					stat <- split(stat, f=stat$GSE)

					## Compute FDR and keep the gene of interest
					stat <- lapply(stat, function(x) {
						x$FDR <- p.adjust(x$Pval, method= "fdr")
						out <- x[x$ENSGid== gene, ]
						return(out)
					})
					stat <- do.call("rbind", stat)
					rownames(stat) <- NULL
					
					## Retrieve dataset-related info to add to statistics table
					samples <- dbGetQuery(	
						conn= fibromine_db,
						statement= '
							SELECT 
								DatasetsDescription.DatasetID, nExpCtrl, nExpCtrlPostCuration, 
								Species, Tech, Tissue, DescrContrast
							FROM 
								DatasetsDescription JOIN Datasets 
							ON 
								DatasetsDescription.DatasetID = Datasets.DatasetID
							WHERE 
								DatasetsDescription.DatasetID = :x AND DescrContrast = :y
						;',
						params= list(
							x= stat$GSE, 
							y= stat$StatContrast
						)
					)

					samples <- merge(samples, stars, 
						by= c("DatasetID", "DescrContrast"), 
						all.x= TRUE
					)

					## Format table
					stat$Species <- samples$Species
					stat$Tissue <- samples$Tissue
					stat$"#Exp/\n#Ctrl" <- samples$nExpCtrlPostCuration
					stat$Tech <- samples$Tech
					stat$"Stars count" <- samples$Stars.count
					stat$Name <- annot_temp$Symbol

					## Abbreviate to gain some space
					stat$Species <- gsub("Homo sapiens", "Hsa", stat$Species)
					stat$Species <- gsub("Mus musculus", "Mmu", stat$Species)

					## Wrap comparisons to gain more space
					stat$StatContrast <- gsub("_vs_", "\nvs\n", stat$StatContrast)

					stat <- stat[, c("Name", "Species", "Tissue",
				       "GSE", "StatContrast", "Stars count", "#Exp/\n#Ctrl",
				       "Tech", "log2FC", "Pval", "FDR")]
					colnames(stat)[c(4:5)] <- c("DatasetID","Comparison")

					## Shape RefSeq annotation--------------------------------------------------------------
					refseq <- annot_temp[,c("RefSeqmRNA","RefSeqncRNA","RefSeqPeptide")]
					refseq <- apply(refseq, 2, function(x){
						strsplit(x, split="|||", fixed=TRUE)[[1]]}
					)
					if (!is.matrix(refseq) & !is.data.frame(refseq)) {
						refseq <- as.data.frame(t(refseq), stringsAsFactors=FALSE)
					} else {
						refseq <- as.data.frame(refseq, stringsAsFactors=FALSE)
					}
					colnames(refseq) <- c("mRNA","ncRNA","Peptide")

					## miRNA - targets info-----------------------------------------------------------------
					miRDB <- dbGetQuery(
						conn=fibromine_db,
						statement= '
							SELECT 
								* 
							FROM 
								miRDB 
							WHERE 
								RefSeqmRNA = :x
						;',
						params= list(x= refseq$mRNA)
					)
					miRDB <- subset(miRDB, select= -miRDBid)

					## Error:
					## 	There may be genes whose refseq sequences are NOT INCLUDED
					## 	into miRDB. This results into an empty miRDB data.frame which
					## 	if not treated properly returns an error, which in turn
					## 	disconnects the session from the server.
					## 	Such a gene is e.g. MMP23 when organism is mouse
					## Solution: 
					## 	Check data.frame
					if (nrow(miRDB)) {
						miRDB$targetGene <- annot_temp$Symbol

						## Render table
						miRDB <- miRDB[, c(4,1,2,3)]
						colnames(miRDB) <- c("Gene_name", "Target", "miRNA", "Score") 
					} else {
						miRDB <- data.frame(
							Gene_name= "-",
							Target= "-",
							miRNA= "-",
							Score= "-"
						)
					}
						
					## Retrieve GO annotation---------------------------------------------------------------
					go <- dbGetQuery(
						conn= fibromine_db,
						statement= '
							SELECT 
								GO.GOid, GOname, GOnamespace 
							FROM 
								GO JOIN ENSGid2GOid 
							ON 
								GO.GOid = ENSGid2GOid.GOid 
							WHERE 
								ENSGid2GOid.ENSGid = :x
						;',
						params= list(x= annot_temp$ENSGid)
					)

					## DEP data-------------------------------------------------------------------------
					dep <- dbGetQuery(
						conn= fibromine_db,
						statement= '
							SELECT 
								DEP.*, DatasetsDescription.Tissue
							FROM 
								ENSGid2UniP JOIN DEP JOIN DatasetsDescription
							ON
								ENSGid2UniP.UniprotAC = DEP.UniprotAC
							AND
								DEP.DatasetID = DatasetsDescription.DatasetID
							WHERE
								ENSGid2UniP.ENSGid = :x
						;',
						params= list(x= annot_temp$ENSGid)
					)

					## Shape output-------------------------------------------------------------------------
					out <- list(annot, stat, dep, refseq, go, miRDB)
					names(out) <- c("Annotation", "Statistics", "DEP",
						"RefSeq", "GO", "miRDB")
					return(out)

				} else if (length(grep("^MIMAT", gene))) { ## If non-coding gene (ma data)

					## Retrieve annotation------------------------------------------------------------------
					miRBaseInfo <- dbGetQuery(
						conn= fibromine_db,
						statement= '
							SELECT 
								* 
							FROM 
								miRBase 
							WHERE 
								prodAC = :x
						;',
						params= list(x= gene)
					)

					ensemblInfo <- dbGetQuery(
						conn= fibromine_db,
						statement= '
							SELECT 
								* 
							FROM 
								GeneAnnotation 
							WHERE 
								premiRNAac = :x
						;',
						params= list(x= miRBaseInfo$premiRNAac)
					)
					
					annot_temp <- cbind(miRBaseInfo, ensemblInfo)

					## annot for display
					annot <- subset(annot_temp, select= c(prodID, Aliases, prodAC, Chromosome, 
					StartPosition, EndPosition, Biotype))
					annot$Aliases <- gsub("|||", ", ", annot$Aliases, fixed= TRUE)
					annot$IsTF <- "No"
					colnames(annot)[1:3] <- c("Name", "Aliases", "Code")

					## Retrieve statistics and compute FDR--------------------------------------------------
					stat <- dbGetQuery(
						conn=fibromine_db,
						statement='
							SELECT 
								prodAC, log2FC, Pval, StatContrastNonCoding, GSE
							FROM 
								StatComparisonsNonCoding
						;'
					)
					stat <- split(stat, f=stat$GSE)

					## Compute FDR and keep the gene of interest
					stat <- lapply(stat, function(x) {
						x$FDR <- p.adjust(x$Pval, method= "fdr")
						out <- x[x$prodAC== gene, ]
						return(out)
					})
					stat <- do.call("rbind", stat)
					rownames(stat) <- NULL

					## Retrieve dataset-related info to add to statistics table
					samples <- dbGetQuery(	
						conn= fibromine_db,
						statement= '
							SELECT 
								DatasetsDescription.DatasetID, nExpCtrl, nExpCtrlPostCuration, 
								Species, Tech, Tissue, DescrContrast 
							FROM 
								DatasetsDescription JOIN Datasets 
							ON 
								DatasetsDescription.DatasetID = Datasets.DatasetID
							WHERE 
								DatasetsDescription.DatasetID = :x AND DescrContrast = :y
						;',
						params= list(
							x= stat$GSE, 
							y= stat$StatContrastNonCoding
						)
					)

					samples <- merge(samples, stars, 
						by= c("DatasetID", "DescrContrast"), 
						all.x= TRUE
					)

					stat$Species <- samples$Species
					stat$Tissue <- samples$Tissue
					stat$nExpCtrlPostCuration <- samples$nExpCtrlPostCuration
					stat$Technology <- samples$Tech
					stat$Stars.count <- samples$Stars.count

					stat$Name <- unique(annot_temp$prodID)

					## Format table
					stat$Species <- samples$Species
					stat$Tissue <- samples$Tissue
					stat$"#Exp/\n#Ctrl" <- samples$nExpCtrlPostCuration
					stat$Tech <- samples$Tech
					stat$"Stars count" <- samples$Stars.count
					stat$Name <- unique(annot_temp$prodID)

					## Abbreviate to gain some space
					stat$Species <- gsub("Homo sapiens", "Hsa", stat$Species)
					stat$Species <- gsub("Mus musculus", "Mmu", stat$Species)

					## Wrap comparisons to gain more space
					stat$StatContrastNonCoding <- gsub("_vs_", "\nvs\n", 
						stat$StatContrastNonCoding)

					stat <- stat[, c("Name", "Species", "Tissue",
				       "GSE", "StatContrastNonCoding", "Stars count", "#Exp/\n#Ctrl",
				       "Tech", "log2FC", "Pval", "FDR")]
					colnames(stat)[c(4:5)] <- c("DatasetID","Comparison")
					
					## RefSeq info already as "-"-----------------------------------------------------------
					refseq <- annot_temp[,c("RefSeqmRNA","RefSeqncRNA","RefSeqPeptide")]
					colnames(refseq) <- c("mRNA", "ncRNA", "Peptide")

					## Retrieve GO annotation if any--------------------------------------------------------
					go <- dbGetQuery(
						conn= fibromine_db,
						statement= '
							SELECT 
								GO.GOid, GOname, GOnamespace 
							FROM 
								GO JOIN ENSGid2GOid 
							ON 
								GO.GOid = ENSGid2GOid.GOid
							WHERE 
								ENSGid2GOid.ENSGid = :x
						;',
						params= list(x= annot_temp$ENSGid)
					)
					if (nrow(go)==0) { # If miRNA not mapped to an ENSGid
						go <- data.frame(GOid= "-", GOname= "-", GOnamespace= "-")
					}

					## miRNA - targets info-----------------------------------------------------------------
					miRDB <- dbGetQuery(
						conn=fibromine_db,
						statement= '
							SELECT 
								* 
							FROM 
								miRDB 
							WHERE 
								prodID = :x
						;',
						params= list(x= annot_temp$prodID)
					)
					miRDB <- subset(miRDB, select= -miRDBid)

					targetName <- dbGetQuery(
						conn= fibromine_db,
						statement= '
							SELECT 
								Symbol, RefSeqmRNA FROM GeneAnnotation 
							WHERE 
								RefSeqmRNA = :x
						;',
						params= list(x= unique(miRDB$RefSeqmRNA))
					)
					miRDB <- merge(miRDB, targetName, by= "RefSeqmRNA")

					## Render table
					miRDB <- miRDB[, c(4,1,2,3)]
					colnames(miRDB) <- c("Gene_name", "Target", "miRNA", "Score") 
					
					## DEP data-------------------------------------------------------------------------
					dep <- data.frame(
						UniprotAC= as.character(),
						ExpressionDirection= as.character(),
						DatasetID= as.character(),
						Contrast= as.character(),
						Tissue= as.character()
					)

					## Shape output-------------------------------------------------------------------------
					out <- list(annot, stat, dep, refseq, go, miRDB)
					names(out) <- c("Annotation", "Statistics", "DEP",
						"RefSeq", "GO", "miRDB")
					return(out)

				}
			}))

		} else { ## If invalid return notification
			message <- showNotification(
				ui=paste("No valid or existent genes found"), duration=3, 
				id= "geneMessage", type= "error")
		}
		progress$inc(0.25)

		## Store gene names to use for the names of downloadable .xlsx files
		annot <- lapply(geneVals$geneData, function(gene) {
			return(gene$Annotation)
		})
		annot <- do.call("rbind", annot)
		geneVals$geneNames <- paste(unique(annot$Name))
		progress$inc(0.25)
	})
	
	## Display fetched info=================================================================================

	## Display geneInfo table and helpText
	geneInfo <- eventReactive(geneVals$geneData, {
		annot <- lapply(geneVals$geneData, function(gene) {
			return(gene$Annotation)
		})
		annot <- do.call("rbind", annot)
		rownames(annot) <- NULL
		return(annot)
	})

	output$geneInfo <- DT::renderDataTable({
		#req(geneInfo())
		validate(
			need(!is.null(geneVals$geneData),
				"General gene information will be presented here."
			)
		)
		out <- geneInfo()

		datatable(out, 
			selection= "none", 
			rownames= FALSE,
			class= "compact",
			options= list(
				sDom= '<"top">lrt<"bottom">ip',
				columnDefs= list(
					list(
						className= "dt-center",
						targets= "_all"
					)
				) 
			),
			extensions= "Responsive"
		)		
	})

	output$geneMapSc <- DT::renderDataTable({
		validate(
			need(!is.null(geneVals$geneData),
				"Active hyperlinks to map gene to single cell expression data will be presented here"
			)
		)

		reyf_prefix <- "https://www.nupulmonary.org/resources/?ds=fig1&gene="
		joshi_prefix <- "https://www.nupulmonary.org/resources/?ds=asbestos-4a&gene="
		xie_prefix <- "https://www.nupulmonary.org/resources/?ds=asbestos-s4h&layout=0&gene="

		# Keep protein coding genes only
		temp <- lapply(geneVals$geneData, function(gene) {
			if(gene$Annotation$Biotype == "protein_coding") {
				return(gene$Annotation)
			} else {
				return(NA)
			}
		})
		temp[is.na(temp)] <- NULL

		# Check the existence of protein coding genes and shape 
		# links to NUPulmonary
		if (length(temp)) {

			temp <- lapply(temp, function(x) {

				# If mouse gene send to Joshi-Watanabe and Xie datasets
				if(isTRUE(grepl("^ENSM", x$Code))) { 
					# Create 2 identical rows 
					out <- rbind(x,x)

					out$sc <- "lalakis"
					out$sc[1] <- paste0("<a href='", joshi_prefix, unique(out$Name),
						"' rel='noopener noreferrer' target='_blank'>", 
						"Joshi, Watanabe et al.", "</a>"
					)
					out$sc[2] <- paste0("<a href='", xie_prefix, unique(out$Name),
						"' rel='noopener noreferrer' target='_blank'>", 
						"Xie et al.", "</a>"
					)
				} else { # If human gene send to Reyfman dataset
					out <- x
					out$sc <- paste0("<a href='", reyf_prefix, out$Name,
						"' rel='noopener noreferrer' target='_blank'>", 
						"Reyfman et al.", "</a>"
					)
				}
				return(out)
			})

			# Format table
			temp <- do.call("rbind", temp)
			temp <- temp[, c("Name", "Code", "sc")]
			colnames(temp)[3] <- "Single cell dataset mapping"
		} else {
			temp <- data.frame(
				Name= character(),
				Code= character(),
				"sc"= character()
			)
			colnames(temp)[3] <- "Single cell dataset mapping"
		}

		dtable <- datatable(temp, 
			selection= "none", 
			rownames= FALSE,
			escape= FALSE, 
			class= "compact",
			options= list(
				rowsGroup= list(0,1),
				sDom= '<"top">lrt<"bottom">ip',
				columnDefs= list(
					list(
						className= "dt-center",
						targets= "_all"
					)
				) 
			),
			extensions= "Responsive"
		)		

		path <- "./www"
		depend <- htmltools::htmlDependency("RowsGroup", "2.0.0",
			path, script= "dataTables.rowsGroup.js")
		dtable$dependencies <- c(dtable$dependencies, list(depend))

		return(dtable)
	})

	## Display expressionPerGene table, helpText and download handler
	expressionPerGene <- eventReactive(geneVals$geneData, {
		stat <- lapply(geneVals$geneData, function(x) {
			return(x$Statistics)
		})
		stat <- do.call("rbind", stat)
		rownames(stat) <- NULL

		## Convert to factors for easier client-side filtering
		stat$Name <- as.factor(stat$Name)
		stat$Species <- as.factor(stat$Species)
		stat$Tissue <- as.factor(stat$Tissue)
		stat$DatasetID <- as.factor(stat$DatasetID)
		stat$Comparison <- as.factor(stat$Comparison)
		stat$Tech <- as.factor(stat$Tech)

		return(stat)
	})

	output$expressionPerGeneHelp <- renderUI({
		if (is.null(geneVals$geneData))
			helpText("Gene expression statistics will be presented here.")
	})

	output$expressionPerGene <- DT::renderDataTable({	
		req(expressionPerGene())
		
		out <- expressionPerGene()

		## Color log2FC column
		brks <- quantile(out$log2FC, probs= seq(0, 1, 0.1), na.rm= TRUE)
		clrsPal <- colorRampPalette(c("green","white","red"))
		clrs <- clrsPal(length(brks)+1)

		## Color stars column
		starBrks <- 0:7
		starClrsPal <- colorRampPalette(c("blue", "red"))
		starClrs <- starClrsPal(length(starBrks)+1)

		## Color significant FDR red
		out$fdrColor <- ifelse(out$FDR < 0.05, 1, 0)

		dtable <- datatable(out, 
			selection="none", 
			rownames= FALSE,
			filter= "top",
			class="compact",
			options= list(
				rowsGroup= list(0),
				order= list(list(0, "asc")),
				sDom= '<"top">lrt<"bottom">ip',
				columnDefs= list(
					list(
						targets= 11, 
						visible = FALSE
					),
					list(
						className= "dt-center",
						targets= "_all"
					)
				)
			)#,
			#extensions= "Responsive"
		) %>% formatStyle(1:ncol(out), 
			cursor = 'pointer'
		) %>% formatStyle("log2FC",
			backgroundColor= styleInterval(brks, clrs)
		) %>% formatStyle("Stars count",
			color= styleInterval(starBrks, starClrs)
		) %>% formatStyle("FDR", "fdrColor",
			backgroundColor= styleEqual(1, "#ff8e8e")
		) %>% formatRound(c("log2FC","Pval","FDR"),
			digits= 5
		)

		# path <- "./www"
		# depend <- htmltools::htmlDependency("RowsGroup", "2.0.0",
		# 	path, script= "dataTables.rowsGroup.js")
		# dtable$dependencies <- c(dtable$dependencies, list(depend))
		
		return(dtable)

	})

	output$geneStatDown <- downloadHandler(
		filename= function() {
			paste0(paste(geneVals$geneNames, collapse="-"), 
				"_", "DE_statistics_", Sys.Date(), ".xlsx"
			)
		},
		content= function(file) {
			write.xlsx(expressionPerGene()[input$expressionPerGene_rows_all, ],file)
		}
	)

	## Display expressionPerProtein table, helpText and download handler
	expressionPerProtein <- eventReactive(geneVals$geneData, {
		dep <- lapply(geneVals$geneData, function(gene) {
			return(gene$DEP)
		})
		dep <- do.call("rbind", dep)
		out <- data.frame(Name= rownames(dep), dep, row.names= NULL)
		out <- out[,c(1:4,6,5)]
		out$Name <- gsub("\\..*", "", out$Name)
		return(out)
	})

	output$expressionPerProteinHelp <- renderUI({
		if (is.null(geneVals$geneData))
			helpText("Protein expression data will be presented here.")
	})

	output$expressionPerProtein <- DT::renderDataTable({	
		req(expressionPerProtein())
		
		out <- expressionPerProtein()

		dtable <- datatable(out, 
			selection="none", 
			rownames= FALSE,
			filter= "top",
			class= "compact",
			options= list(
				rowsGroup= list(0),
				order= list(list(0, "asc")),
				sDom= '<"top">lrt<"bottom">ip',
				columnDefs= list(
					list(
						className= "dt-center",
						targets= "_all"
					)
				)
			),
			extensions= "Responsive"
		) %>% formatStyle("ExpressionDirection", "ExpressionDirection",
			backgroundColor= styleEqual(c("Up", "Down"), 
				c("#FF8E8E", "#7AFF59")))

		# path <- "./www"
		# depend <- htmltools::htmlDependency("RowsGroup", "2.0.0",
		# 	path, script= "dataTables.rowsGroup.js")
		# dtable$dependencies <- c(dtable$dependencies, list(depend))
		return(dtable)
	})

	output$prtnStatDown <- downloadHandler(
		filename= function() {
			paste0(paste(geneVals$geneNames, collapse="-"), 
				"_", "DEP_statistics_", Sys.Date(), ".xlsx"
			)
		},
		content= function(file) {
			write.xlsx(expressionPerProtein()[input$expressionPerProtein_rows_all, ],file)
		}
	)

	## Display goTable, helpText and download handler
	goTable <- eventReactive(geneVals$geneData, {
		go <- lapply(geneVals$geneData, function(x) {
			out <- data.frame(Name= x$Annotation$Name, x$GO)
			return(out)
		})
		go <- do.call("rbind", go)
		rownames(go) <- NULL

		## Convert to factors for easier client-side fitlering
		go$GOid <- as.factor(go$GOid)
		go$GOname <- as.factor(go$GOname)
		go$GOnamespace <- as.factor(go$GOnamespace)

		return(go)
	})

	output$goTableHelp <- renderUI({
		if (is.null(geneVals$geneData))
			helpText("GO annotation will be presented here.")
	})

	output$goTable <- DT::renderDataTable({	
		req(goTable())
		
		dtable <- datatable(goTable(), 
			selection="none", 
			rownames= FALSE,
			filter= "top", 
			class= "compact",
			options= list(
				rowsGroup= list(0),
				order= list(list(0, "asc")),
				sDom= '<"top">lrt<"bottom">ip',
				columnDefs= list(
					list(
						className= "dt-center",
						targets= "_all"
					)
				)
			),
			extensions= "Responsive"
		)

		# path <- "./www"
		# depend <- htmltools::htmlDependency("RowsGroup", "2.0.0",
		# 	path, script= "dataTables.rowsGroup.js")
		# dtable$dependencies <- c(dtable$dependencies, list(depend))
		
		return(dtable)

	})

	output$goDown <- downloadHandler(
		filename= function() {
			paste0(paste(geneVals$geneNames, collapse="-"),
				"_", "GO", "_", Sys.Date(), ".xlsx"
			)
		},
		content= function(file) {
			write.xlsx(goTable()[input$goTable_rows_all, ], file)
		}
	)

	## Display "refseqTable" data table, helpText and download handler
	refseqTable <- eventReactive(geneVals$geneData, {
		refseq <- lapply(geneVals$geneData, function(x) {
			return(x$RefSeq)
		})
		for (i in names(refseq)) {
			refseq[[i]]$Name <- i
		}
		refseq <- unique(do.call("rbind", refseq)) ## Get rid of duplicate lines (mainly due to ma tech miRNAs)
		refseq <- refseq[,c(4,1,2,3)]
		rownames(refseq) <- NULL

		## Convert to factors for easier client-side filtering
		refseq$mRNA <- as.factor(refseq$mRNA)
		refseq$ncRNA <- as.factor(refseq$ncRNA)
		refseq$Peptide <- as.factor(refseq$Peptide)

		return(refseq)
	})

	output$refseqTableHelp <- renderUI({
		if (is.null(geneVals$geneData))
			helpText("RefSeq information will be presented here.")
	})

	output$refseqTable <- DT::renderDataTable({
		req(refseqTable())

		dtable <- datatable(refseqTable(), 
			selection="none", 
			rownames= FALSE,
			filter= "top", 
			class= "compact",
			options= list(
				rowsGroup= list(0),
				order= list(list(0, "asc")),
				sDom= '<"top">lrt<"bottom">ip',
				columnDefs= list(
					list(
						className= "dt-center",
						targets= "_all"
					)
				)
			),
			extensions= "Responsive"
		)

		# path <- "./www"
		# depend <- htmltools::htmlDependency("RowsGroup", "2.0.0",
		# 	path, script= "dataTables.rowsGroup.js")
		# dtable$dependencies <- c(dtable$dependencies, list(depend))
		
		return(dtable)
	})

	output$refSeqDown <- downloadHandler(
		filename= function() {
			paste0(paste(geneVals$geneNames, collapse="-"), "_", 
				"Refseq_", Sys.Date(), ".xlsx"
			)
		},
		content= function(file) {write.xlsx(refseqTable()[input$refseqTable_rows_all,], file)}
	)

	## Display miRNAinteractorsTable, helpText and download handler
	miRNAinteractorsTable  <- eventReactive(geneVals$geneData, {
		out <- lapply(geneVals$geneData, function(x) {
			return(x$miRDB)
		})
		out <- unique(do.call("rbind", out)) ## Get rid of duplicate lines (mainly due to ma tech miRNAs)
		rownames(out) <- NULL

		## Convert to factors for easier client-side filtering
		out$Gene_name <- as.factor(out$Gene_name)
		out$Target <- as.factor(out$Target)
		out$miRNA <- as.factor(out$miRNA)

		return(out)
	})

	output$miRNAinteractorsTableHelp <- renderUI({
		if (is.null(geneVals$geneData))
			helpText("miRNAinteractors information will be presented here.")
	})

	output$miRNAinteractorsTable <- DT::renderDataTable({
		
		req(miRNAinteractorsTable())

		dtable <- datatable(miRNAinteractorsTable(), 
			selection="none", 
			rownames= FALSE, 
			filter= "top", 
			class= "compact",
			options = list(
				rowsGroup= list(0),
				order= list(list(0, "asc")),
				sDom= '<"top">lrt<"bottom">ip',
				columnDefs= list(
					list(
						className= "dt-center",
						targets= "_all"
					)
				)
			),
			extensions= "Responsive"
		) %>% formatRound("Score",
			digits= 0	
		)

		# path <- "./www"
		# depend <- htmltools::htmlDependency("RowsGroup", "2.0.0",
		# 	path, script= "dataTables.rowsGroup.js")
		# dtable$dependencies <- c(dtable$dependencies, list(depend))
		
		return(dtable)
	})

	output$miRNAinterDown <- downloadHandler(
		filename= function() {
			paste0(paste(geneVals$geneNames, collapse="-"), "_", "miRNA_interactors_", 
				Sys.Date(), ".xlsx")
		},
		content= function(file) {
			write.xlsx(miRNAinteractorsTable(),file)
		}
	)

	# ============================================================================
	# "Protein explorer" tab Items
	# ============================================================================
	prtnVals <- reactiveValues(geneNamePPI=NULL, uniprot=NULL, 
		string=NULL, annotData=NULL, annotDataSel=NULL, annotDataUsed=NULL)

	dataProtein <- eventReactive(eventExpr=input$geneNameSearchPPI, 
		valueExpr=input$geneNamePPI
	)
	dataProtein_example <- eventReactive(eventExpr=input$geneNameSearchPPIExample, 
		valueExpr= "ACTA2"
	)
	
	# =============================
	## Protein explorer's main tour
	stepsProtein <- reactive(
	    data.frame(
	        element= c(".step_3", "#geneNamePPI", "#uniprotAC", "#isItDE", 
	        	".step1_proteinExplorer"),
	        intro= c(
	            paste("Via <b>Protein explorer</b> the user can discover the differenitally",
	            	"expressed proteins in the lung of IPF patients compared to the respective",
	            	"healthy controls.", "In addition, condition-specific protein-protein networks",
	            	"can be plotted."
	            ),
	            paste("To <b>search</b> for a protein, <b>first</b>, type its coding gene's symbol or",
	            	"Ensembl accession number in the box and then press the <b>'Search'</b> button below.",
	            	"For detailed instructions <b>hover</b> the cursor over the search box."
	            ),
	            paste("<b>General</b> information about the protein queried will be presented in these",
	            	"infoboxes."
	            ),
	            paste("A <b>quick</b> reference about if the queried protein is found <b>differentially expressed</b>",
	            	"in any of the Fibromine's currently included proteomic datasets will be here displayed."
	            ),
	            paste("The <b>main results</b> of each search can be found here. <br>",
	            	"<b>Concerning protein</b> tab lists more detailed information regarding the protein of interest. <br>",
	            	"<b>Differential expression</b> tab will host analytical information about the differential expression",
	            	"pattern of the protein, if any. <br>",
	            	"<b>PPI network</b> tab consists the 'crown jewel' of this explorer as enables the user to plot",
	            	"<b>condition-specific protein-protein interaction networks</b> revolving around the queried protein.",
	            	"Detailed instructions about its operation can be found inside the respective tab.<br>",
	            	"Through the <b>last tab</b>, the user can inspect the datasets used to annotate the protein-protein",
	            	"interaction network plotted."
	            )
	        ),
	        position= c(rep(c("right", "bottom"), each= 2), "top")
	    )
	)

	## Take the tour
	observeEvent(input$introProt,
	introjs(session, 
		options = list(
			steps= stepsProtein(),
			"nextLabel" = "Next",
			"prevLabel" = "Previous", 
			"doneLabel" = "Let's begin!"
			)
		)
	)

	# =============================
	# PPI tour
	stepsPPI <- reactive(
	    data.frame(
	        element= c("step1_proteinExplorer", "#geneNamePPI", "#ppiPlot", ".step1_ppi",
	        	".step2_ppi", "#speciesInNet", "#visExpression", "#clearAnnotationParams"
	        	),
	        intro= c(
	            paste("<b>Condition specific protein-protein interaction network</b> creation tool",
	            	"is the Protein explorer's <i>crown jewel</i>. Via this feature the user can",
	            	"<b>create</b> PPI networks centring around the <b>queried</b> protein and then",
	            	"<b>annotate them</b> with any of the supported experimental comparisons."
	            ),
	            paste("<b>The first step</b> for any network creation is the <b>search</b> for a",
	            	"specific protein as described in the <b>About the explorer</b> tour."
	            ),
	            paste("Then a PPI network can be created by <b>clicking this button</b>",
	            	"<b>Attention!</b> When a <b>new</b> protein is queried <b>this button</b>",
	            	"must be <b>again clicked</b> for a successfull network <b>update</b>"
	            ),
	            paste("All networks can be annotated by selecting any of the <b>supported tissues</b>..."
	            ),
	            paste("and <b>comparisons</b> in which the queried protein is detected."
	            ),
	            paste("<b>Species</b> are selected <b>automatically</b>, as this explorer's search is",
	            	"case sensitive."
	            ),
	            paste("After the required parameters selection, the user can <b>annotate</b> the network",
		            "using the <b>default p-value and fold change thresholds</b> or <b>fine-tune</b> them at will."
	            ),
	            paste("To annotate the <b>same network</b> using <b>different parameters</b>,",
	            	" first <b>reset</b> the latters using this button."
	            )
	        ),
	        position= rep("auto", 8)
	    )
	)

	## Take the tour
	observeEvent(input$introPPI,
	introjs(session, 
		options = list(
			steps= stepsPPI(),
			"nextLabel" = "Next",
			"prevLabel" = "Previous", 
			"doneLabel" = "Let's begin!"
			)
		)
	)

	# =============================
	## Example case
	observeEvent(dataProtein_example(),{

		## Update input textbox to display the example gene
		updateTextInput(session, inputId= "geneNamePPI", 
			value= "ACTA2")

		progress <- shiny::Progress$new()
		on.exit(progress$close())
		progress$set(message= "Fetching data", value=0)

		# Transform HGNC symbol to ensembl gene ID
		prtnVals$geneNamePPI <- dbGetQuery(
			conn= fibromine_db,
			statement= 'SELECT * FROM GeneAnnotation WHERE Symbol = :x',
			params= list(x= "ACTA2"))$ENSGid
		progress$inc(0.5)	

		## Retrieve Uniprot info
		prtnVals$uniprot <- dbGetQuery(
			conn= fibromine_db,
			statement= '
				SELECT 
					Uniprot.* 
				FROM 
					GeneAnnotation JOIN ENSGid2UniP JOIN Uniprot 
				ON 
					GeneAnnotation.ENSGid = ENSGid2UniP.ENSGid 
					AND ENSGid2UniP.UniprotAC = Uniprot.UniprotAC
				WHERE 
					GeneAnnotation.ENSGid = :x
			;',
			params= list(x= prtnVals$geneNamePPI)
		)

		## Retrieve STRINGdb info
		prtnVals$string <- dbGetQuery(
			conn= fibromine_db,
			statement= '
				SELECT 
					* 
				FROM 
					STRINGdb 
				WHERE 
					UniprotAC = :x
			;',
			params= list(x= prtnVals$uniprot$UniprotAC)
		)

		## Retrieve expression data
		if (length(grep("^ENSG", prtnVals$geneNamePPI))) {
			prtnVals$dep <- dbGetQuery(
				conn= fibromine_db,
				statement= '
					SELECT
						*
					FROM
						DEP 
					WHERE 
						UniprotAC = :x
				;',
				params= list(x = prtnVals$uniprot$UniprotAC)
			)

			if (nrow(prtnVals$dep)) {
				prtnVals$isItDE <- "Yes"
			} else {
				prtnVals$isItDE <- "No"
				prtnVals$dep <- NA
			}
		}
		progress$inc(0.5)
	})

	## Fetch gene-related protein info
	observeEvent(dataProtein(),{	
		
		progress <- shiny::Progress$new()
		on.exit(progress$close())
		progress$set(message= "Fetching data", value=0)

		## Check name given. If ensembl OK. If HGNC symcol transform to ensembl gene ID
		if (length(grep("^ENSG|^ENSMUSG", input$geneNamePPI))) {
			prtnVals$geneNamePPI <- input$geneNamePPI
		} else {
			prtnVals$geneNamePPI <- dbGetQuery(
				conn= fibromine_db,
				statement= 'SELECT * FROM GeneAnnotation WHERE Symbol = :x',
				params= list(x= input$geneNamePPI))$ENSGid
		}
		progress$inc(0.5)	

		if (length(prtnVals$geneNamePPI)) {	## If valid gene 
			
			## Retrieve Uniprot info
			prtnVals$uniprot <- dbGetQuery(
				conn= fibromine_db,
				statement= '
					SELECT 
						Uniprot.* 
					FROM 
						GeneAnnotation JOIN ENSGid2UniP JOIN Uniprot 
					ON 
						GeneAnnotation.ENSGid = ENSGid2UniP.ENSGid 
						AND ENSGid2UniP.UniprotAC = Uniprot.UniprotAC
					WHERE 
						GeneAnnotation.ENSGid = :x
				;',
				params= list(x= prtnVals$geneNamePPI)
			)

			## Retrieve STRINGdb info
			prtnVals$string <- dbGetQuery(
				conn= fibromine_db,
				statement= '
					SELECT 
						* 
					FROM 
						STRINGdb 
					WHERE 
						UniprotAC = :x
				;',
				params= list(x= prtnVals$uniprot$UniprotAC)
			)

			## Retrieve expression data
			if (length(grep("^ENSG", prtnVals$geneNamePPI))) {
				prtnVals$dep <- dbGetQuery(
					conn= fibromine_db,
					statement= '
						SELECT
							*
						FROM
							DEP 
						WHERE 
							UniprotAC = :x
					;',
					params= list(x = prtnVals$uniprot$UniprotAC)
				)

				if (nrow(prtnVals$dep)) {
					prtnVals$isItDE <- "Yes"
				} else {
					prtnVals$isItDE <- "No"
					prtnVals$dep <- NA
				}

			} else { ## I currently have no data for mouse proteomics
				prtnVals$dep <- NA
				prtnVals$isItDE <- "No"
			}

		} else {	## If invalid gene
			message <- showNotification(
				ui=paste("This is not a valid or existent gene"), duration=3, 
				id= "geneMessageII", type= "error")
		}
		progress$inc(0.5)
	})

	## Display results====================================================================

	## Fill InfoBoxes
	output$uniprotAC <- renderInfoBox({
		infoBox("Uniprot accession", 
			value= prtnVals$uniprot$UniprotAC, 
			icon=shiny::icon("barcode"), 
			color="red", 
			fill=TRUE
		)
	})

	output$fullName <- renderInfoBox({
		infoBox("Full name", 
			value= {
				fullName <- prtnVals$uniprot$Names 									## Retrieve protein's preferred full name
				if (length(fullName)) {
					fullName <- strsplit(fullName, split="|||", fixed=TRUE)[[1]][1]
				} else fullName <- NULL}, 
			icon=shiny::icon("id-card"), 
			color="red", 
			fill=TRUE
		)
	})

	output$CD <- renderInfoBox({
		infoBox("CD", 
			value= prtnVals$uniprot$CD, 
			icon=shiny::icon("id-card-alt"), 
			color="red", 
			fill=TRUE
		)
	})

	output$isItDE <- renderInfoBox({
		infoBox("Is it DE?",
			value= prtnVals$isItDE,
			icon= shiny::icon("check"),
			color= "red",
			fill= TRUE
		)
	})

	## Fill "Concerning protein" box
	prtnAliases <- reactive({
		if (length(prtnVals$uniprot$Names)) {

			aliases <- prtnVals$uniprot$Names 										## Retrieve protein's aliases
			aliases <- strsplit(aliases, split="|||", fixed=TRUE)[[1]][-1]					## Remove preferred name
			if (!length(aliases)) aliases <- "-"											## If there are no aliases
			aliases <- paste0("<li>", aliases, "</li>")
			aliases <- paste0(aliases, collapse="")
			aliases <- paste0("<ul>", aliases, "</ul>")
			return(aliases)

		} else NULL
	})
	output$Aliases <- renderText(prtnAliases())

	prtnFunction <- reactive({
		if (length(prtnVals$uniprot$Function)) {

			## Shape data
			PrtnFunction <- prtnVals$uniprot$Function
			## Remove supporting evidence
			#PrtnFunction <- gsub(". {", " {", PrtnFunction, fixed=TRUE)					
			PrtnFunction <- gsub("\\s\\{.*\\}.", "", PrtnFunction)
			PrtnFunction <- unlist(strsplit(PrtnFunction, ". ", fixed=TRUE))
			
			## Organize into bullets
			PrtnFunction <- paste0("<li>", PrtnFunction, "</li>")
			PrtnFunction <- paste0(PrtnFunction, collapse="")
			PrtnFunction <- paste0("<ul>", PrtnFunction, "</ul>")
			return(PrtnFunction)

		} else NULL
	})
	output$PrtnFunction <- renderText(prtnFunction())


	subcelLocation <- reactive({
		if (length(prtnVals$uniprot$SubcellularLocation)) {

			## Shape data
			SubcelLocation <- prtnVals$uniprot$SubcellularLocation
			## Remove supporting evidence
			#SubcelLocation <- gsub(". {", " {", SubcelLocation, fixed=TRUE)
			SubcelLocation <- gsub("\\s\\{.*\\}.", "", SubcelLocation)
			SubcelLocation <- unlist(strsplit(SubcelLocation, ". ", fixed=TRUE))

			## Organize into bullets
			SubcelLocation <- paste0("<li>", SubcelLocation, "</li>")
			SubcelLocation <- paste0(SubcelLocation, collapse="")
			SubcelLocation <- paste0("<ul>", SubcelLocation, "</ul>")
			return(SubcelLocation)

		} else NULL
	})
	output$SubcelLocation <- renderText(subcelLocation())

	prtnDisease <- reactive({
		if (length(prtnVals$uniprot$Disease)) {

			## Shape data
			PrtnDisease <- prtnVals$uniprot$Disease
			## Remove supporting evidence
			#PrtnDisease <- unlist(strsplit(PrtnDisease, split="|||", fixed=TRUE))
			PrtnDisease <-  gsub("\\s\\{.*\\}.", "", PrtnDisease)

			## Organize into bullets
			PrtnDisease <- paste0("<li>", PrtnDisease, "</li>")
			PrtnDisease <- paste0(PrtnDisease, collapse="")
			PrtnDisease <- paste0("<ul>", PrtnDisease, "</ul>")
			return(PrtnDisease)				

		} else NULL
	})
	output$PrtnDisease <- renderText(prtnDisease())

	output$PE <- renderText(prtnVals$uniprot$PE)

	## Display differential expression data if any
	depData <- reactive({
		req(prtnVals$dep)
		
		temp <- dbGetQuery(
			conn= fibromine_db,
			statement= '
				SELECT 
					Tissue, nExpCtrl
				FROM
					DatasetsDescription
				WHERE
					DatasetID = :x
			;',
			params= list(x= prtnVals$dep$DatasetID)
		)
		nSamples <- strsplit(temp$nExpCtrl, split= "/")
		nSamples <- do.call("rbind", nSamples)

		out <- data.frame(
			DatasetID= prtnVals$dep$DatasetID,
			Tissue= temp$Tissue,
			Comparison= prtnVals$dep$Contrast,
			ExpressionDirection= prtnVals$dep$ExpressionDirection,
			Exp= nSamples[,1],
			Ctrl= nSamples[,2]
		)
		colnames(out)[c(5,6)] <- c("#Exp", "#Ctrl")
		return(out)
	})

	output$depData <- DT::renderDataTable({
		req(prtnVals$dep)

		datatable(depData(), selection = "multiple", 
			rownames= FALSE,
			escape= FALSE, 
			class= "compact",
			options= list(
				sDom= '<"top">lrt<"bottom">ip', # to enable search bar use "flrt" instead (f for Filtering)
				columnDefs= list(
					list(
						className= "dt-center", 
						targets= "_all"
					)
				)
			),	
	  		extensions= "Responsive",
	  	) %>% formatStyle("ExpressionDirection", "ExpressionDirection",
			backgroundColor= styleEqual(c("Up", "Down"), 
				c("#FF8E8E", "#7AFF59"))) 
	})

	## Create PPI plot upon request
	network <- eventReactive(input$ppiPlot, {

		progress <- shiny::Progress$new()												## Create a progress object
		on.exit(progress$close())
		progress$set(message= "Creating network. Please be patient.", value=0)
		progress$inc(0.2)																
		
		net <- dbGetQuery(																## Retrieve queried protein's top 9 interactors
			conn= fibromine_db,
			statement= '
				SELECT 
					* 
				FROM 
					PPI 
				WHERE 
					Protein1 = :x AND InteractionCombinedScore >= 700
				ORDER BY 
					InteractionCombinedScore DESC 
				LIMIT 
					9
			;',
			params= list(x= prtnVals$string$StringID)
		)
		progress$inc(0.2)																

		## Shape network in a dataframe
		net_df <- shapeNetwork(net, prtnVals$string,
			fibromine_db)
		progress$inc(0.2)	

		## Plot the network
		out <- plotNetwork(net_df)					
		progress$inc(0.2)

		## Update species selection of the "color by expression" control panel 
		if (grepl("^ENSG", prtnVals$geneNamePPI)) {
			sps <- "Homo sapiens"
		} else {
			sps <- "Mus musculus"
		}
		updateTextInput(session, inputId= "speciesInNet", label= "Species", 
			value= sps)

		## Update tissue selection 
		prtnVals$annotData <- dbGetQuery(									## Query dataset information for species selected
			conn=fibromine_db,
			statement='
				SELECT 
					* 
				FROM 
					Datasets JOIN DatasetsDescription 
				ON 
					Datasets.DatasetID = DatasetsDescription.DatasetID
				WHERE 
					Datasets.Species = :x 
					AND Datasets.Tech = "Expression profiling by array"
			;',
			params= list(x= sps)
		)

		tissueChoicesNet <- unique(prtnVals$annotData$Tissue)
		shinyjs::enable("tissueInNet")													## Update tissueInNet selection input
		updateSelectizeInput(session, 
			inputId= "tissueInNet", 
			choices= tissueChoicesNet,
			options= list(
				placeholder= "Select tissue",
				onInitialize= I('function() { this.setValue(""); }')
			)
		)
		progress$inc(0.2)
		return(out)
	})

	output$ppiNetwork <- renderVisNetwork({
		validate(
			need(!is.null(prtnVals$string),
				"Search a protein by coding gene name first."
			)
		)
		network()
	})

	## Network annotation parameters update===============================================

	## Updare comparison based on tissue selection
	observeEvent(input$tissueInNet, {
		req(input$tissueInNet)

		shinyjs::disable("tissueInNet")
		shinyjs::enable("comparisonInNet")

		prtnVals$annotDataSel <- subset(prtnVals$annotData, 
			Tissue == input$tissueInNet)

		comparisonChoicesNet <- unique(prtnVals$annotDataSel$DescrContrast)
		shinyjs::enable("comparisonInNet")
		updateSelectizeInput(session, 
			inputId= "comparisonInNet", 
			choices= comparisonChoicesNet,
			options= list(
				placeholder= "Select comparison",
				onInitialize= I('function() { this.setValue(""); }')
			)
		)

	})

	## Updare contrast based on tissue selection
	observeEvent(input$visExpression, {

		req(input$comparisonInNet)
		shinyjs::disable("comparisonInNet")

		progress <- shiny::Progress$new()													
		on.exit(progress$close())
		progress$set(message= "Annotating network", value=0)
		progress$inc(0.25)																	
		
		prtnVals$annotDataSel <- subset(prtnVals$annotDataSel,
			DescrContrast== input$comparisonInNet)
		prtnVals$annotDataSel <- subset(prtnVals$annotDataSel,
			DatasetID != "GSE31934")

		stat <- dbGetQuery(
			conn= fibromine_db,
			statement='
				SELECT 
					* 
				FROM 
					StatComparisonsCoding WHERE GSE = :x
				AND 
					StatContrast = :y
			;',
			params=list(
				x= prtnVals$annotDataSel$DatasetID, 
				y= prtnVals$annotDataSel$DescrContrast
			)
		)

		## Find DEGs based on a given Pval and log2FC threshold
		log2fc <- log2(input$fcInNet)														
		deg <- subset(stat, (Pval <= input$pvalInNet & log2FC >= log2fc) | 
			(Pval <= input$pvalInNet & log2FC <= -log2fc))
		deg <- split(deg, f= deg$"ENSGid")												
		progress$inc(0.25)											

		## Keep the interacting genes
		nam <- network()$x$nodes$id
		if (any(grep("ENSP", nam))) {
			str2uni <- nam[-grep("ENSP", nam)]	## Remove STRING proteins not found in uniprotKB 
		} else {
			str2uni <- nam
		} 

		str2uni <- dbGetQuery(
			conn= fibromine_db,
			statement= '
				SELECT 
					UniprotAC, StringPreferredName 
				FROM 
					STRINGdb 
				WHERE 
					StringPreferredName = :x
			;',
			params= list(x= str2uni)
		)

		uni2ensg <- dbGetQuery(
			conn= fibromine_db,
			statement= '
				SELECT 
					ENSGid2UniP.ENSGid, ENSGid2UniP.UniprotAC 
				FROM 
					ENSGid2UniP JOIN Uniprot 
				ON 
					ENSGid2UniP.UniprotAC = Uniprot.UniprotAC
				WHERE 
					Uniprot.UniprotAC = :x
			;',
			params= list(x= str2uni$UniprotAC)
		)

		str2uni2ensg <- merge(str2uni, uni2ensg, by= "UniprotAC")
		rm(str2uni, uni2ensg)
		progress$inc(0.25)	

		## Isolate interactors from all DEGs and define their expression orientation
		deg <- deg[as.character(str2uni2ensg$ENSGid)]
		names(deg) <- str2uni2ensg$StringPreferredName

		orientation <- lapply(deg, function(x) {
			if (nrow(x) == 1 && !is.null(x)) {											## If found DEG in only one sample, take it as is
				
				orientation <- sign(x$log2FC)
				return(orientation)

			} else if (nrow(x) == 2 && !is.null(x)) {									## If found DEG in 2 samples, 
				
				# Retrieve "nExpCtrlPostCuration" data
				gse <- x$GSE 		 													## choose the orientation of the bigger one
				gse <- dbGetQuery(
					conn= fibromine_db,
					statement= '
						SELECT 
							DatasetID, nExpCtrlPostCuration
						FROM 
							DatasetsDescription 
						WHERE 
							DatasetID = :x
					;',
					params= list(x= gse)
				)
				gse <- unique(gse)

				# Sum the total number of samples per GSE 
				nSamples <- lapply(strsplit(gse[,2], split="/"), function(x) {
					x <- as.integer(x)
					sum(x)
				})
				nSamples <- unlist(nSamples)
				gse <- gse[which(nSamples == max(nSamples)), "DatasetID"]

				orientation <- as.numeric(sign(subset(x, GSE == gse, log2FC)))
				return(orientation)

			} else if (nrow(x) > 2 && !is.null(x)) {									## If in >2 samples take the prevailing orientation
				
				count <- table(sign(x$log2FC)) 
				count <- count[which(count == max(count))]
				orientation <- as.numeric(names(count))
				return(orientation)
			
			} else return("Non DE")

		})
		orientation <- unlist(orientation)

		if (any(grep("ENSG", nam))){
			
			rest <- rep(0, length(nam[grep("ENSP", nam)]))								## Bring in STRING proteins not found in uniprotKB 
			names(rest) <- nam[grep("ENSP", nam)]
			orientation <- c(orientation, rest)
		
		} else {
			orientation <- orientation
		}

		## Annotate the diagram

		nodesAnnot <- network()$x$nodes
		nodesAnnot[names(orientation[orientation== "-1"]),
			"degOrientation"] <- "Downregulated" 
		nodesAnnot[names(orientation[orientation== "-1"]),
			"groups"] <- "Downregulated," 	
		nodesAnnot[names(orientation[orientation== "-1"]),
			"color"] <- "#00FFFF" 	

		nodesAnnot[names(orientation[orientation== "1"]),
			"degOrientation"] <- "Upregulated"
		nodesAnnot[names(orientation[orientation== "1"]),
			"groups"] <- "Upregulated,"
		nodesAnnot[names(orientation[orientation== "1"]),
			"color"] <- "#FF3232"

		nodesAnnot[names(orientation[orientation== "Non DE"]),
			"degOrientation"] <- "Non DE"
		nodesAnnot[names(orientation[orientation== "Non DE"]),
			"groups"] <- "Non DE,"
		nodesAnnot[names(orientation[orientation== "Non DE"]),
			"color"] <- "#BABABA"

		nodesAnnot$x <- nodesAnnot$x *400										## Multiply positions. Otherwise nodes will collapse 
		nodesAnnot$y <- nodesAnnot$y *400 

		visNetworkProxy("ppiNetwork") %>%
			visUpdateNodes(nodesAnnot) %>%
			visOptions(selectedBy= list(variable="groups",multiple=T))

		progress$inc(0.25)																	

		## Save data used into "Data used" tab
		## There may be for some MA datasets that none of 
		## the interacting genes are DE. In that case a 
		## different output must be returned to 'dataUsed' table.
		## e.g.
		## core protein= "DES"
		## species= "Homo sapiens" 
		## tissue= "HFL-1" 
		## comparison= "CtrlIL1_vs_CtrlUntrt"
		## pval= 0.05
		## fc= 1.2
		dataUsed <- do.call("rbind", deg)
		
		if (!is.null(dataUsed)) {
			dataUsed <- dataUsed[, c("ENSGid", "log2FC", "Pval", "GSE")]
			dataUsed$Symbol <- gsub("\\..*", "", rownames(dataUsed))
			dataUsed <- dataUsed[, c(ncol(dataUsed), 1:(ncol(dataUsed)-1))]
			colnames(dataUsed)[c(1:2, 5)] <- c("Gene name", "Code", "DatasetID")
		}

		prtnVals$annotDataUsed <- dataUsed

	})

	## dataUsed table
	output$dataUsed <- DT::renderDataTable({
		
		validate(
			need(!is.null(prtnVals$annotDataUsed),
				"No interacting member was differentially expressed in the datasets defined by parameters chosen."
			)
		)

		out <- prtnVals$annotDataUsed

		## Color log2FC column
		brks <- quantile(out$log2FC, probs= seq(0, 1, 0.1), na.rm= TRUE)
		clrsPal <- colorRampPalette(c("green","white","red"))
		clrs <- clrsPal(length(brks)+1)

		## Color significant FDR red
		out$pvalColor <- ifelse(out$Pval < 0.05, 1, 0)

		dtable <- datatable(
			data= out,
			selection="none", 
			rownames= FALSE,
			filter= "top",
			class= "compact",
			options= list(
				rowsGroup = list(0),
				sDom  = '<"top">lrt<"bottom">ip',
				columnDefs= list(
					list(
						targets= 5, 
						visible = FALSE
					),
					list(
						className= "dt-center",
						targets= "_all"
					)
				) 
			)#,
			#extensions= "Responsive"
		) %>% formatStyle("log2FC",
			backgroundColor= styleInterval(brks, clrs)
		) %>% formatStyle("Pval", "pvalColor",
			backgroundColor= styleEqual(1, "#ff8e8e")
		) %>% formatRound(c("log2FC", "Pval"),
			digits= 5
		)

		path <- "./www"
		depend <- htmltools::htmlDependency("RowsGroup", "2.0.0",
			path, script= "dataTables.rowsGroup.js")
		dtable$dependencies <- c(dtable$dependencies, list(depend))
		return(dtable)
	})

	## Reset annotation parameters======================================================

	## Modal to warn the user
	observeEvent(input$clearAnnotationParams, {
		modal <- modalDialog(												
			title="Attention!",
			"Annotation parameters will be reseted.",
			footer= tagList(
				actionButton("okAnnot","OK"),
				modalButton("Cancel")
			)
		)
		showModal(modal)
	})

	resetAnnotConfirm <- eventReactive(input$okAnnot, {
		removeModal()
		return(TRUE)
	})

	## Reset intersection parameters
	observeEvent(resetAnnotConfirm(), {

		shinyjs::enable("tissueInNet")
		shinyjs::disable("comparisonInNet")

		tissueChoicesNet <- unique(prtnVals$annotData$Tissue)
		updateSelectizeInput(session, 
			inputId= "tissueInNet", 
			label= "Select tissue",
			choices= tissueChoicesNet,
			options= list(
				placeholder= "Select tissue",
				onInitialize= I('function() { this.setValue(""); }')
			)
		)

		updateSelectizeInput(session, 
			inputId="comparisonInNet", 
			label="Select comparison", 
			choices= "",
			options= list(
				placeholder = "Select comparison",
				onInitialize = I('function() { this.setValue(""); }')
			)
		)

		updateNumericInput(session,
			inputId= "pvalInNet",
			label= "Type a p-value",
			value= 0.05,
			step= 0.01 
		)

		updateNumericInput(session,
			inputId= "fcInNet",
			label= "Type a Fold change",
			value= 1.2,
			step= 0.01 
		)

	})

	# ============================================================================
	# Datasets benchmarking tab
	# ============================================================================
	
	# Display benchmarking results when interrogating all coding transcriptomic
	# datasets/contrasts per species and technology
	output$decorRes <- DT::renderDataTable({

		out <- merge(
			dbGetQuery(conn= fibromine_db,
				statement='
					SELECT 
						DatasetsDescription.DatasetID, Reference, ReferenceURL, 
						Datasets.Tech, Datasets.Species, Tissue, DescrContrast, 
						nExpCtrlPostCuration 
					FROM 
						DatasetsDescription 
					JOIN 
						Datasets 
					ON 
						DatasetsDescription.DatasetID = Datasets.DatasetID
					WHERE 
						Datasets.Tech != "Proteome profiling techniques"
				;'
			),
			stars,
			by= c("DatasetID", "DescrContrast"), all.x= TRUE
		)

		url_prefix <- "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc="

		## Transform GSE in url
		out$DatasetID <- paste0("<a href='", url_prefix, out$DatasetID, 
			"' rel='noopener noreferrer' target='_blank'>",					 
		out$DatasetID, "</a>")

		## Set PMID as a url
		out$Reference <- gsub("^https.*pubmed\\/", "", out$ReferenceURL)
		out$Reference <- gsub("^https.*gov\\/", "", out$Reference)

		out$Reference <- paste0("<a href='", out$ReferenceURL,
			"' rel='noopener noreferrer' target='_blank'>", 
			out$Reference, "</a>")
		out[out$Reference == "<a href='-' rel='noopener noreferrer' target='_blank'>-</a>",
			"Reference"] <- "-"
		out[out$Reference == "<a href='https://www.biorxiv.org/content/biorxiv/early/2019/03/23/580498.full.pdf' rel='noopener noreferrer' target='_blank'>https://www.biorxiv.org/content/biorxiv/early/2019/03/23/580498.full.pdf</a>",
			"Reference"] <- "<a href='https://www.biorxiv.org/content/biorxiv/early/2019/03/23/580498.full.pdf' rel='noopener noreferrer' target='_blank'>biorxiv link</a>"

		## Split nExpCtrlPostCuration so as to be properly sortable on the client side
		nSamples <- strsplit(out$nExpCtrlPostCuration, split= "/")
		nSamples <- do.call("rbind", nSamples)
		out$Exp <- as.integer(nSamples[,1])
		out$Ctrl <- suppressWarnings(as.integer(nSamples[,2]))
		# Set the "pooled" Ctrl samples of a specific dataset into 1 (as if they were 1 sample)
		out$Ctrl[is.na(out$Ctrl)] <- 1

		out <- subset(out, select= -c(ReferenceURL, nExpCtrlPostCuration))
		out <- out[,c(1,3,2,7,4:6,8:9)]
		colnames(out) <- c("GEO accession", "PMID", "Comparison", 
			"Stars count", "Technology", "Species", "Tissue", 
			"#Experimental", "#Control")

		## Convert to factors for easier client-side filtering of the table
		out$Technology <- as.factor(out$Technology)
		out$Species <- as.factor(out$Species)
		out$Tissue <- as.factor(out$Tissue)
		out$Comparison <- as.factor(out$Comparison)

		# Color Stars count column
		brks <- 0:7
		clrsPal <- colorRampPalette(c("green", "yellow", "red"))
		clrs <- clrsPal(length(brks)+1)

		datatable(out,
			rownames= FALSE,
			escape= FALSE, 
			filter= "top",
			width= "100%",
			class= "compact",
			options= list(
				sDom= '<"top">lrt<"bottom">ip',
				columnDefs= list(
					list(
						className= "dt-center", 
						targets= "_all"
					)
				),
				scrollX= TRUE
			)
		)  %>% formatStyle("Stars count",
			backgroundColor= styleInterval(brks, clrs)
		)
	})


	# Display benchmarking results when interrogating IPF_vs_Ctrl lung coding transcriptomic
	# datasets/contrasts per technology
	output$decorResII <- DT::renderDataTable({

		out <- merge(
			dbGetQuery(conn= fibromine_db,
				statement='
					SELECT 
						DatasetsDescription.DatasetID, Reference, ReferenceURL, 
						Datasets.Tech, Datasets.Species, Tissue, DescrContrast, 
						nExpCtrlPostCuration 
					FROM 
						DatasetsDescription 
					JOIN 
						Datasets 
					ON 
						DatasetsDescription.DatasetID = Datasets.DatasetID
					WHERE 
						Datasets.Tech != "Proteome profiling techniques"
				;'
			),
			starsII,
			by= c("DatasetID", "DescrContrast"), all.y= TRUE
		)

		url_prefix <- "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc="

		## Transform GSE in url
		out$DatasetID <- paste0("<a href='", url_prefix, out$DatasetID, 
			"' rel='noopener noreferrer' target='_blank'>",					 
		out$DatasetID, "</a>")

		## Set PMID as a url
		out$Reference <- gsub("^https.*pubmed\\/", "", out$ReferenceURL)
		out$Reference <- gsub("^https.*gov\\/", "", out$Reference)

		out$Reference <- paste0("<a href='", out$ReferenceURL,
			"' rel='noopener noreferrer' target='_blank'>", 
			out$Reference, "</a>")
		out[out$Reference == "<a href='-' rel='noopener noreferrer' target='_blank'>-</a>",
			"Reference"] <- "-"
		out[out$Reference == "<a href='https://www.biorxiv.org/content/biorxiv/early/2019/03/23/580498.full.pdf' rel='noopener noreferrer' target='_blank'>https://www.biorxiv.org/content/biorxiv/early/2019/03/23/580498.full.pdf</a>",
			"Reference"] <- "<a href='https://www.biorxiv.org/content/biorxiv/early/2019/03/23/580498.full.pdf' rel='noopener noreferrer' target='_blank'>biorxiv link</a>"

		## Split nExpCtrlPostCuration so as to be properly sortable on the client side
		nSamples <- strsplit(out$nExpCtrlPostCuration, split= "/")
		nSamples <- do.call("rbind", nSamples)
		out$Exp <- as.integer(nSamples[,1])
		out$Ctrl <- suppressWarnings(as.integer(nSamples[,2]))
		# Set the "pooled" Ctrl samples of a specific dataset into 1 (as if they were 1 sample)
		out$Ctrl[is.na(out$Ctrl)] <- 1

		out <- subset(out, select= -c(ReferenceURL, nExpCtrlPostCuration))
		out <- out[,c(1,3,2,7,4:6,8:9)]
		colnames(out) <- c("GEO accession", "PMID", "Comparison", 
			"Stars count", "Technology", "Species", "Tissue", 
			"#Experimental", "#Control")

		## Convert to factors for easier client-side filtering of the table
		out$Technology <- as.factor(out$Technology)
		out$Species <- as.factor(out$Species)
		out$Tissue <- as.factor(out$Tissue)
		out$Comparison <- as.factor(out$Comparison)

		# Color Stars count column
		brks <- 0:7
		clrsPal <- colorRampPalette(c("green", "yellow", "red"))
		clrs <- clrsPal(length(brks)+1)

		datatable(out,
			rownames= FALSE,
			escape= FALSE, 
			filter= "top",
			width= "100%",
			class= "compact",
			options= list(
				sDom= '<"top">lrt<"bottom">ip',
				columnDefs= list(
					list(
						className= "dt-center", 
						targets= "_all"
					)
				),
				scrollX= TRUE
			)
		)  %>% formatStyle("Stars count",
			backgroundColor= styleInterval(brks, clrs)
		)
	})

	# Display benchmarking results when interrogating BleomD14_vs_Ctrl lung coding transcriptomic
	# datasets/contrasts per technology
	output$decorResIII <- DT::renderDataTable({

		out <- merge(
			dbGetQuery(conn= fibromine_db,
				statement='
					SELECT 
						DatasetsDescription.DatasetID, Reference, ReferenceURL, 
						Datasets.Tech, Datasets.Species, Tissue, DescrContrast, 
						nExpCtrlPostCuration 
					FROM 
						DatasetsDescription 
					JOIN 
						Datasets 
					ON 
						DatasetsDescription.DatasetID = Datasets.DatasetID
					WHERE 
						Datasets.Tech != "Proteome profiling techniques"
				;'
			),
			starsIII,
			by= c("DatasetID", "DescrContrast"), all.y= TRUE
		)

		url_prefix <- "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc="

		## Transform GSE in url
		out$DatasetID <- paste0("<a href='", url_prefix, out$DatasetID, 
			"' rel='noopener noreferrer' target='_blank'>",					 
		out$DatasetID, "</a>")

		## Set PMID as a url
		out$Reference <- gsub("^https.*pubmed\\/", "", out$ReferenceURL)
		out$Reference <- gsub("^https.*gov\\/", "", out$Reference)

		out$Reference <- paste0("<a href='", out$ReferenceURL,
			"' rel='noopener noreferrer' target='_blank'>", 
			out$Reference, "</a>")
		out[out$Reference == "<a href='-' rel='noopener noreferrer' target='_blank'>-</a>",
			"Reference"] <- "-"
		out[out$Reference == "<a href='https://www.biorxiv.org/content/biorxiv/early/2019/03/23/580498.full.pdf' rel='noopener noreferrer' target='_blank'>https://www.biorxiv.org/content/biorxiv/early/2019/03/23/580498.full.pdf</a>",
			"Reference"] <- "<a href='https://www.biorxiv.org/content/biorxiv/early/2019/03/23/580498.full.pdf' rel='noopener noreferrer' target='_blank'>biorxiv link</a>"

		## Split nExpCtrlPostCuration so as to be properly sortable on the client side
		nSamples <- strsplit(out$nExpCtrlPostCuration, split= "/")
		nSamples <- do.call("rbind", nSamples)
		out$Exp <- as.integer(nSamples[,1])
		out$Ctrl <- suppressWarnings(as.integer(nSamples[,2]))
		# Set the "pooled" Ctrl samples of a specific dataset into 1 (as if they were 1 sample)
		out$Ctrl[is.na(out$Ctrl)] <- 1

		out <- subset(out, select= -c(ReferenceURL, nExpCtrlPostCuration))
		out <- out[,c(1,3,2,7,4:6,8:9)]
		colnames(out) <- c("GEO accession", "PMID", "Comparison", 
			"Stars count", "Technology", "Species", "Tissue", 
			"#Experimental", "#Control")

		## Convert to factors for easier client-side filtering of the table
		out$Technology <- as.factor(out$Technology)
		out$Species <- as.factor(out$Species)
		out$Tissue <- as.factor(out$Tissue)
		out$Comparison <- as.factor(out$Comparison)

		# Color Stars count column
		brks <- 0:7
		clrsPal <- colorRampPalette(c("green", "yellow", "red"))
		clrs <- clrsPal(length(brks)+1)

		datatable(out,
			rownames= FALSE,
			escape= FALSE, 
			filter= "top",
			width= "100%",
			class= "compact",
			options= list(
				sDom= '<"top">lrt<"bottom">ip',
				columnDefs= list(
					list(
						className= "dt-center", 
						targets= "_all"
					)
				),
				scrollX= TRUE
			)
		)  %>% formatStyle("Stars count",
			backgroundColor= styleInterval(brks, clrs)
		)
	})

	# Display benchmarking results when interrogating non-coding microarray datasets
	output$decorResIV <- DT::renderDataTable({

		out <- merge(
			dbGetQuery(conn= fibromine_db,
				statement='
					SELECT 
						DatasetsDescription.DatasetID, Reference, ReferenceURL, 
						Datasets.Tech, Datasets.Species, Tissue, DescrContrast, 
						nExpCtrlPostCuration 
					FROM 
						DatasetsDescription 
					JOIN 
						Datasets 
					ON 
						DatasetsDescription.DatasetID = Datasets.DatasetID
					WHERE 
						Datasets.Tech != "Proteome profiling techniques"
				;'
			),
			starsIV,
			by= c("DatasetID", "DescrContrast"), all.y= TRUE
		)

		url_prefix <- "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc="

		## Transform GSE in url
		out$DatasetID <- paste0("<a href='", url_prefix, out$DatasetID, 
			"' rel='noopener noreferrer' target='_blank'>",					 
		out$DatasetID, "</a>")

		## Set PMID as a url
		out$Reference <- gsub("^https.*pubmed\\/", "", out$ReferenceURL)
		out$Reference <- gsub("^https.*gov\\/", "", out$Reference)

		out$Reference <- paste0("<a href='", out$ReferenceURL,
			"' rel='noopener noreferrer' target='_blank'>", 
			out$Reference, "</a>")
		out[out$Reference == "<a href='-' rel='noopener noreferrer' target='_blank'>-</a>",
			"Reference"] <- "-"
		out[out$Reference == "<a href='https://www.biorxiv.org/content/biorxiv/early/2019/03/23/580498.full.pdf' rel='noopener noreferrer' target='_blank'>https://www.biorxiv.org/content/biorxiv/early/2019/03/23/580498.full.pdf</a>",
			"Reference"] <- "<a href='https://www.biorxiv.org/content/biorxiv/early/2019/03/23/580498.full.pdf' rel='noopener noreferrer' target='_blank'>biorxiv link</a>"

		## Split nExpCtrlPostCuration so as to be properly sortable on the client side
		nSamples <- strsplit(out$nExpCtrlPostCuration, split= "/")
		nSamples <- do.call("rbind", nSamples)
		out$Exp <- as.integer(nSamples[,1])
		out$Ctrl <- suppressWarnings(as.integer(nSamples[,2]))
		# Set the "pooled" Ctrl samples of a specific dataset into 1 (as if they were 1 sample)
		out$Ctrl[is.na(out$Ctrl)] <- 1

		out <- subset(out, select= -c(ReferenceURL, nExpCtrlPostCuration))
		out <- out[,c(1,3,2,7,4:6,8:9)]
		colnames(out) <- c("GEO accession", "PMID", "Comparison", 
			"Stars count", "Technology", "Species", "Tissue", 
			"#Experimental", "#Control")

		## Convert to factors for easier client-side filtering of the table
		out$Technology <- as.factor(out$Technology)
		out$Species <- as.factor(out$Species)
		out$Tissue <- as.factor(out$Tissue)
		out$Comparison <- as.factor(out$Comparison)

		# Color Stars count column
		brks <- 0:7
		clrsPal <- colorRampPalette(c("green", "yellow", "red"))
		clrs <- clrsPal(length(brks)+1)

		datatable(out,
			rownames= FALSE,
			escape= FALSE, 
			filter= "top",
			class= "compact", 
			width= "100%",
			options= list(
				sDom= '<"top">lrt<"bottom">ip',
				columnDefs= list(
					list(
						className= "dt-center", 
						targets= "_all"
					)
				),
				scrollX= TRUE
			)
		)  %>% formatStyle("Stars count",
			backgroundColor= styleInterval(brks, clrs)
		)
	})

	# ======================================================================
	# "Dataset explorer" --> "Benchmarking backstage" --> 
	# "All trans. coding datasets"
	# ======================================================================
	starsExt <- read.delim("./www/benchBackstage/allCoding/gseHsaMmu_extensive.txt")

	output$benchAllCoding <- DT::renderDataTable({

		out <- merge(
			dbGetQuery(conn= fibromine_db,
				statement='
					SELECT 
						DatasetsDescription.DatasetID, Reference, ReferenceURL, 
						Datasets.Tech, Tissue, DescrContrast, 
						nExpCtrlPostCuration 
					FROM 
						DatasetsDescription 
					JOIN 
						Datasets 
					ON 
						DatasetsDescription.DatasetID = Datasets.DatasetID
					WHERE 
						Datasets.Tech != "Proteome profiling techniques"
				;'
			),
			starsExt,
			by= c("DatasetID", "DescrContrast"), all.y= TRUE
		)

		url_prefix <- "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc="

		## Transform GSE in url
		out$DatasetID <- paste0("<a href='", url_prefix, out$DatasetID, 
			"' rel='noopener noreferrer' target='_blank'>",					 
		out$DatasetID, "</a>")

		## Set PMID as a url
		out$Reference <- gsub("^https.*pubmed\\/", "", out$ReferenceURL)
		out$Reference <- gsub("^https.*gov\\/", "", out$Reference)

		out$Reference <- paste0("<a href='", out$ReferenceURL,
			"' rel='noopener noreferrer' target='_blank'>", 
			out$Reference, "</a>")
		out[out$Reference == "<a href='-' rel='noopener noreferrer' target='_blank'>-</a>",
			"Reference"] <- "-"
		out[out$Reference == "<a href='https://www.biorxiv.org/content/biorxiv/early/2019/03/23/580498.full.pdf' rel='noopener noreferrer' target='_blank'>https://www.biorxiv.org/content/biorxiv/early/2019/03/23/580498.full.pdf</a>",
			"Reference"] <- "<a href='https://www.biorxiv.org/content/biorxiv/early/2019/03/23/580498.full.pdf' rel='noopener noreferrer' target='_blank'>biorxiv link</a>"

		# Keep important columns
		out <- out[,c("ID", "Reference", "Species", "Tissue", "Technology", 
			"genesStar", "detectedStar", "degsStar", "ratioStar", "fcBinsStar",
			"pvalStar", "adjpStar")]
		colnames(out)[2] <- "PMID"

		## Convert to factors for easier client-side filtering of the table
		out$ID <- as.factor(out$ID)
		out$Technology <- as.factor(out$Technology)
		out$Species <- as.factor(out$Species)
		out$Tissue <- as.factor(out$Tissue)

		datatable(out,
			rownames= FALSE,
			escape= FALSE, 
			filter= "top",
			class= "compact", 
			width= "100%",
			options= list(
				sDom= '<"top">lrt<"bottom">ip',
				columnDefs= list(
					list(
						className= "dt-center", 
						targets= "_all"
					)
				),
				scrollX= TRUE
			)
		)
	})

	# ======================================================================
	# "Dataset explorer" --> "Benchmarking backstage" --> 
	# "IPF_vs_Ctrl lung coding"
	# ======================================================================
	starsIIExt <- read.delim("./www/benchBackstage/IPF_vs_Ctrl/ipf_vs_ctrl_lung_coding_extensive.txt")

	output$benchIPFCtrl <- DT::renderDataTable({

		out <- merge(
			dbGetQuery(conn= fibromine_db,
				statement='
					SELECT 
						DatasetsDescription.DatasetID, Reference, ReferenceURL, 
						Datasets.Tech, Tissue, DescrContrast, 
						nExpCtrlPostCuration 
					FROM 
						DatasetsDescription 
					JOIN 
						Datasets 
					ON 
						DatasetsDescription.DatasetID = Datasets.DatasetID
					WHERE 
						Datasets.Tech != "Proteome profiling techniques"
				;'
			),
			starsIIExt,
			by= c("DatasetID", "DescrContrast"), all.y= TRUE
		)

		url_prefix <- "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc="

		## Transform GSE in url
		out$DatasetID <- paste0("<a href='", url_prefix, out$DatasetID, 
			"' rel='noopener noreferrer' target='_blank'>",					 
		out$DatasetID, "</a>")

		## Set PMID as a url
		out$Reference <- gsub("^https.*pubmed\\/", "", out$ReferenceURL)
		out$Reference <- gsub("^https.*gov\\/", "", out$Reference)

		out$Reference <- paste0("<a href='", out$ReferenceURL,
			"' rel='noopener noreferrer' target='_blank'>", 
			out$Reference, "</a>")
		out[out$Reference == "<a href='-' rel='noopener noreferrer' target='_blank'>-</a>",
			"Reference"] <- "-"
		out[out$Reference == "<a href='https://www.biorxiv.org/content/biorxiv/early/2019/03/23/580498.full.pdf' rel='noopener noreferrer' target='_blank'>https://www.biorxiv.org/content/biorxiv/early/2019/03/23/580498.full.pdf</a>",
			"Reference"] <- "<a href='https://www.biorxiv.org/content/biorxiv/early/2019/03/23/580498.full.pdf' rel='noopener noreferrer' target='_blank'>biorxiv link</a>"

		# Keep important columns
		out <- out[,c("ID", "Reference", "Technology", "genesStar", "detectedStar", "degsStar",
			"ratioStar", "fcBinsStar", "pvalStar", "adjpStar")]
		colnames(out)[2] <- "PMID"

		## Convert to factors for easier client-side filtering of the table
		out$ID <- as.factor(out$ID)
		out$Technology <- as.factor(out$Technology)

		datatable(out,
			rownames= FALSE,
			escape= FALSE, 
			filter= "top",
			class= "compact", 
			width= "100%",
			options= list(
				sDom= '<"top">lrt<"bottom">ip',
				columnDefs= list(
					list(
						className= "dt-center", 
						targets= "_all"
					)
				),
				scrollX= TRUE
			)
		)
	})

	# ======================================================================
	# "Dataset explorer" --> "Benchmarking backstage" --> 
	# "BleomD14_vs_Ctrl lung coding"
	# ======================================================================
	starsIIIExt <- read.delim("./www/benchBackstage/BleomD14_vs_Ctrl/bleoD14_vs_ctrl_lung_coding_extensive.txt")

	output$benchBleomD14Ctrl <- DT::renderDataTable({

		out <- merge(
			dbGetQuery(conn= fibromine_db,
				statement='
					SELECT 
						DatasetsDescription.DatasetID, Reference, ReferenceURL, 
						Datasets.Tech, Tissue, DescrContrast, 
						nExpCtrlPostCuration 
					FROM 
						DatasetsDescription 
					JOIN 
						Datasets 
					ON 
						DatasetsDescription.DatasetID = Datasets.DatasetID
					WHERE 
						Datasets.Tech != "Proteome profiling techniques"
				;'
			),
			starsIIIExt,
			by= c("DatasetID", "DescrContrast"), all.y= TRUE
		)

		url_prefix <- "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc="

		## Transform GSE in url
		out$DatasetID <- paste0("<a href='", url_prefix, out$DatasetID, 
			"' rel='noopener noreferrer' target='_blank'>",					 
		out$DatasetID, "</a>")

		## Set PMID as a url
		out$Reference <- gsub("^https.*pubmed\\/", "", out$ReferenceURL)
		out$Reference <- gsub("^https.*gov\\/", "", out$Reference)

		out$Reference <- paste0("<a href='", out$ReferenceURL,
			"' rel='noopener noreferrer' target='_blank'>", 
			out$Reference, "</a>")
		out[out$Reference == "<a href='-' rel='noopener noreferrer' target='_blank'>-</a>",
			"Reference"] <- "-"
		out[out$Reference == "<a href='https://www.biorxiv.org/content/biorxiv/early/2019/03/23/580498.full.pdf' rel='noopener noreferrer' target='_blank'>https://www.biorxiv.org/content/biorxiv/early/2019/03/23/580498.full.pdf</a>",
			"Reference"] <- "<a href='https://www.biorxiv.org/content/biorxiv/early/2019/03/23/580498.full.pdf' rel='noopener noreferrer' target='_blank'>biorxiv link</a>"

		# Keep important columns
		out <- out[,c("ID", "Reference", "Technology", "genesStar", "detectedStar", "degsStar",
			"ratioStar", "fcBinsStar", "pvalStar", "adjpStar")]
		colnames(out)[2] <- "PMID"

		## Convert to factors for easier client-side filtering of the table
		out$ID <- as.factor(out$ID)
		out$Technology <- as.factor(out$Technology)

		datatable(out,
			rownames= FALSE,
			escape= FALSE, 
			filter= "top",
			class= "compact", 
			width= "100%",
			options= list(
				sDom= '<"top">lrt<"bottom">ip',
				columnDefs= list(
					list(
						className= "dt-center", 
						targets= "_all"
					)
				),
				scrollX= TRUE
			)
		)
	})

	# ======================================================================
	# "Dataset explorer" --> "Benchmarking backstage" --> 
	# "All non-coding datasets"
	# ======================================================================
	starsExtIV <- read.delim("./www/benchBackstage/nonCoding/gseNonCoding_extensive.txt")

	output$benchNonCoding <- DT::renderDataTable({

		out <- merge(
			dbGetQuery(conn= fibromine_db,
				statement='
					SELECT 
						DatasetsDescription.DatasetID, Reference, ReferenceURL, 
						Datasets.Tech, Tissue, DescrContrast, 
						nExpCtrlPostCuration 
					FROM 
						DatasetsDescription 
					JOIN 
						Datasets 
					ON 
						DatasetsDescription.DatasetID = Datasets.DatasetID
					WHERE 
						Datasets.Tech != "Proteome profiling techniques"
				;'
			),
			starsExtIV,
			by= c("DatasetID", "DescrContrast"), all.y= TRUE
		)

		url_prefix <- "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc="

		## Transform GSE in url
		out$DatasetID <- paste0("<a href='", url_prefix, out$DatasetID, 
			"' rel='noopener noreferrer' target='_blank'>",					 
		out$DatasetID, "</a>")

		## Set PMID as a url
		out$Reference <- gsub("^https.*pubmed\\/", "", out$ReferenceURL)
		out$Reference <- gsub("^https.*gov\\/", "", out$Reference)

		out$Reference <- paste0("<a href='", out$ReferenceURL,
			"' rel='noopener noreferrer' target='_blank'>", 
			out$Reference, "</a>")
		out[out$Reference == "<a href='-' rel='noopener noreferrer' target='_blank'>-</a>",
			"Reference"] <- "-"
		out[out$Reference == "<a href='https://www.biorxiv.org/content/biorxiv/early/2019/03/23/580498.full.pdf' rel='noopener noreferrer' target='_blank'>https://www.biorxiv.org/content/biorxiv/early/2019/03/23/580498.full.pdf</a>",
			"Reference"] <- "<a href='https://www.biorxiv.org/content/biorxiv/early/2019/03/23/580498.full.pdf' rel='noopener noreferrer' target='_blank'>biorxiv link</a>"

		# Keep important columns
		out <- out[,c("ID", "Reference", "Species", "Tissue", "Technology", 
			"detectedStar", "degsStar", "ratioStar", "fcBinsStar",
			"pvalStar", "adjpStar")]
		colnames(out)[2] <- "PMID"

		## Convert to factors for easier client-side filtering of the table
		out$ID <- as.factor(out$ID)
		out$Technology <- as.factor(out$Technology)
		out$Species <- as.factor(out$Species)
		out$Tissue <- as.factor(out$Tissue)

		datatable(out,
			rownames= FALSE,
			escape= FALSE, 
			filter= "top",
			class= "compact", 
			width= "100%",
			options= list(
				sDom= '<"top">lrt<"bottom">ip',
				columnDefs= list(
					list(
						className= "dt-center", 
						targets= "_all"
					)
				),
				scrollX= TRUE
			)
		)
	})

	# ============================================================================
	# Single cell data tab
	# ============================================================================
	scDatasets <- reactiveValues(mainScTable= NULL)

	## Shape Single cell tab's main table================================================================
	scDatasets$mainScTable <- dbGetQuery(
		conn= fibromine_db,
		statement='SELECT * FROM scDatasetsDescription;'
	)

	output$scDatasetsTable <- DT::renderDataTable({

		out <- scDatasets$mainScTable

		## Transform GSE in url
		out$DatasetID <- paste0("<a href='", out$DatasetURL, 
			"' rel='noopener noreferrer' target='_blank'>",					 
			out$DatasetID, "</a>"
		)
		out[out$DatasetID == "<a href='-' rel='noopener noreferrer' target='_blank'>-</a>",
			"DatasetID"] <- "-"

		## Set PMID as a url
		out$Reference <- gsub("https://pubmed.ncbi.nlm.nih.gov/|/", "", out$ReferenceURL)
		out$Reference <- paste0("<a href='", out$ReferenceURL, 
			"' rel='noopener noreferrer' target='_blank'>",					 
			out$Reference, "</a>"
		)

		## Format table
		out <- subset(out, select= -c(ID, DatasetURL, ReferenceURL, nCells))
		colnames(out)[c(2,6)] <- c("PMID","#Exp/#Ctrl")

		## Convert to factors for easier client-side filtering of the table
		out$Species <- as.factor(out$Species)
		out$Tissue <- as.factor(out$Tissue)
		out$Comparison <- as.factor(out$Comparison)

		datatable(out, 
			selection = "none", 
			rownames= FALSE,
			escape= FALSE, 
			filter= "top",
			class= "compact", 
			options= list(
				sDom= '<"top">lrt<"bottom">ip', # to enable search bar use "flrt" instead (f for Filtering)
				columnDefs= list(
					list(
						className= "dt-center", 
						targets= "_all"
					)
				)
			)#,	
	  		#extensions= "Responsive",
	  	)
	})
	
	# ============================================================================
	# Download data tab
	# ============================================================================
	output$normData <- downloadHandler(
		filename= function() {
			paste0(input$normDataRequest, 
				"normalizedExpressionValues", 
				"_", Sys.Date(), ".xlsx"
			)
		},
		content= function(file) {
			## Get the normalized values
			datasetID <- input$normDataRequest

			progress <- shiny::Progress$new()
			on.exit(progress$close())
			progress$set(message= "Retrieving data", value=0)
			progress$inc(0.25)

			normValues <- dbGetQuery(
				conn= fibromine_db,
				statement= '
					SELECT 
						* 
					FROM 
						NormExprValues 
					WHERE
						GSE = :x
				;',
				params= list(x= datasetID)
			)
			progress$inc(0.5)

			## Reconstruct expression values table
			outTemp <- split(normValues, f= normValues$GSMcode)
			outTemp <- lapply(outTemp, function(x) {
				out <- data.frame(x$NormValues, stringsAsFactors=FALSE)
			})
			out <- data.frame(matrix(unlist(outTemp), ncol=length(outTemp),
				byrow=FALSE), stringsAsFactors= FALSE)
			out$Name <- normValues$Symbol[1:nrow(out)]
			colnames(out) <- c(names(outTemp),"Name")
			out <- cbind(Name= out$Name, out[, (1:ncol(out))-1])
			progress$inc(0.25)

			write.xlsx(out, file)
		}
	)

	output$protData <- downloadHandler(
		filename= function() {
			paste0(input$protDataRequest, 
				"_", "expressionData", 
				"_", Sys.Date(), ".xlsx"
			)
		},
		content= function(file) {
			datasetID <- input$protDataRequest

			progress <- shiny::Progress$new()
			on.exit(progress$close())
			progress$set(message= "Retrieving data", value=0)
			progress$inc(0.25)

			out <- dbGetQuery(
				conn= fibromine_db,
				statement= '
					SELECT 
						DEP.UniprotAC, GeneAnnotation.Symbol,
						GeneAnnotation.ENSGid, DEP.ExpressionDirection, 
						DEP.DatasetID, DEP.Contrast
					FROM 
						DEP JOIN ENSGid2UniP JOIN GeneAnnotation
					ON 
						DEP.UniprotAC = ENSGid2UniP.UniprotAC
					AND
						ENSGid2UniP.ENSGid = GeneAnnotation.ENSGid
					WHERE
						DatasetID = :x
				;',
				params= list(x= datasetID)
			)
			colnames(out)[ncol(out)] <- "Comparison"
			write.xlsx(out, file)
		}
	)

	# ============================================================================
	# Remove 'temp' DB table created for network plotting purposes
	# ============================================================================
	
	## On session ended (local use)
	session$onSessionEnded(function() {
		tbl <- paste("temp", session$token, sep="_")
		if (tbl %in% dbListTables(fibromine_db))
			dbRemoveTable(fibromine_db, tbl)		
		stopApp()
	})

	## On browser closed 
	if(!interactive()) {
		session$onSessionEnded(function() { 
			tbl <- paste("temp", session$token, sep="_")
			if (tbl %in% dbListTables(fibromine_db))
				dbRemoveTable(fibromine_db, tbl)
    		stopApp()
    	})
	}

})
