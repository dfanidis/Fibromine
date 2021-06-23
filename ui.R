# Fibromine "ui.R" for Shiny Server

# ============================================================================
# Source global objects
# ============================================================================
source("./global.R")

# ============================================================================
# shinyUI()
# ============================================================================
shinyUI(dashboardPage(
	dashboardHeader(
		title= "Fibromine"
	),
	dashboardSidebar(
		introjsUI(), # required for rintrojs to function properly
		sidebarMenu(
			id = "tabs",

			## menuItems
			menuItem("Home", tabName= "Home", icon= shiny::icon("home")),

			menuItem("Dataset explorer", tabName= "ByDataset", icon= shiny::icon("database"),
				menuItem("Transcriptomics datasets", tabName = "ByTransDataset"),
				menuItem("Proteomic datasets", tabName = "ByProtDataset")
			) %>% add_class("step_1"),
			menuItem("Gene explorer", tabName= "ByGene", icon= shiny::icon("dna")) %>% add_class("step_2"),
			menuItem("Protein explorer", tabName= "ByProtein", icon= shiny::icon("arrows-alt")) %>% add_class("step_3"),
			menuItem("Datasets benchmarking", tabName= "Decor", icon= shiny::icon("star")) %>% add_class("step_4"),
			menuItem("Single cell data", tabName= "SingleCell", icon= shiny::icon("braille")) %>% add_class("step_5"),

			menuItem("Download data", tabName= "DownData", icon= shiny::icon("download")) %>% add_class("step_6"),
			menuItem("Docs", tabName= "Docs", icon= shiny::icon("book")) %>% add_class("step_7"),
			menuItem("How to", tabName= "HowTo", icon= shiny::icon("question")) %>% add_class("step_8"),

			menuItem("About us", icon= shiny::icon("info"),href= "https://www.fleming.gr/aidinis-lab") %>% add_class("step_9"),
			menuItem("Report issues", icon= shiny::icon("github"), href= "https://github.com/dfanidis/Fibromine") %>% add_class("step_10")
		)
	),
	dashboardBody(
		tabItems(
			# ============================================================================
			# Home tab
			# ============================================================================
			tabItem(tabName= "Home",
				wellPanel(
					fluidRow(
						h1("Fibromine"),
						h3("An interactive multi-omics data mining tool for Idiopathic Pulmonary Fibrosis")
					),
					fluidRow(
						column(
							actionButton(
								inputId= "introHome", 
								label= "About the tool", 
								icon= shiny::icon("info-circle"), 
								style= "background-color: #008d4c; color: white;
									border-color: #008d4c;"
							),
							bsTooltip(
								id= "introHome",
								title= "General info about Fibromine",
								placement= "bottom"
							),
							width= 1,
							offset= 5
						)
					),
					fluidRow(
						column(
							includeHTML("./www/aboutIPF.html"),
							width= 4
						),
						column(
							includeHTML("./www/aboutFibromine.html"),
							tags$script(HTML(
								'$(document).keyup(function(event) {
									if ($("#geneName").is(":focus") && (event.key == "Enter" && !event.shiftKey)) {
										$("#geneNameSearch").click();
									}
									if ($("#geneNamePPI").is(":focus") && (event.key == "Enter" && !event.shiftKey)) {
										$("#geneNameSearchPPI").click();
									}
								})'
							)),
							width= 4
						),
						column(
							h2("Useful links"),
							tags$hr(),

							fluidRow(
								column(
									tags$a(
										imageOutput("gene",
											inline= TRUE
										),
										href= "https://www.ncbi.nlm.nih.gov/gene/",
										rel= "noopener noreferrer",
										target= "_blank"
									),
									tags$br(),

									tags$a(
										imageOutput("ensembl",
											inline= TRUE
										),
										href= "http://www.ensembl.org/index.html",
										rel= "noopener noreferrer",
										target= "_blank"
									),
									tags$br(),

									tags$a(
										imageOutput("uniprot",
											inline= TRUE
										),
										href= "https://www.uniprot.org/",
										rel= "noopener noreferrer",
										target= "_blank"
									),
									tags$br(),

									width= 6,
									# Increase padding to the right to 
									# avoid colliding of the logo images
									# with those in the right column
									style= "padding-left:0px;
										padding-top:5px;
										padding-bottom:5px;
										padding-right:2px;"

								),
								column(
									tags$a(
										imageOutput("pulmon",
											inline= TRUE
										),
										href= "http://pulmondb.liigh.unam.mx/",
										rel= "noopener noreferrer",
										target= "_blank"
									),
									tags$br(),

									tags$a(
										imageOutput("nupulmon",
											inline= TRUE
										),
										href= "https://www.nupulmonary.org/",
										rel= "noopener noreferrer",
										target= "_blank"
									),
									tags$br(),

									tags$a(
										imageOutput("ipfatlas",
											inline= TRUE
										),
										href= "http://www.ipfcellatlas.com/",
										rel= "noopener noreferrer",
										target= "_blank"
									),
									width= 6,
									# Increase padding to the left to 
									# avoid colliding of the logo images
									# with those in the left column
									style= "padding-left:2px;
										padding-top:5px;
										padding-bottom:5px;
										padding-right:0px;"
								),
								width= 12
							),
							fluidRow(
								column(
									tags$a(
										imageOutput("string",
											inline= TRUE
										),
										href= "https://string-db.org/",
										rel= "noopener noreferrer",
										target= "_blank"
									),
									tags$br(),

									tags$a(
										imageOutput("go",
											inline= TRUE
										),
										href= "http://geneontology.org/",
										rel= "noopener noreferrer",
										target= "_blank"
									),
									tags$br(),
									width= 6
								),
								column(
									tags$a(
										imageOutput("lungAgingAtlas",
											inline= TRUE
										),
										href= "http://146.107.176.18:3838/MLAA_backup/",
										rel= "noopener noreferrer",
										target= "_blank"
									),
									tags$br(),
									tags$a(
										imageOutput("emouse",
											inline= TRUE
										),
										href= "http://www.fleming.gr/emouse/",
										rel= "noopener noreferrer",
										target= "_blank"
									),
									tags$br(),
									width= 6
								),
								width= 12
							),
							width= 4
						)
					)
				)
			),

			# ============================================================================
			# Dataset explorer tab
			# ============================================================================

			# Transcriptomic datasets tabItem
			tabItem(tabName= "ByTransDataset",
				fluidRow(
					includeCSS("./www/styles.css"),
					useShinyjs(),
					fluidRow(
						h3("Transcriptomic datasets explorer") %>% add_class("step1_trans"),
					),
					fluidRow(
						column(width=6, align= "center",
							actionButton(
								inputId= "introTrans",
								label= "About",
								icon= shiny::icon("info-circle"), 
								style= "background-color: #008d4c; color: white;
									border-color: #008d4c;"
							),
							bsTooltip(
								id= "introTrans",
								title= paste0("Info about exploring the transcriptomic datasets"),
								placement= "bottom"
							),
							offset= 3
						)
					),
					shinydashboard::tabBox(
						id= "transDatasetsBox",
						tabPanel(title= "Datasets",
							fluidRow(
								tipify(
									el= h3("Datasets table",shiny::icon("question-circle")) %>% add_class("step1_trans"),
									title= paste0("Click one or multiple rows (they will turn blue) ",
										"to choose a dataset(-s) and then press the Search button ",
										"to explore. Results are presented at the next 2 tabs of the explorer. ",
										"Use the GEO accession, Reference and Platform ",
										"column hyperlinks for more information about a dataset."
									),
									placement= "bottom"
								)
							),
							fluidRow(
								column(width= 4,
									actionButton(
										inputId= "transDtstsSearch",
										label= "Search",
										icon= shiny::icon("search"),
										style="background-color: #d42132; color: white;
											border-color: #d42132;"
									) %>% add_class("step2_trans"),
									bsTooltip(
										id= "transDtstsSearch",
										title= paste0("Choose one or multiple datasets from ", 
											"the table below and then press search. ",
											"Results are presented at the next 2 tabs of the explorer."	
										),
										placement="bottom"
									),
									actionButton(
										inputId= "clearTransDtstsSearch", 
										label= "Reset", 
										icon= shiny::icon("refresh"), 
										style= " background-color: #008d4c; color: white;
											border-color: #008d4c"
									) %>% add_class("step3_trans"),
									bsTooltip(
										id= "clearTransDtstsSearch",
										title= "Clear dataset selection.",
										placement="bottom"
									),
									actionButton(
										inputId= "selectTransAll",
										label= "Select all",
										icon= shiny::icon("check"),
										style="background-color: #d42132; color: white;
											border-color: #d42132;"
									),
									bsTooltip(
										id= "selectTransAll",
										title= paste0("Select all table rows (based on current ",
											"used filters). To select specific datasets ",
											"click the respective table row."
										),
										placement= "top"
									),
									offset= 8
								)
							),
							fluidRow(
								column(width= 12,
									DT::dataTableOutput("datasetsTable")
								)
							),
							width= 12
						), # Close "Datasets" tab
						tabPanel(title= "DEA statistics",
							fluidRow(
								column(width=12, 
									tipify(
										h3("Common DEGs and DEPs",
											shiny::icon("question-circle")
										),
										title= paste0("Common DEGs and respective DEPS across selected ",
											"datasets using p-value and FC thresholds ",
											"of 0.05 and 1.2 respectively."
										)
									)
								)
							),
							fluidRow(
								column(width=6, align= "center",
									actionButton(
										inputId= "introTransStats",
										label= "About",
										icon= shiny::icon("info-circle"), 
										style= "background-color: #008d4c; color: white;
											border-color: #008d4c;"
									),
									bsTooltip(
										id= "introTransStats",
										title= paste0("Info about exploration/integration results"),
										placement= "bottom"
									),
									offset= 3
								)
							),
							fluidRow(
								column(width=2,
									wellPanel(
										h5("Change DEA\nthresholds"),
										numericInput(
											inputId= "pvalCommonIn",
											label= "Type a p-value",
											value= 0.05,
											step= 0.01
										),
										numericInput(
											inputId= "fcCommonIn",
											label= "Type a Fold change",
											value= 1.2,
											step= 0.01
										),
										actionButton(
											inputId= "filterCommon",
											label= "Filter",
											icon = shiny::icon("filter"),
											style= "background-color: #d42132; color: white;
												border-color: #d42132;"
										),
										bsTooltip(
											id= "filterCommon",
											title= paste0(
												"Use the above control panel to change ",
												"thresholds and then press this button to ",
												"apply changes."
											)
										)
									) %>% add_class("step2_transStats"),
									wellPanel(
										h5("Pathway\nanalyses"),
										helpText("Interrogate one/multiple datasets first."),
										radioButtons(
											inputId= "paChoice",
											label= "Choose a pathway analysis method",
											choices= c(
												"ORA" = "ora"#,
												# "Pre-ranked GSEA" = "preRnk"
											)
										),
										actionButton(
											inputId= "paRun",
											label="Go",
											icon= shiny::icon("cog"),
											style="background-color: #d42132; color: white;
												border-color: #d42132;"
										),
										helpText(paste("Note: Filter the 'Database' column of the 'Pathway analysis results'",
											"table to interrogate enrichment of terms from different databases",
											"including a COVID-19 gene set(!).")
										)
									)  %>% add_class("step3_transStats")
								),
								column(width=10,
									shinydashboard::tabBox(
										id= "degStatBox",
										tabPanel("Transcriptomics summary",
											DT::dataTableOutput("degStatsSum",
												) %>% withSpinner(color="#008d4c"),

											downloadButton(outputId= "degSumAll",
												label="Download table .xlsx",
												style= "background-color: #d42132; color: white;
													border-color: #d42132;"
											),
											bsTooltip(
												id= "degSumAll", 
												title= "Download the whole table.",
												placement= "top"
											),

											downloadButton(outputId= "degSumFiltered",
												label="Download filtered data .xlsx",
												style= "background-color: #d42132; color: white;
													border-color: #d42132;"
											),
											bsTooltip(
												id= "degSumFiltered", 
												title= paste0("Filter any column of the table and then press ", 
													"this button to download the filtered data."),
												placement= "top"
											)
										),
										tabPanel("Proteomics summary",
											DT::dataTableOutput("depSum",
												) %>% withSpinner(color="#008d4c"),

											downloadButton(outputId= "depSumDw",
												label="Download table .xlsx",
												style= "background-color: #d42132; color: white;
													border-color: #d42132;"
											),
											bsTooltip(
												id= "depSumDw", 
												title= "Download the whole table.",
												placement= "top"
											)
										),
										tabPanel("Transcriptomics analytically",
											uiOutput(outputId= "commonDegsHelp"),
											
											DT::dataTableOutput("degStats",
												) %>% withSpinner(color="#008d4c"),

											downloadButton(outputId= "degStatAll",
												label="Download table .xlsx",
												style= "background-color: #d42132; color: white;
													border-color: #d42132;"
											),
											bsTooltip(
												id= "degStatAll", 
												title= "Download the whole table.",
												placement= "top"
											),
											downloadButton(outputId= "degStatFiltered",
												label="Download filtered data .xlsx",
												style= "background-color: #d42132; color: white;
													border-color: #d42132;"
											),
											bsTooltip(
												id= "degStatFiltered", 
												title= paste0("Filter any column of the table and then press ", 
													"this button to download the filtered data."),
												placement= "top"
											),
											downloadButton(outputId= "degStatSel",
												label="Download selected .xlsx",
												style= "background-color: #d42132; color: white;
													border-color: #d42132;"
											),
											bsTooltip(
												id= "degStatSel", 
												title= paste0("Select a number of rows from the above table ", 
													"and then press this button to download them."),
												placement= "top"
											)
										),
										width= 12
									) %>% add_class("step1_transStats"), # Close "degStatBox"
									shinydashboard::tabBox(
										id= "paResBox",
										tabPanel("Pathway analysis results",
											DT::dataTableOutput("paResTable",
												) %>% withSpinner(color="#008d4c"),
											downloadButton(outputId= "paResDown",
												label="Download results .xlsx",
												style= "background-color: #d42132; color: white;
													border-color: #d42132;"
											),
											bsTooltip(
												id= "paResDown", 
												title= "Download the full PA report",
												placement= "top"
											)
										),
										width = 12
									) # Close "paResBox"
								)
							),
							width= 12
						), # Close "DEA statistics" tab
						tabPanel(title= "Dataset plots",
							fluidRow(
								column(width=12,
									tipify(
										h3("Exploratory plots", shiny::icon("question-circle")) %>% add_class("step1_transConc"),
										title= paste0("Choose a dataset from the table below and then press Plot ",
											"at the bottom of the respective box to create an exploratory plot."
										),
										placement= "top"
									)
								)
							),
							fluidRow(
								column(width=6, align= "center",
									actionButton(
										inputId= "introTransConc",
										label= "About",
										icon= shiny::icon("info-circle"), 
										style= "background-color: #008d4c; color: white;
											border-color: #008d4c;"
									),
									bsTooltip(
										id= "introTransConc",
										title= paste0("Info about this tab"),
										placement= "bottom"
									),
									offset= 3
								)
							),
							fluidRow(
								column(width= 12,
									uiOutput(outputId= "datasetsConserningHelp")
								)
							),
							fluidRow(
								column(width= 12,
									DT::dataTableOutput("datasetsConserning")
								)
							),
							fluidRow(
								box(
									uiOutput(outputId= "heatmapQCHelp"),
									uiOutput(outputId= "heatmapQCstatWarning"),
									plotlyOutput(outputId= "heatmapQC",
										height= "600px") %>% withSpinner(color="#008d4c"),
									actionButton(
										inputId= "plotHeatmap",
										label= "Plot heatmap",
										icon= shiny::icon("paint-brush"),
										style= "background-color: #d42132; color: white;
											border-color: #d42132;"
									),
									width= 6, title= "Heatmap", status= "primary",
									collapsible= TRUE, collapsed= TRUE
								),
								box(
									uiOutput(outputId= "volcanoQCHelp"),
									plotlyOutput(outputId= "volcanoQC",
										height= "600px") %>% withSpinner(color="#008d4c"),
									actionButton(
										inputId= "plotVolcano",
										label= "Plot volcano",
										icon= shiny::icon("paint-brush"),
										style= "background-color: #d42132; color: white;
											border-color: #d42132;"
									),
									width= 6, title= "Volcano", status= "primary", 
									collapsible= TRUE, collapsed= TRUE
								)
							),
							width= 12
						), # Close "Concerning dataset(-s)" tab
						width= 12
					), # Close "transDatasetsBox"
					width= 12
				)
			),  # Close "ByTransDataset" menu item

			# Proteomic datasets tabItem
			tabItem(tabName= "ByProtDataset",
				fluidRow(
					fluidRow(
						h3("Proteomics datasets explorer") %>% add_class("step1_protDat"),
					),
					fluidRow(
						column(width=6, align= "center",
							actionButton(
								inputId= "introProtDat",
								label= "About",
								icon= shiny::icon("info-circle"), 
								style= "background-color: #008d4c; color: white;
									border-color: #008d4c;"
							),
							bsTooltip(
								id= "introProtDat",
								title= paste0("Info about exploring the proteomic datasets"),
								placement= "bottom"
							),
							offset= 3
						)
					),
					shinydashboard::tabBox(
						id= "protDatasetsBox",
						tabPanel(title= "Datasets",
							fluidRow(
								tipify(
									el= h3("Datasets table",shiny::icon("question-circle")),
									title= paste0("Click one or multiple rows (they will turn blue) ",
										"to choose a dataset(-s) and then press the Search button ",
										"to explore. Results are presented at the next 2 tabs of the explorer. ",
										"Use the GEO accession, Reference and Platform ",
										"column hyperlinks for more information about a dataset."
									),
									placement= "bottom"
								)
							),
							fluidRow(
								column(width= 3,
									actionButton(
										inputId= "protDtstsSearch",
										label= "Search",
										icon= shiny::icon("search"),
										style="background-color: #d42132; color: white;
											border-color: #d42132;"
									) %>% add_class("step2_protDat"),
									bsTooltip(
										id= "protDtstsSearch",
										title= paste0("Choose one or multiple datasets from ", 
											"the table below and then press search. ",
											"Results are presented at the next 2 tabs of the explorer."	
										),
										placement="bottom"
									),
									actionButton(
										inputId= "clearProtDtstsSearch", 
										label= "Reset", 
										icon= shiny::icon("refresh"), 
										style= " background-color: #008d4c; color: white;
											border-color: #008d4c"
									) %>% add_class("step3_protDat"),
									bsTooltip(
										id= "clearProtDtstsSearch",
										title= "Clear dataset selection.",
										placement="bottom"
									),
									actionButton(
										inputId= "selectProtAll",
										label= "Select all",
										icon= shiny::icon("check"),
										style="background-color: #d42132; color: white;
											border-color: #d42132;"
									),
									bsTooltip(
										id= "selectProtAll",
										title= paste0("Select all table rows (based on current ",
											"used filters). To select specific datasets ",
											"click the respective table row."
										),
										placement= "top"
									),
									offset= 9
								)
							),
							fluidRow(
								column(width= 12,
									DT::dataTableOutput("proteomicsTable")
								)
							),
							width= 12
						), # Close "Datasets" tab
						tabPanel(title= "DEA data",
							fluidRow(
								column(width=12, 
									tipify(
										h3("Common DEPs",
											shiny::icon("question-circle")
										),
										title= paste0("Common DEPs across selected ",
											"datasets."
										)
									)
								)
							),
							fluidRow(
								column(width=6, align= "center",
									actionButton(
										inputId= "introProtStats",
										label= "About",
										icon= shiny::icon("info-circle"), 
										style= "background-color: #008d4c; color: white;
											border-color: #008d4c;"
									),
									bsTooltip(
										id= "introProtStats",
										title= paste0("Info about exploration/integration results"),
										placement= "bottom"
									),
									offset= 3
								)
							),
							fluidRow(
								shinydashboard::tabBox(
									id= "depStatBox",
									tabPanel("Proteomics summary",
										DT::dataTableOutput("depStatsSum",
											) %>% withSpinner(color="#008d4c"),

										downloadButton(outputId= "depStatsSumAll",
											label="Download table .xlsx",
											style= "background-color: #d42132; color: white;
												border-color: #d42132;"
										),
										bsTooltip(
											id= "depStatsSumAll", 
											title= "Download the whole table.",
											placement= "top"
										),
										downloadButton(outputId= "depStatsSumFiltered",
											label="Download filtered data .xlsx",
											style= "background-color: #d42132; color: white;
												border-color: #d42132;"
										),
										bsTooltip(
											id= "depStatsSumFiltered", 
											title= paste0("Filter any column of the table and then press ", 
												"this button to download the filtered data."),
											placement= "top"
										)
									),
									tabPanel("Proteomics analytically",
										DT::dataTableOutput("depStats",
											) %>% withSpinner(color="#008d4c"),

										downloadButton(outputId= "depStatAll",
											label="Download table .xlsx",
											style= "background-color: #d42132; color: white;
												border-color: #d42132;"
										),
										bsTooltip(
											id= "depStatAll", 
											title= "Download the whole table.",
											placement= "top"
										),
										downloadButton(outputId= "depStatFiltered",
											label="Download filtered data .xlsx",
											style= "background-color: #d42132; color: white;
												border-color: #d42132;"
										),
										bsTooltip(
											id= "depStatFiltered", 
											title= paste0("Filter any column of the table and then press ", 
												"this button to download the filtered data."),
											placement= "top"
										),
										downloadButton(outputId= "depStatSel",
											label="Download selected .xlsx",
											style= "background-color: #d42132; color: white;
												border-color: #d42132;"
										),
										bsTooltip(
											id= "depStatSel", 
											title= paste0("Select a number of rows from the above table ", 
												"and then press this button to download them."),
											placement= "top"
										)
									),
									width= 12
								) %>% add_class("step1_protStats") # Close "depStatBox"
							),	
							width = 12
						), # Close "DEA data" tab
						width = 12
					) # Close "protDatasetsBox"
				)
			),			

			# ============================================================================
			# Gene explorer tab
			# ============================================================================
			tabItem(tabName= "ByGene",# class="active",
				fluidRow(
					column(width=2,
						wellPanel(
							textAreaInput(
								inputId= "geneName",
								label= "Type your gene(-s) of interest",
								placeholder= "Search by gene name...",
								resize= "none"
							),
							radioButtons(
								inputId= "speciesChoice",
								label= "Filter by species",
								choices= c(
									"Both"= "Both",
									"Human"= "Human",
									"Mouse"= "Mouse"
								)
							),
							actionButton(
								inputId= "geneNameSearch",
								label="Search",
								icon= shiny::icon("search"),
								style="background-color: #d42132; color: white;
									border-color: #d42132;"
							),
							bsTooltip(
								id= "geneName",
								title= paste0("Type one or multiple gene symbols/ ensembl ",
									"IDs on the box above. To search for an miRNA, please ",
									"provide its name, e.g. hsa-miR-21-5p. ",
									"In case of an official HGNC alias given, please choose ",
									"the required species below. ",
									"Gene search is case in-sensitive and can be filtered using ",
									"the radio buttons below. ",
									"Non-existent, misspelled names will be auto-removed."),
								placement= "bottom"
							),
							actionButton(
								inputId= "geneNameSearchExample",
								label="Example",
								icon= shiny::icon("play"),
								style="background-color: #d42132; color: white;
									border-color: #d42132;"
							),
							actionButton(
								inputId= "introGene",
								label= "About",
								icon= shiny::icon("info-circle"), 
								style= "background-color: #008d4c; color: white;
									border-color: #008d4c;"
							),
							bsTooltip(
								id= "introGene",
								title= paste0("Info about the Gene explorer"),
								placement= "bottom"
							),
							helpText("Note: batch search for a great number of genes 
								can take some time; please, be patient."
							)
						)
					),
					column(width=10,
						wellPanel(
							h3("General information"),
							DT::dataTableOutput(outputId= "geneInfo"),
							br(),
							tipify(
								h3("Map to single cell data",
									shiny::icon("question-circle")
								),
								title= paste0("Query for one or multiple protein-coding genes and then ",
									"use hyperlinks from the table below to map ",
									"the queried genes in same species datasets of NUPulmonary. "
								)
							),
							DT::dataTableOutput(outputId= "geneMapSc")
						)
					)
				),
				fluidRow(
					shinydashboard::tabBox(
						tabPanel(title="Expression data",
							fluidRow(
								column(width= 12,
									tipify(
										h3("DEG statistics", 
											shiny::icon("question-circle")
										),
										title= paste0("Gene expression statistics ",
											"for all datasets where the gene is ",
											"detected and/or not filtered out during analysis. ",
											"FDR < 0.05 is colored red."
										)
									),
									uiOutput(outputId= "expressionPerGeneHelp")
								)
							),
							fluidRow(
								column(width= 12,
									DT::dataTableOutput(outputId= "expressionPerGene"),
									downloadButton(outputId= "geneStatDown",
										label="Download .xlsx",
										style= "background-color: #d42132; color: white;
											border-color: #d42132;"
									)
								)
							),
							fluidRow(
								column(width= 12,
									tipify(
										h3("DEP data", 
											shiny::icon("question-circle")
										),
										title= paste0("DEP data ",
											"matching the genes queried."
										)
									),
									uiOutput(outputId= "expressionPerProteinHelp")
								)
							),
							fluidRow(
								column(width= 12,
									DT::dataTableOutput(outputId= "expressionPerProtein"),
									downloadButton(outputId= "prtnStatDown",
										label= "Download .xlsx",
										style= "background-color: #d42132; color: white;
											border-color: #d42132;"
									)
								)
							)
						),
						tabPanel(title= "Gene ontology",
							fluidRow(
								column(width= 12,
									tipify(
										h3("GO categories", shiny::icon("question-circle")),
										title= paste0("GO categories including the ",
											"queried genes."
										),
										placement= "bottom"
									),
									uiOutput(outputId= "goTableHelp")
								)
							),
							fluidRow(
								column(width= 12,
									DT::dataTableOutput(outputId= "goTable"),
									downloadButton(outputId = "goDown",
										label= "Download .xlsx",
										style= "background-color: #d42132; color: white;
											border-color: #d42132;"
									)
								)
							)
						),
						tabPanel(title="RefSeq - miRNA",
							fluidRow(
								column(width= 6,
									tipify(
										h3("Refseq data", shiny::icon("question-circle")),
										title= "Refseq-sourced info for the queried genes.",
										placement= "bottom"
									),
									uiOutput(outputId= "refseqTableHelp")
								),
								column(width= 6,
									tipify(
										h3("miRDB data", shiny::icon("question-circle")),
										title= "miRDB-sourced info about miRNA-target interactions.",
										placement= "bottom"
									),
									uiOutput(outputId= "miRNAinteractorsTableHelp")
								)
							),
							fluidRow(
								column(width=6,
									DT::dataTableOutput(outputId= "refseqTable"
										) %>% withSpinner(color="#008d4c"),
									downloadButton(outputId= "refSeqDown",
										label="Download .xlsx",
										style= "background-color: #d42132; color: white;
											border-color: #d42132;"
									)
								),
								column(width=6,
									DT::dataTableOutput("miRNAinteractorsTable"
										) %>% withSpinner(color="#008d4c"),
									downloadButton(outputId= "miRNAinterDown", 
										label= "Download .xlsx",
										style= "background-color: #d42132; color: white;
											border-color: #d42132;"
									)
								)
							)
						),
						width=12
					) %>% add_class("step1_geneExplorer")
				)
			),

			# ============================================================================
			# Protein explorer tab
			# ============================================================================
			tabItem(tabName= "ByProtein",
				wellPanel(
					fluidRow(
						column(width=2,
							textInput(
								inputId= "geneNamePPI",
								label="Type an official gene name (case sensitive)",
								placeholder="Gene name"
							),						
							bsTooltip(
								id= "geneNamePPI",
								title= paste0("Type a gene name and then press Search ",
									"to display annotation about its product."
								),
								placement= "bottom"
							),
							actionButton(
								inputId= "geneNameSearchPPI",
								label="Search",
								icon= shiny::icon("search"), 
								style="background-color: #d42132; color: white;
									border-color: #d42132;"
							),
							actionButton(
								inputId= "geneNameSearchPPIExample",
								label="Example",
								icon= shiny::icon("search"), 
								style="background-color: #d42132; color: white;
									border-color: #d42132;"
							),
							actionButton(
								inputId= "introProt",
								label= "About",
								icon= shiny::icon("info-circle"), 
								style= "background-color: #008d4c; color: white;
									border-color: #008d4c;"
							),
							bsTooltip(
								id= "introProt",
								title= paste0("Info about the Protein explorer"),
								placement= "bottom"
							)
						),
						column(width=10,
							infoBoxOutput(outputId= "uniprotAC", width= 3),
							infoBoxOutput(outputId= "fullName", width= 3),
							infoBoxOutput(outputId= "CD", width= 3),
							infoBoxOutput(outputId= "isItDE", width= 3)
						)
					)
				),
				fluidRow(
					shinydashboard::tabBox(
						tabPanel(title="Concerning protein",
							fluidRow(
								column(width=12,
									h4("Aliases"),
									htmlOutput(outputId= "Aliases")
								),
								column(width=12,
									h4("Protein function"),
									htmlOutput(outputId= "PrtnFunction")
								),
								column(width=12,
									h4("Subcellular location"),
									htmlOutput(outputId= "SubcelLocation")
								),
								column(width=12,
									h4("Disease implication"),
									htmlOutput(outputId= "PrtnDisease")
								),
								column(width=12,
									h4("Evidence level"),
									htmlOutput(outputId= "PE")
								)
							)							
						),
						tabPanel(title="Differential expression",
							fluidRow(
								column(width= 12,
									DT::dataTableOutput("depData")
								)
							)							
						),
						tabPanel(title="PPI network",
							fluidRow(
								column(width= 3,
									actionButton(
										inputId="ppiPlot",
										label= "Plot",
										icon= shiny::icon("paint-brush"),
										style= "background-color: #d42132; color: white;
											border-color: #d42132;"
									),
									bsTooltip(
										id= "ppiPlot",
										title= paste0("Search a protein by coding gene name ",
											"and then use this button to plot a PPI network ",
											"for the selected protein."
										),
										placement= "bottom"
									),
									actionButton(
										inputId= "introPPI",
										label= "About",
										icon= shiny::icon("info-circle"), 
										style= "background-color: #008d4c; color: white;
											border-color: #008d4c;"
									)
								)#,
								# column(width= 6,
								# 	helpText("After a new search please press 'Plot' again to get
								# 		the new PPI plot.")
								# )
							),
							fluidRow(
								## Network plotting area
								column(width=6,
									visNetworkOutput(outputId= "ppiNetwork",
										height= "650px") %>% withSpinner(color="#008d4c")
								),

								## Color by expression control panel
								column(width=3,
									wellPanel(
										shinyjs::disabled(
											textInput(
												inputId= "speciesInNet",
												label= "Species",
												value= NULL,
												placeholder= "Species will be auto-filled"
											),
											selectizeInput(
												inputId= "comparisonInNet",
												choices= "",
												label= "Select comparison",
												options= list(
													placeholder = "Select comparison",
													onInitialize = I('function() { this.setValue(""); }')
												)
											) %>% add_class("step2_ppi")
										),
										numericInput(
											inputId= "fcInNet",
											label= "Type a Fold change",
											value= 1.2,
											step= 0.01
										)
									)
								),
								column(width=3,
									wellPanel(
										shinyjs::disabled(
											selectizeInput(
												inputId= "tissueInNet",
												choices= "",
												label= "Select tissue",
												options= list(
													placeholder = "Select tissue",
													onInitialize = I('function() { this.setValue(""); }')
												)
											) %>% add_class("step1_ppi")
										),
										numericInput(
											inputId= "pvalInNet",
											label= "Type a p-value",
											value= 0.05,
											step= 0.01
										),
										actionButton(
											inputId= "visExpression",
											label= "Annotate",
											style= "background-color: #d42132; color: white;
												border-color: #d42132;"
										),
										actionButton(
											inputId= "clearAnnotationParams",
											label= "Reset",
											icon= shiny::icon("refresh"),
											style= "background-color: #008d4c; color: white;
												border-color: #008d4c1;"
										),
										helpText("Define parameters to annotate the network using
											DEA data. Datasets used to perform the annotation will
											be presented in 'Data used' tab.")
									)
								)
							)
						),
						tabPanel(title="Data used",
							fluidRow(
								column(width=12,
									DT::dataTableOutput("dataUsed")
								)
							)
						),
						width=12
					) %>% add_class("step1_proteinExplorer")
				)
			),

			# ============================================================================
			# Datasets benchmarking tab
			# ============================================================================
			tabItem(tabName= "Decor",
				fluidRow(
					wellPanel(
						fluidRow(
							column(width= 12,
								h2("Datasets benchmarking"),
								p(),
								shinydashboard::tabBox(
									id= "benchmarking",
									tabPanel(title= "Benchmarking results",
										fluidRow(
											column(width= 12,
												shinydashboard::tabBox(
													tabPanel(title= "All trans. coding datasets",
														DT::dataTableOutput("decorRes"),
														width= 12
													),
													tabPanel(title= "IPF_vs_Ctrl lung coding",
														DT::dataTableOutput("decorResII"),
														width= 12
													),
													tabPanel(title= "BleomD14_vs_Ctrl lung coding",
														DT::dataTableOutput("decorResIII"),
														width= 12
													),
													tabPanel(title= "All non-coding datasets",
														DT::dataTableOutput("decorResIV"),
														width= 12
													),
													width= 12
												)
											)
										),
										width= 12
									),
									tabPanel(title= "Benchmarking backstage",
										fluidRow(
											column(width= 12,
												shinydashboard::tabBox(
													tabPanel(title= "All trans. coding datasets",
														includeMarkdown("./www/benchBackAllCoding.md"),
														h4("Stars analytically"),
														DT::dataTableOutput("benchAllCoding"),
														width= 12
													),
													tabPanel(title= "IPF_vs_Ctrl lung coding",
														includeMarkdown("./www/benchBackIPF_vs_Ctrl.md"),
														h4("Stars analytically"),
														DT::dataTableOutput("benchIPFCtrl"),
														width= 12
													),
													tabPanel(title= "BleomD14_vs_Ctrl lung coding",
														includeMarkdown("./www/benchBackBleomD14_vs_Ctrl.md"),
														h4("Stars analytically"),
														DT::dataTableOutput("benchBleomD14Ctrl"),
														width= 12
													),
													tabPanel(title= "All non-coding datasets",
														includeMarkdown("./www/benchBackNonCoding.md"),
														h4("Stars analytically"),
														DT::dataTableOutput("benchNonCoding"),
														width= 12
													),
													width= 12
												)
											)
										),
										width= 12
									),
									width= 12
								)
							)
						)
					)
				)
			),

			# ============================================================================
			# Single cell data tab
			# ============================================================================
			tabItem(tabName= "SingleCell",
				wellPanel(
					fluidRow(
						tipify(
							el= h3("IPF scRNA-seq datasets",shiny::icon("question-circle")),
							title= "The following table summarizes the published IPF scRNA-seq datasets.",
							placement= "bottom"
						)
					),
					fluidRow(
						p(
							paste(
								"Single cell RNA-seq is a quickly rising omics technology",
								"with several scientific articles already published in the",
								"context of IPF, enabling the study of gene expression",
								"patterns pathological divergence at an unprecedented resolution level.",
								"While this tab currently holds merely a catalogue of the IPF-related",
								"scRNA-seq publications, Fibromine's Gene explorer 'Map to single cell data'",
								"feature can map any of the queried protein coding gene(-s) to the single cell",
								"level."
							)
						),
						p(
							paste(
								"Important IPF and lung-related single cell online resources can be",
								"found at Fibromine's Home tab --> 'Useful links'"
							)
						)
					),
					fluidRow(
						column(width= 12,
							DT::dataTableOutput("scDatasetsTable")
						)
					)
				)
			),

			# ============================================================================
			# Download data tab
			# ============================================================================
			tabItem(tabName= "DownData",
				fluidRow(
					tipify(
						el= h3("Download normalized transcriptomics data", 
							shiny::icon("question-circle")),
						title= paste0("Choose a dataset from the drop-down menu and press download ",
							"to acquire normalized values for that dataset."
						),
						placement= "top"
					)
				),
				fluidRow(
					column(width=12,
						wellPanel(
							selectizeInput(
								inputId= "normDataRequest",
								label= NULL,
								choices= transDatasets,
								options= list(
									placeholder="Select dataset", 
									onInitialize= I('function() { this.setValue(""); }')
								)
							),
							downloadButton(outputId= "normData",
								label="Download data .xlsx",
								style= "background-color: #d42132; color: white;
									border-color: #d42132;"
							),
							bsTooltip(
								id= "normData", 
								title= "Select a dataset to download.",
								placement= "bottom"
							)
						)
					)				
				),
				fluidRow(
					tipify(
						el= h3("Download proteomics data", shiny::icon("question-circle")),
						title= paste0("Choose a dataset from the drop-down menu and press download ",
							"to acquire expression data for that dataset."
						),
						placement= "top"
					)
				),
				fluidRow(
					column(width=12,
						wellPanel(
							selectizeInput(
								inputId= "protDataRequest",
								label= NULL,
								choices= protDatasets,
								options= list(
									placeholder="Select dataset", 
									onInitialize= I('function() { this.setValue(""); }')
								)
							),
							downloadButton(outputId= "protData",
								label="Download data .xlsx",
								style= "background-color: #d42132; color: white;
									border-color: #d42132;"
							),
							bsTooltip(
								id= "protData", 
								title= "Select a dataset to download.",
								placement= "bottom"
							)
						)
					)				
				)
			),

			# ============================================================================
			# Documentation tab
			# ============================================================================
			tabItem(tabName= "Docs",
				wellPanel(
					fluidRow(
						includeMarkdown("./www/docs.md")
					)
				)
			),

			# ============================================================================
			# How to tab
			# ============================================================================
			tabItem(tabName= "HowTo",
				wellPanel(
					fluidRow(
						includeMarkdown("./www/howTo.md")
					)
				)
			)
		)
	),
skin="green"
))
