# Server side used functions

# ============================================================================
# Add class to shinydashboard elements so as to be able to 
# point to them in the into tour
# ============================================================================

add_class <- function(x, class) {
  x$attribs <- append(x$attribs, list(class = class))
  x
}

# ============================================================================
# Create heatmap
# ============================================================================

## Specify heatmap colors
#colorPallete <- colorRampPalette(rev(brewer.pal(9, "RdBu")))(100)

colorPallete <- c("#2166AC", "#2369AD", "#266DAF", "#2970B1", "#2B74B3", "#2E78B5", "#317BB7", "#347FB9", "#3683BA", 
"#3986BC", "#3C8ABE", "#3F8EC0", "#4191C2", "#4695C4", "#4D99C6", "#539DC8", "#5AA1CA", "#60A5CD", 
"#66A9CF", "#6DADD1", "#73B1D3", "#7AB5D5", "#80B9D8", "#86BDDA", "#8DC1DC", "#93C5DE", "#98C8DF", 
"#9DCAE1", "#A2CDE2", "#A7CFE4", "#ACD2E5", "#B1D5E7", "#B6D7E8", "#BCDAEA", "#C1DCEB", "#C6DFEC", 
"#CBE2EE", "#D0E4EF", "#D3E6F0", "#D6E7F1", "#D9E9F1", "#DCEAF2", "#DFECF2", "#E3EDF3", "#E6EFF3", 
"#E9F0F4", "#ECF1F5", "#EFF3F5", "#F2F4F6", "#F5F6F6", "#F7F5F5", "#F7F3F1", "#F8F1ED", "#F8EFE9", 
"#F9ECE5", "#F9EAE1", "#FAE8DD", "#FAE6D9", "#FBE3D6", "#FBE1D2", "#FCDFCE", "#FCDCCA", "#FCDAC6", 
"#FCD6C0", "#FBD1BB", "#FACDB5", "#FAC8AF", "#F9C4AA", "#F8C0A4", "#F7BB9F", "#F7B799", "#F6B394", 
"#F5AE8E", "#F4AA88", "#F4A683", "#F2A07E", "#EF9B7A", "#ED9576", "#EA9071", "#E88A6D", "#E68469", 
"#E37F65", "#E17960", "#DE745C", "#DC6E58", "#D96953", "#D7634F", "#D45D4B", "#D25749", "#CF5246", 
"#CC4C43", "#C94640", "#C6403E", "#C33A3B", "#C03538", "#BD2F35", "#BA2933", "#B72330", "#B41D2D", 
"#B2182B")

## Condition sidebar colors vector to pick from
colorsVector <- c("#FF0000", "#0000FF", "#FF7F50")

plotHeatmapQC <- function(normExprTable, colorPallete, conditions) {
	progress <- shiny::Progress$new()
	on.exit(progress$close())
	progress$set(message= "Creating heatmap", value=0)
	progress$inc(0.25)	

	gene_ints <- subset(normExprTable, select= -Name)
	gene_ints <- na.omit(gene_ints)
	scale_gene_ints <- as.data.frame(scale(t(gene_ints)))			## Scale
	colnames(scale_gene_ints) <- normExprTable$Name					## Assign Name
	scale_gene_ints$"Experimental condition" <- conditions 			## Assign conditions
	progress$inc(0.25)

	## Assign colors to condition sidebar	
	colors <- colorsVector[1:length(unique(conditions))]
	names(colors) <- sort(unique(conditions))
	progress$inc(0.25)


	## Create plot
	heatmap <- heatmaply::heatmaply(
		x=scale_gene_ints[,-ncol(scale_gene_ints)],	# ncol(scale_gene_ints) is used instead of n_genes until I figure out how to remove duplicate genes and NAs
		colors=colorPallete,
		showticklabels= c(FALSE,TRUE),						
		row_side_colors= data.frame("Experimental Conditions"= 
			scale_gene_ints[,ncol(scale_gene_ints)]),
		row_side_palette= colors,
		plot_method="plotly",						
		file=NULL
	)
	progress$inc(0.25)
	return(heatmap)
}


# ============================================================================
# Create volcano plot
# Note: Species info is required to pick the right reference genes 
# ============================================================================
plotVolcanoQC <- function(data, species, tech) { 
	progress <- shiny::Progress$new()
	on.exit(progress$close())
	progress$set(message= "Creating volcano plot", value=0)
	progress$inc(0.25)
	
	fcThres <- 0.2630344
	clrs <- c('dodgerblue','grey30','coral','red2')
	names(clrs) <- c("FC", "NS", "P", "P & FC")

	## Color points in respect to the P-value and FC thresholds of 0.05 and 1.2
	data$DEG <- rep("NS",nrow(data))
	data$DEG[which(data$Pval > 0.05 & data$log2FC < -fcThres)] <- "P"
	data$DEG[which(data$Pval > 0.05 & data$log2FC > fcThres)] <- "P"
	data$DEG[which(data$Pval < 0.05 & data$log2FC < -fcThres)] <- "P & FC"
	data$DEG[which(data$Pval < 0.05 & data$log2FC > fcThres)] <- "P & FC"
	data$DEG[which(data$Pval < 0.05 & data$log2FC < fcThres & data$log2FC > -fcThres)] <- "FC"
	progress$inc(0.25)
	
	## Annotate specific genes - markers of IPF
	if (tech != "Non-coding RNA profiling by array") {	## If technology is of coding genes 
		
		gn <- c("ACTA2","FN1","COL1A1","COL3A1","SMAD2","SMAD3") 
		if (species== "Mus musculus") {
			gn <- paste0(
				substr(gn, 1, 1), 
				tolower(substr(gn, 2, nchar(gn)))
			)
		}
	
		qcGenes <- data[which(data$Name %in% gn), ]

		a <- list()
		for (i in seq_len(nrow(qcGenes))) {
			 	m <- qcGenes[i, ]
  				a[[i]] <- list(
   		 			x = m$log2FC,
   		 			y = -log10(m$Pval),
    				text = m$Name,
   		 			xref = "x",
    				yref = "y",
   		 			showarrow = TRUE,
   		 			arrowhead = 0.5,
    				arrowcolor= "black",
    				font = list(color = "black", 
    					size= 18),
    				ax = 20,
    				ay = -40
  				)
			}
		offset <- c(50, 100, 150, 200, 250)
		for(i in length(a)) {
			a[[i]]$ax <- a[[i]]$ax + offset[i] 
		}

	} else a <- NULL	## If Non-coding RNA profiling by array tech

	## ggplot2 object
	figure_volcano <- ggplot(data= data, aes(x= log2FC, y= -log10(Pval), colour= DEG, label= Name)) +
		geom_point()+
		scale_color_manual(values=clrs) +
		geom_vline(xintercept=fcThres, linetype="dashed", color="black") +
		geom_vline(xintercept=-fcThres, linetype="dashed", color="black") +
		geom_hline(yintercept=1.30103, linetype="dashed", color="black") +
		xlab("log2FC") +
		ylab("-log10 P-value") +
		theme(legend.position = "none",
    	          plot.title = element_text(size = rel(1.5), hjust = 0.5),
    	          axis.title = element_text(size = rel(1.25))) 
	progress$inc(0.25)

	## Return plotly object
	progress$inc(0.25)
	ggplotly(figure_volcano, tooltip= "label") %>%
		layout(annotations= a)

}

# ============================================================================
# Retrieve within network PPIs for the interactors of the queried protein
# Network is a network represented by a dataframe (internal use only)
# ============================================================================
# Note:
# The solution of creating a temp db table is not applicable for 2 reasons:
# 1. I (most probably) must place shapeNetwork into shinyServer(), otherwise 
# 	"session" object is not found
# 2. DB is not writtable in this framework (it would have been a risk anyway) 

shapeNetwork <- function(network, string, conn) {
	interactors <- data.frame("Interactors"=network$Protein2, 
		stringsAsFactors=FALSE)
	
	## Retrieve all first shell interactions
	rest_interactions <- dbGetQuery(
		conn= conn,
			statement= '
				SELECT
					*
				FROM
					PPI
				WHERE
					PPI.Protein1 = :x
			;',
		params= list(
			x= interactors[,1]
		)
	) 

	firstShell <- rest_interactions[which(								
		rest_interactions$Protein2 %in% network$Protein2),]	

	## Get up to 20 second shell interactors
	per_interactor <- split(rest_interactions,
		f= rest_interactions$Protein1)

	per_interactor <- lapply(per_interactor, function(x) {
		x <- x[order(x$InteractionCombinedScore, decreasing= TRUE), ]
		
		# Remove first shell interactors from Protein2 col to avoid repeat
		x <- x[-which(x$Protein2 %in% interactors[,1]), ]

		# Get for each 1st shell interactor the top two 2nd shell interactors only
		out <- x[1:2, ]
		return(out)
	})
	per_interactor <- do.call("rbind", per_interactor)

	sec_interactors <- data.frame("secInteractors"= 
		unique(per_interactor$Protein2), stringsAsFactors= FALSE)

	## Retrieve all second shell interactions
	rest_interactions <- dbGetQuery(
		conn= conn,
			statement= '
				SELECT
					*
				FROM
					PPI
				WHERE
					PPI.Protein1 = :x
			;',
		params= list(
			x= sec_interactors[,1]
		)
	) 

	# Limit to within second shell interactions
	secondShell <- rest_interactions[which(
		rest_interactions$Protein2 %in% network$Protein2 |
		rest_interactions$Protein2 %in% per_interactor$Protein2),]

	## Bring data together
	out <- rbind(network, 
		firstShell[,c("PPIid", "Protein1","Protein2", "InteractionCombinedScore")],
		secondShell[,c("PPIid", "Protein1","Protein2", "InteractionCombinedScore")]
	)
	out <- out[,2:4]

	## Replace STRING protein codes with STRING preferred names
	# Replace queried protein
	out[out==string$StringID] <- string$StringPreferredName				
	
	interactors <- dbGetQuery(
		conn= conn,
		statement= 'SELECT StringID, StringPreferredName 
			FROM STRINGdb WHERE StringID = :x',
		params= list(x= unique(c(out$Protein1, out$Protein2)))
	)

	# Replace interactor names
	for(i in interactors$StringID) {
		x <- subset(interactors, StringID== i)
		out[out==x$StringID] <- x$StringPreferredName
	}

	return(out)
}

# ============================================================================
# Plot PPI network based on the object returned by shapeNetwork
# using visNetwork package
# ============================================================================
plotNetwork <- function(network) {

	nodes <- unique(c(network$Protein1, network$Protein2))
	edges <- network
	
	g <- graph_from_data_frame(edges, 
		directed= FALSE, vertices= nodes)
	V(g)$degOrientation <- rep("Unknown", length(nodes))				## Initialize to use during annotation
	V(g)$color <- "#97C2FC"
	V(g)$groups <- paste(V(g)$degOrientation, 
		V(g)$cluster, sep=",")

	data <- toVisNetworkData(g)											## Transform to visNetwork object

	out <- visNetwork(nodes=data$nodes, edges=data$edges) %>%
    	visEdges(color="#97C2FC") %>%
    	visIgraphLayout(layout= "layout_with_drl", randomSeed=1724,
			weights= network$InteractionCombinedScore*.001) %>%
    	visOptions(selectedBy= list(variable="groups",multiple=T))
    return(out)
}