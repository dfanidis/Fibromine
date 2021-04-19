#### Non-coding microarray datasets benchmarking

In this tab we **visually** present the **process** followed for the benchmarking of all **non-coding microarray datasets** along with the respective **per metric boxplots**. At the **end** of this **tab**, the user can also **analytically interrogate** the stars assigned to each dataset/comparison.

##### Benchmarking pipeline

<a href= "benchBackstage/nonCoding/bench_nonCoding_Pipeline.png" target="_blank" rel='noopener noreferrer'> 
	<img src= "./benchBackstage/nonCoding/bench_nonCoding_Pipeline.png" alt="image" style="width:400px;height:400px" class="center">
</a>

##### Fibrotic genes expression

_In this metric we count **how many of each dataset's DEGs** have been **previously reported as profibrotic**. (Dataset's DE orientation must match to this reported in literature for a gene to be counted.) The genes assessed were those reported by **Vukmirovic et al. 2018**, along with **some already known**. In the case of **mouse datasets**, the **homologues** to human genes (as defined into Ensembl) were used._

This metric was **not** used for non-coding datasets, as all used fibrotic genes are protein-coding.

##### Detected genes 

In this metric we calculate how many genes were **detected** per dataset/comparison.

<a href= "benchBackstage/nonCoding/detPlot_nonCoding.png" target="_blank" rel='noopener noreferrer'> 
	<img src= "./benchBackstage/nonCoding/detPlot_nonCoding.png" alt="image" style="width:500px;height:500px" class="center">
</a>

##### Differentially expressed genes (DEGs)

For this metric we calculate DEGs found in each dataset/comparison (DEA thresholds: 1.2 FC; 0.05 FDR).

<a href= "benchBackstage/nonCoding/degPlot_nonCoding.png" target="_blank" rel='noopener noreferrer'> 
	<img src= "./benchBackstage/nonCoding/degPlot_nonCoding.png" alt="image" style="width:500px;height:500px" class="center">
</a>

##### Up/Down DEGs ratio

Up/Down DEGs ratio metric is based on the distribution of up-regulated to down-regulated genes found in each dataset/comparison. (Ratio=1 signifies an even participation of Up and Down regulated genes.)

<a href= "benchBackstage/nonCoding/ratioPlot_nonCoding.png" target="_blank" rel='noopener noreferrer'> 
	<img src= "./benchBackstage/nonCoding/ratioPlot_nonCoding.png" alt="image" style="width:500px;height:500px" class="center">
</a>

##### FC bins

The following boxplots depict the proportions of DEGs found in three bins of **absolute FC** values for each Fibromine dataset/comparison where DEGs were found. (Low: 1.2-2, Intermediate= 2-5, High= >5)

In order to assign a star based on this metric to a dataset/comparison, a **two steps procedure** is followed:

1. Each dataset/comparison gets from 0 to 3 stars according to its position at the distribution of **each one** of the \|FC\| bins 
2. The aforementioned stars are **summarized** into a consensus star assigned to each dataset/comparison if the latter has collected **2/3** of the individual stars

	<a href= "benchBackstage/nonCoding/fcBinPlot_nonCoding.png" target="_blank" rel='noopener noreferrer'> 
		<img src= "./benchBackstage/nonCoding/fcBinPlot_nonCoding.png" alt="image" style="width:500px;height:500px" class= "center">
	</a>

##### P-value distribution Area Under the Curve

<a href= "benchBackstage/nonCoding/pAucPlot_nonCoding.png" target="_blank" rel='noopener noreferrer'> 
	<img src= "./benchBackstage/nonCoding/pAucPlot_nonCoding.png" alt="image" style="width:500px;height:500px" class="center">
</a>


##### Adjusted p-value distribution Area Under the Curve

<a href= "benchBackstage/nonCoding/adjpAucPlot_nonCoding.png" target="_blank" rel='noopener noreferrer'> 
	<img src= "./benchBackstage/nonCoding/adjpAucPlot_nonCoding.png" alt="image" style="width:500px;height:500px" class="center">
</a>