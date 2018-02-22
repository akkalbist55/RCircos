#*		RCircos Plot with Wildlife Seizure Data
#*
#*		From: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4491747/.
#*
#*		Trade networks for elephants, rhinoceros, and tigers were 
#*		mapped using Circos (mkweb.bcgsc.ca/tableviewer), software 
#*		more widely used in genetics (25). Networks consisted of 
#*		nodes joined by directed connections. The nodes in the 
#*		network represented the countries of origin and destination 
#*		of shipments based on the HealthMap Wildlife Trade database. 
#*		Each connection was characterized by the direction of the 
#*		shipment and its corresponding number of reported shipments. 
#*		A pair of nodes could have two connections if trade was 
#*		occurring in both directions. A connection that began and 
#*		ended at the same node was not included in the analyses.
#*
#*
#*		I want to me plot similar to one (attached one):
#*
#*		1.	Name of country is replaced by district name
#*
#*		2.	First circle will be broken down based on the number of 
#*			cases (wildlife seizure cases) for each district with 
#*			different colors for each of four seasons (Autumn, 
#*			Spring, Summer, winter). 
#*
#*		3.	Connections between districts will denote number of cases 
#*			in five year. 
#*
#*



#*		1).	Creat a genome-like data with format same as the human
#*			genome version 19. Check the format with
install.packages("RCircos")
			 library(RCircos)
#*			> data(UCSC.HG19.Human.CytoBandIdeogram)
#*			> head(UCSC.HG19.Human.CytoBandIdeogram)
#
#*			  Chromosome ChromStart ChromEnd   Band  Stain
#*			1       chr1          0  2300000 p36.33   gneg
#*			2       chr1    2300000  5400000 p36.32 gpos25
#*			3       chr1    5400000  7200000 p36.31   gneg
#*			4       chr1    7200000  9200000 p36.23 gpos25
#*			5       chr1    9200000 12700000 p36.22   gneg
#*			6       chr1   12700000 16200000 p36.21 gpos50
#*


#*		Peudo distric names. Replace with real distract names
#*		===========================================================
Districts <- c(  "district_01", "district_02", "district_03",
				"district_04", "district_05", "district_06",
				"district_07", "district_08", "district_09",
				"district_10", "district_11", "district_12",
				"district_13", "district_14", "district_15",
				"district_16", "district_17", "district_18",
				"district_19", "district_20");

chrom_name=rep(Districts, each=4);
chrom_start=rep(c(0, 5001, 10001, 15001), length(Districts));
chrom_end=rep(c(chrom_start[2:4] - 1, 20000), length(Districts));
chrom_band=rep(c("Spring", "Autumn", "Summer", "winter"), length(Districts));
chrom_stain=rep("gneg", length(Districts)*4);

district_map <- data.frame(
	Chromosome=chrom_name,
	ChromStart=chrom_start,
	ChromEnd=chrom_end,
	Band=chrom_band,
	Stain=chrom_stain
)

dim(district_map)		#	[1] 80  5
district_map[1:8,]

#*		   Chromosome ChromStart ChromEnd   Band  Stain
#*		1 district_01          0     5000 Spring  gneg
#*		2 district_01       5001    10000 Autumn  gneg
#*		3 district_01      10001    15000 Summer  gneg
#*		4 district_01      15001    20000 winter  gneg
#*		5 district_02          0     5000 Spring  gneg
#*		6 district_02       5001    10000 Autumn  gneg
#*		7 district_02      10001    15000 Summer  gneg
#*		8 district_02      15001    20000 winter  gneg


#*		2).	Generate link plot data from raw data. The raw data 
#*			should have 4 columns for each line:
#*
#*			from district, 
#*			to district, 
#*			season, 
#*			amount 
#*		===========================================================
getwd()
setwd("/home/akkal/Downloads")
trade_data=read.csv("trade_data.csv", header = TRUE, sep="\t")
trade_data
#getwd("/user/akkal/Download")
trade_data <- read.table("/user/akkal/Download/trade_data.txt",  header=TRUE, sep="/t", quote="")
trade_data <- trade_data[order(trade_data$From),]
trade_data

#*		         From          To Season Amount
#*		1 district_01 district_20 Spring   1000
#*		2 district_02 district_06 Summer   2000
#*		7 district_04 district_07 Autumn   3000
#*		5 district_09 district_12 Spring    300
#*		6 district_15 district_05 Summer    500
#*		3 district_16 district_06 Autumn   4000
#*		4 district_18 district_13 Winter    100
#*		8 district_18 district_02 Winter     50

Spring <- which(trade_data$Season == "Spring");
Summer <- which(trade_data$Season == "Summer");
Autumn <- which(trade_data$Season == "Autumn");
Winter <- which(trade_data$Season == "Winter");

From_district <- as.character(trade_data$From);
To_district <- as.character(trade_data$To);

season_len <- 20000/4;
extra_len <- 1000;

From_start <- rep(extra_len, nrow(trade_data));
From_start[Summer] <- From_start[Summer] + season_len;
From_start[Autumn] <- From_start[Autumn] + season_len*2;
From_start[Winter] <- From_start[Winter] + season_len*3;

From_end <- rep(season_len - extra_len, nrow(trade_data));
From_end[Summer] <- From_end[Summer] + season_len;
From_end[Autumn] <- From_end[Autumn] + season_len*2;
From_end[Winter] <- From_end[Winter] + season_len*3;


To_start <- rep(extra_len, nrow(trade_data));
To_start[Summer] <- To_start[Summer] + season_len;
To_start[Autumn] <- To_start[Autumn] + season_len*2;
To_start[Winter] <- To_start[Winter] + season_len*3;

To_end <- rep(season_len - extra_len, nrow(trade_data));
To_end[Summer] <- To_end[Summer] + season_len;
To_end[Autumn] <- To_end[Autumn] + season_len*2;
To_end[Winter] <- To_end[Winter] + season_len*3;

amount_base <- max(as.numeric(trade_data$Amount)) / 10;
line_width <- pmax(1, as.numeric(trade_data$Amount)/amount_base)
line_width <- round(line_width, digits=0);

plot_colors <- rep("green", nrow(trade_data));
plot_colors[Summer] <- "red";
plot_colors[Autumn] <- "yellow";
plot_colors[Winter] <- "gray";

plot_data <- data.frame(
	Chromosome=From_district,
	chromStart=From_start,
	chromEnd=From_end,
	Chromosome.1=To_district,
	chromStart.1=To_start,
	chromEnd.1=To_end,
	PlotColor=plot_colors);

plot_data



#*		3).	Plot link lines with different colors for four seasons  
#*			and different line widths for numbers of cases in five 
#*			years.
#*		===========================================================			 


library(RCircos)
RCircos.Set.Core.Components(cyto.info=district_map, chr.exclude = NULL, 
		tracks.inside=10, tracks.outside=10);


#*		Modify the base.per.unit value. Default is 30000. 
#*		Increase it if more than 20 districts. Decreast  
#*		it for less than 20 districts
#*		
params <- RCircos.Get.Plot.Parameters()
params$base.per.unit <- 2000;
RCircos.Reset.Plot.Parameters(params);


RCircos.Set.Plot.Area();
RCircos.Chromosome.Ideogram.Plot();
RCircos.Link.Plot(link.data=plot_data, track.num=1, 
	lineWidth=line_width) 
title("Wildlife Trading Path")

#*	End
#*	==================================================================













