#########################################################################################################################
#########################################################################################################################
###### PROJECT:        GWI Diet
###### NAME:           CSTsSpecificityIndex.R
###### AUTHOR:         Vitalii Stebliankin, PhD Student
###### AFFILIATION:    Florida International University
###### 
###### DESCRIPTION:    This file generates the allingtraphs with diversity and stacked bar plots
#########################################################################################################################
#########################################################################################################################

#rm(list=ls())

#projectPath = "/Users/stebliankin/Desktop/GWI_2022/experiment_runs/03-25-2022-bwt2_ABR/"

dyn.load(paste("RPluMA", .Platform$dynlib.ext, sep=""))
source("RPluMA.R")
source("RIO.R")

#setwd(projectPath)

library(gplots)
library("markovchain"); packageVersion("markovchain")
library("igraph"); packageVersion("igraph")
# library(caret)
library(vegan)

set.seed(1)
library(mixOmics)
library(plotly)
library(plyr)
require(reshape2)
require(ggplot2)
require(data.table)
library(rgl)
library(stringr)

hash <- function(name) {return(sum(utf8ToInt(name))/nchar(name))}



library(RColorBrewer)

######### Diaversity index for RPKM #########

input <- function(inputfile) {
	parameters <<- readParameters(inputfile)
ABR_X = as.data.frame(read.csv2(paste(prefix(), parameters["inputfile", 2], sep="/"), sep=','))
X<-ABR_X
rownames(X) = X[,1]
X = X[,-1]
X = X[,-1]
X = as.matrix(X)
(storage.mode(X) = "numeric")


## SIMPSON
library(ggpubr)
index <<- parameters["index", 2]
diversityIndex <<- diversity(X,index = index)
diversityIndex <<- cbind(factor(names(diversityIndex), levels = names(diversityIndex)),as.data.frame(diversityIndex))
colnames(diversityIndex) <<- c("name","value")
diversityIndex$label<<-readSequential(paste(prefix(), parameters["categories", 2], sep="/"))
levels(diversityIndex$name) <<- as.character(diversityIndex$name)
mypairs <- read.table(
  paste(prefix(), parameters["pairs", 2], sep="/"),
  sep="\t")

thepairs <<- list()
for (i in 1:nrow(mypairs)) {
   thepairs <<- append(thepairs, list(c(mypairs[i, 1], mypairs[i, 2])))
}

}

run <- function() {}

output <- function(outputfile) {

but_plot <- ggboxplot(diversityIndex, x="label" , y = "value",
                      color = "black", palette = "jco", fill="label") +
  theme(axis.title.x = element_text(size=0, face="bold"), axis.text.x =element_text(size=12, face="bold")) +
    stat_compare_means(method = 'wilcox.test', comparisons = rev(thepairs))  + 
  theme_gray() #+ ylim(0,150) #+ coord_flip()
but_plot
ggsave(outputfile, path='./', plot=but_plot, device = "png", dpi = 1200)

}

