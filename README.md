# AlphaPairwise
# Language: R
# Input: TXT
# Output: PNG
# Tested with: PluMA 1.1, R 4.0.0
# Dependencies: [1] stringr_1.4.0     rgl_0.104.16      data.table_1.14.0 reshape2_1.4.4
# [5] plyr_1.8.6        plotly_4.9.3      mixOmics_6.14.0   ggplot2_3.3.5
# [9] MASS_7.3-53       vegan_2.5-7       lattice_0.20-41   permute_0.9-5
#[13] igraph_1.2.6      markovchain_0.8.9 gplots_3.1.1

Pairwise alpha diversity plot (PNG format)

Input is a tab-delimited file of keyword-value pairs:
inputfile: Dataset
index: Diversity index
categories: Groups
pairs: Group pairs to plot
