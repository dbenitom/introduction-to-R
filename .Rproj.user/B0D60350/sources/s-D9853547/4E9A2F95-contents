# Plotting 16S data

# Load packages:
library(tidyr)
library(dplyr)
library(ggplot2)

# Load data:
data <- read.table(file = "data/16S_data_phyloflash.tsv", header=T)

# Check it out:
head(data)

# Create tibble (dataframe readable for tidyverse):
data.tb <- tibble(data)

# Create subset: filter only the sample "Methane70":
data.tb.subset <- data.tb %>% filter(sample=="Methane70")

# Create a column with relative abundances of each taxon:
data.tb.subset <- data.tb.subset %>% mutate(rel_abun=(reads/sum(reads)))

# the plot 
ggplot(data=data.tb.subset, aes(x=sample, y=rel_abun, fill=taxonomy))+ 
  geom_bar(stat="identity", position="stack", color="black")
# Can you see the problem in the plot?

# Let's collapse taxa with <1% relative abundance into a generic category.
# Rename the taxonomy row where the relative abundance is lower than 0.01:
data.tb.subset$taxonomy[ data.tb.subset$rel_abun<0.01 ] <- "taxa <1% relative abundance"

# Let's generate the plot with the new dataset:
data.plot <- ggplot(data=data.tb.subset, aes(x=sample, y=rel_abun, fill=taxonomy))+ 
  geom_bar(stat="identity", position="stack", color="black")
# Here's the plot:
data.plot

# Let's save the plot for further processing
ggsave(filename = "16S_plot.pdf", plot=data.plot, device="pdf", width=25, height=15, units="cm")
