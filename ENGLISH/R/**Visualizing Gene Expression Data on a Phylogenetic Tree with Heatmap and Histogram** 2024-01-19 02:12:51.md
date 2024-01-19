```r
# Load required libraries
library(tidyverse)
library(ggplot2)
library(ggtree)
library(janitor)
library(patchwork)
library(RColorBrewer)

# Read data files
samples <- read_delim("samples.csv", delim = ",")
meta <- read_delim("metadata.csv", delim = ",")
tree <- read_tree("phylogenetic_tree.tre")

# Data preprocessing
# Convert species names to lowercase and remove spaces
samples$species <- tolower(samples$species)
samples$species <- gsub(" ", "_", samples$species)

# Add species names to metadata
meta$species <- rownames(meta)

# Merge samples and metadata
data <- left_join(samples, meta, by = "species")

# Create a phylogenetic tree with branch lengths
tree <- ggtree(tree)

# Calculate branch lengths using Grafen's method
tree <- compute.brlen(tree, method = "grafendijk")

# Add tip labels to the tree
tree <- set_tip_label(tree, data$species)

# Create a heatmap of expression values
heatmap <- ggplot(data, aes(x = species, y = gene, fill = expression)) +
  geom_tile() +
  theme_minimal()

# Create a phylogenetic tree with tip labels and branch lengths
phylotree <- ggtree(tree) +
  geom_tiplab() +
  geom_branchlabel()

# Add the heatmap to the tree as a track
phylotree <- phylotree +
  geom_track(data = data, aes(y = gene, fill = expression)) +
  scale_fill_gradient(low = "blue", high = "red")

# Create a plot of the tree with a histogram of expression values
tree_histogram <- ggtree(tree) +
  geom_histogram(aes(x = species, y = expression, fill = expression)) +
  coord_flip()

# Combine the heatmap, tree, and histogram into a single plot
plot <- heatmap / phylotree / tree_histogram

# Add a legend to the plot
plot <- plot +
  theme(legend.position = "bottom")

# Save the plot to a file
ggsave(plot, filename = "expression_heatmap_tree_histogram.pdf", width = 10, height = 8)
```

Explanation:

1. We start by loading the necessary libraries.
2. Then, we read the data files, including the samples' expression values, metadata, and phylogenetic tree.
3. We preprocess the data by converting species names to lowercase and removing spaces, and we add species names to the metadata.
4. We merge the samples and metadata data frames to combine the expression values with the metadata.
5. We create a phylogenetic tree with branch lengths using the `ggtree` package.
6. We add tip labels to the tree using the species names from the data frame.
7. We create a heatmap of expression values using the `ggplot2` package.
8. We add the heatmap to the tree as a track using the `geom_track()` function.
9. We use `coord_flip()` to change the orientation of the tree.
10. We create a plot of the tree with a histogram of expression values using the `geom_histogram()` function.
11. We combine the heatmap, tree, and histogram into a single plot using the `/` operator.
12. We add a legend to the plot using the `theme()` function.
13. Finally, we save the plot to a PDF file.