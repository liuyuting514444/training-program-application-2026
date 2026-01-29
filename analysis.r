# ---------------------------------------------------------

# Melbourne Bioinformatics Training Program

# This exercise to assess your familiarity with R and git. Please follow
# the instructions on the README page and link to your repo in your application.
# If you do not link to your repo, your application will be automatically denied.

# Leave all code you used in this R script with comments as appropriate.
# Let us know if you have any questions!


# You can use the resources available on our training website for help:
# Intro to R: https://mbite.org/intro-to-r
# Version Control with Git: https://mbite.org/intro-to-git/

# ----------------------------------------------------------

# Load libraries -------------------
# You may use base R or tidyverse for this exercise
library(tidyverse)
# ex. library(tidyverse)

# Load data here ----------------------
# Load each file with a meaningful variable name.

metadata <- read.csv(
  "data/GSE60450_filtered_metadata.csv",
  stringsAsFactors = FALSE
)

expression_data <- read.csv(
  "data/GSE60450_GeneLevel_Normalized(CPM.and.TMM)_data.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)


# Inspect the data -------------------------

# What are the dimensions of each data set? (How many rows/columns in each?)
# Keep the code here for each file.

## Expression data
dim(expression_data)

## Metadata
dim(metadata)

# Prepare/combine the data for plotting ------------------------
# How can you combine this data into one data.frame?
colnames(expression_data)[1:5]
colnames(metadata)

expression_data <- expression_data[, colnames(expression_data) != ""]

gene_of_interest <- expression_data[1, ]
gene_name <- gene_of_interest$gene_symbol

expression_long <- gene_of_interest %>%
  select(-gene_symbol) %>%
  pivot_longer(cols = everything(),names_to = "sample",values_to = "expression")

combined_data <- expression_long %>%
  left_join(metadata, by = c("sample" = "X"))

# Plot the data --------------------------
## Plot the expression by cell type
## Can use boxplot() or geom_boxplot() in ggplot2

p <- ggplot(combined_data,
            aes(x = immunophenotype,y = expression,fill = immunophenotype)) +
  
  geom_boxplot(width = 0.6,outlier.shape = NA,alpha = 0.7) +
  
  geom_jitter(aes(color = immunophenotype),width = 0.15,alpha = 0.5,size = 1.5,
              show.legend = FALSE) +
  
  scale_fill_brewer(palette = "RdBu") +
  scale_color_brewer(palette = "RdBu") +
  
  theme_minimal(base_size = 13) +
  
  labs(title = paste("Expression of", gene_name, "by Immunophenotype"),
    x = "Immunophenotype",
    y = "Normalized Expression (CPM/TMM)") +
  
  theme(legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank())

print(p)


## Save the plot
### Show code for saving the plot with ggsave() or a similar function

ggsave(
  "results/Expression_trail.png",
  plot = p,
  width = 8,
  height = 6,
  dpi = 1200
)
