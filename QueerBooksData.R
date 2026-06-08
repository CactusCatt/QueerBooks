library(tidyverse)
library(tidytext)
library(ggplot2)
library(plotly)
library("scales")
library(dplyr)
library(readr)
library(hrbrthemes)

# Reading the QBD data
qbd_data <- read.csv("https://raw.githubusercontent.com/CactusCatt/QueerBooks/refs/heads/main/Queer%20Books%20Database_Fiction%20Database.csv", stringsAsFactors = FALSE)

# Number of books published each year
year_count <- qbd_data %>% count(Year, sort=TRUE)

# Separating each identity into its own line
qbd_identity <- qbd_data %>% unnest_tokens(Identity, Identity)

# Counting each identity, excluding "sc" (side characters)
identity_count <- qbd_identity %>% count(Identity, sort=TRUE) %>% filter(Identity != "sc")

# Visualization 1: Breakdown of Queer Identities
ggplot(data = identity_count, aes(y = n, x = Identity, label=n)) +
  geom_col(aes(fill=Identity)) +
  labs(
    title="Representation of Queer Identities in Fiction Literature",
    subtitle="From Queer Books Database",
    x="Identities",
    y="Number of Appearances"
  ) +
  geom_text(vjust = -0.5, size = 3)

# Taking the top 12 identities (exluding "sc")
top_identities <- identity_count %>% slice_max(n=12, order_by=n)

top_identities_vector <- top_identities$Identity

# Grouping number of books per identity by year
identities_per_year <- qbd_identity %>% group_by(Identity, Year) %>%
  count(Identity) %>% filter(Identity %in% top_identities_vector)

# Visualization 2: Interactive Identities Over Time
year_plot <- ggplot(data = identities_per_year) +
  geom_line(mapping = aes(x=Year, y=n, group=1, color=Identity, text=paste("Identity:",Identity,"\nYear:",Year,"\nAppearances:",n)), linewidth = 0.5) +
  labs(
    title="Top Identities Over Time in Queer Fiction Literature",
    x="Publication Year",
    y="Number of Appearances",
    color="Identity"
  ) + scale_x_continuous(
    limits = c(2010, 2025)
  )
ggplotly(year_plot, tooltip="text")

# Combining the three columns pertaining to disability/neurodivergence for easier use, and separating each value into its own line
qbd_disability <- unite(qbd_data, col = "Disability", Disability, Mental.Health, Neurodivergence, sep = ",") %>% unnest_tokens(Disability, Disability, token="regex", pattern=",")

# Removing extra white space
qbd_disability$Disability <- trimws(qbd_disability$Disability)

disability_count <- qbd_disability %>% count(Disability, sort=TRUE)

# Taking the top 12 disabilities, excluding "sc"
top_disabilities <- disability_count %>% slice_max(n=13, order_by=n) %>% filter(Disability != "sc")

# To replace the abbreviations with actual names
replacements <- c("anx" = "Anxiety", "adhd" = "ADHD", "a/p" = "Amputee", "add-alc" = "Addiction", "aut" = "Autism", "chr-i" = "Chronic Illness", "chr-p" = "Chronic Pain", "dep" = "Depression", "misc" = "Other", "mobile" = "Mobility", "ptsd" = "PTSD", "trauma" = "Trauma")

translated_disabilities <- top_disabilities %>% mutate(Disability = str_replace_all(top_disabilities$Disability, replacements))

# Visualization 3: Breakdown of Disability/Neurodivergence
ggplot(data = translated_disabilities, aes(y = n, x = Disability, label=n)) +
  geom_col(aes(fill=Disability)) +
  labs(
    title="Representation of Disabilities in Queer Fiction Literature",
    subtitle="From Queer Books Database",
    y="Number of Appearances",
    x="Disability",
  ) + 
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1)) +
  geom_text(vjust = -0.5, size = 3)

# Effectively combining the Disability and Identities datasets
qbd_intersect <- qbd_disability %>% unnest_tokens(Identity, Identity)

# Grouping by both Identity and Disability
intersect_count <- qbd_intersect %>% count(Identity, Disability, sort=TRUE)

# Filtering for only the top 12 Identities and Disabilities
intersect_count <- intersect_count %>% filter(Identity %in% top_identities_vector)

top_disabilities_vector <- top_disabilities$Disability

intersect_count <- intersect_count %>% filter(Disability %in% top_disabilities_vector)

# Using translated disabilities
translated_intersect <- intersect_count %>% mutate(Disability = str_replace_all(intersect_count$Disability, replacements))

# Tooltip for the heatmap
translated_intersect <- translated_intersect %>%
  mutate(text = paste0("Identity: ", Identity, "\n", "Disability: ", Disability, "\n", "Appearances: ", n))

# Visualization 4: An interactive heatmap to highlight the intersection of Identity and Disability
heatmap <- ggplot(data = translated_intersect, aes(x = Disability, y = Identity, fill = n, text=text)) +
  geom_tile() +
  scale_fill_distiller(palette = "BuPu", direction=+1) +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1)) +
  labs(x = "Disability", y = "Identity", title = "Intesection of Identity and Disability in Queer Fiction Literature") 

ggplotly(heatmap, tooltip="text")