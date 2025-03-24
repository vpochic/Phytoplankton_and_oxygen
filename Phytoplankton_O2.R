### Phytoplankton do NOT produce 50% of the oxygen we breathe ###

# R script for a blog post on phycoplankton.wordpress.com

# V. POCHIC, 2025/03/24

### Packages ####
library(tidyverse)
library(packcircles)

### Data ####

## The data were extracted from this blog post (and sources therein):
# https://scottstoll.com/how-much-oxygen-is-in-the-atmosphere/
# And this article:
# https://doi.org/10.1016/j.scib.2018.07.023

Oxygen_stocks <- read.csv2('Data/Oxygen_stocks.csv', header = TRUE,
                           fileEncoding = 'ISO-8859-1')
Oxygen_budget <- read.csv2('Data/Oxygen_budget.csv', header = TRUE,
                           fileEncoding = 'ISO-8859-1')

### Circle packing chart - Oxygen stocks in atmosphere ####

# We're going to use circle packing charts with the packcircles package to 
# visualise proportions

## I got it mainly from this tutorial: 
# https://r-graph-gallery.com/circle-packing.html

# First, we need to create a layer with the info to pass on to ggplot
packing_stocks <- circleProgressiveLayout(Oxygen_stocks$Oxygen.Gt, sizetype='area')

# Get this new layer in the data tibble
Oxygen_stocks <- cbind(Oxygen_stocks, packing_stocks)

# This step allows to give coordinates to our circles
coord_stocks <- circleLayoutVertices(packing_stocks, npoints=50)

# Plot - oxygen stocks and fluxes

# color palette
palette_o2_2colors <- c('#BBD4F2', '#695A44')

ggplot() + 
  
  # Circles
  geom_polygon(data = coord_stocks, 
               aes(x, y, group = id, fill=as.factor(id)), 
               color = "grey10", linewidth = .2) +
  # Color
  scale_fill_discrete(type = palette_o2_2colors) +
  
  # Text (name of reservoirs and Gt of O2)
  geom_text(data = Oxygen_stocks, aes(x, y, label = Stock,
                                      size = Oxygen.Gt),
            nudge_y = 30,
            nudge_x = -45) +
  geom_text(data = Oxygen_stocks, aes(x, y, label = Oxygen.Gt,
                                      size = Oxygen.Gt),
            nudge_y = -30,
            nudge_x = -45) +
  scale_size_continuous(range = c(3,7)) +
  
  # Title
  ggtitle(label = "Oxygen in the Earth's atmosphere (Gt)") +
  
  # Theme
  theme_void() + 
  theme(legend.position="none") +
  coord_equal()

# ggsave('Oxygen_stocks.png', height = 164, width = 164, units = 'mm',
#        dpi = 300)

### Doughnut plot - Oxygen budget ####

# Let's now plot the oxygen budget (positive and negative fluxes) as a 
# doughnut plot

# Many thanks to the writers of this tutorial:
# https://r-graph-gallery.com/128-ring-or-donut-plot.html



# Plot - oxygen budget

# We'll need to compute another variable that is the absolute value of the
# fluxes
Oxygen_budget <- Oxygen_budget %>%
  mutate(abs.Oxygen.Gt = abs(Oxygen.Gt))

# And a ymin/ymax variable to create the coordinates of the doughnut plot
# Compute fractions
Oxygen_budget$fraction = Oxygen_budget$abs.Oxygen.Gt / sum(Oxygen_budget$abs.Oxygen.Gt)

# Compute the cumulative sum (top of each rectangle)
Oxygen_budget$ymax = cumsum(Oxygen_budget$fraction)

# Compute the bottom of each rectangle
Oxygen_budget$ymin = c(0, head(Oxygen_budget$ymax, n=-1))

# Great

# Let's get the Process variable as a factor to reorder it as we like
Oxygen_budget <- Oxygen_budget %>%
  mutate(Process = as_factor(Process)) %>%
  mutate(Process = fct_relevel(Process, 'Land photosynthesis',
                               'Ocean photosynthesis', 'Fires',
                               'Livestock respiration', 'Human respiration',
                               'Fossil fuels combustion', 'Residual'))

# color palettes
palette_o2_7colors <- c('#1F3700', '#9DA51E',
                        '#FF6448', '#8D6456',
                        '#C7AEAA', 'black',
                        'grey65')
palette_labels <- c('white', 'black', 'black', 'black', 
                    'black', 'white', 'black')

ggplot(data = Oxygen_budget) + 
  
  # Barplot + coord_polar + doughnut hole
  geom_rect(data = Oxygen_budget, 
               aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Process), 
               linewidth = .8,
            color = '#695A44') +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  
  # Color
  scale_fill_discrete(type = palette_o2_7colors) +
  
  # Text (name of process and Gt of O2)
  geom_text(data = Oxygen_budget, aes(x = 3.5, y = (ymax + ymin)/2, 
                                      label = Oxygen.Gt, color = Process)) +
  scale_color_discrete(type = palette_labels, guide = 'none') +
  
  # Title
  ggtitle(label = "Atmospheric oxygen budget (Gt of O2 per year)") +
  
  # Theme
  theme_void() + 
  theme(legend.position="bottom")

# ggsave('Oxygen_budget.png', height = 164, width = 164, units = 'mm',
#        dpi = 300)
