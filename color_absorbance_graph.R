
# Code from this answer to my question
# https://stackoverflow.com/a/58417825/3832941


# Libraries ---------------------------------------------------------------
#
#library(photobiology)
library(grid)
library(tidyverse)


# Constants ---------------------------------------------------------------
#
# Filter some data to mimic traditional absorbance courves
# without going all the to zero
absorbance_minimum = 0.10

spectrum_min <-  400  # For x-axis
spectrum_max <- 650

line_weight = 1  # For opsin curves in plot



# Data manipulation -------------------------------------------------------

# Data downloaded from http://www.cvrl.org/cones.htm
# read.csv("http://www.cvrl.org/database/data/cones/linss10e_1.csv")
wavelengths <- read_csv("linss10e_1.csv", 
                        col_names = TRUE)


# Separate the data to plot individual curves
hum_blue <- wavelengths %>% 
  select(wavelength, blue) %>% 
  filter(blue >= absorbance_minimum)

hum_green <- wavelengths %>% 
  select(wavelength, green) %>% 
  filter(green >= absorbance_minimum)

hum_red <- wavelengths %>% 
  select(wavelength, red) %>% 
  filter(red >= absorbance_minimum)

## From colourvision package. No longer used but left for legacy.
## colourvision loads X11 automatically, which crashed Rstudio when quit.

# These use the photor() function from the colourvision package.
# The package loads X11, which crashes the R session when quitting X11.

#hum_blue <- photor(lambda.max = c(442), lambda = seq(400, 500, 1))
#hum_blue <- hum_blue[hum_blue$lambda.max442 >= absorbance_minimum,]

# hum_green <- photor(lambda.max = c(543), lambda = seq(430, 610, 1))
# hum_green <- hum_green[hum_green$lambda.max543 >= absorbance_minimum,]
# 
# hum_red <- photor(lambda.max = c(570), lambda = seq(460, 640, 1))
# hum_red <- hum_red[hum_red$lambda.max570 >= absorbance_minimum,]

# 10 added to maximum spectrum to shift the gradient enough
# so the rightmost curve peaks in yellow
#
# Of the CIE XYZ models in the photobiology package, 
# CMF10 gave the best matching spectrum
# gradient <- t(w_length2rgb(spectrum_min:spectrum_max, sens = ciexyzCMF10.spct))

wavelengths[is.na(wavelengths)] <- 0
wavelengths$colours <- rgb(wavelengths$red, wavelengths$green, wavelengths$blue)

gradient <- t(wavelengths$colours[wavelengths$wavelength>=400 & wavelengths$wavelength<=700])

#gradient <- t(rev(rainbow(20))) # higher value for smoother gradient
g <- rasterGrob(gradient, 
                width = unit(1, "npc"), 
                height = unit(1, "npc"), 
                interpolate = TRUE) 



# Spectrum plots ----------------------------------------------------------
# These are plots with the background spectrums, then adding curves.

base_plot <- 
  ggplot(data = NULL, aes(x = wavelength)) +
  annotation_custom(g, xmin = 400, xmax = 700, ymin = -Inf, ymax = Inf) +
  scale_x_continuous(limits = c(spectrum_min, spectrum_max), 
                     breaks = seq(spectrum_min, spectrum_max, 10), 
                     expand = c(0.01, 0)) +
  theme(panel.background = element_blank(), 
        panel.border = element_blank(),
        panel.grid = element_blank()) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(x = "Wavelength (nm)",
       y = NULL) +
  theme(text = element_text(family = "Linux Biolinum O",
                            size = 12))

ggsave(filename = "~/Pictures/teach/163/activities/spectrum_base_plot.png",
       plot = base_plot,
       height = 4,
       width = 6,
       units = "in")

dichromat_plot <- base_plot +
  geom_vline(xintercept = c(450, 500, 550, 600), color = "gray75", size = 0.25) +
  geom_line(data = hum_blue,
            aes(y = blue),
            color = "white",
            size = line_weight) +
  geom_line(data = hum_green,
            aes(y = green),
            color = "white",
            size = line_weight) +
  ylab("Relative sensitivity") +
  annotate("text",
           x = 448,
           y = 1.05,
           label = "Short",
           color = "white",
           size = 4,
           family = "Linux Biolinum O",
           fontface = "bold") +
  annotate("text",
           x = 541,
           y = 1.05,
           label = "Medium",
           color = "black",
           size = 4,
           family = "Linux Biolinum O",
           fontface = "bold")

trichromat_plot <- dichromat_plot + 
  geom_line(data = hum_red,
            aes(y = red),
            color = "white",
            size = line_weight) +
  annotate("text",
           x = 569,
           y = 1.05,
           label = "Long",
           color = "black",
           size = 4,
           family = "Linux Biolinum O",
           fontface = "bold")

ggsave(filename = "~/Pictures/teach/163/activities/dichromat_plot.png", 
       plot = dichromat_plot,
       height = 4,
       width = 6,
       units = "in")

ggsave(filename = "~/Pictures/teach/163/activities/trichromat_plot.png",
       plot = trichromat_plot,
       height = 4,
       width = 6,
       units = "in")

# Plots without spectrum --------------------------------------------------
#
# These plots do not have the spectrum for the student handouts.

dichromat_plain_plot <- 
  ggplot(data = NULL, aes(x = wavelength)) +
  geom_vline(xintercept = c(450, 500, 550, 600), color = "gray75", size = 0.25) +
  geom_line(data = hum_blue,
            aes(y = blue),
            size = line_weight) +
  geom_line(data = hum_green,
            aes(y = green),
            size = line_weight) +
  scale_x_continuous(limits = c(spectrum_min, spectrum_max), 
                     breaks = seq(spectrum_min, spectrum_max, 10), 
                     expand = c(0.01, 0)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(x = "Wavelength (nm)",
       y = "Relative sensitivity") +
  annotate("text",
           x = 448,
           y = 1.05,
           label = "Short",
           size = 4,
           family = "Linux Biolinum O",
           fontface = "bold") +
  annotate("text",
           x = 541,
           y = 1.05,
           label = "Medium",
           color = "black",
           size = 4,
           family = "Linux Biolinum O",
           fontface = "bold") +
  theme(text = element_text(family = "Linux Biolinum O",
                            size = 12))

trichromat_plain_plot <- dichromat_plain_plot +
  geom_line(data = hum_red,
            aes(y = red),
            size = line_weight) +
  annotate("text", 
           x = 569, 
           y = 1.05, 
           label = "Long", 
           color = "black", 
           size = 4,
           family = "Linux Biolinum O",
           fontface = "bold")
  
ggsave(filename = "~/pictures/teach/163/activities/dichromat_plain_plot.png",
       plot = dichromat_plain_plot,
       width = 6,
       height = 4,
       units = "in")

ggsave(filename = "~/pictures/teach/163/activities/trichromat_plain_plot.png",
       plot = trichromat_plain_plot,
       width = 6,
       height = 4,
       units = "in")
