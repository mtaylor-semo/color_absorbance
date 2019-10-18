#library(colourvision) No longer need.
#library(ggplot2)
library(photobiology)
library(grid)
library(tidyverse)


# Data downloaded from http://www.cvrl.org/cones.htm
wavelengths <- read_csv("linss10e_1.csv", 
                        col_names = TRUE)

absorbance_minimum = 0.10

hum_blue <- wavelengths %>% 
  select(wavelength, blue) %>% 
  filter(blue >= absorbance_minimum)

hum_green <- wavelengths %>% 
  select(wavelength, green) %>% 
  filter(green >= absorbance_minimum)

hum_red <- wavelengths %>% 
  select(wavelength, red) %>% 
  filter(red >= absorbance_minimum)

# These use the photor() function from the colourvision package.
# The package loads X11, which crashes the R session when quitting X11.

#hum_blue <- photor(lambda.max = c(442), lambda = seq(400, 500, 1))
#hum_blue <- hum_blue[hum_blue$lambda.max442 >= absorbance_minimum,]

# hum_green <- photor(lambda.max = c(543), lambda = seq(430, 610, 1))
# hum_green <- hum_green[hum_green$lambda.max543 >= absorbance_minimum,]
# 
# hum_red <- photor(lambda.max = c(570), lambda = seq(460, 640, 1))
# hum_red <- hum_red[hum_red$lambda.max570 >= absorbance_minimum,]

spectrum_min <-  400
spectrum_max <- 650

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

line_weight = 1

ggplot(data = NULL, aes(x = wavelength)) +
  annotation_custom(g, xmin = 400, xmax = 700, ymin = -Inf, ymax = Inf) +
  geom_vline(xintercept = c(450, 500, 550, 600), color = "gray75", size = 0.25) +
  geom_line(data = hum_blue, 
            aes(y = blue), 
            color = "white", 
            size = line_weight) +
  geom_line(data = hum_green, 
            aes(y = green), 
            color = "white",
            size = line_weight) +
  geom_line(data = hum_red, 
            aes(y = red), 
            color = "white",
            size = line_weight) +
  scale_x_continuous(limits = c(spectrum_min, spectrum_max), 
                     breaks = seq(spectrum_min, spectrum_max, 10), 
                     expand = c(0.01, 0)) +
#  scale_y_continuous(breaks = seq(0, 1, 0.1)) +
  theme(panel.background = element_blank(), 
        panel.border = element_blank(),
        panel.grid = element_blank()) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(x = "Wavelength (nm)",
       y = "Relative absorbance") +
  annotate("text", 
           x = 448, 
           y = 1.05, 
           label = "Short", 
           color = "white",
           size = 7,
           family = "Linux Biolinum O",
           fontface = "bold") +
  annotate("text", 
           x = 541, 
           y = 1.05, 
           label = "Medium", 
           color = "black",
           size = 7,
           family = "Linux Biolinum O",
           fontface = "bold") +
  annotate("text", 
           x = 569, 
           y = 1.05, 
           label = "Long", 
           color = "black", 
           size = 7,
           family = "Linux Biolinum O",
           fontface = "bold") +
  theme(text = element_text(family = "Linux Biolinum O",
                            size = 16))


