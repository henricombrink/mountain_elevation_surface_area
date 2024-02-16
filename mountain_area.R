##############################################
# MOUNTAIN SURFACE AREA PER ELEVATION INTERVAL
# Code by: Henri Combrink (henricombrink@gmail.com)
# First compiled: 021324
# Last edit: 021624
#############################################

#############
# Load packages
#############

if (!require("pacman")) install.packages("pacman")
pacman::p_load(terra, ggplot2, tidyterra, patchwork, ggspatial, png, here)

if(!require("ggfx")) devtools::install_github('thomasp85/ggfx')
require(ggfx)


##############
# Load data 
##############
#Zipped tif available here:
#https://drive.google.com/file/d/1D313HL_lqXZ07S0GhVa_Q9u_3WNCAQF4/view?usp=drive_link

mtg_clipped <- terra::rast(here::here("mntg_clipped_dem.tif"))
mtg_clipped_df <- as.data.frame(mtg_clipped, xy = TRUE)

#mtgpng <- readPNG(here::here("mountgraham_outline.png"))

cs <- terra::cellSize(mtg_clipped, unit = "km")
#terra::global(cs, fun = "sum")
#cs_exp <- terra::expanse(mtg_clipped, unit = "km")

temp <- c(mtg_clipped,cs)
names(temp) <- c("dem","area")

dem_seq <- seq(min(values(temp$dem), na.rm = TRUE), max(values(temp$dem), na.rm = TRUE),
               by = 50)

dem_area <- data.frame(
  dem = dem_seq,
  area = NA
)

for(i in 1:(length(dem_seq)-1)){
  sumval <- sum(as.data.frame(temp[temp$dem > dem_seq[i] & temp$dem <= dem_seq[i+1]])[,"area"])
  dem_area[i,"area"] <- sumval
}#
dem_area <- dem_area[-(i+1),]


# clean up
rm(mtg_clipped_df, mtg_clipped, temp, dem_seq, cs); gc()


##############
# Plots
##############

# Area plots
# Area plot 1

area_plot <-
ggplot(data = dem_area,
       aes(x = dem, y = area, color = dem)) +
  geom_line(color = "black") +
  #geom_path(linewidth = 1.7, color = "black", linejoin = "bevel") + 
  #geom_path(linewidth = 1.3, linejoin = "bevel") + 
  geom_point(aes(fill = dem), size = 3, shape = 21, color = "black") +
  #annotation_raster(mtgpng, ymin = 30,ymax= 80,xmin = 2500,xmax = 3000) +
  ylab(expression ("Surface area"~(km^2))) +
  xlab("Elevation (m)") +
  coord_flip() +
  scale_color_distiller(palette = "Spectral",
                       name ="Elevation (m)") +
  scale_fill_distiller(palette = "Spectral",
                        name ="Elevation (m)") +
  #ggtitle("Surface area by 100m elevation intervals") +
  theme_classic()


# Making a raster for the mask layer below
rasterdf <- data.frame(
  seqdem = seq(min(dem_area$dem), max(dem_area$dem), 5),
  seqarea = seq(min(dem_area$area), max(dem_area$area), length.out = 341),
  maxarea = rep(max(dem_area$area), 341),
  maxdem = rep(max(dem_area$dem), 341)
)


area_fill_plot <-
ggplot(data = dem_area,
         aes(x = dem, y = area, fill = dem)) +
  as_reference(
      geom_area(),
      id = "mask_layer"
    ) +
  with_mask(
      geom_tile(data = rasterdf, inherit.aes = F,
                aes(x = seqdem, y = maxarea/2, height = maxarea, fill = seqdem, color = seqdem)),
      mask = "mask_layer",
      invert = F
    ) +
  geom_line(color = "black") +
  geom_point(aes(fill = dem), size = 2, shape = 21, color = "black") +
  scale_color_distiller(palette = "Spectral",
                        name ="Elevation (m)") +
  scale_fill_distiller(palette = "Spectral",
                         name ="Elevation (m)") +
  ylab(expression ("Surface area"~(km^2))) +
  xlab("Elevation (m)") +
  coord_flip() +
  theme_classic()

# Elevation plot
# Plot 1

elevation_plot <-
  ggplot() +
  geom_raster(data = mtg_clipped_df,
              aes(x=x, y=y, fill=mntg_clipped_dem)) +
  coord_equal() +
  #coord_cartesian() +
  #annotation_scale(plot_unit = "m") +
  scale_fill_distiller(palette = "Spectral",
                       name ="Elevation (m)") +
  ylab("Latitude") +
  xlab("Longitude") +
  theme_classic() 

##############
# Merged Plots
##############

# Set layout
d1 <- "
122
"

###
# 1
###

final_plot <-
elevation_plot +
  area_plot + 
  plot_layout(design = d1,
              guides = "collect") +
  plot_annotation(title = "Pinaleño Mountains",
                  tag_levels = "a")
final_plot

ggsave(plot = final_plot,
       filename = here::here("elevation_area_plot.pdf"),
       scale = 2,
       width = NA,
       height = NA,
       units = "mm",
       dpi = 300)

#####
# 2
#####

final_plot_2 <-
area_plot +
  inset_element(elevation_plot + theme(legend.position = "none"), 0.35, 0.25, 1, 1.03) +
  plot_annotation(#title = "Pinaleño Mountains",
                  tag_levels = "a")
final_plot_2
ggsave(plot = final_plot_2,
       filename =  here::here("elevation_area_plot_2.pdf"),
       scale = 2,
       width = NA,
       height = NA,
       units = "mm",
       dpi = 300)

#####
# 3
#####

final_plot_3 <-
area_fill_plot +
  inset_element(elevation_plot + theme(legend.position = "none"), 0.35, 0.25, 1, 1.03) +
  plot_annotation(#title = "Pinaleño Mountains",
    tag_levels = "a")
final_plot_3
ggsave(plot = final_plot_3,
       filename =  here::here("elevation_area_plot_3.pdf"),
       scale = 2,
       width = NA,
       height = NA,
       units = "mm",
       dpi = 300)

###### END ########