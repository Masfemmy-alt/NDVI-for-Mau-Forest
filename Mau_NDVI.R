
setwd("C:/Users/Ayoola_John/OneDrive/Desktop/Application of GIS/Mau area Stacked images for Land cover analysis")

library(terra)
library(tmap)
library(ggplot2)

Mau_1995 <- rast("Mau_1995_01_21_stack_v3.tif")


Mau_2000 <- rast("Mau_2000_02_12_stack_v1.tif")


Mau_2015 <- rast("Mau_2015_02_13_stack_v1.tif")


Mau_2025 <- rast("Mau_2025_02_08_stack_v1.tif")

       
NDVI_1995 <- (Mau_1995[[5]] - Mau_1995[[4]]) / (Mau_1995[[5]] + Mau_1995[[4]])

NDVI_2000 <- (Mau_2000[[5]] - Mau_2000[[4]]) / (Mau_2000[[5]] + Mau_2000[[4]])

NDVI_2015 <- (Mau_2015[[5]] - Mau_2015[[4]]) / (Mau_2015[[5]] + Mau_2015[[4]])

NDVI_2025 <- (Mau_2025[[5]] - Mau_2025[[4]]) / (Mau_2025[[5]] + Mau_2025[[4]])


# Align all NDVI layers to the same grid (using 1995 as reference)

NDVI_2000 <- resample(NDVI_2000, NDVI_1995)

NDVI_2015 <- resample(NDVI_2015, NDVI_1995)

NDVI_2025 <- resample(NDVI_2025, NDVI_1995)

tmap_mode("plot")




NDVI_1995T <- tm_shape(NDVI_1995) +
  tm_raster(
    col.scale = tm_scale_continuous(values = "brewer.greens"),
    col.legend = tm_legend(
      title = "NDVI (1995)"
  )) +
  tm_layout(legend.outside = TRUE)

NDVI_1995T

NDVI_2000T <-tm_shape(NDVI_2000) +
  tm_raster(
    col.scale = tm_scale_continuous(values = "brewer.greens"),
    col.legend = tm_legend(
      title = "NDVI (2000)"
    )
  ) +
  tm_layout(legend.outside = FALSE)

NDVI_2000T

NDVI_2015T <- tm_shape(NDVI_2015) +
  tm_raster(
    col.scale = tm_scale_continuous(values = "brewer.greens"),
    col.legend = tm_legend(
      title = "NDVI (2015)"
    )
  ) +
  tm_layout(legend.outside = TRUE)

NDVI_2015T

NDVI_2025T <- tm_shape(NDVI_2025) +
  tm_raster(
    col.scale = tm_scale_continuous(values = "brewer.greens"),
    col.legend = tm_legend(
      title = "NDVI (2025)"
    )
  ) +
  tm_layout(legend.outside = TRUE)

NDVI_2025T

NDVI_Values <- c(0.58, 0.64, 0.32, 0.34)
Year <- c(1995, 2000, 2015, 2025)

Time_Series <- data.frame(Year,NDVI_Values)

Plot <- ggplot(Time_Series, aes(Year, NDVI_Values)) +
  geom_line(color = "darkgreen", linewidth = 1.2) +
  geom_point(color = "darkgreen") +
  labs(title = "NDVI Time-Series (1995–2025)",
       x = "Year", y = "NDVI") +
  theme_minimal()

Plot

NDVI_Arrange <- tmap_arrange(NDVI_1995T, NDVI_2000T, NDVI_2015T, NDVI_2025T)

NDVI_Arrange

tmap_save(NDVI_Arrange, "NDVI.jpeg")

nr <- as.data.frame(NDVI_2025, cell=TRUE)
str(nr)

kmncluster <- kmeans(nr[,-1], centers=10, iter.max = 500, nstart = 5, algorithm="Lloyd")

str(kmncluster)

knr <- rast(NDVI_2025, nlyr=1)

knr[nr$cell] <- kmncluster$cluster
knr

mycolor <- c("#fef65b","#ff0000", "#daa520","#0000ff","#0000ff","#00ff00","#cbbeb5",
             "#c3ff5b", "#ff7373", "#00ff00")

plot(knr, main = 'Unsupervised classification', col = mycolor, type="classes")

hist(NDVI_2025, main = "NDVI values for 2025", xlab = "NDVI", ylab= "Frequency",
     col = "wheat", xlim = c(-0.5, 0.2), breaks = 10, xaxt = "n")

axis(side=1, at = seq(-0.6, 1, 0.2), labels = seq(-0.6, 1, 0.2))

veg <- clamp(NDVI_2000, 0.4, values=FALSE)
plot(veg, main='Vegetation')




