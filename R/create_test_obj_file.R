# remotes::install_github('coolbutuseless/devout')
# remotes::install_github('coolbutuseless/devoutrgl')
# remotes::install_github('coolbutuseless/triangular')
# remotes::install_github('coolbutuseless/snowcrash')
# remotes::install_github('coolbutuseless/cryogenic')
# remotes::install_github('coolbutuseless/ggrgl', ref='main')

library(ggplot2)
library(rgl)
library(ggrgl)

source("R/my_writeOBJ.R")

p <- ggplot(iris, aes(
  x = Sepal.Length, y = Sepal.Width, z = Petal.Length, Y, color = Species
)) + 
  geom_sphere_3d(size = 4) +
  theme_minimal()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Render Plot in 3d with {devoutrgl}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
devoutrgl::rgldev(fov = 30, view_angle = -30)
df <- rgl.ids("all")
p
my_writeOBJ("~/Dropbox/test_rgl.obj", separateDatapoints = TRUE)
invisible(dev.off())

# https://blackthread.io/gltf-converter/ to convert to gltf