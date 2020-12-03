library(rgl)

plotids <- with(iris, plot3d(Sepal.Length, Sepal.Width, Petal.Length, 
                             type="s", col=as.numeric(Species)))
writeOBJ("~/Dropbox/test_rgl.obj")

# Use blender to convert to glb (levy dropbox for that)
# store in media