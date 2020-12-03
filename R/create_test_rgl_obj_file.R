library(rgl)

plotids <- with(iris, plot3d(Sepal.Length, Sepal.Width, Petal.Length, 
                             type="s", col=as.numeric(Species)))
writeOBJ("~/Dropbox/test_rgl.obj")
writeSTL("~/Dropbox/test_rgl.stl")
writePLY("~/Dropbox/test_rgl.ply")

# All files do not contain text...

# Use blender to convert to gltf (separate) (levy docker for that?)
# store in media