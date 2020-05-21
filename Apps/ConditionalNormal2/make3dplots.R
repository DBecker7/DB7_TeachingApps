library(mvtnorm)


myx <- seq(-4,4,0.2)
mymat <- expand.grid(x = myx, y = myx)
sigma <- matrix(c(2,1,1,2), ncol = 2)
z <- dmvnorm(mymat, sigma = sigma)
z2 <- matrix(z, ncol = length(myx), byrow = FALSE)
cols <- cut(z2, breaks = 20, labels = FALSE)
persp(z2, xlab = "x", ylab = "y", zlab = "z", theta = 45, phi = 40, 
    col = terrain.colors(max(cols))[cols])







library(ggplot2)
myx <- seq(-4,4,0.1)
mydf <- as.data.frame(expand.grid(x = myx, y = myx))
sigma <- matrix(c(2,1,1,2), ncol = 2)
mydf$z <- dmvnorm(mymat, sigma = sigma)
ggplot(mydf, aes(x = x, y = y, fill = z)) + geom_tile()




library(plot3D)

layout(mat = matrix(c(1,1,2,3), ncol = 2))
par(mar = c(5,2,2,1))
persp3D(myx, myx, z2, col = terrain.colors(30), phi = 45, theta = 30, colkey = FALSE)
points3D(x = myx[20] + myx*0.001, myx, z2[20,]+0.001, type = "l", add = TRUE, col = "darkorchid", lwd = 4)
points3D(y = myx[20] + myx*0.001, x = myx, z2[,20]+0.001, type = "l", add = TRUE, col = "blue", lwd = 4)
plot(myx, z2[2,])
plot(myx, z2[2,])


library(rgl)
library(mvtnorm)
myx <- seq(-4,4,0.2)
mymat <- expand.grid(x = myx, y = myx)
sigma <- matrix(c(2,1,1,2), ncol = 2)
z <- dmvnorm(mymat, sigma = sigma)
z2 <- matrix(z, ncol = length(myx), byrow = FALSE)
cols <- cut(z2, breaks = 20, labels = FALSE)
rgl.open(useNULL = FALSE)

userMatrix <- matrix(c(0.86,0.4,-0.3,0, -0.5,0.65,-0.57,0, 0,0.65,0.75,0, 0,0,0,1), ncol = 4)
par3d(userMatrix = userMatrix)

persp3d(x = myx, y = myx, z = z2, col = terrain.colors(max(cols))[cols])
lines3d(x = myx[20], y = myx, z = z2[20,], lwd = 3, col = 3)









