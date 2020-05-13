# Animation for K-function, just for fun.
library(ggforce)
library(dplyr)
library(gganimate)
library(plotrix)


pointx <- runif(50, 0, 10)
pointy <- runif(50, 0, 10)

laminv <- length(pointx)/(4*4)

#plot(pointx, pointy, xlim = c(0,1), ylim = c(0,1))

rseq <- seq(0,3,0.25)
mydata <- expand.grid(r = rseq, point = 1:length(pointx))
mydata$x <- rep(pointx, each = length(rseq))
mydata$y <- rep(pointy, each = length(rseq))
mydata$K <- NA

#head(mydata)

for(i in 1:nrow(mydata)){
    x <- mydata$x[i]
    y <- mydata$y[i]
    xx <- pointx[-which(pointx == x)]
    yy <- pointy[-which(pointy == y)]
    mydata$K[i] <- sum(sqrt((x - xx)^2 + (y - yy)^2) < mydata$r[i])
}

library(tidyr)
cumk <- mydata %>% 
    pivot_wider(names_from = r, values_from = K) %>% 
    select(-point, -x, -y) %>% 
    apply(2, cumsum) %>% 
    magrittr::divide_by(length(pointx)*laminv) %>% 
    as.data.frame() %>% 
    mutate(id = 1:n()) %>% 
    pivot_longer(cols = 1:length(rseq), names_to = "r", values_to = "K")

cumki <- list()
for(i in 1:nrow(mydata)){
    if(i <= length(rseq)){
        rdiv <- rep(mydata$point[i], i)
    } else {
        rdiv <- rep(mydata$point[i] - 1, length(rseq))
        rdiv[1:which(rseq == mydata$r[i])] <- mydata$point[i]
    }
    
    cumki[[i]] <- mydata %>% 
        slice(1:i) %>% 
        pivot_wider(names_from = r, values_from = K) %>% 
        select(-point, -x, -y) %>% 
        apply(2, sum, na.rm=TRUE) %>% t() %>% 
        magrittr::divide_by(rdiv*laminv) %>% 
        as.data.frame() %>% 
        mutate(id = 1:n()) %>% 
        pivot_longer(cols = 1:length(unique(mydata$r[1:i])), names_to = "r", values_to = "K")
}

i <- nrow(mydata)
#for(i in 1:nrow(mydata))# uncomment for animation
    { 
    par(mfrow = c(1,2))
    par(mar = c(0,0,0,0))
    plot(mydata$x, mydata$y, asp = 1, xlim = c(0,10), ylim = c(0,10),
        xaxt="n", yaxt = "n", bty = "n", xlab = "", ylab = "")
    draw.circle(x = mydata$x[i], y = mydata$y[i], r = mydata$r[i])
    text(x = mydata$x[i], y = mydata$y[i], labels = mydata$K[i], adj = c(1,1))
    
    par(mar = c(5,4,4,2) + 0.1)
    plot(NULL, xlim = c(0,max(rseq)), ylim = c(0,length(pointx)/laminv^2),
        xlab = "r", ylab = "K function")
    newcols <- c(rep("grey", mydata$point[i] - 1), 2)
    for(j in 1:mydata$point[i]){
        lines(I(K/laminv) ~ r, data = subset(mydata[1:i,], point == j), col = newcols[j])
    }
    lines(K ~ r, data = cumki[[i]], lwd = 2)
    lines(rseq, pi*rseq^2/laminv^2, col = 3, lwd = 2)
    legend("topleft", legend = c("K-Function (avg of contribs)", 
        "current contribution", "previous contributions", "theoretical"), 
        col = c(1,2,8,3), lty=1, lwd=c(2,1,1,2), bg = rgb(0,0,0,0), bty = "n")
    Sys.sleep(0.05)
}

















