par(mar=c(0,0,0,0))
plot(c(-20, 120), c(-20, 120), type="n")
# outer triangle, segment: x0 y0 x1 y1
segments(0,0,100,0); segments(100,0,50,100); segments(50,100,0,0)
for (i in seq(1,4)) {
  segments(20*i,0,(100-20*i)/2+20*i,100-20*i, lty = 3)
  segments(20*i,0,20*i/2,20*i, lty = 3)
  segments(20*i/2,20*i,100-20*i/2,20*i, lty = 3)
}
for (i in seq(0,5)) {
  text(20*i, -5, labels = i*20)
  text((100-20*i)/2+20*i+5/sqrt(2)*2, 100-20*i+5/sqrt(2), labels=100-i*20)
  text(20*i/2-5/sqrt(2)*2, 20*i+5/sqrt(2), labels=100-i*20)
}
p1 <- c(1,2,7)
points(p1[1]/sum(p1)*100+p1[2]/sum(p1)*100/2, p1[2]/sum(p1)*100, pch=19, cex=1, col="red")
p1 <- c(5,5,5)
points(p1[1]/sum(p1)*100+p1[2]/sum(p1)*100/2, p1[2]/sum(p1)*100, pch=19, cex=1, col="green")

d = matrix(rep(0,150*3), ncol=3)
for (j in 1:3) {
  for (i in 1:150) {
    d[i,j] = as.numeric(dist(rbind(iris[i,1:4], colMeans(iris[(1+50*(j-1)):(50*j),1:4]))))
  }
}

par(mar=c(0,0,0,0))
plot(c(-20, 120), c(-20, 120), type="n")
# outer triangle, segment: x0 y0 x1 y1
segments(0,0,100,0); segments(100,0,50,100); segments(50,100,0,0)
for (i in seq(1,4)) {
  segments(20*i,0,(100-20*i)/2+20*i,100-20*i, lty = 3)
  segments(20*i,0,20*i/2,20*i, lty = 3)
  segments(20*i/2,20*i,100-20*i/2,20*i, lty = 3)
}
for (i in seq(0,5)) {
  text(20*i, -5, labels = i*20)
  text((100-20*i)/2+20*i+5/sqrt(2)*2, 100-20*i+5/sqrt(2), labels=100-i*20)
  text(20*i/2-5/sqrt(2)*2, 20*i+5/sqrt(2), labels=100-i*20)
}

for (i in seq(1,50)) {
  p1 <- d[i,]
  points(p1[1]/sum(p1)*100+p1[2]/sum(p1)*100/2, p1[2]/sum(p1)*100, pch=19, cex=1, col="red")
}
for (i in seq(51,100)) {
  p1 <- d[i,]
  points(p1[1]/sum(p1)*100+p1[2]/sum(p1)*100/2, p1[2]/sum(p1)*100, pch=19, cex=1, col="green")
}
for (i in seq(101,150)) {
  p1 <- d[i,]
  points(p1[1]/sum(p1)*100+p1[2]/sum(p1)*100/2, p1[2]/sum(p1)*100, pch=19, cex=1, col="yellow")
}