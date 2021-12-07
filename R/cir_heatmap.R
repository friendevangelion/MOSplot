library(bezier)
library(shape)

r_outer = 100
r_mid = 70
r_inner = 40

#pdf("~/test.pdf")
par(mar=c(0,0,0,0))
plot(c(-r_outer,r_outer), c(-r_outer,r_outer), type="n")

#plotcircle(mid=c(0,0), r=r_outer)
#plotcircle(mid=c(0,0), r=r_inner)

PI = 3.1415926*2

n=100

randlist = runif(n,0,1)


for (i in seq(1,n/2)) {
  theta_st = randlist[i*2-1]*PI*2
  theta_en = randlist[i*2]*PI*2
  if (theta_en<theta_st) {
    theta_st = theta_en
    theta_en = randlist[i*2-1]*PI*2
  }
  #theta_st = 0
  #theta_en = 0.01*i*PI
  #while (theta_en->2*PI) { theta_en = theta_en - 2*PI }
  if (theta_en-theta_st<=PI/2) {
    temp_mat <- matrix(c(r_outer*cos(theta_st), r_outer*sin(theta_st), 
                         r_mid*cos(theta_st), r_mid*sin(theta_st), 
                         (16*r_inner*cos((theta_en+theta_st)/2)-r_outer*cos(theta_st)-4*r_mid*cos(theta_st)-4*r_mid*cos(theta_en)-r_outer*cos(theta_en))/6,
                         (16*r_inner*sin((theta_en+theta_st)/2)-r_outer*sin(theta_st)-4*r_mid*sin(theta_st)-4*r_mid*sin(theta_en)-r_outer*sin(theta_en))/6,
                         r_mid*cos(theta_en), r_mid*sin(theta_en),
                         r_outer*cos(theta_en), r_outer*sin(theta_en)
    ), ncol=2, byrow=T)
  } else if (theta_en-theta_st<=3/2*PI) {
    temp_mat <- matrix(c(r_outer*cos(theta_st), r_outer*sin(theta_st), 
                         r_mid*cos(theta_st), r_mid*sin(theta_st), 
                         (16*r_inner*cos((theta_en+theta_st)/2+PI/2)-r_outer*cos(theta_st)-4*r_mid*cos(theta_st)-4*r_mid*cos(theta_en)-r_outer*cos(theta_en))/6,
                         (16*r_inner*sin((theta_en+theta_st)/2+PI/2)-r_outer*sin(theta_st)-4*r_mid*sin(theta_st)-4*r_mid*sin(theta_en)-r_outer*sin(theta_en))/6,
                         r_mid*cos(theta_en), r_mid*sin(theta_en),
                         r_outer*cos(theta_en), r_outer*sin(theta_en)
    ), ncol=2, byrow=T)
  } else {
    temp_mat <- matrix(c(r_outer*cos(theta_st), r_outer*sin(theta_st), 
                         r_mid*cos(theta_st), r_mid*sin(theta_st), 
                         (16*r_inner*cos((theta_en+theta_st)/2+PI)-r_outer*cos(theta_st)-4*r_mid*cos(theta_st)-4*r_mid*cos(theta_en)-r_outer*cos(theta_en))/6,
                         (16*r_inner*sin((theta_en+theta_st)/2+PI)-r_outer*sin(theta_st)-4*r_mid*sin(theta_st)-4*r_mid*sin(theta_en)-r_outer*sin(theta_en))/6,
                         r_mid*cos(theta_en), r_mid*sin(theta_en),
                         r_outer*cos(theta_en), r_outer*sin(theta_en)
    ), ncol=2, byrow=T)
  }
  lines(bezier(t=seq(0,1, length=30), temp_mat))
}

#dev.off()