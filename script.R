#配列初期化
x <- c();
y <- c();
width <- 1000;
height <- 1000;

#グラフ領域の準備
par(bg="black")
plot(-100,-100,xlim=c(0,width),ylim=c(0.,height),xlab="",ylab="", ann=FALSE, axes=FALSE)

# 打ち上げ~爆発
uchiage <- function() {
  #打ち上げ座標の決定
  x_start <- rnorm(1, mean=width/2, 200)
  y_start <- 0

  #爆発高さの決定
  y_explosion <- rnorm(1, mean=height*2/3,50)

  #点の生成
  for (i in 1:600) {
    #  x <- append(x, x_start)
    #  y <- append(y, y_explosion/500*i)
    if (i<=500) {
       points(x_start+10*sin(y_explosion/500*i/50), y_explosion/500*i,pch="I", col=rgb(242/255,203/255,7/255))
    }
    if (i >= 100) {
        points(x_start+10*sin(y_explosion/500*(i-100)/50), y_explosion/500*(i-100),pch="I",col="black")
     }
    Sys.sleep(0.001)
  }
  #爆発
  length <- rnorm(1, mean=200,50)
  random_color <- unlist(sample(c(rainbow(7)),1))
  for (i in 1:length) {
    if (i <= 0.8*length) {
       color <- rgb(242/255,203/255,7/255)
    }else {
      color <- random_color
    }
     for (j in 1:36) {
        points(cos(2*pi/36*j)*i+x_start,sin(2*pi/36*j)*i+y_explosion, pch=".", col=color)
     }
  }
}
uchiage()