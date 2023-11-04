library(data.table)
library(ggplot2)
flights <- data.table::fread("http://ml.nau.edu/flights14.csv")
grid.point <- seq(0, 5000, by=1000)
grid.dt <- data.table(grid.point, distance=grid.point, key="distance")
setkey(flights, distance)

for(roll in list(-Inf,Inf,"nearest")){
  (join.dt <- grid.dt[flights, roll=roll])
  hist.dt <- join.dt[, .(num_flights=.N, mean_minutes=mean(air_time)), by=grid.point]
  join.dt[, grid.fac := factor(grid.point)]
  grid.colors <- c(
    "#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F")
  names(grid.colors) <- grid.point
  gg.roll <- ggplot()+
    ggtitle(sprintf(
      'Rolling join, grid.dt[flights, roll=%s]',
      capture.output(dput(roll))))+
    geom_point(aes(
      distance, air_time, color=grid.fac),
      data=join.dt, shape=1)+
    geom_vline(aes(
      xintercept=grid.point),
      data=grid.dt,color="red")+
    theme(text=element_text(size=30))
  dl.roll <- directlabels::direct.label(
    gg.roll, list(cex=2, "smart.grid"))+
    scale_color_manual(values=grid.colors)
  roll.png <- sprintf("figure-roll-%s.png", paste(roll))
  png(roll.png, width=11, height=5, units="in", res=100)
  print(dl.roll)
  dev.off()
  gg.hist <- ggplot()+
    geom_point(aes(
      distance, air_time),
      data=flights,
      shape=1)+
    theme(text=element_text(size=30))+
    geom_vline(aes(
      xintercept=grid.point),
      data=grid.dt,
      color="red")+
    geom_point(aes(
      grid.point, mean_minutes),
      data=hist.dt,
      size=5, color="red")
  hist.png <- sprintf("figure-roll-%s-hist.png", paste(roll))
  png(hist.png, width=11, height=5, units="in", res=100)
  print(gg.hist)
  dev.off()
}

