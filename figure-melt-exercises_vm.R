
library(data.table)
data.table::update_dev_pkg()
library(ggplot2)

flores.dt <- data.table(datos::flores)[, flor := .I]

largo.flores <- melt(
  flores.dt,
  measure.vars=measure(dim, parte, sep="."),
  value.name="cm")
ggplot()+
  facet_grid(dim ~ parte, labeller=label_both)+
  geom_histogram(aes(
    cm, fill=Especie),
    color='black',
    data=largo.flores)+
  ylab("cantidad") # add in spanish

partes.flores <- melt(
  flores.dt,
  measure.vars=measure(dim, value.name, sep="."))
ggplot()+
  facet_grid(. ~ dim, labeller=label_both)+
  geom_abline(
    slope=1, intercept=0)+
  coord_equal()+
  geom_point(aes(
    Petalo, Sepalo, color=Especie),
    data=partes.flores)

vuelos.dt <- data.table(datos::vuelos)
vuelos.largo <- melt(
  vuelos.dt,
  measure.vars=patterns("atraso"),
  value.name="minutos")
ggplot()+
  geom_histogram(aes(
    minutos, fill=variable),
    color='black',
    data=vuelos.largo)+
  coord_cartesian(ylim=c(0,250000))+ # add
  ylab("cantidad")# add in spanish
  

