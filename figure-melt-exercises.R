library(data.table)
data.table::update_dev_pkg()
library(ggplot2)
flores.dt <- data.table(datos::flores)[, flor := .I]
flores.largo <- melt(
  flores.dt,
  measure.vars=measure(dim, parte, sep="."),
  value.name="cm")
ggplot()+
  facet_grid(dim ~ parte, labeller=label_both)+
  geom_histogram(aes(
    cm, fill=Especie),
    color='black',
    data=flores.largo)

flores.partes <- melt(
  flores.dt,
  measure.vars=measure(dim, value.name, sep="."))
ggplot()+
  facet_grid(. ~ dim, labeller=label_both)+
  geom_abline(
    slope=1, intercept=0)+
  coord_equal()+
  geom_point(aes(
    Petalo, Sepalo, color=Especie),
    data=flores.partes)

vuelos.dt <- data.table(datos::vuelos)
vuelos.largo <- melt(
  vuelos.dt,
  measure.vars=patterns("atraso"),
  value.name="minutos")
ggplot()+
  geom_histogram(aes(
    minutos, fill=variable),
    color='black',
    data=vuelos.largo)
