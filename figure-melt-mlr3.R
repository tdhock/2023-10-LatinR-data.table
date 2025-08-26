if(!file.exists("learners.csv.gz")){
  download.file("https://github.com/tdhock/imbalanced-paper/raw/refs/heads/main/2025-07-22-conv-linear-prop0.01/learners.csv.gz", "learners.csv.gz")
}
library(data.table)
learners_dt <- fread("learners.csv.gz")
names(learners_dt)
learners_dt[1]

(out.names <- grep("epoch|learner_id|classif", names(learners_dt), value=TRUE))
learners_some <- learners_dt[grepl("log", learner_id) & test.fold==1 & test.subset=="balanced" & train.subsets=="same" & task_id=="MNIST_seed1_prop0.01", out.names, with=FALSE]
fwrite(learners_some, "figure-melt-mlr3.csv")
learners_long <- melt(
  learners_some,
  measure.vars=measure(
    set, measure, pattern="(train|valid)[.]classif[.](.*)"),
  id.vars=c("epoch", "learner_id")
)[
, Set := c(train="subtrain", valid="validation")[set]
]
table(learners_long$measure)
learners_long[learner_id=="conv_logistic"]
library(ggplot2)
ggplot()+
  geom_line(aes(
    epoch, value, color=Set),
    data=learners_long)+
  facet_grid(measure ~ learner_id, labeller=label_both, scales="free")
