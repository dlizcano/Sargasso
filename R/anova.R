


library(vegan)
library (ggplot2)


coral <- read.csv("data/corales.csv", header = T, sep = ";")

ggplot(coral, aes(x = sitio, y = Corallites)) +
  geom_boxplot(fill = "grey80", colour = "blue") +
  scale_x_discrete() + xlab("sitio") +
  ylab("reclutamiento") + facet_wrap(~Locality)

coral.mod1 = glm(Corallites ~ sitio, data = coral, family = poisson)
summary(coral.mod1) # no hay diferencias
anova(coral.mod1) # no hay diferencias

