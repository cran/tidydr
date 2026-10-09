## ----style, echo=FALSE, results="asis", message=FALSE-------------------------
knitr::opts_chunk$set(tidy = FALSE,
		   message = FALSE)

## ----echo=FALSE, results='hide', message=FALSE--------------------------------
library(ggplot2)
library(tidydr) 

## -----------------------------------------------------------------------------
library(tidydr)
x <- dr(data = iris[,1:4], fun = prcomp)

## ----message = TRUE-----------------------------------------------------------
available_methods()

## ----message = TRUE-----------------------------------------------------------
available_methods("distance")

## -----------------------------------------------------------------------------
d <- dist(iris[, 1:4])
y <- dr(d, stats::cmdscale)
autoplot(y, aes(color = Species), metadata = iris[, 5, drop = FALSE]) + theme_dr()

## -----------------------------------------------------------------------------
autoplot(dr(iris[, 1:4], prcomp, scale. = TRUE),
         aes(color = Species), metadata = iris[, 5, drop = FALSE]) + theme_dr()

## -----------------------------------------------------------------------------
library(ggplot2)
## metadata as a vector
ggplot(x, aes(Dim1, Dim2), metadata=iris$Species) + 
  geom_point(aes(color=.group))

## -----------------------------------------------------------------------------
## metadata as a data frame
autoplot(x, aes(color=Species), metadata = iris[, 5, drop=FALSE]) +
  theme_dr()

## -----------------------------------------------------------------------------
r <- dr_compare(iris[, 1:4],
                funs = list(prcomp = stats::prcomp,
                            cmdscale = function(z) stats::cmdscale(dist(z))),
                dim = 1:2)
r$summary

## -----------------------------------------------------------------------------
autoplot(r) + theme_dr()

## -----------------------------------------------------------------------------
si <- nk(iris[, 1:4], 2:4)
autoplot(si)

## -----------------------------------------------------------------------------
si_km <- nk(iris[, 1:4], 2:4, fun = stats::kmeans)
autoplot(si_km)

## -----------------------------------------------------------------------------
si_hc <- nk(iris[, 1:4], 2:4, fun = stats::hclust)
autoplot(si_hc)

## -----------------------------------------------------------------------------
autoplot(si, k = 3) + theme_dr()

## -----------------------------------------------------------------------------
w <- silinfo_widths(si, 3)
head(w)
attr(w, "clus.avg.widths")

## -----------------------------------------------------------------------------
autoplot(si, k = 3, type = "silhouette") + theme_dr()

