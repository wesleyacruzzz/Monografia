library(shiny)
library(shinydashboard)
library(tidyverse)
library(e1071)
library(gridExtra)


distr <- c("Normal","Exponencial","Gama","Beta","Uniforme Contínua","Poisson",
           "Binomial","Geometrica", "Hipergeometrica")
distr1 <- c("Normal","Normal Padrão","Exponencial","Uniforme Padrão",
            "Beta","Beta Padrão","Gama","Qui-Quadrado")

# Funções ####

"%in%" <- function(x, table) match(x, table, nomatch = 0) > 0

getmode1 <- function(x) {
  d <- density(x)
  d$x[which.max(d$y)]
}

is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
