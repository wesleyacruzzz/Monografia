shinyServer(function(input, output, session) {
    # Inputs ####
    output$escl <- renderUI({
        if (input$var == "Normal"){
            wellPanel(
                splitLayout(
                    numericInput('n',label="Tamanho da amostra", value=1000),
                    numericInput('a',label='Média', value=0),
                    numericInput('b',label='Desvio Padrão', value=1),
                    cellWidths = c("33%","33%","33%"))
            )
        }
        else if (input$var == "Exponencial"){
            wellPanel(
                splitLayout(
                    numericInput('n',label="Tamanho da amostra", value=1000),
                    numericInput('a',label='Taxa', value=1),
                    cellWidths = c("50%","50%")))
        }
        else if (input$var == "Gama"){
            wellPanel(
                splitLayout(
                    numericInput('n',label="Tamanho da amostra", value=1000),
                    numericInput('a',label="Alfa", value=1),
                    numericInput('b',label="Beta", value=2),
                    cellWidths = c("33%","33%","33%")))
        }
        else if (input$var == "Beta"){
            wellPanel(
                splitLayout(
                    numericInput('n',label="Tamanho da amostra", value=1000),
                    numericInput('a',label="Alfa", value=1),
                    numericInput('b',label="Beta", value=2),
                    cellWidths = c("33%","33%","33%")))
        }
        else if (input$var == "Uniforme Contínua"){
            wellPanel(
                splitLayout(
                    numericInput('n',label="Tamanho da amostra", value=1000),
                    numericInput('a',label="Mínimo", value=0),
                    numericInput('b',label="Máximo", value=1),
                    cellWidths = c("33%","33%","33%")))
        }
        else if (input$var == "Poisson"){
            wellPanel(
                splitLayout(
                    numericInput('n',label="Tamanho da amostra", value=1000),
                    numericInput('a',label="Taxa", value=1),
                    cellWidths = c("50%","50%")))
        }
        else if (input$var == "Binomial"){
            wellPanel(
                splitLayout(
                    numericInput('n',label="Tamanho da amostra", value=1000),
                    numericInput('a',label="# Observações", value=10),
                    numericInput('b',label="Probabilidade", value=0.5),
                    cellWidths = c("33%","33%","33%")))
        }
        else if (input$var == "Geometrica"){
            wellPanel(
                splitLayout(
                    numericInput('n',label="Tamanho da amostra", value=1000),
                    numericInput('a',label="Probabilidade", value=0.5),
                    cellWidths = c("50%","50%")))
        }
        else if (input$var == "Hipergeometrica"){
            wellPanel(
                splitLayout(
                    numericInput('n',label="Tamanho da amostra", value=1000),
                    numericInput('a',label="M", value=50),
                    numericInput('b',label="N", value=25),
                    numericInput('c',label="K", value=10),
                    cellWidths = c("40%","20%","20%","20%")))
        } else if (is.null(input$var) == T){
            wellPanel(
                splitLayout(
                    numericInput('n',label="Tamanho da amostra", value=1000),
                    numericInput('a',label='Média', value=0),
                    numericInput('b',label='Desvio Padrão', value=1),
                    cellWidths = c("33%","33%","33%")))
        }
    })
    
    output$vizu1 <- renderUI({
        if (input$var1 == "Normal"){
            wellPanel(
                splitLayout(
                    numericInput('a1',label='Média', value=0),
                    numericInput('b2',label='Desvio Padrão', value=1),
                    numericInput('c3',label='Constante', value=1),
                    cellWidths = c("33%","33%","33%")))
        }
        else if (input$var1 == "Normal Padrão"){
            
        }
        else if (input$var1 == "Exponencial"){
            wellPanel(
                splitLayout(
                    numericInput('a1',label='Taxa', value=1),
                    numericInput('c3',label="Constante", value=1),
                    cellWidths = c("50%","50%")))
        }
        else if (input$var1 == "Uniforme Padrão"){
            wellPanel(
                splitLayout(
                    numericInput('c3',label="Constante", value=10),
                    cellWidths = c("100%")))
        }
        else if (input$var1 == "Beta"){
            wellPanel(
                splitLayout(
                    numericInput('a1',label="Alfa", value=1),
                    numericInput('b2',label="Beta", value=2),
                    numericInput('c3',label="Constante", value=10),
                    cellWidths = c("33%","33%","33%")))
        }
        else if (input$var1 == "Beta Padrão"){
            wellPanel(
                splitLayout(
                    numericInput('a1',label="Alfa", value=1),
                    numericInput('c3',label="Constante", value=10),
                    cellWidths = c("50%","50%")))
        }
        else if (input$var1 == "Gama"){
            wellPanel(
                splitLayout(
                    numericInput('a1',label="Alfa", value=1),
                    numericInput('b2',label="Beta", value=2),
                    numericInput('c3',label="Constante", value=10),
                    cellWidths = c("33%","33%","33%")))
        }
        else if (input$var1 == "Qui-Quadrado"){
            wellPanel(
                splitLayout(
                    numericInput('a1',label="Graus de Liberdade", value=1),
                    numericInput('c3',label="Constante", value=10),
                    cellWidths = c("50%","50%")))
        }
    })
    
    output$vizu2 <- renderUI({
        if (input$prob1 == "Normal"){
            wellPanel(
                splitLayout(numericInput("min1","Primeiro quartil",value = -1.96),
                            numericInput("max1","Segundo quaril",value = 1.96),
                            cellWidths = c("50%","50%")),
                splitLayout(
                    numericInput('a11',label='Média', value=0),
                    numericInput('b22',label='Desvio Padrão', value=1),
                    cellWidths = c("50%","50%")),
                
                infoBox("Probabilidade: ", textOutput("textt"), color = "blue", width = 20))
        }
        else if (input$prob1 == "t-Student"){
            wellPanel(
                splitLayout(numericInput("min1","Primeiro quantil",value = -1.96),
                            numericInput("max1","Segundo quantil",value = 1.96),
                            cellWidths = c("50%","50%")),
                numericInput('a11',label='Graus de Liberdade', value=100),
                infoBox("Probabilidade: ", textOutput("textt"), color = "blue", width = 20))
        }
        else if (input$prob1 == "F-Snedecor"){
            wellPanel(
                splitLayout(numericInput("min1","Primeiro quantil",value = 0.4),
                            numericInput("max1","Segundo quantil",value = 2.3),
                            cellWidths = c("50%","50%")),
                splitLayout(
                    numericInput('a11',label='Graus de liberdade (df1)', value=20),
                    numericInput('b22',label='Graus de liberdade (df2)', value=25),
                    cellWidths = c("50%","50%")),
                
                infoBox("Probabilidade: ", textOutput("textt"), color = "blue", width = 20))
        }
    })
    
    # Valores reativos ####
    choice_a <- reactiveVal(NULL)
    choice2 <- reactiveVal(NULL)
    outputs <- reactiveVal(NULL)
    teste1 <- reactiveVal(NULL)
    ka <- reactiveVal(NULL)
    
    # Funções reativas ####
    
    ka <- function(x){
        k <- x
        if (k == "Normal"){
            a <- "normal.html"
        }
        if (k == "Normal Padrão"){
            a <- "Normal_padrao.html"
        }
        if (k == "Exponencial"){
            a <- "Exponencial.html"
        }
        if (k == "Uniforme Padrão"){
            a <- "Uniforme_padrao.html"
        }
        if (k == "Gama"){
            a <- "Gama.html"
        }
        if (k == "Beta"){
            a <- "Beta.html"
        }
        if (k == "Beta Padrão"){
            a <- "Beta_padrao.html"
        }
        if (k == "Qui-Quadrado"){
            a <- "Qui.html"
        }
        return(a)
    }
    transf <- function(x){
        k <- switch(x,
                    "Normal" = c("(X-m)/s","X + a","bX"),
                    "Normal Padrão" = "X^2",
                    "Exponencial" = "aX",
                    "Uniforme Padrão" = c("1 - X^(1/n)","X^2","-ln(X)/A"),
                    "Beta" = "1 - X",
                    "Beta Padrão" = "-ln(X)",
                    "Gama" = "X",
                    "Qui-Quadrado" =  "X/c"
        )
        return(k)
    }
    choice <- function(x,n,a,b,c){
        distr_co <- c("Normal","Exponencial","Gama","Beta","Uniforme Contínua")
        vet <- x
        func <- NULL
        if (vet %in% distr_co){
            # Conditions ####
            if (vet %in% "Normal"){
                simul <- rnorm(n,a,b)
            }
            if (vet %in% "Exponencial"){
                simul <- rexp(n,a)
            }
            if (vet %in% "Gama"){
                simul <- rgamma(n,a,b)
            }
            if (vet %in% "Beta"){
                simul <- rbeta(n,a,b)
            }
            if (vet %in% "Uniforme Contínua"){
                simul <- runif(n,a,b)
            }
            
            s1 <- mean(simul)
            s2 <- median(simul)
            s3 <- getmode1(simul)
            
            if (vet %in% "Normal" & a==0 & b==1){
                p <- qplot(simul, geom = 'blank') +   
                    geom_histogram(aes(y = ..density..),
                                   alpha = 0.4, fill = "cadetblue4", color = "white") +
                    geom_vline(xintercept=s1,col="red", alpha = 0.7) +
                    geom_vline(xintercept=s2,col="blue", alpha = 0.7) +
                    geom_vline(xintercept=s3,col="green", alpha = 0.7) +
                    ggtitle(str_replace("Histograma de uma variável aleatória ttt Padrão","ttt",vet))+
                    ylab(" ")+
                    xlab(expression(x))+
                    theme_light()+
                    theme(panel.grid = element_blank(),
                          title = element_text(size = 8))
            } else {
                p <- qplot(simul, geom = 'blank') +   
                    geom_histogram(aes(y = ..density..),
                                   alpha = 0.4, fill = "cadetblue4", color = "white") +
                    geom_vline(xintercept=s1,col="red", alpha = 0.7) +
                    geom_vline(xintercept=s2,col="blue", alpha = 0.7) +
                    geom_vline(xintercept=s3,col="green", alpha = 0.7) +
                    ggtitle(str_replace("Histograma de uma variável aleatória ttt","ttt",vet))+
                    ylab(" ")+
                    xlab(expression(x))+
                    theme_light()+
                    theme(panel.grid = element_blank(),
                          title = element_text(size = 8))
            }
            
        } else {
            # Conditions ####
            if (vet == "Poisson"){
                simul <- rpois(n,a)
            }
            if (vet == "Binomial"){
                simul <- rbinom(n,a,b)
            }
            if (vet == "Geometrica"){
                simul <- rgeom(n,a)
            }
            if (vet == "Hipergeometrica"){
                simul <- rhyper(n,a,b,c)
            }
            
            s1 <- mean(simul)
            s2 <- median(simul)
            s3 <- getmode1(simul)
            
            p <- qplot(simul, geom = 'blank') +
                geom_histogram(aes(x = simul, y = ..density..), alpha = 0.4,
                               fill = "cadetblue4", color = "white") +
                geom_vline(xintercept = s1, col="red", alpha = 0.7)+
                geom_vline(xintercept = s2,col="blue", alpha = 0.7) +
                geom_vline(xintercept = s3,col="green", alpha = 0.7) +
                ggtitle(str_replace("Histograma de uma variável aleatória ttt","ttt",vet))+
                ylab(" ")+
                xlab(expression(x))+
                theme_light()+
                theme(panel.grid = element_blank(),
                      legend.position = "None",
                      title = element_text(size = 8))
        }
        return(list(simul,p))
    }
    choice2 <- function(x,n,a,b,c){
        vet <- x
        if (vet == "Normal"){
            media <- a
            mediana <- a
            moda <- a
            var <- b^2
            curtose <- 0
            assim <- 0
        }
        if (vet == "Exponencial"){
            media <- 1/a
            mediana <- log(2)/a
            moda <- 0
            var <- 1/a^2
            curtose <- 6
            assim <- 2
        }
        if (vet == "Gama"){
            media <- a/b
            mediana <- NA
            moda <-ifelse(a >= 1,(a-1)/b,NA)
            var <- a/b^2
            curtose <- 6/a
            assim <- 2/sqrt(a)
        }
        if (vet == "Beta"){
            media <- a/(a+b)
            mediana <- ifelse(a > 1 & b > 1,(a-1/3)/(a+b-2/3),NA)
            if (a > 1 & b > 1){
                moda <- (a-1)/(a+b-2)
            } else if (a == 1 & b == 1){
                moda <- runif(1,0,1)
            } else if (a < 1 & b < 1){
                moda <- c(0,1)
            } else if (a < 1 & b >= 1){
                moda <- 0
            } else if (a >= 1 & b < 1){
                moda <- 1
            } else if (a == 1 & b > 1){
                moda <- 0
            } else if (b == 1 & a > 1){
                moda <- 1
            }
            var <- a*b/((a+b+1)*(a+b)^2)
            curtose <- 6*((a+b+1)*(a-b)^2 - a*b*(a+b+2))/(a*b*(a+b+2)*(a+b+3))
            assim <- 2*((b-a)*sqrt(a+b+1))/((a+b+2)*sqrt(a*b))
        }
        if (vet == "Uniforme Contínua"){
            media <- (a+b)/2
            mediana <- (a+b)/2
            moda <- runif(1,a,b)
            var <- ((b-a)^2)/12
            curtose <- -6/5
            assim <- 0
        }
        if (vet == "Poisson"){
            media <- a
            mediana <- floor(a + 1/3 - (0.02/a))
            moda <- ceiling(a)-1
            var <- a
            curtose <- 1/a
            assim <- 1/sqrt(a)
        }
        if (vet == "Binomial"){
            media <- a*b
            mediana <- c(floor(a*b),ceiling(a*b))
            moda <- ifelse(is.wholenumber((a+1)*b), list(c((a+1)*b,(a+1)*b - 1)),
                           floor((a+1)*b))
            var <- a*b*(1-b)
            curtose <- (1 - 6*b*(1-b))/a*b*(1-b)
            assim <- (1 - 2*b)/sqrt(a*b*(1-b))
        }
        if (vet == "Geometrica"){
            media <- (1-a)/a
            mediana <- ifelse(is.wholenumber(-1/log2(1 - a)),NA,
                              ceiling(-1/log2(1 - a))-1)
            moda <- 0
            var <- (1-a)/a^2
            curtose <- 6 + a^2/(1-a)
            assim <- (2-a)/sqrt(1-a)
        }
        if (vet == "Hipergeometrica"){
            N = a+b
            K = a
            n = c
            k1 <- (N-1)*(N^2)*(N*(N+1)-6*K*(N-K)-6*n*(N-n))
            k2 <- 6*n*K*(N-K)*(N-n)*(5*N-6)
            k3 <- n*K*(N-K)*(N-n)*(N-2)*(N-3)
            a1 <- (N-2*K)*sqrt(N-1)*(N-2*n)
            a2 <- n*K*(N-K)*(N-n)
            v1 <- K/N
            v2 <- (N-K)/N
            v3 <- (N-n)/(N-1)
            
            media <- n*K/N
            mediana <- NA
            moda <- floor((n+1)*(K+1)/(N+2))
            var <- n*v1*v2*v3
            curtose <- (k1+k2)/k3
            assim <- a1/(sqrt(a2)*(N-2))
        }
        return(list(media,mediana,moda,var,curtose,assim))
    }
    choice_a <- function(x,n,a,b,c){
        distr_co <- c("Normal","Exponencial","Beta","Gama","Uniforme Contínua")
        vet <- x
        func <- NULL
        x1 <- a
        x2 <- b
        s4 <- as.numeric(unlist(choice2(x,n,a,b,c)[1]))
        s5 <- as.numeric(unlist(choice2(x,n,a,b,c)[2]))
        s6 <- as.numeric(unlist(choice2(x,n,a,b,c)[3]))
        
        if (vet %in% distr_co){
            # Conditions ####
            if (vet == "Gama"){
                func <- dgamma
                argu <- list(shape = a, rate = b)
                cond <- seq(0,qgamma(0.999,a,b),length.out = 1000)
            }
            if (vet == "Beta"){
                func <- dbeta
                argu <- list(shape1 = a, shape2 = b)
                cond <- seq(0,1,length.out = 1000)
            }
            if (vet == "Normal"){
                func <- dnorm
                argu <- list(mean = a, sd = b)
                cond <- seq(a-4*b,a+4*b,length.out = 1000)
            }
            if (vet == "Exponencial"){
                func <- dexp
                argu <- list(rate = a)
                cond <- seq(0,qexp(0.999,a),length.out = 1000)
            }
            if (vet == "Uniforme Contínua"){
                func <- dunif
                argu <- list(min = a, max = b)
                cond <- seq(a,b,length.out = 1000)
            }
            
            if (vet %in% "Normal" & x1==0 & x2==1){
                p <- qplot(cond,geom = 'blank') +   
                    stat_function(fun = func, n=length(cond),
                                  args = argu,
                                  color = "red",
                                  inherit.aes = FALSE)+
                    geom_vline(xintercept = s4, col="red", alpha = 0.7)+
                    geom_vline(xintercept = s5,col="blue", alpha = 0.7) +
                    geom_vline(xintercept = s6,col="green", alpha = 0.7) +
                    theme_light()+
                    ggtitle(str_replace("Função de probabilidade de uma variável aleatória ttt Padrão","ttt",vet))+
                    theme_light()+
                    ylab("Função densidade")+
                    xlab(expression(x)) +
                    theme(panel.grid = element_blank(),
                          legend.position = "None",
                          title = element_text(size = 8))
            } else {
                p <- qplot(cond,geom = 'blank') +
                    stat_function(fun = func, n=length(cond),
                                  args = argu,
                                  color = "red",
                                  inherit.aes = FALSE)+
                    geom_vline(xintercept = s4, col="red", alpha = 0.7)+
                    geom_vline(xintercept = s5,col="blue", alpha = 0.7) +
                    geom_vline(xintercept = s6,col="green", alpha = 0.7) +
                    theme_light()+
                    ggtitle(str_replace("Função de densidade de uma variável aleatória ttt","ttt",vet))+
                    ylab("Função densidade")+
                    xlab(expression(x)) +
                    theme(panel.grid = element_blank(),
                          legend.position = "None",
                          title = element_text(size = 8))
            }
            
        } else {
            # Conditions ####
            if (vet == "Poisson"){
                cond <- seq(0,qpois(0.999,a))
                func <- dpois(cond,a)
            }
            if (vet == "Binomial"){
                cond <- seq(0,qbinom(0.999,a,b))
                func <- dbinom(cond,a,b)
            }
            if (vet == "Geometrica"){
                cond <- seq(0,qgeom(0.999,a))
                func <- dgeom(cond,a)
            }
            if (vet == "Hipergeometrica"){
                cond <- seq(0,qhyper(0.999,a,b,c))
                func <- dhyper(cond,a,b,c)
            }
            
            p <- ggplot() +
                geom_point(aes(x = cond, y = func),col = "red", size = 3)+
                geom_segment(aes(x=cond, xend=cond, y=0, yend=func), col = "red")+
                geom_vline(xintercept = s4, col="red", alpha = 0.7)+
                geom_vline(xintercept = s5,col="blue", alpha = 0.7) +
                geom_vline(xintercept = s6,col="green", alpha = 0.7) +
                theme_light()+
                ggtitle(str_replace("Função de probabilidade de uma variável aleatória ttt","ttt",vet))+
                ylab("Função de probabilidade")+
                xlab(expression(x))+
                theme(panel.grid = element_blank(),
                      legend.position = "None",
                      title = element_text(size=8))
        }
        return(p)
    }
    choice_b <- function(x,n,a,b,c){
        distr_co <- c("Normal","Exponencial","Beta","Gama","Uniforme Contínua")
        vet <- x
        func <- NULL
        
        if (vet %in% distr_co){
            # Conditions ####
            if (vet == "Gama"){
                cond <- seq(0,qgamma(0.999,a,b),length.out = 1000)
                cumulative<-pgamma(cond,a,b)
            }
            if (vet == "Beta"){
                cond <- seq(0,1,length.out = 1000)
                cumulative<-pbeta(cond,a,b)
            }
            if (vet == "Normal"){
                cond<-seq(a-4*b,a+4*b,length.out = 1000)
                cumulative<-pnorm(cond, a, b)
            }
            if (vet == "Exponencial"){
                cond<-seq(0,qexp(0.999,a),length.out = 1000)
                cumulative<-pexp(cond, a)
            }
            if (vet == "Uniforme Contínua"){
                cond <- seq(a,b,length.out = 1000)
                cumulative<-punif(cond, a, b)
            }
            
            if (vet == "Normal" & a==0 & b==1){
                p <- ggplot()+
                    geom_line(aes(x = cond, y=cumulative),col = "red", alpha = 0.5)+
                    theme_light()+
                    ggtitle(str_replace("Distribuição acumulada de uma variável aleatória ttt Padrão","ttt",vet))+
                    ylab("Acumulada")+
                    xlab(expression(x))+
                    theme(panel.grid = element_blank(),
                          title = element_text(size = 8))
            } else {
                p <- ggplot()+
                    geom_line(aes(x = cond, y=cumulative),col = "red", alpha = 0.5)+
                    theme_light()+
                    ggtitle(str_replace("Distribuição acumulada de uma variável aleatória ttt","ttt",vet))+
                    ylab("Acumulada")+
                    xlab(expression(x))+
                    theme(panel.grid = element_blank(),
                          title = element_text(size = 8))
            }
            
        } else {
            # Conditions ####
            if (vet == "Poisson"){
                cond <- seq(0,qpois(0.999,a))
                cumulative<-ppois(cond,a)
            }
            if (vet == "Binomial"){
                cond <- seq(0,qbinom(0.999,a,b))
                cumulative<-pbinom(cond,a,b)
            }
            if (vet == "Geometrica"){
                cond <- seq(0,qgeom(0.999,a))
                cumulative<-pgeom(cond, a)
            }
            if (vet == "Hipergeometrica"){
                cond <- seq(0,qhyper(0.999,a,b,c))
                cumulative<-phyper(cond,a,b,c)
            }
            
            p <- ggplot()+
                geom_point(aes(x = cond, y=cumulative), col = "red", size = 3, alpha = 0.5)+
                geom_segment(aes(x=cond, xend=cond, y=0, yend=cumulative), col = "red", alpha = 0.5)+
                theme_light()+
                ggtitle(str_replace("Distribuição acumulada de uma variável aleatória ttt","ttt",vet))+
                ylab("Acumulada")+
                xlab(expression(x))+
                theme(panel.grid = element_blank(),
                      title = element_text(size = 8))
        }
        return(p)
    }
    escolha <- function(x){
        # Normal ####
        if (x == "Normal"){
            func <- dnorm
            lima <- NA
            limb <- NA
            x <- rnorm(10000,input$a1,input$b2)
            Y <- switch (input$tran1,
                         "(X-m)/s" = (x - mean(x))/sd(x),
                         "X + a" = (x + input$c3),
                         "bX" = (input$c3*x)
            )
            argu <- switch (input$tran1,
                            "(X-m)/s" = list(mean = 0, sd = 1),
                            "X + a" = list(mean = input$a1 + input$c3, sd = sqrt(input$b2)),
                            "bX" = list(mean = input$c3*input$a1, sd = input$c3*sqrt(input$b2))
            )
            cond <- switch (input$tran1,
                            "(X-m)/s" = seq(0-4,0+4,length.out = 1000),
                            "X + a" = seq((input$a1 + input$c3)-4*input$b2,(input$a1 + input$c3)+4*input$b2,length.out = 1000),
                            "bX" = seq(input$a1-4*input$c3*sqrt(input$b2),input$a1+4*input$c3*sqrt(input$b2),length.out = 1000)
            )
        }
        # Normal Padrão ####
        if (x == "Normal Padrão"){
            lima <- 0
            limb <- 100
            func <- dchisq
            x <- rnorm(10000,0,1)
            limite <- a
            Y <- switch (input$tran1,
                         "X^2" = rchisq(10000,1)
            )
            argu <- switch (input$tran1,
                            "X^2" = list(df = 1)
            )
            cond <- switch (input$tran1,
                            "X^2" = seq(0,15,length.out = 10000)
            )
        }
        # Exponencial ####
        if(x == "Exponencial"){
            func <- dexp
            lima <- NA
            limb <- NA
            x <- rexp(10000,input$a1)
            Y <- switch (input$tran1,
                         "aX" = rexp(10000,1)
            )
            argu <- switch (input$tran1,
                            "aX" = list(rate = 1)
            )
            cond <- switch (input$tran1,
                            "aX" = seq(0,10,length.out = 10000)
            )
        }
        # Uniforme Padrão ####
        if (x == "Uniforme Padrão"){
            if (input$tran1 == "-ln(X)/A"){
                func <- dexp
            } else {
                func <- dbeta
            }
            
            lima <- NA
            limb <- NA
            x <- runif(10000,input$a1,input$b2)
            Y <- switch (input$tran1,
                         "1 - X^(1/n)" = rbeta(10000,1,input$c3),
                         "X^2" = rbeta(10000,1/2,1),
                         "-ln(X)/A" = rexp(10000,input$c3)
            )
            argu <- switch (input$tran1,
                            "1 - X^(1/n)" = list(shape1 = 1, shape2 = input$c3),
                            "X^2" = list(shape1 = 1/2, shape2 = 1),
                            "-ln(X)/A" = list(rate = input$c3)
            )
            cond <- switch (input$tran1,
                            "1 - X^(1/n)" = seq(0,qbeta(0.99,1,input$c3),length.out = 1000),
                            "X^2" = seq(0,qbeta(0.99,1/2,1),length.out = 1000),
                            "-ln(X)/A" = seq(0,qexp(0.99,input$c3),length.out = 1000)
            )
        }
        # Gama ####
        if (x == "Gama"){
            lima <- NA
            limb <- NA
            func <- dchisq
            x <- rgamma(10000,input$c3/2,1/2)
            Y <- switch (input$tran1,
                         "X" = rchisq(10000,input$c3)
            )
            argu <- switch (input$tran1,
                            "X" = list(df = input$c3)
            )
            cond <- switch (input$tran1,
                            "X" = seq(0,qchisq(0.99,input$c3),length.out = 1000)
            )
        }
        
        # Beta ####
        if (x == "Beta"){
            lima <- NA
            limb <- NA
            func <- dbeta
            x <- rbeta(10000,input$a1,input$b2)
            Y <- switch (input$tran1,
                         "1 - X" = rbeta(10000,input$b2,input$a1)
            )
            argu <- switch (input$tran1,
                            "1 - X" = list(shape1 = input$b2, shape2 = input$a1)
            )
            cond <- switch (input$tran1,
                            "1 - X" = seq(0,qbeta(0.99,input$b2,input$a1),length.out = 1000)
            )
        }
        # Beta Padrão ####
        if (x == "Beta Padrão"){
            lima <- NA
            limb <- NA
            func <- dexp
            x <- rbeta(10000,input$a1,1)
            Y <- switch (input$tran1,
                         "-ln(X)" = rexp(10000,input$a1)
            )
            argu <- switch (input$tran1,
                            "-ln(X)" = list(rate = input$a1)
            )
            cond <- switch (input$tran1,
                            "-ln(X)" = seq(0,qexp(0.99,input$a1),length.out = 1000)
            )
        }
        # Qui-Quadrado ####
        if (x == "Qui-Quadrado"){
            lima <- NA
            limb <- NA
            func <- dgamma
            x <- rchisq(10000,input$a1)
            Y <- switch (input$tran1,
                         "X/c" = rgamma(10000,input$a1/2,input$c3/2)
            )
            argu <- switch (input$tran1,
                            "X/c" = list(shape = input$a1/2, rate = input$c3/2)
            )
            cond <- switch (input$tran1,
                            "X/c" = seq(0,qgamma(0.99,input$a1/2,input$c3/2),length.out = 1000)
            )
        }
        return(list(func,Y,argu,cond))
    }
    
    
    # Primeira tab ####
    
    output$txt_1 <- renderText("Aluno: Wesley Almeida Cruz")
    output$txt_2 <- renderText("Orientador: Moisés")
    
    outputs <- reactive({
        if (is.null(input$n)){
            if (input$var == distr[1]){
                choice(input$var, 1000, 0, 1, 1)
            } else if (input$var == distr[2]){
                choice(input$var, 1000, 1, 1, 1)
            } else if (input$var == distr[3]){
                choice(input$var, 1000, 1, 1, 1)
            } else if (input$var == distr[4]){
                choice(input$var, 1000, 1, 1, 1)
            } else if (input$var == distr[5]){
                choice(input$var, 1000, 0, 1, 1)
            } else if (vinput$var == distr[6]){
                choice(input$var, 1000, 1, 1, 1)
            } else if (input$var == distr[7]){
                choice(input$var, 1000, 10, 0.5, 1)
            } else if (input$var == distr[8]){
                choice(input$var, 1000, 0.5, 1, 1)
            } else if (input$var == distr[9]){
                choice(input$var, 1000, 50, 25, 10)
            }
        } else {
            choice(input$var, input$n, input$a, input$b, input$c)
        }
    })
    
    output$plot <- renderPlot({
        outputs()[[2]]
    })
    
    output$plot1 <- renderPlot({
        if (is.null(input$n)){
            if (input$var == distr[1]){
                choice_a(input$var, 1000, 0, 1, 1)
            } else if (input$var == distr[2]){
                choice_a(input$var, 1000, 1, 1, 1)
            } else if (input$var == distr[3]){
                choice_a(input$var, 1000, 1, 1, 1)
            } else if (input$var == distr[4]){
                choice_a(input$var, 1000, 1, 1, 1)
            } else if (input$var == distr[5]){
                choice_a(input$var, 1000, 0, 1, 1)
            } else if (vinput$var == distr[6]){
                choice_a(input$var, 1000, 1, 1, 1)
            } else if (input$var == distr[7]){
                choice_a(input$var, 1000, 10, 0.5, 1)
            } else if (input$var == distr[8]){
                choice_a(input$var, 1000, 0.5, 1, 1)
            } else if (input$var == distr[9]){
                choice_a(input$var, 1000, 50, 25, 10)
            }
        } else {
            choice_a(input$var, input$n, input$a, input$b, input$c)
        }
    })
    
    output$plot2 <- renderPlot({
        if (is.null(input$n)){
            if (input$var == distr[1]){
                choice_b(input$var, 1000, 0, 1, 1)
            } else if (input$var == distr[2]){
                choice_b(input$var, 1000, 2, 2, 2)
            } else if (input$var == distr[3]){
                choice_b(input$var, 1000, 1, 1, 1)
            } else if (input$var == distr[4]){
                choice_b(input$var, 1000, 1, 1, 1)
            } else if (input$var == distr[5]){
                choice_b(input$var, 1000, 0, 1, 1)
            } else if (vinput$var == distr[6]){
                choice_b(input$var, 1000, 1, 1, 1)
            } else if (input$var == distr[7]){
                choice_b(input$var, 1000, 10, 0.5, 1)
            } else if (input$var == distr[8]){
                choice_b(input$var, 1000, 0.5, 1, 1)
            } else if (input$var == distr[9]){
                choice_b(input$var, 1000, 50, 25, 10)
            }
        } else {
            choice_b(input$var, input$n, input$a, input$b, input$c)
        }
    })
    
    output$tex <- renderText({
        if (is.null(input$a)){
            round(mean(rnorm(1000)),2)
        } else {
            round(mean(outputs()[[1]]),2)
        }
    })
    output$texa <- renderText({
        if(is.null(input$a)){
            0
        } else {
            round(as.numeric(choice2(input$var, input$n, input$a, input$b, input$c)[1]),2)
        }
        
    })
    output$tex2 <- renderText({
        if (is.null(input$a)){
            round(median(rnorm(1000)),2)
        } else {
            round(median(outputs()[[1]]),2)
        }
    })
    output$texb <- renderText({
        if(is.null(input$a)){
            0
        } else {
            if (is.na(unlist(choice2(input$var, input$n, input$a, input$b, input$c)[2]))){
                print("Sem forma fechada")
            } else {
                round(as.numeric(unlist(choice2(input$var, input$n, input$a, input$b, input$c)[2])),2)
            }
        }
    })
    output$tex4 <- renderText({
        if (is.null(input$a)){
            0
        } else {
            round(getmode1(outputs()[[1]]),2)
        }
    })
    output$texe <- renderText({
        if(is.null(input$a)){
            0
        } else {
            if (is.na(unlist(choice2(input$var, input$n, input$a, input$b, input$c)[3]))){
                print("Sem forma fechada")
            } else {
                round(as.numeric(unlist(choice2(input$var, input$n, input$a, input$b, input$c)[3])),2)
            }
        }
    })
    output$tex1 <- renderText({
        if (is.null(input$a)){
            round(var(rnorm(1000)),2)
        } else {
            round(var(outputs()[[1]]),2)
        }
    })
    output$texc <- renderText({
        if(is.null(input$a)){
            1
        } else {
            round(as.numeric(choice2(input$var, input$n, input$a, input$b, input$c)[4]),2)
        }
    })
    output$tex3 <- renderText({
        if (is.null(input$a)){
            round(kurtosis(rnorm(1000)),2)
        } else {
            round(kurtosis(outputs()[[1]]),2)
        }
    })
    output$texd <- renderText({
        if(is.null(input$a)){
            0
        } else {
            round(as.numeric(choice2(input$var, input$n, input$a, input$b, input$c)[5]),2)
        }
        
    })
    output$tex5 <- renderText({
        if (is.null(input$a)){
            round(skewness(rnorm(1000)),2)
        } else {
            round(skewness(outputs()[[1]]),2)
        }
    })
    output$texf <- renderText({
        if(is.null(input$a)){
            0
        } else {
            round(as.numeric(choice2(input$var, input$n, input$a, input$b, input$c)[6]),2)
        }
    })
    
    # Segunda tab ####
    
    output$txt_3 <- renderText("Aluno: Wesley Almeida Cruz")
    output$txt_4 <- renderText("Orientador: Moisés")
    
    observe({
        x <- transf(input$var1)
        
        # Can use character(0) to remove all choices
        if (is.null(x))
            x <- character(0)
        
        # Can also set the label and select items
        updateSelectInput(session, "tran1", "Transformação",
                          choices = x,
                          selected = NULL
        )
    })
    
    output$txt1 <- renderUI({
        tags$iframe(style="height:150px; width:100%; border:none", src=ka(input$var1))
    })
    output$plot3 <- renderPlot({
        
        escolha1 <- escolha(input$var1)
        
        if (input$var1 == "Normal Padrão"){
            p1 <- qplot(rnorm(10000), geom = 'blank') +   
                stat_function(fun = dnorm, n=10000,
                              args = list(mean = 0, sd = 1), colour = "red",
                              inherit.aes = FALSE)+
                geom_histogram(aes(y = ..density..),
                               alpha = 0.4, fill = "cadetblue4", color = "white") +
                theme_light()+
                theme(panel.grid = element_blank(),title = element_text(size = 10))+
                ggtitle("Histograma de uma amostra da variável aleatória Normal Padrão")+
                ylab(" ")+
                xlab(expression(x))
            p2 <- qplot(escolha1[[2]], geom = 'blank') +   
                stat_function(fun = escolha1[[1]], n=length(escolha1[[4]]),
                              args = escolha1[[3]], colour = "red",
                              inherit.aes = FALSE)+
                geom_histogram(aes(y = ..density..),
                               alpha = 0.4, fill = "cadetblue4", color = "white") +
                theme_light()+
                theme(panel.grid = element_blank(),title = element_text(size = 10))+
                ylim(0,1)+
                ggtitle("Histograma da amostra da variável aleatória transformada")+
                ylab(" ")+
                xlab(expression(x))
        }
        else if (input$var1 == "Uniforme Padrão"){
            p1 <- qplot(runif(10000), geom = 'blank') +   
                stat_function(fun = dunif, n=10000,
                              args = list(min = 0, max = 1), colour = "red",
                              inherit.aes = FALSE)+
                geom_histogram(aes(y = ..density..),
                               alpha = 0.4, fill = "cadetblue4", color = "white") +
                theme_light()+
                theme(panel.grid = element_blank(),title = element_text(size = 10))+
                ggtitle("Histograma de uma amostra da variável aleatória Uniforme Padrão")+
                ylab(" ")+
                xlab(expression(x))
            p2 <- qplot(escolha1[[2]], geom = 'blank') +
                stat_function(fun = escolha1[[1]], n=length(escolha1[[4]]),
                              args = escolha1[[3]], colour = "red",
                              inherit.aes = FALSE)+
                geom_histogram(aes(y = ..density..),
                               alpha = 0.4, fill = "cadetblue4", color = "white") +
                theme_light()+
                theme(panel.grid = element_blank(),title = element_text(size = 10))+
                ggtitle("Histograma da amostra da variável aleatória transformada")+
                ylab(" ")+
                xlab(expression(x))+
                ylim(0,6)
        }
        else if (input$var1 == "Qui-Quadrado"){
            p1 <- qplot(rchisq(10000,input$a1), geom = 'blank') +   
                stat_function(fun = dchisq, n=10000,
                              args = list(df = input$a1), colour = "red",
                              inherit.aes = FALSE)+
                geom_histogram(aes(y = ..density..),
                               alpha = 0.4, fill = "cadetblue4", color = "white") +
                ylim(0,6)+
                theme_light()+
                theme(panel.grid = element_blank(),title = element_text(size = 10))+
                ggtitle("Histograma de uma amostra da variável aleatória Qui-Quadrado")+
                ylab(" ")+
                xlab(expression(x))
            p2 <- qplot(escolha1[[2]], geom = 'blank') +
                stat_function(fun = escolha1[[1]], n=length(escolha1[[4]]),
                              args = escolha1[[3]], colour = "red",
                              inherit.aes = FALSE)+
                geom_histogram(aes(y = ..density..),
                               alpha = 0.4, fill = "cadetblue4", color = "white") +
                ylim(0,6)+
                theme_light()+
                theme(panel.grid = element_blank(),title = element_text(size = 10))+
                ggtitle("Histograma da amostra da variável aleatória transformada")+
                ylab(" ")+
                xlab(expression(x))
        } else if (input$var1 == "Normal"){
            p1 <- qplot(rnorm(10000,input$a1,input$b2), geom = 'blank') +   
                stat_function(fun = dnorm, n=10000,
                              args = list(mean = input$a1, sd = input$b2), colour = "red",inherit.aes = FALSE)+
                geom_histogram(aes(y = ..density..),
                               alpha = 0.4, fill = "cadetblue4", color = "white") +
                theme_light()+
                theme(panel.grid = element_blank(),title = element_text(size = 10))+
                ggtitle("Histograma de uma amostra da variável aleatória Normal")+
                ylab(" ")+
                xlab(expression(x))
            p2 <- qplot(escolha1[[2]], geom = 'blank') +   
                stat_function(fun = escolha1[[1]], n=length(escolha1[[4]]),
                              args = escolha1[[3]], colour = "red",
                              inherit.aes = FALSE)+
                geom_histogram(aes(y = ..density..),
                               alpha = 0.4, fill = "cadetblue4", color = "white") +
                theme_light()+
                theme(panel.grid = element_blank(),title = element_text(size = 10))+
                ggtitle("Histograma da amostra da variável aleatória transformada")+
                ylab(" ")+
                xlab(expression(x))
            
        } else if (input$var1 == "Exponencial"){
            p1 <- qplot(rexp(10000,input$a1), geom = 'blank') +   
                stat_function(fun = dexp, n=10000,
                              args = list(rate = input$a1), colour = "red",
                              inherit.aes = FALSE)+
                geom_histogram(aes(y = ..density..),
                               alpha = 0.4, fill = "cadetblue4", color = "white") +
                theme_light()+
                theme(panel.grid = element_blank(),title = element_text(size = 10))+
                ggtitle("Histograma de uma amostra da variável aleatória Exponencial")+
                ylab(" ")+
                xlab(expression(x))
            p2 <- qplot(escolha1[[2]], geom = 'blank') +   
                stat_function(fun = escolha1[[1]], n=length(escolha1[[4]]),
                              args = escolha1[[3]], colour = "red",
                              inherit.aes = FALSE)+
                geom_histogram(aes(y = ..density..),
                               alpha = 0.4, fill = "cadetblue4", color = "white") +
                theme_light()+
                theme(panel.grid = element_blank(),title = element_text(size = 10))+
                ggtitle("Histograma da amostra da variável aleatória transformada")+
                ylab(" ")+
                xlab(expression(x))
        } else if (input$var1 == "Beta"){
            p1 <- qplot(rbeta(10000,input$a1,input$b2), geom = 'blank') +   
                stat_function(fun = dbeta, n=10000,
                              args = list(shape1 = input$a1, shape2 = input$b2), colour = "red",inherit.aes = FALSE)+
                geom_histogram(aes(y = ..density..),
                               alpha = 0.4, fill = "cadetblue4", color = "white") +
                theme_light()+
                theme(panel.grid = element_blank(),title = element_text(size = 10))+
                ggtitle("Histograma de uma amostra da variável aleatória Beta")+
                ylab(" ")+
                xlab(expression(x))
            p2 <- qplot(escolha1[[2]], geom = 'blank') +   
                stat_function(fun = escolha1[[1]], n=length(escolha1[[4]]),
                              args = escolha1[[3]], colour = "red",
                              inherit.aes = FALSE)+
                geom_histogram(aes(y = ..density..),
                               alpha = 0.4, fill = "cadetblue4", color = "white") +
                theme_light()+
                theme(panel.grid = element_blank(),title = element_text(size = 10))+
                ggtitle("Histograma da amostra da variável aleatória transformada")+
                ylab(" ")+
                xlab(expression(x))
        } else if (input$var1 == "Beta Padrão"){
            p1 <- qplot(rbeta(10000,input$a1,1), geom = 'blank') +   
                stat_function(fun = dbeta, n=10000,
                              args = list(shape1 = input$a1, shape2 = 1), colour = "red",inherit.aes = FALSE)+
                geom_histogram(aes(y = ..density..),
                               alpha = 0.4, fill = "cadetblue4", color = "white") +
                theme_light()+
                theme(panel.grid = element_blank(),title = element_text(size = 10))+
                ggtitle("Histograma de uma amostra da variável aleatória Beta Padrão")+
                ylab(" ")+
                xlab(expression(x))
            p2 <- qplot(escolha1[[2]], geom = 'blank') +   
                stat_function(fun = escolha1[[1]], n=length(escolha1[[4]]),
                              args = escolha1[[3]], colour = "red",
                              inherit.aes = FALSE)+
                geom_histogram(aes(y = ..density..),
                               alpha = 0.4, fill = "cadetblue4", color = "white") +
                theme_light()+
                theme(panel.grid = element_blank(),title = element_text(size = 10))+
                ggtitle("Histograma da amostra da variável aleatória transformada")+
                ylab(" ")+
                xlab(expression(x))
        } else if (input$var1 == "Gama"){
            p1 <- qplot(rgamma(10000,input$a1,input$b2), geom = 'blank') +   
                stat_function(fun = dgamma, n=10000,
                              args = list(shape = input$a1, rate = input$b2), colour = "red",inherit.aes = FALSE)+
                geom_histogram(aes(y = ..density..),
                               alpha = 0.4, fill = "cadetblue4", color = "white") +
                theme_light()+
                theme(panel.grid = element_blank(),title = element_text(size = 10))+
                ggtitle("Histograma de uma amostra da variável aleatória Gama")+
                ylab(" ")+
                xlab(expression(x))
            p2 <- qplot(escolha1[[2]], geom = 'blank') +   
                stat_function(fun = escolha1[[1]], n=length(escolha1[[4]]),
                              args = escolha1[[3]], colour = "red",
                              inherit.aes = FALSE)+
                geom_histogram(aes(y = ..density..),
                               alpha = 0.4, fill = "cadetblue4", color = "white") +
                theme_light()+
                theme(panel.grid = element_blank(),title = element_text(size = 10))+
                ggtitle("Histograma da amostra da variável aleatória transformada")+
                ylab(" ")+
                xlab(expression(x))
        }
        
        gridExtra::grid.arrange(p1,p2,nrow=1)
    })
    
    # Terceira tab ####
    output$txt_5 <- renderText("Aluno: Wesley Almeida Cruz")
    output$txt_6 <- renderText("Orientador: Moisés")
    
    cauda <- function(x,a,b,c,d){
        if(x == "Normal"){
            func <- dnorm
            argu <- list(mean = a, sd = b)
            cond1 <- seq(a-4*b,a+4*b,length.out = 1000)
            
            if(input$escol == "Esquerda"){
                esqu <- seq(a-4*b,c,length.out = 1000)
                prob <- dnorm(esqu,a,b)
                probabi <- pnorm(c,a,b)
                p <- qplot(cond1,geom = 'blank') +   
                    stat_function(fun = func, n=length(cond1),
                                  args = argu, color = "blue",
                                  inherit.aes = FALSE)+
                    geom_ribbon(aes(x = esqu, ymin = 0, ymax = prob),fill = "blue", alpha = 0.4) +
                    ylab("Função densidade")+
                    xlab(expression(x)) +
                    theme_light()+
                    theme(legend.position = "None",
                          panel.grid = element_blank())
            } else if(input$escol == "Direita"){
                dire <- seq(d,a+4*b,length.out = 1000)
                prob <- dnorm(dire,a,b)
                probabi <- pnorm(d,a,b,lower.tail = F)
                p <- qplot(cond1,geom = 'blank') +   
                    stat_function(fun = func, n=length(cond1),
                                  args = argu, color = "blue",
                                  inherit.aes = FALSE)+
                    geom_ribbon(aes(x = dire, ymin = 0, ymax = prob),fill = "blue", alpha = 0.4) +
                    ylab("Função densidade")+
                    xlab(expression(x)) +
                    theme_light()+
                    theme(legend.position = "None",
                          panel.grid = element_blank())
            } else if(input$escol == "Bilateral"){
                esqu <- seq(a-4*b,c,length.out = 500)
                dire <- seq(d,a+4*b,length.out = 500)
                prob <- dnorm(esqu,a,b)
                prob1 <- dnorm(dire,a,b)
                probabi <- pnorm(c,a,b)+pnorm(d,a,b,lower.tail = F)
                
                p <- qplot(cond1,geom = 'blank') +   
                    stat_function(fun = func, n=length(cond1),
                                  args = argu, color = "blue",
                                  inherit.aes = FALSE)+
                    geom_ribbon(aes(x = esqu, ymin = 0, ymax = prob),fill = "blue", alpha = 0.4) +
                    geom_ribbon(aes(x = dire, ymin = 0, ymax = prob1),fill = "blue", alpha = 0.4) +
                    ylab("Função densidade")+
                    xlab(expression(x)) +
                    theme_light()+
                    theme(legend.position = "None",
                          panel.grid = element_blank())
            } else {
                cond <- seq(c,d,length.out = 1000)
                prob <- dnorm(cond,a,b)
                probabi <- pnorm(d,a,b)- pnorm(c,a,b)
                
                p <- qplot(cond1,geom = 'blank') +   
                    stat_function(fun = func, n=length(cond1),
                                  args = argu, color = "blue",
                                  inherit.aes = FALSE)+
                    geom_ribbon(aes(x = cond, ymin = 0, ymax = prob),fill = "blue", alpha = 0.4) +
                    ylab("Função densidade")+
                    xlab(expression(x)) +
                    theme_light()+
                    theme(legend.position = "None",
                          panel.grid = element_blank())
            }
        }
        else if(x == "t-Student"){
            func <- dt
            argu <- list(df = a)
            cond1 <- seq(qt(0.001,a),qt(0.999,a),length.out = 1000)
            
            if(input$escol == "Esquerda"){
                esqu <- seq(qt(0.001,a),c,length.out = 1000)
                prob <- dt(esqu,a)
                probabi <- pt(c,a)
                p <- qplot(cond1,geom = 'blank') +   
                    stat_function(fun = func, n=length(cond1),
                                  args = argu, color = "blue",
                                  inherit.aes = FALSE)+
                    geom_ribbon(aes(x = esqu, ymin = 0, ymax = prob),fill = "blue", alpha = 0.4) +
                    ylab("Função densidade")+
                    xlab(expression(x)) +
                    theme_light()+
                    theme(legend.position = "None",
                          panel.grid = element_blank())
            } else if(input$escol == "Direita"){
                dire <- seq(d,qt(0.999,a),length.out = 1000)
                prob <- dt(dire,a)
                probabi <- pt(d,a,lower.tail = F)
                p <- qplot(cond1,geom = 'blank') +   
                    stat_function(fun = func, n=length(cond1),
                                  args = argu, color = "blue",
                                  inherit.aes = FALSE)+
                    geom_ribbon(aes(x = dire, ymin = 0, ymax = prob),fill = "blue", alpha = 0.4) +
                    ylab("Função densidade")+
                    xlab(expression(x)) +
                    theme_light()+
                    theme(legend.position = "None",
                          panel.grid = element_blank())
            } else if(input$escol == "Bilateral"){
                esqu <- seq(qt(0.001,a),c,length.out = 500)
                dire <- seq(d,qt(0.999,a),length.out = 500)
                prob <- dt(esqu,a)
                prob1 <- dt(dire,a)
                probabi <- pt(c,a)+pt(d,a,lower.tail = F)
                
                p <- qplot(cond1,geom = 'blank') +   
                    stat_function(fun = func, n=length(cond1),
                                  args = argu, color = "blue",
                                  inherit.aes = FALSE)+
                    geom_ribbon(aes(x = esqu, ymin = 0, ymax = prob),fill = "blue", alpha = 0.4) +
                    geom_ribbon(aes(x = dire, ymin = 0, ymax = prob1),fill = "blue", alpha = 0.4) +
                    ylab("Função densidade")+
                    xlab(expression(x)) +
                    theme_light()+
                    theme(legend.position = "None",
                          panel.grid = element_blank())
            } else {
                cond <- seq(c,d,length.out = 1000)
                prob <- dt(cond,a)
                probabi <- pt(d,a)- pt(c,a)
                
                p <- qplot(cond1,geom = 'blank') +   
                    stat_function(fun = func, n=length(cond1),
                                  args = argu, color = "blue",
                                  inherit.aes = FALSE)+
                    geom_ribbon(aes(x = cond, ymin = 0, ymax = prob),fill = "blue", alpha = 0.4) +
                    ylab("Função densidade")+
                    xlab(expression(x)) +
                    theme_light()+
                    theme(legend.position = "None",
                          panel.grid = element_blank())
            }
        }
        else if(x == "F-Snedecor"){
            func <- df
            argu <- list(df1 = a, df2=b)
            cond1 <- seq(qf(0.001,a,b),qf(0.999,a,b),length.out = 1000)
            
            if(input$escol == "Esquerda"){
                esqu <- seq(qf(0.001,a,b),c,length.out = 1000)
                prob <- df(esqu,a,b)
                probabi <- pf(c,a,b)
                p <- qplot(cond1,geom = 'blank') +   
                    stat_function(fun = func, n=length(cond1),
                                  args = argu, color = "blue",
                                  inherit.aes = FALSE)+
                    geom_ribbon(aes(x = esqu, ymin = 0, ymax = prob),fill = "blue", alpha = 0.4)+
                    ylab("Função densidade")+
                    xlab(expression(x)) +
                    theme_light()+
                    xlim(0,5)+
                    theme(legend.position = "None",
                          panel.grid = element_blank())
            } else if(input$escol == "Direita"){
                dire <- seq(d,qf(0.999,a,b),length.out = 1000)
                prob <- df(dire,a,b)
                probabi <- pf(d,a,b,lower.tail = F)
                p <- qplot(cond1,geom = 'blank') +   
                    stat_function(fun = func, n=length(cond1),
                                  args = argu, color = "blue",
                                  inherit.aes = FALSE)+
                    geom_ribbon(aes(x = dire, ymin = 0, ymax = prob),fill = "blue", alpha = 0.4) +
                    xlim(0,5)+
                    ylab("Função densidade")+
                    xlab(expression(x)) +
                    theme_light()+
                    theme(legend.position = "None",
                          panel.grid = element_blank())
            } else if(input$escol == "Bilateral"){
                esqu <- seq(qf(0.001,a,b),c,length.out = 500)
                dire <- seq(d,qf(0.999,a,b),length.out = 500)
                prob <- df(esqu,a,b)
                prob1 <- df(dire,a,b)
                probabi <- pf(c,a,b)+pf(d,a,b,lower.tail = F)
                
                p <- qplot(cond1,geom = 'blank') +   
                    stat_function(fun = func, n=length(cond1),
                                  args = argu, color = "blue",
                                  inherit.aes = FALSE)+
                    geom_ribbon(aes(x = esqu, ymin = 0, ymax = prob),fill = "blue", alpha = 0.4) +
                    geom_ribbon(aes(x = dire, ymin = 0, ymax = prob1),fill = "blue", alpha = 0.4) +
                    xlim(0,5)+
                    ylab("Função densidade")+
                    xlab(expression(x)) +
                    theme_light()+
                    theme(legend.position = "None",
                          panel.grid = element_blank())
            } else {
                cond <- seq(c,d,length.out = 1000)
                prob <- df(cond,a,b)
                probabi <- pf(d,a,b)- pf(c,a,b)
                
                p <- qplot(cond1,geom = 'blank') +   
                    stat_function(fun = func, n=length(cond1),
                                  args = argu, color = "blue",
                                  inherit.aes = FALSE)+
                    geom_ribbon(aes(x = cond, ymin = 0, ymax = prob),fill = "blue", alpha = 0.4) +
                    xlim(0,5)+
                    ylab("Função densidade")+
                    xlab(expression(x)) +
                    theme_light()+
                    theme(legend.position = "None",
                          panel.grid = element_blank())
            }
        }
        
        return(list(p,probabi))
    }
    
    output$plot4 <- renderPlot({
        cauda(input$prob1,input$a11,input$b22,input$min1,input$max1)[[1]]
    })
    output$textt <- renderText({
        round(as.numeric(cauda(input$prob1,input$a11,input$b22,input$min1,input$max1)[[2]]),3)
    })
})