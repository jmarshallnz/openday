library(shiny)
library(lubridate)

#source("sample_db.R")
source("sample_csv.R")
source("bezier_stuff.R")

# size of noodles
L <- 210
D <- 276.5/4

current_year = lubridate::year(Sys.Date())

# create a bunch of random noodles
B <- replicate(100, gen_bezier(L=L/D), simplify = FALSE)

# noodles to plot
num_noodles <- 100

# estimate pi
calc_pi <- function(meanX, L, D) {
  1/meanX * 2*L/D
}

shinyServer(function(input, output, session) {

  n <- reactiveValues(noodles = list(), points = NULL,
                      run=list(n=0, K=0, Ex=0, Ex2=0),
                      Gx=6, Gy=3)

  observeEvent(input$slider, {
    # remove a noodle
    if (length(n$noodles) > num_noodles) {
      n$noodles <- n$noodles[-1]
    }
    # generate a new noodle
    noodle <- rand_bezier(B, n$Gx, n$Gy)
    # count how many times it crosses
    points <- intersect_bezier(noodle)
    x <- nrow(points)
    # update our values
    n$run$n = n$run$n + 1
    n$run$Ex = n$run$Ex + x
    n$run$Ex2 = n$run$Ex2 + x^2
    # add to our noodle list to update the plot
    n$noodles[[length(n$noodles)+1]] <- noodle
    n$points <- points
  })

  # plot the simulation
  output$history <- renderPlot( {
    par(mar=c(1,1,2,1))
    # compute n$Gy from n$Gx and dim
    n$Gy <- n$Gx * par()$pin[2] / par()$pin[1]
    plot(NULL, xlim=c(0,n$Gx), ylim=c(0,n$Gy), asp=1, axes=FALSE, xlab="", ylab="")
    abline(v=seq(0, n$Gx, by=1))
    if (length(n$noodles) > 1) {
      lapply(1:(length(n$noodles)-1), function(i) {
        lines(n$noodles[[i]], col=rgb(0,0,0,alpha=0.5-0.5*(length(n$noodles)-i)/num_noodles), lwd=2)
        })
      lines(n$noodles[[length(n$noodles)]], col='black', lwd=3)
      points(n$points, col='red', pch=19, cex=2)
    }
    title("Simulated noodles")
    if (n$run$n > 1) {
      meanX <- n$run$Ex/n$run$n
      sdX   <- sqrt((n$run$Ex2 - (n$run$Ex*n$run$Ex)/n$run$n) / (n$run$n-1))
      seX  <-  sdX / sqrt(n$run$n)
      PI <- calc_pi(meanX, L, D)
      text(n$Gx,n$Gy,substitute(paste(pi, phantom() %~~% phantom(), PI, sep=''), list(PI=sprintf("%.03f", PI))), adj=c(1,1), cex=5, col='blue')
      p1 <- calc_pi(meanX + 1.96*seX, L, D)
      p2 <- calc_pi(meanX - 1.96*seX, L, D)
      text(n$Gx,n$Gy,sprintf("%.03f - %.03f", p1, p2), adj=c(1,3.5), cex=2, col='blue')
      text(n$Gx,0,sprintf("n=%i", n$run$n), adj=c(1,0), col='blue', cex=2)
    }
    box()
  } )

})
