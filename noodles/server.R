library(shiny)
library(lubridate)

source("sample_db.R")
#source("sample_csv.R")
source("bezier_stuff.R")

# size of noodles
L <- 200
D <- 70

# storage
vars    <- paste0("count", 1:5)
columns <- c(vars, "year")

current_year = lubridate::year(Sys.Date())

# protect against silly input
clamp <- function(x, xmin=0, xmax=20) {
  min(max(x, xmin), xmax)
}

# create database for noodles
if (!create_database("noodle", columns)) {
  cat("Unable to create database\n", file=stderr());
}
sim_columns <- c("n", "Ex", "Ex2", "year")
if (!create_database("simulation", sim_columns)) {
  cat("Unable to create database\n", file=stderr());
}
sim_start <- na.omit(read_rows("simulation", sim_columns))
if (!is.null(sim_start) && nrow(sim_start) > 0) {
  sim_start <- sim_start[nrow(sim_start),]
  cat("read simulation info from database\n", file=stderr())
} else {
  sim_start <- matrix(0, 1, length(sim_columns))
  colnames(sim_start) = sim_columns;
  sim_start <- as.data.frame(sim_start)
}

# create a bunch of random noodles
B <- replicate(100, gen_bezier(L=L/D), simplify = FALSE)

# noodles to plot
num_noodles <- 100

# estimate pi
calc_pi <- function(meanX, L, D) {
  1/meanX * 2*L/D
}

shinyServer(function(input, output, session) {

  v <- reactiveValues(samples = read_rows("noodle", columns))
  n <- reactiveValues(noodles = list(), points = NULL,
                      run=sim_start,
                      Gx=6, Gy=3)

  # reactive that converts input into something we can plot/use
  get_sample <- reactive({
    # Grab the variables
    sample <- numeric(length(vars))
    for (i in seq_along(vars))
      sample[i] <- clamp(as.numeric(input[[vars[i]]]))

    sample
  })

  # make the submit button do something useful
  observeEvent(input$submit, {
    sample <- get_sample()

    if (sum(!is.na(sample)) > 0) {
      # write the results to the database
      v$samples <- rbind(v$samples, c(sample, current_year))
      if (!write_row("noodle", columns, c(sample, current_year))) {
        cat("Unable to write sample to database\n", file=stderr())
      }
    }

    # reset our input controls
    for (i in seq_along(vars))
      updateNumericInput(session, vars[i], value=NA)
  })
  
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
    if (n$run$n %% 100 == 0) # save after every 100
      write_row("simulation", sim_columns, c(n$run, current_year))
    # add to our noodle list to update the plot
    n$noodles[[length(n$noodles)+1]] <- noodle
    n$points <- points
  })

  # plot on the left shows actual data
  output$data <- renderPlot( {
    sample <- get_sample()
    par(mfrow=c(2,1),mar=c(2,3,3,0), omi=c(0.5,0,0,0))
#    par(mfrow=c(3,1),mar=c(0,3,3,0), omi=c(0.5,0,0,0))
    max_count <- max(sample, 5, na.rm = TRUE)
    t <- table(factor(sample, levels=0:max_count))
    height <- max(t, 5, na.rm=TRUE)
    barplot(t, ylim=c(0,height), space=0, main="Sample")
    crosses <- mean(sample, na.rm=TRUE)
    if (!is.nan(crosses)) {
      meanX <- 2/pi*L/D
      abline(v=meanX+0.5, col='black', lty='dotted', lwd=2)
      abline(v=crosses+0.5, col='red', lwd=2)
      if (crosses < 2/pi * L/D) {
        mtext(side=3, at=crosses+0.5, 'Average crosses', col='red', adj=1.1)
        mtext(side=3, at=2/pi*L/D+0.5, 'Expected crosses', col='black', adj=-0.1)
      } else {
        mtext(side=3, at=crosses+0.5, 'Average crosses', col='red', adj=-0.1)
        mtext(side=3, at=2/pi*L/D+0.5, 'Expected crosses', col='black', adj=1.1)
      }
      PI <- calc_pi(crosses, L, D)
      text(max_count+1,height,substitute(paste(pi, phantom() %~~% phantom(), PI, sep=''), list(PI=sprintf("%.03f", PI))), adj=c(1,1.1), cex=1, col='blue')
    }

    wch = which(!names(v$samples) %in% "year")
    samples = na.omit(as.numeric(as.matrix(v$samples[v$samples$year == current_year,wch])))
    last_year = na.omit(as.numeric(as.matrix(v$samples[v$samples$year == current_year - 1,wch])))
    max_count <- max(samples, last_year, 5, na.rm = TRUE)
    if (length(samples) > 0) {
      t <- table(factor(samples, levels=0:max_count))
      height <- max(t, 4, na.rm=TRUE)+1
      barplot(t, ylim=c(0,height), space=0, main="All Noodles", xlab="Number of crosses")
      crosses <- mean(samples, na.rm=TRUE)
      meanX <- 2/pi*L/D
      abline(v=meanX+0.5, col='black', lty='dotted', lwd=2)
      abline(v=crosses+0.5, col='red', lwd=2)
      if (crosses < meanX) {
        mtext(side=3, at=crosses+0.5, 'Average crosses', col='red', adj=1.1)
        mtext(side=3, at=meanX+0.5, 'Expected crosses', col='black', adj=-0.1)
      } else {
        mtext(side=3, at=crosses+0.5, 'Average crosses', col='red', adj=-0.1)
        mtext(side=3, at=meanX+0.5, 'Expected crosses', col='black', adj=1.1)
      }
      PI <- calc_pi(crosses, L, D)
      text(max_count+1,height,substitute(paste(pi, phantom() %~~% phantom(), PI, sep=''), list(PI=sprintf("%.02f", PI))), adj=c(1,1.1), cex=2, col='blue')
    }
#    if (!is.null(last_year)) {
#      t <- table(factor(last_year, levels=0:max_count))
#      barplot(t, ylim=c(0,max(t, 5, na.rm=TRUE)), space=0, main="Last Year")
#    }
  } )

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
