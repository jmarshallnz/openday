library(shiny)

source("sample_db.R")

colours <- read.csv("colours.csv", stringsAsFactors=FALSE)
vars    <- unlist(lapply(seq_len(nrow(colours)), function(x) { paste0("col", x) }))

clamp <- function(x, xmin=0, xmax=20) {
  min(max(x, xmin), xmax)
}

if (!create_database(colours)) {
  cat("Unable to create database\n", file=stderr());
}

# some sample data for the other plot
probs <- c(0.3, 0.2, 0.15, 0.2, 0.1, 0.05)
n <- 100

size <- sample(12:20, replace=TRUE, n)
dat <- t(rmultinom(n, size, probs))

lci <- function(x, n) {
  qbeta(0.025, x+1, n - x + 1)
}
uci <- function(x, n) {
  qbeta(0.975, x+1, n - x)
}

shinyServer(function(input, output, session) {

  v <- reactiveValues(samples = read_samples())

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
    # TODO:reset our input controls
    cat("button hit\n")

    sample <- get_sample()

    if (sum(sample) > 0) {
      # write the results to the database
      v$samples <- rbind(v$samples, sample)
      if (!write_sample(colours, sample)) {
        cat("Unable to write sample to database\n", file=stderr())
      }
    }

    # reset our input controls
    for (i in seq_along(vars))
      updateNumericInput(session, vars[i], value=0)
  })
  
  # plot on the left shows actual data
  output$data <- renderPlot( {
    sample <- get_sample()
    par(mfrow=c(2,1),mar=c(0,3,3,0), omi=c(0.5,0,0,0))
    barplot(sample, names = "", col=colours$col, ylim=c(0,max(sample, 5, na.rm=TRUE)), main="Sample")
    if (!is.null(v$samples)) {
      popn <- colSums(v$samples)
      barplot(popn, names = "", col=colours$col, ylim=c(0,max(popn, 5, na.rm=TRUE)), main="Population", border=NA)
    }
  } )
  
  # plot the history...
  output$history <- renderPlot( {
    par(mfrow=c(nrow(colours), 1), mar=c(0,3,1,0), omi=c(0.5,0,0.5,0))

    # plot the last K items or so, along with cummulative information
    K <- min(100, nrow(v$samples))
    if (K > 0 && !is.null(v$samples)) {
      history <- v$samples[1:K + nrow(v$samples) - K,, drop=FALSE]

      # Point estimates and CIs for our history
      n_hist <- rowSums(history)
      p_hist <- sweep(history, 1, n_hist, FUN="/")
      l_hist <- lci(history, n_hist)
      u_hist <- uci(history, n_hist)
      
      # cummulative history
      cum_hist <- history
      cum_hist[1,] <- cum_hist[1,] + colSums(v$samples[seq_len(nrow(v$samples)-K),, drop=FALSE])
      if (nrow(cum_hist) > 1) # silly apply dropping dimensions...
        cum_hist <- apply(cum_hist, 2, cumsum)
      cum_hist <- sweep(cum_hist, 1, rowSums(cum_hist), FUN="/")
    }

    hist_step <- 0.07
    hist_breaks <- seq(0,hist_step*ceiling(1/hist_step),by=hist_step)
    # plots...
    for (i in seq_len(nrow(colours))) {
      plot(NULL, xlim=c(0,K+5), ylim=c(0,1), type="n", xaxt="n", xaxs="i", ann=FALSE, bty="n", las=2)

      if (K > 0 && !is.null(v$samples)) {
        # plot cummulative population values
        lines(1:K, cum_hist[,i], col=colours$col[i], lwd=2)
  
        # plot each estimate as a point estimate and CI
        segments(1:K, l_hist[,i], 1:K, u_hist[,i], col=colours$col[i])
        points(1:K, p_hist[,i], col=colours$col[i], pch=19, cex=1)

        # and at the end plot a histogram of the sample point estimates
        h <- hist(p_hist[,i], breaks=hist_breaks, plot=FALSE)
        rect(rep(K+2,10), hist_breaks[-length(hist_breaks)],
             rep(K+2,10)+ 3 * h$density/max(h$density), hist_breaks[-1], col=colours$col[i], border=NA)
      }
    }
    mtext("Estimating the population using samples", side=3, outer=TRUE, at=0.5, line=1, font=2, cex=1.2)
  })

})
