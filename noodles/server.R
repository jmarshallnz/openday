library(shiny)
library(lubridate)

source("sample_db.R")

vars    <- paste0("count", 1:5)
columns <- c(vars, "year")

current_year = lubridate::year(Sys.Date())

clamp <- function(x, xmin=0, xmax=20) {
  min(max(x, xmin), xmax)
}

if (!create_database(columns)) {
  cat("Unable to create database\n", file=stderr());
}

shinyServer(function(input, output, session) {

  v <- reactiveValues(samples = read_rows(columns))

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
      if (!write_row(columns, c(sample, current_year))) {
        cat("Unable to write sample to database\n", file=stderr())
      }
    }

    # reset our input controls
    for (i in seq_along(vars))
      updateNumericInput(session, vars[i], value=NA)
  })

  # plot on the left shows actual data
  output$data <- renderPlot( {
    sample <- get_sample()
    par(mfrow=c(3,1),mar=c(0,3,3,0), omi=c(0.5,0,0,0))
    t <- table(factor(sample, levels=0:5))
    barplot(t, ylim=c(0,max(sample, 5, na.rm=TRUE)), space=0, main="Sample")
    abline(v=mean(sample)+0.5, col='red')

    wch = which(!names(v$samples) %in% "year")
    samples = as.numeric(as.matrix(v$samples[v$samples$year == current_year,wch]))
    last_year = as.numeric(as.matrix(v$samples[v$samples$year == current_year - 1,wch]))
    max_count <- max(samples, last_year, 5)
    if (!is.null(samples)) {
      t <- table(factor(samples, levels=0:max_count))
      barplot(t, ylim=c(0,max(t, 5, na.rm=TRUE)), space=0, main="Population")
      abline(v=mean(sample)+0.5, col='red')
    }
    if (!is.null(last_year)) {
      t <- table(factor(last_year, levels=0:max_count))
      barplot(t, ylim=c(0,max(t, 5, na.rm=TRUE)), space=0, main="Last Year")
    }
  } )

  # plot the history...
  # output$history <- renderPlot( {
  #   par(mfrow=c(nrow(colours), 1), mar=c(0,3,1,0), omi=c(0.5,0,0.5,0))
  # 
  #   # plot the last K items or so, along with cummulative information
  #   wch = which(!names(v$samples) %in% "year")
  #   samples = as.matrix(v$samples[v$samples$year == current_year,wch])
  #   K <- min(100, nrow(samples))
  #   if (K > 0 && !is.null(samples)) {
  #     history <- samples[1:K + nrow(samples) - K,, drop=FALSE]
  # 
  #     # Point estimates and CIs for our history
  #     n_hist <- rowSums(history)
  #     p_hist <- sweep(history, 1, n_hist, FUN="/")
  #     l_hist <- lci(history, n_hist)
  #     u_hist <- uci(history, n_hist)
  #     
  #     # cummulative history
  #     cum_hist <- history
  #     cum_hist[1,] <- cum_hist[1,] + colSums(samples[seq_len(nrow(samples)-K),, drop=FALSE])
  #     if (nrow(cum_hist) > 1) # silly apply dropping dimensions...
  #       cum_hist <- apply(cum_hist, 2, cumsum)
  #     cum_hist <- sweep(cum_hist, 1, rowSums(cum_hist), FUN="/")
  #   }
  # 
  #   hist_step <- 0.07
  #   hist_breaks <- seq(0,hist_step*ceiling(1/hist_step),by=hist_step)
  #   # plots...
  #   for (i in seq_len(nrow(colours))) {
  #     plot(NULL, xlim=c(0,K+5), ylim=c(0,1), type="n", xaxt="n", xaxs="i", ann=FALSE, bty="n", las=2)
  # 
  #     if (K > 0 && !is.null(samples)) {
  #       # plot cummulative population values
  #       lines(1:K, cum_hist[,i], col=colours$col[i], lwd=2)
  # 
  #       # plot each estimate as a point estimate and CI
  #       segments(1:K, l_hist[,i], 1:K, u_hist[,i], col=colours$col[i])
  #       points(1:K, p_hist[,i], col=colours$col[i], pch=19, cex=1)
  # 
  #       # and at the end plot a histogram of the sample point estimates
  #       h <- hist(p_hist[,i], breaks=hist_breaks, plot=FALSE)
  #       rect(rep(K+2,10), hist_breaks[-length(hist_breaks)],
  #            rep(K+2,10)+ 3 * h$density/max(h$density), hist_breaks[-1], col=colours$col[i], border=NA)
  #     }
  #   }
  #   mtext("Estimating the population using samples", side=3, outer=TRUE, at=0.5, line=1, font=2, cex=1.2)
  # })

})
