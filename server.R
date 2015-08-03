library(shiny)

# generate our data - this never changes
set.seed(2015)
n <- 100
x <- rnorm(n, 3, 1)
y <- 0.5 + 0.5*x + rnorm(n, 0, 0.3)
line <- lm(y ~ x)
mx <- mean(x)
my <- mean(y)
vy <- var(y)

shinyServer(function(input, output, session) {

#   point <- reactiveValues(x = NULL, y = NULL)
# 
#   observeEvent(input$plot_click, {
#     dist <- NULL
#     if (!is.null(point$x)) {
#       # assess whether our click is near our current point (depends on range of x/y)
#       dist <- ((input$plot_click$x - point$x)/diff(range(x)))^2 +
#         ((input$plot_click$y - point$y)/diff(range(y)))^2
#     }
#     if (!is.null(dist) && dist < 0.001) {
#       point$x <- NULL
#     } else {
#       point$x <- input$plot_click$x
#       point$y <- input$plot_click$y
#       px <- c(x, point$x)
#       py <- c(y, point$y)
#       point$l2 <- lm(py ~ px)
#     }
#   })
#   output$data <- renderPlot({
# 
#     # plot the points, residuals as lines, model fit and (mean(x),mean(y))
#     par(mar=c(0,1,0,0), cex=2)
#     plot(NULL, xlim=range(x), ylim=range(y), xaxt="n", yaxt="n", xlab="", ylab="")
#     points(x, y, col="#00000050", pch=19, xlab="", ylab="", xaxt="n", yaxt="n")
#     abline(coef(line), col="#00000080", lwd=2)
#     if (!is.null(point$x)) {
#       # plot additional point and line
#       points(point$x, point$y, pch=19, col="#ff00009f", cex=1.5)
#       abline(coef(point$l2), col="#ff000090", lwd=3)
#     }
#     text(3.2, 0.77, "Click to add a point", cex=1, col="grey70")
#   })
# 
#   output$cooks <- renderPlot({
# 
#     # plot the points, residuals as lines, model fit and (mean(x),mean(y))
#     par(mar=c(0,0,0,1.5), cex=2)
#     lev_max <- 0.08
#     plot(NULL, xlim=c(0, lev_max), ylim=c(-6,6), xaxt="n", yaxt="n", xlab="Leverage", ylab="Residuals", xaxs="i")
#     text(0.04, -6, "Leverage", cex=1, col="grey50")
#     text(0.003, 0, "Residual", srt=90, cex=1, col="grey50")
#     if (!is.null(point$x)) {
#       r <- rstandard(point$l2)
#       h <- hatvalues(point$l2)
#       col <- c(rep("#00000050", length(x)), "#ff00009f")
#       cex <- c(rep(1, length(x)), 1.5)
#     } else {
#       r <- rstandard(line)
#       h <- hatvalues(line)
#       col <- "#00000050"
#       cex <- 1
#     }
#     points(h, r, col=col, cex=cex, pch=19)
#     
#     cooks <- 0.5
#     hi <- seq(0.02,lev_max,by=0.01)
#     ri <- sqrt((1-hi)/hi * 2)
#     lines(hi, ri*sqrt(0.5), col="red", lty="dashed")
#     lines(hi, -ri*sqrt(0.5), col="red", lty="dashed")
# 
#     lines(hi, ri, col="red", lty="dashed")
#     lines(hi, -ri, col="red", lty="dashed")
# 
#     mtext(c(1,0.5,0.5,1), side=4, at=c(-1,-sqrt(0.5),sqrt(0.5),1)*ri[length(ri)], line=0.2, las=1, col="red", cex=1.4)
#   })
})
