# Buffon's Noodle simulator.
library(bezier)

# generate some points uniformly, then fit a bezier curve through them, and truncate at
# length L. Then count the number of intersections.

gen_bezier <- function(L) {
  while (TRUE) {
    N <- 3
    # Hmm, maybe try brownian motion?
    u <- matrix(0, N+1, 2)
    u[1:N+1,] <- rnorm(2*N, mean=0, sd=L/3)
    u[,1] <- cumsum(u[,1])
    u[,2] <- cumsum(u[,2])

    # Generate a random bezier
    b <- bezier(t=seq(0,5,length=100), u)
    # check for high curvature (low segment length)
    min_arc_len <- min((diff(b)[,1])^2 + (diff(b)[,2])^2)
    if (min_arc_len > L/nrow(b) / 10) {
      # Not too high curvature
      # Truncate b
      d <- cumsum(sqrt(diff(b[,1])^2 + diff(b[,2])^2))
      n1 <- min(which(d >= L))
      if (!is.infinite(n1))
        break
    }
  }
  # mayaswell move the last point as close as possible to what we want
  alpha <- approx(x=d[(n1-1):n1], y=0:1, xout=L)$y
  nood <- rbind(b[1:n1,], b[n1,]*(1-alpha) + alpha*b[n1+1,])
  # DONE!
  nood
}

rand_bezier <- function(B, Gx, Gy) {
  if (is.list(B)) {
    # randomly generate from the list
    B = B[[sample(length(B),1)]]
  }
  # generate a random angle and position
  midpoint <- c(runif(1, min=0, max=Gx), runif(1, min=-0.5, max=Gy+0.5))
  theta <- runif(1,max=2*pi)
  # transform
  s <- sin(theta); c <- cos(theta)
  m <- matrix(c(c, s, -s, c), 2, 2, byrow=TRUE)
  C <- B %*% m
  C[,1] <- C[,1] + midpoint[1]
  C[,2] <- C[,2] + midpoint[2]
  C
}

mapprox <- function(x, y, xout) {
  # this is really easy - we just have two x values, two y values
  # and we want to interpolate to xout
  (xout-x[1]) / (x[2]-x[1]) * (y[2]-y[1]) + y[1]
}

intersect_bezier <- function(b) {
  # find points of intersection between the lines x=lines[0]..lines[1]
  # and a bezier
  p <- matrix(0, 0, 2)
  for (i in 1:(nrow(b)-1)) {
    x = b[i:(i+1),1]
    if (ceiling(x[1]) == floor(x[2])) {
      lx <- ceiling(x[1])
      ly <- mapprox(x=x, y=b[i:(i+1),2], xout=lx)
      p <- rbind(p, c(lx, ly))
    } else {
    if (floor(x[1]) == ceiling(x[2])) {
      # round up/down to same number - hit
      lx <- floor(x[1])
      ly <- mapprox(x=x, y=b[i:(i+1),2], xout=lx)
      p <- rbind(p, c(lx, ly))
    }
    }
  }
  p
}
