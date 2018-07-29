# Buffon's Noodle simulator.
library(bezier)

D <- 1 # grid width
L <- 2 # noodle length (can be longer than D)

Gx <- 5 # grid size in x
Gy <- 5 # grid size in y

# generate some points uniformly, then fit a bezier curve through them, and truncate at
# length L. Then count the number of intersections.

gen_bezier <- function(L) {
  while (TRUE) {
    N <- 10
    # Hmm, maybe try brownian motion?
    u <- matrix(0, N+1, 2)
    u[1:N+1,] <- rnorm(2*N, mean=0, sd=L/3)
    u[,1] <- cumsum(u[,1])
    u[,2] <- cumsum(u[,2])

    # generate 5 points uniformly on a circle (we don't want certain directions prioritised?)
    # does this even happen??
#    u <- matrix(c(runif(N, min=-3*L, max=3*L), runif(N, min=-3*L, max=3*L)), ncol=2)
    
    # Generate a random bezier
    b <- bezier(t=seq(0,1,length=100), u)
    # Truncate b
    d <- cumsum(sqrt(diff(b[,1])^2 + diff(b[,2])^2))
    n1 <- min(which(d >= L))
    if (!is.infinite(n1))
      break
  }
  # mayaswell move the last point as close as possible to what we want
  alpha <- approx(x=d[(n1-1):n1], y=0:1, xout=L)$y
  nood <- rbind(b[1:n1,], b[n1,]*(1-alpha) + alpha*b[n1+1,])
  # once we have the bezier, we need to move it so it is inside our region for visualisation/counting.
  # We can move it a set number of grid lines left or right to ensure this.
#  noodmid <- colMeans(nood)
#  nood[,1] <- nood[,1] - noodmid[1]
#  nood[,2] <- nood[,2] - noodmid[2]
  nood
}

rand_bezier2 <- function(B, Gx, Gy) {
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

rand_bezier <- function(L, Gx, Gy) {
  # generate a bunch of points to fit our bezier across
  while (TRUE) {
    N <- 5
    # Hmm, maybe try brownian motion?
#    rnorm(10, mean=0, sd=L/3)
    
    # generate 5 points uniformly on a circle (we don't want certain directions prioritised?)
    # does this even happen??
    u <- matrix(c(runif(N, min=-3*L, max=3*L), runif(N, min=-3*L, max=3*L)), ncol=2)
    
    # Generate a random bezier
    b <- bezier(t=seq(0,1,length=100), u)
    # Truncate b
    d <- cumsum(sqrt(diff(b[,1])^2 + diff(b[,2])^2))
    n1 <- min(which(d >= L))
    if (!is.infinite(n1))
      break
  }
  # mayaswell move the last point as close as possible to what we want
  alpha <- approx(x=d[(n1-1):n1], y=0:1, xout=L)$y
  nood <- rbind(b[1:n1,], b[n1,]*(1-alpha) + alpha*b[n1+1,])
  # once we have the bezier, we need to move it so it is inside our region for visualisation/counting.
  # We can move it a set number of grid lines left or right to ensure this.
  noodmid <- colMeans(nood)
  
  # sample our actual midpoint
  midpoint <- c(runif(1, min=0, max=Gx), runif(1, min=-0.5, max=Gy+0.5))
  # in the x direction is closest to 0. Then we'll arbitrary place it around one of our grid lines
  nood[,1] <- nood[,1]-noodmid[1] + midpoint[1]
  nood[,2] <- nood[,2]-noodmid[2] + midpoint[2]
  nood
}

intersect_bezier <- function(b, lines=0:Gx) {
  # find points of intersection between the lines x=lines[0]..lines[1]
  # and a bezier
  p <- matrix(0, 0, 2)
  for (i in 1:(nrow(b)-1)) {
    x = b[i:(i+1),1]
    # check if x1 < lines < x2 or x2 < lines < x1
    # run through the lines (this isn't very efficent - lots of these won't be near anything)
    for (lx in lines) {
      if ((x[1] < lx && lx <= x[2]) ||
          (x[2] <= lx && lx < x[1])) {
        # hit - find the y coordinate as a check
        ly <- approx(x=x, y=b[i:(i+1),2], xout=lx)$y
        p <- rbind(p, c(lx, ly))
      }
    }
  }
  p
}

mapprox <- function(x, y, xout) {
  # this is really easy - we just have two x values, two y values
  # and we want to interpolate to xout
  (xout-x[1]) / (x[2]-x[1]) * (y[2]-y[1]) + y[1]
}

intersect_bezier2 <- function(b) {
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

noodle_outside <- function(n, lines=0:Gx) {
  return (max(n) < min(lines)-0.5 ||
      min(n) > max(lines)+0.5)
}

plot(NULL, xlim=c(-0.5,Gx+0.5), ylim=c(-0.5,Gy+0.5), asp=1)
abline(v=0:Gx)
#for (i in 1:100) {
num_noodles <- 10000
  r <- replicate(num_noodles, rand_bezier(N, Gx, Gy))
  # throwaway any noodles that are completely outside
#  r <- r[!unlist(lapply(r, noodle_outside))]
  p <- lapply(r, intersect_bezier)
  junk <- lapply(r, lines, col="#0000003f")
  junk <- lapply(p, points)
  i <- unlist(lapply(p, nrow))
  sum(i)/length(r)

  num_noodles <- 10000
  L <- 2
  r <- replicate(num_noodles, rand_bezier(L, Gx, Gy))
  p <- lapply(r, intersect_bezier, lines=-ceiling(L):(Gx+ceiling(L)))
  i <- unlist(lapply(p, nrow))
  expected <- 2*L/(D*pi)*length(r)
  sum(i) / expected

  plot(NULL, xlim=c(-0.5,Gx+0.5), ylim=c(-0.5,Gy+0.5), asp=1)
#  abline(v=0:Gx)
  num_noodles <- 10000
  L <- 4
  B <- replicate(10,gen_bezier(L))

  r <- replicate(num_noodles, rand_bezier2(B, Gx, Gy), simplify=FALSE)
  p <- lapply(r, intersect_bezier2)
  junk <- lapply(r, lines, col='#0000001f')
  junk <- lapply(p, points, pch='.', col='black')
  i <- unlist(lapply(p, nrow))
  expected <- 2*L/(D*pi)*length(r)
  sum(i) / expected
  
  lines(r[[10]], col='red')
#  lines(r, type='l', col=i)
#}

a <- bezierArcLength(u, 0)
a <- bezierAL(u, 0)
