############################################################################
# Matrix manipulation methods
#
# For simplicity we have avoided to create generic functions for 'flip' etc.
# and therefore we have to call the corresponding methods coupled to the
# 'matrix' class explicitly, i.e. flip.matrix().
############################################################################
# Flip matrix (upside-down)
flip.matrix <- function(x) {
  mirror.matrix(rotate180.matrix(x))
}

# Mirror matrix (left-right)
mirror.matrix <- function(x) {
  xx <- as.data.frame(x);
  xx <- rev(xx);
  xx <- as.matrix(xx);
  xx;
}

# Rotate matrix 90 clockworks
rotate90.matrix <- function(x) {
  t(mirror.matrix(x))
}

# Rotate matrix 180 clockworks
rotate180.matrix <- function(x) { 
  xx <- rev(x);
  dim(xx) <- dim(x);
  xx;
}

# Rotate matrix 270 clockworks
rotate270.matrix <- function(x) {
  mirror.matrix(t(x))
}

############################################################################
# Color methods
############################################################################
# The inverse function to col2rgb()
rgb2col <- function(rgb) {
  hexDigit <- c(0:9, "A", "B", "C", "D", "E", "F")
  rgb <- rgb %% 256
  hi <- rgb %/% 16
#  lo <- rgb %% 16
  lo <- rgb - 16*hi  # Faster?
  x <- t(matrix(c(hi,lo), ncol=2)) + 1
  s <- matrix(hexDigit[x], nrow=6)
  s <- apply(s, MARGIN=2, FUN=paste, collapse="")
  paste("#", s, sep="")
}

############################################################################
# Draw methods
############################################################################
image180 <- function(z, ...) {
  image(rotate180.matrix(z), ...)
}

image270 <- function(z, ...) {
  image(rotate270.matrix(z), ...)
}