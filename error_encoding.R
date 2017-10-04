# Reed-Solomon Error Correction Encoding over Galois Field 256

# Add/Subtract modulo 256. Note this is performed by XORing the values
GFAdd <-function(x,y) {
  packBits(xor(intToBits(x), intToBits(y)), type="integer") %% 256
}

# Subtracting is same as addition in GF256
GFSub <- function(x,y) {
  GFAdd(x,y)
}

# Create log/antilog tables, to implment multiplication/division as addition/subtraction.
# The QR code specification states tha powers should be modulo 285
GFLogTable <- function() {

  logtable     <- vector(length=256)
  antilogtable <- vector(length=512)

  val <- 1
  for(i in 1:256) {
    val <- val*2
    if (val >= 256) {
      val <- GFAdd(val, 285)
    }
    logtable[i]       <- val
    antilogtable[val] <- i %% 255
  }

  return(list(to=logtable, from=antilogtable))
}

kGFLT<-GFLogTable()

# Calculate log(x) using logtable.
GFLog <- function(x) {
  result <- 0
  if (x %% 256 != 0) {
    result <- kGFLT$from[x %% 256]
  } else {
    simpleError("Argument cannot be zero mod 256.")
  }

  return(result)
}

# Calculate antilog using logtable.
GFExp <- function(x) {
  result <- 1 # deault ^0 = 1
  if (x %% 255 != 0) {
     result <- kGFLT$to[x %% 255]
  }
  return(result)
}

# Calculate multiplication via logarithmic addition
GFMult <- function(x,y) {
  result <- 0
  if (x!=0 && y != 0) {
    result <- GFExp((GFLog(x) + GFLog(y)))
  }
  return(result)
}

# Calculate division via logarithmic addition
GFDiv <- function(x,y) {
  result <- 0

  if (y == 0) {
    simpleError("Division by zero")
  } else if (x != 0) {
    result <- GFExp((GFLog(x) + 255 - GFLog(y)))
  }
  return(result)
}

# Calculate pow.
GFPow <- function(x,p) {
  return(GFExp((GFLog(x) * p)))
}

# Calculate inerse power.
GFInverse <- function(x) {
  return(GFExp(255 - GFLog(x)))
}

#====================  Polynomial functions

# Polynomials are stored as a vector of coefficients beginning with the 
# largest power.
# e.g. ax^3 + cx + d => (a,0,c,d)

# Multiply a polynomial by a constant.
GFPolyScale <- function(v,n) {
  sapply(v,FUN=function(x) GFMult(x,n))
}

# Add two polynomials. The result should have the same degree as the max degree.
# e.g. (a,b,c,d) + (m,n,o) => (a,b+m,c+n,d+o)
GFPolyAdd<-function(p,q) {

  pl <- length(p)
  ql <- length(q)

  if (pl < ql) {
     p <- append(replicate(ql-pl, 0),p)
  } else if (ql < pl) {
     q <- append(replicate(pl-ql,0),q)
  }

  r <- vector(length=length(p))

  for(i in seq(p)) {
    r[i] <- GFAdd(p[i],q[i])
  }

  return(r)
}

# Multiply two polynomials together.
GFPolyMult<-function(p,q) {

   r <- replicate(length(p) + length(q) - 1, 0)

   for(j in seq(p)) {
     for(i in seq(q)) {
       r[i+j-1] <- GFAdd(r[i+j-1], GFMult(p[j],q[i]))
     }
   }
   return(r)
}

# Calculate the generator polynomial for n terms.
Poly <- function(n) {
  g <- c(1,1)
  if (n > 1) {
  for(i in 1:(n-1)) {
    g = GFPolyMult(g,c(1,GFPow(2,i)))
  }
  }
  return(g)
}

# Calculate 
GFPolyEval <- function(poly,x) {
  y <- poly[1]
  for(i in seq(poly)-1) {
    y <- GFAdd(GFMult(y, x), poly[i+1])
  }
  return(y)
}

GFErrorTerms <- function(poly,divisor) {
  # Multiply polynomial by factor of n (n=error codewords)

  bigpoly <- append(poly, replicate(length(divisor) - 1, 0))

  # Ensure divisor has same degree
  bigdivisor <- append(divisor, replicate(length(bigpoly) - length(divisor), 0))

  p <- bigpoly
  d <- bigdivisor
  for(i in seq(poly)) {
    # Multiply divisor by lead term of poly
    # Convert poly to gf terms
    ds <- GFPolyScale(d, p[1])
 
    # XOR the result with the message poly
    m <- GFPolyAdd(ds, p)
    # Discard lead 0 term

    m <- tail(m,-1)
    d <- head(d,-1)
    p <- m
  }
  return(m)
}
