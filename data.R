# Setup some data tables for referencing

kCapacities      <- read.csv(file = "Capacities.csv",      sep = "\t", row.names = "Name", header = TRUE)
kErrorCorrection <- read.csv(file = "ErrorCorrection.csv", sep = "\t", row.names = "Name")
KFormatStrings   <- read.csv(file = "FormatStrings.csv",   sep = "\t", row.names = "Name")


# Character conversion tables for alphanumeric encoding
kAlphaTable <- strsplit("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ $%*+-./:","")[[1]]

StrPair <- function(text) {
  substring(text, seq(1, nchar(text), 2), seq(2, nchar(text) + 1, 2))
}

AlnumPair <- function(text) {
  if(nchar(text) == 2) {
    c <- (which(kAlphaTable == substr(text, 1, 1)) - 1) * 45 + (which(kAlphaTable == substr(text, 2, 2)) - 1)
    r <- padn(NumToBits(c), 11)
  } else {
    c <- which(kAlphaTable == substr(text, 1, 1)) - 1
    r <- padn(NumToBits(c), 6)
  }

  return(r)
}

AlnumConv <- function(text) {
    v <- {}
    for (p in StrPair(text)) {
      v <- append(v, AlnumPair(p))
    }

    return(v)
}

#===================== Conversion betweens bytes/bits and byte vectors/bit vectors

Pad8Bits <- function (x) {
  PadN(x, 8)
}

Pad9Bits <- function (x) {
  PadN(x, 9)
}

PadN <- function(x,n) {
  tail(append(replicate(n, 0), x), n)
}

NumToBits <- function(i) {
  rev(as.numeric(intToBits(i)))
}

NumToPad8Bits <- function(i) {
  Pad8Bits(NumToBits(i))
}

Reforge <- function(bitVector) {
    rev(sapply(packBits(as.integer(rev(bitVector)), type=c("raw")), as.integer))
}

###################################################################################

# Generate QRCode data for passed in text.

QRCode <- function(text,mode="text",qrversion="1-Q") {
  padding <- c(1,1,1,0,1,1,0,0,0,0,0,1,0,0,0,1)
  message("TEXT=",text)
  if (mode == "text") {
    # Check that the data fits!
    if (nchar(text) > kCapacities[qrversion, "A"]) stop("Text to large for chosen version.",nchar(text),">",kCapacities[qrversion,"A"])
    data <- c(0,0,1,0)
    data <- append(data, PadN(NumToBits(nchar(text)), 9))
    data <- append(data, AlnumConv(text))
  } else {
    # Check that the data fits!
    if (nchar(text) > kCapacities[qrversion,"B"]) stop("Text to large for chosen version.",nchar(text),">",kCapacities[qrversion,"B"])
    data <- c(0,1,0,0)
    data <- append(data, NumToPad8Bits(nchar(text))) # Message Length
    data <- append(data, sapply(as.numeric(charToRaw(text)), NumToPad8Bits))
  }

  errorCodeWords <- kErrorCorrection[qrversion,"T"]

  # Add 4 zeros
  data <- as.vector(append(data, c(0,0,0,0)))

  # Pad with 0s to a length multiple of 8
  if(length(data) %% 8 != 0) {
    data <- append(data, replicate(8-(length(data) %% 8), 0))
  }

  # Fill up missing codewords with padding bits
  if(length(data) < errorCodeWords * 8) {
    data <- head(append(data,rep(padding, errorCodeWords - (length(data) / 8))), errorCodeWords * 8)
  }

  datablocks  <- list()
  errorblocks <- list()

  # Error coding
  # Find the error codewords for this data
  bytedata <- Reforge(data)
  
  errorCodeWordsPerBlock <- kErrorCorrection[qrversion, "PB"]
  dataCodeWordsPerBlock  <- kErrorCorrection[qrversion, "G1B"]

  for(gblock in 1:kErrorCorrection[qrversion, "G1"]) {
    datablocks[[gblock]]  <- bytedata[((gblock-1) * dataCodeWordsPerBlock + 1) : (gblock * dataCodeWordsPerBlock)]
    errorblocks[[gblock]] <- GFErrorTerms(datablocks[[gblock]], Poly(errorCodeWordsPerBlock))
  }

  # Interleave datablocks
  if (length(datablocks) == 1) {
    data  <- datablocks[[1]]
    error <- errorblocks[[1]]
  } else if (length(datablocks) == 2) {
    data  <-c(rbind(datablocks[[1]], datablocks[[2]]))
    error <-c(rbind(errorblocks[[1]], errorblocks[[2]]))    
  } else {
    # 4 groups in corner case 4-H
    data  <- c(rbind(datablocks[[1]],datablocks[[2]],datablocks[[3]],datablocks[[4]]))
    error <- c(rbind(errorblocks[[1]],errorblocks[[2]],errorblocks[[3]],errorblocks[[4]]))
  }

  rawdata <- append(data, error)

  # Convert data into bit array.
  data <- c(sapply(rawdata, NumToPad8Bits))

  # Add remainder bits.
  if (qrversion > "2") {
    data <- append(data, c(0,0,0,0,0,0,0))
  }

  list(data = data, rawdata = rawdata, datablocks = datablocks, errordata = errorblocks)
}

# Module construction cell values
kReservedBlank <- -2
kReservedSet   <- -1
kDefault       <-  0
kBlank         <-  2
kBlankSet      <-  1

# Adds a finder cell pattern to the supplied matrix with top-left coords x,y
#
# 1111111
# 1000001
# 1011101
# 1011101
# 1011101
# 1000001
# 1111111
#
AddFinder <- function(pts,x,y) {
  for(i in 1:7) {
    for (j in 1:7) {
      pts[x+i, y+j] <- kReservedBlank
    }
  }

  for(i in 1:7) {
    pts[x+1, y+i] <- kReservedSet
    pts[x+7, y+i] <- kReservedSet
    
    pts[x+i, y+1] <- kReservedSet
    pts[x+i, y+7] <- kReservedSet

  }

  for(i in 1:3) {
    for (j in 1:3) {
        pts[x+i+2, y+j+2] <- kReservedSet
    }
  }
  return(pts)
}

# Adds an alignment cell pattern to the supplied matrix with top-left coords x,y
#
# 11111
# 10001
# 10101
# 10001
# 11111
# 
AddAlignment <- function(pts,x,y) {
  for(i in 1:5) {
    for (j in 1:5) {
      pts[x+i, y+j] <- kReservedBlank
    }
  }

  for(i in 1:5) {
    pts[x+1, y+i] <- kReservedSet
    pts[x+5, y+i] <- kReservedSet
    
    pts[x+i, y+1] <- kReservedSet
    pts[x+i, y+5] <- kReservedSet
  }
  pts[x+3, y+3] <- kReservedSet

  return(pts)
}

# Adds the blank boundary around a finder location
#
#
AddSeparator <- function(pts,x,y) {
  w <- nrow(pts)
  for(i in 0:8) {
      if (y > 0 && x+i > 0 && x+i <=w) {
        # Line above finder
        pts[x+i,y] <- kReservedBlank
      }
      if (y+8 <=w && x+i>0 && x+i <=w) {
        # Line below finder
        pts[x+i,y+8] <- kReservedBlank
      }
      if (x > 0 && y+i >0 && y+i <=w) {
        # Line left of finder
        pts[x,y+i] <- kReservedBlank
      }
      if (x+8 <=w && y+i > 0 && y+i <=w) {
        # Line right of finder
        pts[x+8,y+i] <- kReservedBlank
      }
  }

  return(pts)
}

# Reserve format information area
#
#
AddVersion <- function(pts,x,y) {
  w <- ncol(pts)
  for(i in 0:9) {
    # Bottom
    if (y+9 <=w && x+i > 0 && x+i <=w) {
      pts[x+i,y+9] <- (if(pts[x+i,y+9] == kDefault) kReservedBlank else pts[x+i,y+9])
    }
    
    # Right
    if (y+i <=w && x+9 <= w && y+i > 0) {
      pts[x+9,y+i] <- (if(pts[x+9,y+i] == kDefault) kReservedBlank else pts[x+9,y+i])
    }
  }

  return(pts)
}

# Add timing pattern which is a 1010101 pattern in the 7th row and column.
#
#
AddTimings <- function(pts) {
  for(i in 1:ncol(pts)) {
    if(pts[7,i] == kDefault) { if(i %% 2 == 1) { pts[7,i] <- kReservedSet } else {pts[7,i] <- kReservedBlank}}
    if(pts[i,7] == kDefault) { if(i %% 2 == 1) { pts[i,7] <- kReservedSet } else {pts[i,7] <- kReservedBlank}}
  }

  return(pts)
}

# Add the data to the data modules
# This is done in a snaking up-down-up traversal ignoring reserved cells.
# There is also a special case when we meet the timing pattern!
#
AddData <- function(pts,data) {
  x    <- ncol(pts)
  y    <- ncol(pts)
  dx   <- -1
  dy   <- 0
  d    <- "up"

  maxY <- y+1

  for(bit in data) {
    # Move to next data module.
    while(pts[x,y] != kDefault) {
      message("Checking ",x," , ",y," = ", pts[x,y])
      y <- y + dy
      x <- x + dx
      if (y == 0) {
        # Change direction
        message("Going down")
        x  <- x - 2
        y  <- 1
        dx <- -1
        dy <- 0
        d  <- "down"
        # Special case for vertical timing line
        if (x == 7) { x = 6 }
      } else if (y == maxY) {
        # Change direction
        message("Going up")
        x  <- x - 2
        y  <- maxY - 1
        dx <- -1
        dy <- 0
        d  <- "up"
      }
      else {
        # Switch x direction
        dx <- -dx
        if (dx == 1 ) {
           dy <- if(d == "up") -1 else 1
        } else dy <- 0
      }
    }
    pts[x,y] <- if(bit == 1) kBlankSet else kBlank
  }

  return(pts)
}

ApplyMask<-function(pts,m) {

    for( i in 1:nrow(pts)) {
      for (j in 1:ncol(pts)) {
        # Axes used when masking are reversed
        row <- j - 1
        column <- i - 1

        if (pts[i,j] >= kDefault) {
          # Mask 0
          if (m == 0) {
             if ((row + column) %% 2 == 0) {
               pts[i,j] <- if(pts[i,j] == kBlankSet) kBlank else kBlankSet
             }
          } else if (m == 1) {
            if (row %% 2 == 0) {
              pts[i,j] <- if(pts[i,j] == kBlankSet) kBlank else kBlankSet
            }
          } else if (m == 2) {
            if (column %% 3 == 0) {
              pts[i,j] <- if(pts[i,j] == kBlankSet) kBlank else kBlankSet
            }
          } else if (m == 3) {
            if ((row + column) %% 3 == 0) {
              pts[i,j] <- if(pts[i,j] == kBlankSet) kBlank else kBlankSet
            }
          } else if (m == 4) {
            if ((floor(row/2) + floor(column/3)) %% 2 == 0) {
              pts[i,j] <- if(pts[i,j] == kBlankSet) kBlank else kBlankSet
            }
          } else if (m == 5) {
            if (((row * column) %% 2) + ((row * column) %% 3) == 0) {
              pts[i,j] <- if(pts[i,j] == kBlankSet) kBlank else kBlankSet
            }
          } else if (m == 6) {
            if ((((row * column) %% 2) + ((row * column) %% 3)) %% 2 == 0) {
              pts[i,j] <- if(pts[i,j] == kBlankSet) kBlank else kBlankSet
            }
          } else {
            if ((((row + column) %% 2) + ((row * column) %% 3)) %% 2 == 0) {
              pts[i,j] <- if(pts[i,j] == kBlankSet) kBlank else kBlankSet
            }
          }
        }
      }
    }  

  return(pts)
}

# Calculate penalty score for image
ScoreMask <- function(pts) {

  pts0<-(ifelse(abs(pts) == 1,1,0))
  
  p01<-PenaltyCount1(pts0)
  p02<-PenaltyCount2(pts0)
  p03<-PenaltyCount3(pts0)
  p04<-PenaltyCount4(pts0)
  message("Score: ",p01, "+", p02, "+", p03, "+",p04,"=",p01+p02+p03+p04)

  return (p01+p02+p03+p04)
}

# Set the version information
#
#
ApplyVersion<-function(pts,v) {

  v <- rev(v)
  w <- nrow(pts)

  pts[9,1] <- if(v[01] == 1) kReservedSet else kReservedBlank
  pts[9,2] <- if(v[02] == 1) kReservedSet else kReservedBlank
  pts[9,3] <- if(v[03] == 1) kReservedSet else kReservedBlank
  pts[9,4] <- if(v[04] == 1) kReservedSet else kReservedBlank
  pts[9,5] <- if(v[05] == 1) kReservedSet else kReservedBlank
  pts[9,6] <- if(v[06] == 1) kReservedSet else kReservedBlank
  pts[9,8] <- if(v[07] == 1) kReservedSet else kReservedBlank
  pts[9,9] <- if(v[08] == 1) kReservedSet else kReservedBlank
  pts[8,9] <- if(v[09] == 1) kReservedSet else kReservedBlank
  pts[6,9] <- if(v[10] == 1) kReservedSet else kReservedBlank
  pts[5,9] <- if(v[11] == 1) kReservedSet else kReservedBlank
  pts[4,9] <- if(v[12] == 1) kReservedSet else kReservedBlank
  pts[3,9] <- if(v[13] == 1) kReservedSet else kReservedBlank
  pts[2,9] <- if(v[14] == 1) kReservedSet else kReservedBlank
  pts[1,9] <- if(v[15] == 1) kReservedSet else kReservedBlank
  
  pts[w,9] <- pts[9,1]
  pts[w-1,9] <- pts[9,2]
  pts[w-2,9] <- pts[9,3]
  pts[w-3,9] <- pts[9,4]
  pts[w-4,9] <- pts[9,5]
  pts[w-5,9] <- pts[9,6]
  pts[w-6,9] <- pts[9,8]
  
  pts[9,w-6] <- pts[8,9]
  pts[9,w-5] <- pts[6,9]
  pts[9,w-4] <- pts[5,9]
  pts[9,w-3] <- pts[4,9]
  pts[9,w-2] <- pts[3,9]
  pts[9,w-1] <- pts[2,9]
  pts[9,w] <- pts[1,9]
  
  return (pts)
}

# Encode the QR data into an image prior to masking
#
#
QRData<-function(text,mode,qrversion) {
  # 4-L codes are 33x33 modules. We will represent dark square locations as a list of pairs.
  # 1-Q codes are 21*21

  version <- as.integer(substr(qrversion,1,1))
  vertype <- substr(qrversion,3,3)
  s <- (version-1)*4 + 21

  message("MatrixSize=",s)
  pts <- matrix(replicate(s*s,kDefault),nrow=s,ncol=s)

  # First add the finder patterns
  # Top left
  pts <- AddFinder(pts,0,0)
  # Top right
  pts <- AddFinder(pts,s-7,0)
  # Bottom left
  pts <- AddFinder(pts,0,s-7)
  
  # Add separators
  pts <- AddSeparator(pts,0,0)
  pts <- AddSeparator(pts,s-7,0)
  pts <- AddSeparator(pts,0,s-7)

  # Add additional alignment patterns.
  if (version > 1) pts <- AddAlignment(pts,12+(version-1)*4,12+(version-1)*4)

  # Add timing patterns
  pts <- AddTimings(pts)

  # Add dark module
  pts[9,s-7] <- kBlankSet

  # Reserve format information area
  pts <- AddVersion(pts,0,0)
  pts <- AddVersion(pts,s-7,0)
  pts <- AddVersion(pts,0,s-7)
  pts <- AddData(pts,QRCode(text,mode=mode,qrversion=qrversion)$data)

  return(pts)
}


# Generate a QRCode bitmap
# This is the main entry point for the system
#
QRPlot<-function(text="HELLO WORLD",mode="text",qrversion="1-Q",mask=-1,file=FALSE,filename="./qr.png") {
  # 4-L codes are 33x33 modules. We will represent dark square locations as a list of pairs.
  # 1-Q codes are 21*21

  version <- as.integer(substr(qrversion,1,1))
  vertype <- substr(qrversion,3,3)
  pts <- QRData(text,mode,qrversion)
  
  # Apply a mask
  if (mask == -1) {
    # Find the mask with the lowest penalty
    minScore <- 0
    minPts <- pts

    for(m in 0:7) {
      message("Scoring mask ",m)
      ptsTmp <- ApplyMask(pts,m)
      ptsTmp <- ApplyVersion(ptsTmp,tail(NumToBits(strtoi(KFormatStrings[paste(vertype,"-",m,sep=""),],base=2)),15))
      tmpScore <- ScoreMask(ptsTmp)
      if (minScore==0 || tmpScore < minScore) {
        minPts <- ptsTmp
        minScore <- tmpScore
      }
    }
    pts <- minPts
  } else {
    pts <- ApplyMask(pts,mask)
    pts <- ApplyVersion(pts,tail(NumToBits(strtoi(KFormatStrings[paste(vertype,"-",mask,sep=""),],base=2)),15))
  }
  
  if (file) png(filename=filename,width=264,height=264)
  par(pty="s",mar=c(0,0,0,0))
  image(pts,col=c("white","black","white","black","white"),breaks=c(-3,-2,-1,0,1,2),ylim=c(1.05,-0.05),xlim=c(-0.05,1.05),axes=FALSE)
  if (file) dev.off()
}
