PenaltyCountSlice1 <- function(v) {
  # For consecutive bits of the same value of length >=5, that score is the number-2
  # We break the vector in a sequence of consecutive counts, then apply score
  # e.g 1,1,1,1,1,0,0,0,1,0,0,1,1,0,0 => 5,3,1,2,2,2
  #     5,3,1,2,2,2 => 3,0,0,0,0,0
  # So total score is 3
  
  # Initial conditions
  counts        <- vector(mode = "integer", length = length(v))
  current.value <- v[1]
  repeat.count  <- 0
  count.pos     <- 1
  vector.pos    <- 1

  while(vector.pos <= length(v)) {
    if (v[vector.pos] == current.value) {
      repeat.count <- repeat.count + 1
    } else {
      counts[count.pos] <- repeat.count
      count.pos <- count.pos+1
      repeat.count <- 1
      current.value <- v[vector.pos]
    }
    vector.pos <- vector.pos + 1
  }

  counts[count.pos] <- repeat.count

  return(sum(ifelse(counts >=5, counts - 2, 0)))
}

PenaltyCount1 <- function(mtx) {
  # Sum the penalty count for each row/column of matrix

  sum <- 0
  for(i in 1:nrow(mtx)) {
      sum = sum + PenaltyCountSlice1(mtx[i, ])
  }
  for(j in 1:ncol(mtx)) {
      sum = sum + PenaltyCountSlice1(mtx[ ,j])
  }

  return(sum)
}

PenaltyCount2 <- function(mtx) {
  # Count number of 2x2 blocks of the same color

  sum <- 0
  for (i in 1:(nrow(mtx)-1)) {
    for (j in 1:(ncol(mtx)-1)) {
      # Test if the immediate 2x2 square is the same (either the sum is 0 or 4)
      s <- mtx[i,j] + mtx[i,j + 1] + mtx[i + 1,j] + mtx[i + 1,j + 1]
      if ((s == 0) || (s == 4)) sum <- sum + 3
    }
  }

  return(sum)
}

PenaltyCount3 <- function(mtx) {
  # Score is 40x number of instances of the pattern 1011101 with at least 4 zeros either side

  sum <- 0

  for (i in 1:nrow(mtx)) {
    # Convert row to a string and test number of substrings.
    strRow <- paste(mtx[i, ], collapse = "")
    sum <- sum + nchar(strRow) - nchar(gsub("10111010000", "----------", strRow))
    sum <- sum + nchar(strRow) - nchar(gsub("00001011101", "----------", strRow))
  }
  
  for (j in 1:ncol(mtx)) {
    # Convert row to a string and test number of substrings.
    strCol <- paste(mtx[ ,j], collapse = "")
    sum <-sum + nchar(strCol) - nchar(gsub("10111010000", "----------", strCol))
    sum <-sum + nchar(strCol) - nchar(gsub("00001011101", "----------", strCol))
  }

  return(sum * 40)
}

PenaltyCount4 <- function(mtx) {
  # Ratio of dark modules to light modules

  dark.count  <- sum(mtx)
  total.count <- ncol(mtx) * nrow(mtx)

  percent <- dark.count/total.count * 100

  # find previous multiple of 5

  bin.start <- percent - (percent %% 5)
  bin.end   <- bin.start + 5

  diff1 <- abs(bin.start - 50)
  diff2 <- abs(bin.end - 50)

  return(min(diff1, diff2) * 2)
}
