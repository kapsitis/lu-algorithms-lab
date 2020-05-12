naiveSearch <- function(pattern, text) {
  patternChars <- as.vector(strsplit(pattern, '')[[1]])
  textChars <- as.vector(strsplit(text, '')[[1]])
  
  # notation from our slides; to make it shorter.
  n <- length(textChars)
  m <- length(patternChars)
  
  result <- numeric(0) # empty array of integers
  # i - shifts/offsets
  for (i in 1:(n - m +1)) {
    found <- 1
    # my location in the pattern 
    for (j in 1:m) {
      if (textChars[i+(j-1)] != patternChars[j]) {
        found <-0
        break
      }
    }
    if (found == 1) {
      # scala/c/python (except R) is 0-based arrays. 
      # concatenate at the end
      result <- c(result,i-1)
    }
  }
  return(result)
}

pattern1 <- "ababaca"
text1 <-    "ababacabababbaababacababab"
naiveSearch(pattern1, text1)


#Do the basic KMP
prefixFun <- function(pattern) {
  # -1, 0,0,1,2,3,0,1
  # ababaca
  
  ## Empty list ("0" -> -1; "1" -> 0; "2" -> 0; "3" -> 1)
  res <- list()
  
  # (1) m=P.length
  patternChars <- as.vector(strsplit(pattern, '')[[1]])
  m <- length(patternChars)
  
  # (2) Initialize the table π[0],π[1],…,π[m] to zeros
  for (i in 0:m) {
    res[paste0("",i)] <- 0
  }
  
  # (3) π[0]=-1; π[1]=0
  res["0"] <- -1

  # (4)	k=0
  k <- 0
  
  
  # (5) for q=2 to m
  for (q in 2:m) {
    
    # (6) while k>0 and P[k]≠P[q−1]
    print(sprintf("q = %d", q))
    
    while (k>0 && (patternChars[k+1] != patternChars[q])) {
      print(sprintf("(q,k) = (%d,%d)", q, k))
      
      # (7) k=π[k]
      k <- res[paste0("", k)]
    }
    
    # () - round parentheses; [] - square brackets; {} - curly braces?? 
    # (8) if P[k]==P[q−1]
    if (patternChars[k+1]==patternChars[q]) {
      
      print("AAA1")
      # (9) k=k+1
      k <- k+1
      print("AAA2")
    }
    
    # π[q]=k
    res[paste0("",q)] <- k
    
  } # (5) 
  return(res)
}


prefixFun("ababaca")


## students <- c("Annija", "Antons", "Arturs", "Boriss", "Bruno", "Edgars", "Elizabete", "Jekabs", "Jevgenija", "Karlis", "Klinta Madara", "Krisjanis", "Martins", "Niklavs", "Reinis", "Roberts", "Roze", "Vladislavs")
students <- c("Klinta Madara", "Reinis", "Roberts", "Roze")
