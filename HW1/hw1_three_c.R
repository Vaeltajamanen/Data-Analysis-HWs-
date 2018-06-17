A <- 0
B <- 0
C <- 0
n <- 2 ^ 8
a <- integer(8)
t <- 0
ways <- integer(5)
counter <- 0 
check1 <- FALSE
check2 <- FALSE
condition <- FALSE
for(i in 0 : n) {
  A <- 0
  B <- 0
  C <- 0
  counter <- 0
  check1 <- FALSE
  check2 <- FALSE
  number <- i
  for(j in 1 : 8) {
    if(number %% 2 == 0) {
      a[j] <- 0
      number <- number / 2
    }
    else{
      a[j] <- 1
      number <- number - 1
      number <- number / 2
    }
  }
  for(j in 2 : 8){
    if(j + 1 != 9) {
      t <- j + 1
    }
    else {
      t <- 1
    }
    con <- (a[j] == 1)
    if(con) {
      condition <-(a[t] + a[j - 1] == 1)
      if(condition) {
        A <- A + 1
      }
      else if(a[j - 1] == 0 & a[t] == 0) {
        B <- B + 1
      }
      else{
        
        break()
      }
    }
    if(a[j] == 0) {
      condtion <- (a[j - 1] + a[t] == 1)
      #A 
      if(condition) {
        C <- C + 1
      }
      #B
      else if((a[j - 1] == 0 & a[t] == 0)) {
        A <- A + 1
      }
      else if(a[j - 1] + a[t] == 1) {
        B <- B + 1
      }
      else {
        break()
      }
    }
    check1 <- TRUE
    if(check1) {
      if(a[1] == 1) {
        #A 
        if((a[2] == 1 & a[8] == 0) | (a[2] == 0 & a[8] == 1)) {
          check2 <- TRUE
          A <- A + 1
        }
        else if(a[2] == 0 & a[8] == 0) {
          check2 <- TRUE
          B <- B + 1
        }
      }
      else {
        #A
        if(a[2] + a[8] == 2) {
          check2 <- TRUE
          C <- C + 1
        }
        else if((a[2] == 0 & a[8] == 0)) {
          check2 <- TRUE
          A <- A + 1
        }
        else if(a[2] + a[8] == 1) {
          check2 <- TRUE
          B <- B + 1
        }
      }
      if(A == 4 & B == 4 & check2 == TRUE) {
        for(j in 1 : 8) {
          if(a[j] == 0)
            counter <- counter + 1
        }
      }
      else if(check2 == TRUE & A < 4 & B < 4 & C > 0) {
        for(k in 0 : C) {
          if(A + k == 4 & B + C - k == 4) {
            for(j in 1 : 8) {
              if(a[j] == 0)
                counter <- counter + 1
            }
            for(i in 1 : 5) {
              if(ways[i] == counter){
                break()
              }
              if(ways[i] == 0){
                ways[i] <- counter
                break()
              }
              
              
            }
          }
        }
      }
    }
  }
  
  
}
print(ways)