  n <- 2 ^ 16
  count <- 0
  a <- integer(16)
  ways <- integer(3)
  for(i in 0 : n) {
    check <- FALSE
    check2 = FALSE
    number <- i
    for(j in 1 : 16) {
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
    for(j in 2 : 15) {
      if(a[j] == 1){
        if(!(a[j - 1] == 0 & a[j + 1] == 0)){
          check2 = TRUE
          break()
        }
      }
      if(a[j] == 0) {
        if(a[j - 1] == 0 & a[j + 1] == 0){
          check2 = TRUE
          break()
        }
          
      }
    }

    if(check2 == FALSE){
      if(a[1] == 1){
        if(a[2] == 0 & a[16] == 0){
          check = TRUE
        }
      }
        else if(a[1] == 0) {
          if(!(a[2] == 0 & a[16] == 0)) {
            if(a[16] == 0) {
              if(a[15] == 1) {
                check = TRUE
              }
            }
            if(a[16] == 1 & a[15] == 0)
              check = TRUE
          }
        }
    }
    if(check == TRUE) {

      count <- 0
      for(j in 1 : 16){
        if(a[j] == 0) {
          count <- count + 1
        }
      }
      for(i in 1 : 3) {
        if(ways[i] == count){
          break()
        }
        if(ways[i] == 0){
          ways[i] <- count
          break()
        }
        
          
      }
      
      
    }
    
  }
  print(ways)
