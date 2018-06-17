library("magic")
matrix_4 <- matrix(1:16, nrow = 4, byrow = TRUE)
matrix_5 <- matrix(integer(25) , nrow = 5, byrow = TRUE)
matrix_6 <- matrix(integer(36), nrow = 6, byrow = TRUE)

matrix_4 <- make_magic_matrix_doubly_even(matrix_4, 4)
matrix_5 <- make_magic_matrix_odd(matrix_5, 5)
matrix_6 <- magic(6)
print("n = 4")
matrix_4
print("n = 5")
matrix_5
print("n = 6")
matrix_6

make_magic_matrix_doubly_even <- function(arr, n){

  
    for (i in 1:(n/4)){
      for ( j in 1:(n/4)){
       
        t <- arr[i , j]
        arr[i,j] = (n*n + 1) - t
   
      }
    }
      
      
      for (i in 1 : (n%/%4)){
        for ( j in ((3 * (n%/%4)) + 1) : n){
          arr[i, j] = (n*n + 1) - arr[i, j]
        }
      }
          
        
        for ( i in ((3 * n/4) + 1) : n){
          for ( j in 1 : (n/4)){
            arr[i, j] = (n*n+1) - arr[i, j]
          }
        }

          for ( i in (3 * n%/%4 + 1) : n)
            for ( j in (3 * n%/%4 + 1) : n)
              arr[i, j] = (n*n + 1) - arr[i, j]
            
            for ( i in (n%/%4 + 1) : (3 * n%/%4))
              for ( j in (n%/%4 + 1) : (3 * n%/%4))
                arr[i, j] = (n*n + 1) - arr[i, j];
              
              return(arr)
        }
      
  

make_magic_matrix_odd <- function(a , n) {

  i = ceiling(n / 2)
  j = n 
  total <- n * n
  num <- 1
  
  while (num <= total)
  {
    
   
    if (i == 0  & j == n + 1) #3rd condition
    {
      
      j = n - 1
      i =  1 
    }
    else
    {
      
      if (j == n + 1)
      j = 1
      
      if (i < 1)
        i = n 
    }
    print(a[i, j])
    if (a[i, j] != 0) #2nd
    {
      print(num)
      j <- j - 2
      i <- i + 1
      
      next()
    
        
    }
    else{
      a[i, j] = num
      num <- num + 1
    
      
    }
    
    j <- j + 1
    i <- i - 1
    
    
  }
  return(a)

}
