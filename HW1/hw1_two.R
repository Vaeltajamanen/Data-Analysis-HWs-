n <- 13
for (i in 1:n){ #a13
  for (j in 1:n){ #a12
    if(i != j) {
      for(k in 1 : n) { #a11
        if(k != i & k != j) {
          for(t in 1 : n) {  #a10
            if(t != k & t != i & t != j){
              if((k + j) / (j + i) == (j + i) /(i + t) & (k + j) / (j + i) < 1)  {
                for(l in 1 : n) { #a9
                  if(l != t & l != k & l != j & l != i ) {
                    for(m in 1 : n) { #a8
                      
                     if(m != l & m != t & m != k & m != j & m != i) {
                        for(o in 1 : n) { #a7
                          if(o != m & o != l & o != t & o != k & o != j & o != i) {
                            for(p in 1 : n) #a6 {
                              if(p != o & p != m & p != l & p != t & p != k & p != j & p != i) {
                                for(q in 1 : n) #a5
                                  if(q != i & q != j & q != k & q != t & q != l & q != m & q != o & q != p) {
                                    if(((p / (p + o)) == ((p + o) / (o + m + l))) & (p / (p + o)) == ((o + m + l) / (q + l + t))){
                                      if(p / (p + o) < 1) {
                                        for(r in 1 : n) #a4{
                                          if(r != i & r != j & r != k & r != t & r != l & r != m & r != o & r != p & r != q) {
                                            for(s in 1 : n) #a3 {
                                              if(s != i & s != j &s != k & s != t & s != l & s != m & s != o & s != p & s != q & s != r) {
                                                for(u in 1 : n) { #a2
                                                  if(u != i & u != j &u != k & u != t & u != l & u != m & u != o & u != p & u != q & u != r & u != s) {
                                                    for(w in 1 : n) { #a1
                                                      if(w != i & w != j & w != k & w != t & w != l & w != m & w != o & w != p & w != q & w != r & w != s & w != u) {
                                                        if(w / (u + s) == (u + s) / (s + r + q) & (w / (u + s))< 1) { 
                                                          x <- c(w, u, s, r, q,  p, o, m, l, t , k , j , i)
                                                          print(x)
                                                          
                                                        }
                                                      } 
                                                    }
                                                  } 
                                                }
                                              }
                                          }
                                       }
                                    
                                    }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              
            }                
          }
        }
      }
      
    }
  }
}

