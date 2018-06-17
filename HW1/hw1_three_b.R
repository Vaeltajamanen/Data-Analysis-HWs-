print(12) #all of them are liars
a[1] <- 1 #at least one of them is telling the truth
a[2] <- 1
counter <- 0
for(i in 2 : 12) {
  if(a[i] == 1) {
    if(a[i - 1] == 1) {
      a[i + 1] <- 0
      counter <- counter + 1
    }
    else{
      a[i + 1] <- 1
    }
  }
  else{
    if(a[i - 1] == 1) {
      a[i + 1] = 1
    }
    else{
      a[i + 1] <- 0
      counter <- counter + 1
    }
  }
}
print(counter)