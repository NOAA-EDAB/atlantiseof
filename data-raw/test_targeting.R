



target1 = Vectorize(function(m,n){
  a = c(m,rep((1-m)/(n-1), n-1))
  b = rep(1/n,n)
  
  diff = abs(a-b)
  
  c = sum(diff)/n
  
  return(c)
})
target2 = Vectorize(function(m,n){
  a = c(m,rep((1-m)/(n-1), n-1))
  b = rep(1/n,n)
  
  diff = abs(a-b)
  
  c = (n*sum(diff))/(2*(n-1))
  
  return(c)
})

target.chi = Vectorize(function(m,n){
  a = c(m,rep((1-m)/(n-1), n-1))
  b = rep(1/n,n)
  
  chi = sum((a-b)^2/b)
  
  return(chi)
})

target.shannon = Vectorize(function(m,n){
  a = c(m,rep((1-m)/(n-1), n-1))
  b = rep(1/n,n)
  
  shannon = sum(a * log(a/b))
  
  return(shannon)
})
target.gini = Vectorize(function(m,n){
  a = c(m,rep((1-m)/(n-1), n-1))
  gini = ineq::Gini(a)
  return(gini)
})

curve(target(x,n =5),0,1, xlab ="m", ylab= 'param')
curve(target2(x,n =5),0,1, xlab ="m", ylab= 'param')
curve(target.chi(x,n =5),0,1, xlab ="m", ylab= 'chi')
curve(target.shannon(x,n =5),0,1, xlab ="m", ylab= 'shannon')
curve(target.gini(x,n =5),0,1, xlab ="m", ylab= 'gini')

m =1
n =10

curve((2*(x-1))/(x^2),1,10)



