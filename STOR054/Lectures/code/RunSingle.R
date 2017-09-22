n=100000
p=.5


set.seed(20110)
u=runif(n)
x=(u>p)

count=0
maxcount=0
for (i in 1:n){
  count=count+1
  count=count*x[i]
  maxcount=max(count,maxcount)
}
print(maxcount)

print(log(n,2)+0.577/log(2)-1.5)
