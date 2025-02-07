
# R script for Theil-Sen Confidence band 
# Compute bootstrapped confidence band around Theil-Sen trend line 
# user inputs: list of x-values, list of y-values, desired confidence level 
# Note: replace numbers in parentheses below with specific x and y values  
#          corresponding to data-specific ordered pairs 
# x-values should be numeric values representing sampling dates or events 
# y-values should be concentration values corresponding to these dates or events 
# Script produces a plot of the Theil-Sen trend line, the confidence band around the trend, 
# and an overlay of the actual data values 



x= c(89.6,90.1,90.8,91.1,92.1,93.1,94.1,95.6,96.1,96.3) 
y= c(56,53,51,55,52,60,62,59,61,63) 
conf = .95 



elimna= function(m){ 
  # 
  # remove any rows of data having missing values 
  m= as.matrix(m) 
ikeep= c(1:nrow(m)) 
for(i in 1:nrow(m)) if (sum(is.na(m[i,])>=1)) ikeep[i]= 0 
elimna= m[ikeep[ikeep>=1],] 
elimna 
} 



theilsen2= function(x,y){ 
  # 
  # Compute the Theil-Sen regression estimator 
  # Do not compute residuals in this version 
  # Assumes missing pairs already removed 
  # 
  ord= order(x) 
  xs= x[ord] 
  ys= y[ord] 
  vec1= outer(ys,ys,"-") 
  vec2= outer(xs,xs,"-") 
  v1= vec1[vec2>0] 
  v2= vec2[vec2>0] 
  slope= median(v1/v2) 
  coef= 0 
  coef[1]= median(y)-slope*median(x) 
  coef[2]= slope 
  list(coef=coef) 
} 



nb= 1000 
temp= matrix(c(x,y),ncol=2) 
temp= elimna(temp) #remove any pairs with missing values 
x= temp[,1] 
y= temp[,2] 
n= length(x) 
ord= order(x) 
cut= min(x) + (0:100)*(max(x)-min(x))/100 #compute 101 cut pts 
t0= theilsen2(x,y) #compute trend line on original data 
tmp= matrix(nrow=nb,ncol=101) 



for (i in 1:nb) { 
  idx= sample(ord,n,rep=T) 
  xboot= x[idx] 
  yboot= y[idx] 
  tboot= theilsen2(xboot,yboot) 
  tmp[i,]= tboot$coef[1] + cut*tboot$coef[2] 
} 



lb= 0; ub= 0 
for (i in 1:101){ 
  lb[i]= quantile(tmp[,i],c((1-conf)/2)) 
  ub[i]= quantile(tmp[,i],c((1+conf)/2)) 
} 
tband= list(xcut=cut,lo=lb,hi=ub,ths0=t0) 
yt= tband$ths0$coef[1] + tband$ths0$coef[2]*tband$xcut 
plot(yt~tband$xcut,type='l',xlim=range(x),ylim=c(min(tband$lo),max(tband$hi)),xlab='Date',ylab='Conc') 
points(x,y,pch=16) 
lines(tband$hi~tband$xcut,type='l',lty=2) 
lines(tband$lo~tband$xcut,type='l',lty=2)  