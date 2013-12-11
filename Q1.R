library(igraph)

g=read.graph("C:\\Users\\Sagar\\Dropbox\\Fall13\\CSC591\\P4\\static.network",format=c("edgelist"),directed=FALSE)

m=get.adjacency(g,type=c("both"))

#e=eigen(m,only.values=TRUE)

#l1=abs(max(e$values))

##########
b1=0.2
d1=0.7
s=l1*b1/d1
cat(paste("Effective strength for beta=0.2 and delta=0.7: ", s))
##########

##########
b1var=seq(0,0.05,0.005)
#svar=c();
#for(b in b1var)
#{
#	svar=c(svar,l1*b/d1);
#}
svar=l1*b1var/d1;
b1thres=1*d1/l1;
plot(b1var,svar,xlab="Beta(Transmission probability)",ylab="Effective Strength(delta=0.7)",type="l",col="red")
abline(h=1,lty=2)
abline(v=b1thres,lty=2)
##########


##########
d1var=seq(0,1,0.005)
#svar=c();
#for(d in d1var)
#{
#	svar=c(svar,l1*b1/d);
#}
svar=l1*b1/d1var
d1thres=l1*b1
windows()
plot(d1var,svar,xlab="Delta(Healing probability)",ylab="Effective Strength(beta=0.2)",type="l",col="red")
abline(h=1,lty=2)
abline(v=d1thres,lty=2)
##########

# New values

##########
b2=0.01
d2=0.6
s=l1*b2/d2
cat(paste("Effective strength for beta=0.01 and delta=0.6: ", s))
##########

##########
b2var=seq(0,0.05,0.005)
#svar=c();
#for(b in b2var)
#{
#	svar=c(svar,l1*b/d2);
#}
svar=l1*b2var/d2
b2thres=1*d2/l1;
windows()
plot(b2var,svar,xlab="Beta(Transmission probability)",ylab="Effective Strength(delta=0.6)",type="l",col="red")
abline(h=1,lty=2)
abline(v=b2thres,lty=2)
##########


##########
d2var=seq(0.2,0.6,0.005)
#svar=c();
#for(d in d2var)
#{
#	svar=c(svar,l1*b2/d);
#}
svar=l1*b2/d2var
d2thres=l1*b2
windows()
plot(d2var,svar,xlab="Delta(Healing probability)",ylab="Effective Strength(beta=0.01)",type="l",col="red")
abline(h=1,lty=2)
abline(v=d2thres,lty=2)
##########
