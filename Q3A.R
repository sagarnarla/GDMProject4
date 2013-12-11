library(igraph)

g=read.graph("C:\\Users\\Sagar\\Dropbox\\Fall13\\CSC591\\P4\\static.network",format=c("edgelist"),directed=FALSE)

gsize=vcount(g)
b=0.2
d=0.7

svar=c()

for(k in seq(3000,5500,100))
{
g=read.graph("C:\\Users\\Sagar\\Dropbox\\Fall13\\CSC591\\P4\\static.network",format=c("edgelist"),directed=FALSE)

remove_nodes=sample(gsize,k)
g=delete.vertices(g,remove_nodes)
m=get.adjacency(g)

func <- function(x, extra=NULL) { as.vector(m %*% x) };
eval = arpack(func, options=list(n=vcount(g), nev=3,ncv=8, which="LM", maxiter=200))$values;


l1=abs(eval[1])
s=l1*b/d;
svar=c(svar,s)
}

plot(seq(3000,5500,100),svar,type="l",xlab="K",ylab="Effective Strength(beta=0.2,delta=0.7)")
abline(h=1,col="red",lty=2)

########
#Simulation

g=read.graph("C:\\Users\\Sagar\\Dropbox\\Fall13\\CSC591\\P4\\static.network",format=c("edgelist"),directed=FALSE)

g=set.vertex.attribute(g,"infected",index=seq(1:gsize),FALSE)
g=set.vertex.attribute(g,"inf_neigh",index=seq(1:gsize),0)

i=sample(vcount(g),vcount(g)/10)

g=set.vertex.attribute(g,"infected",index=i,TRUE)

rm(frac_infected)
frac_infected=0.1


remove_nodes=sample(vcount(g),200)
g=delete.vertices(g,remove_nodes)



for (t in 1:50)
{
	infected=which(V(g)$infected==TRUE)
	suseptible=which(V(g)$infected==FALSE)
	
	# Update the count of infected neighbors for each node
	for(v in V(g))
	{
		inf_neigh=length(which(V(g)[neighbors(g,v)]$infected==TRUE))
		g=set.vertex.attribute(g,"inf_neigh",index=v,inf_neigh)
	}


	# Infect suseptible nodes
	for(s in suseptible)
	{
		p=1-(1-b)^V(g)[s]$inf_neigh
		#infect=sample(c(TRUE,FALSE),1,prob=c(p,1-p))
		if(runif(1,0,1) < p)
		{	infect=TRUE }
		else
		{	infect=FALSE }
		if(infect==TRUE)
		{
			V(g)[s]$infected = TRUE;
		}
	}

	# Heal infected nodes
	for(i in infected)
	{
		p=d
		#heal=sample(c(TRUE,FALSE),1,prob=c(p,1-p))
		if(runif(1,0,1) < p)
		{	heal=TRUE }
		else
		{	heal=FALSE }

		if(heal==TRUE)
		{
			V(g)[i]$infected = FALSE;
		}
	}

	# Calculate stats
	new_infected=length(which(V(g)$infected==TRUE))/vcount(g)
	new_infected
	frac_infected=c(frac_infected,new_infected)
	
}


plot(seq(1,100,2),frac_infected[2:51],col="red",type="l",xlab="Time",ylab="Fraction Infected(beta=0.2,delta=0.7)",ylim=c(0,1))

