library(igraph)

g=read.graph("C:\\Users\\Sagar\\Dropbox\\Fall13\\CSC591\\P4\\static.network",format=c("edgelist"),directed=FALSE)

gsize=vcount(g)
b=0.2
d=0.7

inf_avg=matrix(rep(0,50),nrow=50,ncol=1)

for (iter in 1:1)
{

g=set.vertex.attribute(g,"infected",index=seq(1:gsize),FALSE)
g=set.vertex.attribute(g,"inf_neigh",index=seq(1:gsize),0)

i=sample(gsize,gsize/10)

g=set.vertex.attribute(g,"infected",index=i,TRUE)

rm(frac_infected)
frac_infected=0.1

for (t in 1:100)
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
	new_infected=length(which(V(g)$infected==TRUE))/gsize
	new_infected
	frac_infected=c(frac_infected,new_infected)
	
}

inf_avg=cbind(inf_avg,frac_infected)
	
	
}	

final_avg=rowMeans(inf_avg[,-1])

		

