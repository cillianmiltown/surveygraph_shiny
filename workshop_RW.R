plot(1:100,1:100)
#Run this code chunk by clicking the "play" icon to the left. 
#It will plot the sequence of integers from 1 to 100 on the X access against the same sequence on the Y axis. 


my_data <- "My preciousss."

my_data == "Boo!"

my_data

print("This stuff in the post box")

#The following calculates the mean of the sequence of numbers from 0 to 10.
#Since we don't specify what to do with the object returned by the function, it just dumps it on the screen like a passive-aggressive teenager.

mean(0:10)

stored_mean <- mean(0:10)

stored_mean

print(stored_mean)




#First install pacman(), a package to help with the installation of the other packages.

#install.packages("pacman")
library(pacman)

#Load igraph for network visualization and analysis: 
p_load("igraph")

#Load devtools to allow direct installation of survey graph from Github
p_load(devtools)

#And use the devtools function "install_github" to download and compile the survegraphr package from github:
devtools::install_github("surveygraph/surveygraphr")

#Load the compiled surveygraph
library("surveygraph")


a_new_variable <- "A character string"
ls()

str(a_new_variable)

c(1,2,3,4)

c("a","b","c","d")

c("a","b","c","d")[3]


# Load sample of ESS Round 10 participants: 
#   ESS_500: random sub-sample of 500 participants prepared for this workshop)

ESS_500 <- read.csv("https://www.dropbox.com/s/kotimfih8v0ys2t/ESS10_500.csv?dl=1")

names(ESS_500)
#Note: there are a lot!

nrow(ESS_500)

str(ESS_500)

ESS_500[1,]

ESS_500[c(1,3),1]

table( ESS_500$cntry )

#Here we sent the "cntry" column to the function table(), which tabulates N per response option

which(ESS_500$cntry=="GB")

ESS_GB <- ESS_500[which(ESS_500$cntry=="GB"),]
str(ESS_GB)

# subsetting the 'trust' variables (for a manageable dataframe)
ESS_GB <- ESS_GB[,c("trstprl",
                    "trstlgl",
                    "trstplc",
                    "trstplt",
                    "trstprt",
                    "trstep",
                    "trstun",
                    "trstsci",
                    "lrscale")]


str(ESS_GB )

nrow(ESS_GB)             ##count rows all cases
ESS_GB<-(na.omit(ESS_GB))    ##omitting rows with NA values; save to original object
nrow(ESS_GB)             ##count rows to see if any dropped

nrow(na.omit(ESS_500))

# summary and descriptive statistics
# e.g. summary stats for trstep variable

min(ESS_GB$trstep)
max(ESS_GB$trstep)
mean(ESS_GB$trstep)
sd(ESS_GB$trstep)
median(ESS_GB$trstep)

summary(ESS_GB)

plot(c(1, 5, 11, 8, 1), c(2, 4, 10, 9, 1))

p_load("igraph")  #Just in case not run above, or no longer in memory.
g1 <- graph(edges=c(1,2, 2,3, 3,1, 4,2), directed=FALSE )
plot(g1)

#Make a fully connected graph with 100 nodes
g_fullyconnected<- make_full_graph(100)

#calculate edge density
edge_density(g_fullyconnected)

#igraph can build a number of famous/often used networks with the make_graph() function 
g_zach<-make_graph("Zachary")

#calculate edge density
edge_density(g_zach)

V(g1)
E(g1)

V(g1)$color <- "green"  
plot(g1)

V(g1)$color

V(g1)$color <- ifelse(V(g1)==c(1,2), "lightblue", "pink")
plot(g1)


E(g1)$color <- "orange"
plot(g1)
E(g1)$color

V(g1)$shape <- c("circle", "square", "sphere", "sphere") 
V(g1)$size<- c(6,12,24,48)
plot(g1)

E(g1)$width<-c(1,2,4,8)
E(g1)$lty<-c(1,2,3,4)
E(g1)$label<-c("One","Two", "Three", "Four")
plot(g1)

g_zachary<-make_graph("Zachary")
plot(g_zachary, vertex.size=10, vertex.label=NA)

testdata_unpolarised <- make_synthetic_data(nrow=150, ncol=1, polarisation=0, minority =0.5)
str(testdata_unpolarised)

table(testdata_unpolarised$group)
barplot(table(testdata_unpolarised$item_1))

testdata_unpolarised <- make_synthetic_data(nrow=150, ncol=10, polarisation=0, minority =0.5)
str(testdata_unpolarised)

testdata_unpolarised[,1]

str(testdata_unpolarised[,-1])




#simulate data:
testdata_unpolarised <- make_synthetic_data(nrow=150, ncol=10, polarisation=0, minority =0.5)
str(testdata_unpolarised)

#make the projection
unpolarised_edgelist_agent <- make_projection(
  testdata_unpolarised[,],
  layer = "agent", 
  threshold_method = "raw_similarity", 
  method_value= -1, 
  centre = FALSE)

#Look at the structure of the object returned by the make_projectiuon() function:
str(unpolarised_edgelist_agent)

#define the graph
unpolarised_agent_graph <- graph.data.frame(unpolarised_edgelist_agent, directed=FALSE)

#and plot it
plot(unpolarised_agent_graph)

plot(
  unpolarised_agent_graph, 
  vertex.size=2, 
  vertex.label=NA,
  edge.width=E(unpolarised_agent_graph)$weight^2/50,
  layout=layout.fruchterman.reingold,
  main="Agent projections"
)


unpolarised_edgelist_symbolic <- make_projection(testdata_unpolarised[,-1],layer = "symbolic", threshold_method = "raw_similarity", method_value= -1, centre = TRUE)

#define the graph
unpolarised_symbolic_graph <- graph.data.frame(unpolarised_edgelist_symbolic, directed=FALSE)

#and plot it
plot(
  unpolarised_symbolic_graph,
  vertex.size=4,
  vertex.label=NA,
  edge.width = (E(unpolarised_symbolic_graph)$weight+.5)^20, 
  layout=layout.fruchterman.reingold, 
  main="Symbolic projection"
)

#set.seed(123) #Delete or comment this out to go back to fully random simulation

#Simulate a survey with substantial intentional polarisation:
testdata_polarised <- make_synthetic_data(nrow=150, ncol=10, polarisation=1.5, minority =0.5)

#Generate the agent projection of the survey: 
polarised_edgelist_agent <- make_projection(testdata_polarised[,-1],layer = "agent", threshold_method = "raw_similarity", method_value= -1, centre = FALSE)

#Define the graph:
polarised_agent_graph <- graph.data.frame(polarised_edgelist_agent , directed=FALSE)

#plot it
plot(
  polarised_agent_graph, 
  vertex.size=2, 
  vertex.label=NA, 
  edge.width=E(polarised_agent_graph)$weight/20, 
  layout=layout.fruchterman.reingold, 
  main="Agent Projection"
)
#report the number of nodes left in the network
paste("Nodes in graph:", length(V(polarised_agent_graph)), sep=" ")

#report the edge density in the network; fully connected graph has density of 100% 
paste("Edge density:", edge_density(polarised_agent_graph, loops = FALSE), sep=" ")

##

#Generate the symbolic projection of the survey: 
polarised_edgelist_symbolic <- make_projection(testdata_unpolarised[,-1],layer = "symbolic", threshold_method = "raw_similarity", method_value= -1, centre = TRUE)

#Define the graph:
polarised_symbolic_graph <- graph.data.frame(polarised_edgelist_symbolic, directed=FALSE)

#plot it
plot(
  polarised_symbolic_graph, 
  vertex.size=2, 
  vertex.label=NA, 
  edge.width=(E(polarised_symbolic_graph)$weight+.5)^10, 
  layout=layout.fruchterman.reingold, 
  main="Symbolic projection")



#Generate the agent projection of the survey: 
polarised_edgelist_agent <- make_projection(testdata_polarised[,-1],layer = "agent", threshold_method = "target_lcc", method_value = 1, centre = FALSE)

#Define the graph:
polarised_agent_graph <- graph.data.frame(polarised_edgelist_agent, directed=FALSE)

#plot it
plot(polarised_agent_graph, vertex.size=2, vertex.label=NA, edge.width=0.2, layout=layout.fruchterman.reingold, main="Agent Projection")

#report the number of nodes left in the network
paste("Nodes in graph:", length(V(polarised_agent_graph)), sep=" ")

#report the edge density in the network; fully connected graph has density of 100% 
paste("Edge density:", edge_density(polarised_agent_graph, loops = FALSE), sep=" ")




#### Thresholding ####

#### llc ####

#Generate the agent projection of the survey: 
polarised_edgelist_agent <- make_projection(testdata_polarised[,-1],layer = "agent", threshold_method = "target_lcc", method_value= .95, centre = FALSE)

#Define the graph:
polarised_agent_graph <- graph.data.frame(polarised_edgelist_agent, directed=FALSE)

#plot it
plot(polarised_agent_graph, vertex.size=2, vertex.label=NA, edge.width=0.2, layout=layout.fruchterman.reingold, main="Agent Projection: Target LCC 95%")

#report the number of nodes left in the network
paste("Nodes in graph:", length(V(polarised_agent_graph)), sep=" ")

#report the edge density in the network; fully connected graph has density of 100% 
paste("Edge density:", edge_density(polarised_agent_graph, loops = FALSE), sep=" ")



#Generate the agent projection of the survey: 
polarised_edgelist_agent <- make_projection(testdata_polarised[,-1],layer = "agent", threshold_method = "target_lcc", method_value= .90, centre = FALSE)

#Define the graph:
polarised_agent_graph <- graph.data.frame(polarised_edgelist_agent , directed=FALSE)

#plot it
plot(polarised_agent_graph, vertex.size=2, vertex.label=NA, edge.width=0.2, layout=layout.fruchterman.reingold, main="Agent Projection: Target LCC 95%")

#report the number of nodes left in the network
paste("Nodes in graph:", length(V(polarised_agent_graph)), sep=" ")

#report the edge density in the network; fully connected graph has density of 100% 
paste("Edge density:", edge_density(polarised_agent_graph, loops = FALSE), sep=" ")




#Generate the agent projection of the survey: 
polarised_edgelist_agent <- make_projection(testdata_polarised[,-1],layer = "agent", threshold_method = "target_lcc", method_value= .85, centre = FALSE)

#Define the graph:
polarised_agent_graph <- graph.data.frame(polarised_edgelist_agent, directed=FALSE)

#plot it
plot(polarised_agent_graph, vertex.size=2, vertex.label=NA, edge.width=0.2, layout=layout.fruchterman.reingold, main="Agent Projection: Target LCC 85%")

#report the number of nodes left in the network
paste("Nodes in graph:", length(V(polarised_agent_graph)), sep=" ")

#report the edge density in the network; fully connected graph has density of 100% 
paste("Edge density:", edge_density(polarised_agent_graph, loops = FALSE), sep=" ")

#### Target ad ####

#Set up a character string to store our results by initializing it with some text
results<-"Results:"                             

for (ad in c(0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1)){             #start loop
  
  #Generate the agent projection of the survey: 
  polarised_edgelist_agent <- make_projection(testdata_polarised[,-1],
                                              layer =  "agent", 
                                              threshold_method = "target_ad", 
                                              method_value= ad, 
                                              centre = FALSE)
  
  #Define the graph:
  polarised_agent_graph <- graph.data.frame(polarised_edgelist_agent , directed=FALSE)#
  
  #Add these results to the "results" variable:
  results<-c(results,paste("AD: ", ad, "; Nodes in graph:", length(V(polarised_agent_graph)),"; Edge density:", edge_density(polarised_agent_graph, loops = FALSE), sep=" "))
}                                                               #end loop

#return the contents of the "results" object
results         


#Generate the agent projection of the survey: 
polarised_edgelist_agent <- make_projection(testdata_polarised[,-1],layer = "agent", threshold_method = "target_ad", method_value= .15, centre = FALSE)

#Define the graph:
polarised_agent_graph <- graph.data.frame(polarised_edgelist_agent , directed=FALSE)

#plot it
plot(polarised_agent_graph, vertex.size=2, vertex.label=NA, edge.width=0.2, layout=layout.fruchterman.reingold, main="Agent Projection: Target LCC 95%")

#report the number of nodes left in the network
paste("Nodes in graph:", length(V(polarised_agent_graph)), sep=" ")

#report the edge density in the network; fully connected graph has density of 100% 
paste("Edge density:", edge_density(polarised_agent_graph, loops = FALSE), sep=" ")



#generate the profile:
profile<-make_threshold_profile(testdata_polarised[,-1], layer = "agent")

str(profile)

#plot it:

par(mfrow=c(1,2))   #split the plotting window into two panels 
plot(profile$threshold, profile$ad)
plot(profile$threshold, profile$lcc)

#Simulate a survey with moderate intentional polarisation:
testdata_moderately_polarised <- make_synthetic_data(nrow=150, ncol=10, polarisation=1.1, minority =0.5)

#generate the profile:
profile<-make_threshold_profile(testdata_moderately_polarised[,-1], layer = "agent")

#plot it:
par(mfrow=c(1,2))   #split the plotting window into two panels 
plot(profile$threshold, profile$ad)
plot(profile$threshold, profile$lcc)


#Generate the agent projection of the survey: 
moderately_polarised_edgelist_agent <- make_projection(
  testdata_moderately_polarised[,-1],
  layer = "agent",
  threshold_method = "raw_similarity", 
  method_value= .60, 
  centre = FALSE)

#Define the graph:
moderately_polarised_agent_graph <- graph.data.frame(moderately_polarised_edgelist_agent , directed=FALSE)

#plot it
plot(moderately_polarised_agent_graph, vertex.size=2, vertex.label=NA, edge.width=0.2, layout=layout.fruchterman.reingold, main="Agent Projection: Target LCC 95%")

#report the number of nodes left in the network
paste("Nodes in graph:", length(V(moderately_polarised_agent_graph)), sep=" ")

#report the edge density in the network; fully connected graph has density of 100% 
paste("Edge density:", edge_density(moderately_polarised_agent_graph, loops = FALSE), sep=" ")


g <- graph(edges=c("A","B", "B","C", "C","A", "D","E", "E","F", "F","D", "C","D"), directed=FALSE )

plot(g, edge.label = edge_betweenness(g))


communities<-cluster_edge_betweenness(
  g,
  weights = NULL,
  directed = FALSE,
  edge.betweenness = TRUE,          #whether to return the edge betweenness of removed edges
  bridges = TRUE,                   #whether to return a list the edge removals 
  modularity = TRUE,                #whether to calculate the maximum modularity score
  membership = TRUE                 #whether to return the membership of most modular solution 
)
communities         
plot(communities,g)


g_zachary<-make_graph("Zachary")

communities<-cluster_edge_betweenness(g_zachary) 
#we can simplify things a lot by relying on the defaults

communities         
plot(communities,g_zachary)

#### Another example ####

#Set a new random seed so we get a different dataset; 
#set.seed() only used to make sure we get the same results as when we wrote the workshop document text
# set.seed(1111)
# set.seed(NULL)

#Simulate new moderately polarized data
testdata_moderately_polarised <- make_synthetic_data(nrow=150, ncol=10, polarisation=1.2, minority =0.5)

#Generate the agent projection of the survey: 
moderately_polarised_edgelist_agent <- make_projection(testdata_moderately_polarised[,-1],
                                                       layer = "agent", 
                                                       threshold_method = "raw_similarity", 
                                                       method_value= .57, 
                                                       centre = FALSE)

#Define the graph:
moderately_polarised_agent_graph <- graph.data.frame(moderately_polarised_edgelist_agent , directed=FALSE)

#plot it
plot(moderately_polarised_agent_graph, vertex.size=2, vertex.label=NA, edge.width=0.2, layout=layout.fruchterman.reingold, main="Agent Projection")
summary(moderately_polarised_agent_graph)

#report the number of nodes left in the network
paste("Nodes in graph:", length(V(moderately_polarised_agent_graph)), sep=" ")

#report the edge density in the network; fully connected graph has density of 100% 
paste("Edge density:", edge_density(moderately_polarised_agent_graph, loops = FALSE), sep=" ")


girvan_newman <- cluster_edge_betweenness(moderately_polarised_agent_graph)

#Extract the communities from the Girvan-Newman result and assign them to the node colour attribute in the graph
V(moderately_polarised_agent_graph)$color <- membership(girvan_newman)

#plot it 
plot(moderately_polarised_agent_graph, vertex.size = 5, vertex.label = NA)


plot_dendrogram(girvan_newman)


num_steps<- dim(girvan_newman$merges)[1]
#Breaking this down: (1)we use $merges to get the merge history. This returns a matrix
#(2) We use dim() to get the dimensions of the matrix. The first of these dimensions is the number of steps taken. 
#(3) We select the first number returned by dim() using [1] 


#Set a plotting window with 9 cells
par(mfrow=c(3,3))
#Loop through and plot the state at 16 steps, starting from the highest:
for (i in 1:9){
  com_membership<- cutat(girvan_newman, steps=num_steps-i)
  plot(moderately_polarised_agent_graph, vertex.size = 20, vertex.label = NA, edge.width=0,      vertex.color=com_membership)
}


#And plot the next 9 as well:
#Set a plotting window with 9 cells
par(mfrow=c(3,3))
#Loop through and plot the state at 16 steps, starting from the highest:
for (i in 10:18){
  com_membership<- cutat(girvan_newman, steps=num_steps-i)
  plot(moderately_polarised_agent_graph, vertex.size = 20, vertex.label = NA, edge.width=0,      vertex.color=com_membership)
}
