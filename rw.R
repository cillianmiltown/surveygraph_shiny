devtools::install_github("surveygraph/surveygraphr")
surveygraphr::gensurvey()

library(ggplot2)

S <- surveygraphr::gensurvey(200,25)
class(S)
results <- surveygraphr::exploregraph(S)

df <- data.frame(radius = unlist(results[1]), degree = unlist(results[2]), LCC = unlist(results[3]))

head(df)
rm(list = ls())
df
ggplot(df, aes(x=radius,y=LCC))+geom_point()

edgelist <- surveygraphr::listgraph(S)



igraph::make_graph()
g <- igraph::make_graph(edges=edgelist, directed=FALSE)

plot(g)

plot(g, vertex.size=5, vertex.label=NA)

library(dplyr)

rm(list = ls())

load("../data_with_means.RData")



ICSMP_500 <- dplyr::sample_n(df,500)

variable.names(ICSMP_500)



COVID_measures <- ICSMP_500 %>% 
  select(
    "contact1"
    ,"contact2"
    ,"contact3"
    ,"contact4"
    ,"contact5" 
    ,"psupport1"
    ,"psupport2"
    ,"psupport3"
    ,"psupport4"
    ,"psupport5"
    ,"hygiene1"                 
    ,"hygiene2"
    ,"hygiene3"
    ,"hygiene4"
    ,"hygiene5"
    ,"physical_contact_tot"
    ,"physical_hygiene_tot"
    ,"policy_support_tot")


length(ICSMP_500[,1])

place_holder <- sample(1:7, replace = T, length(ICSMP_500[,1]))

ICSMP_500 <- cbind(place_holder,ICSMP_500)
ICSMP_500

save(ICSMP_500,COVID_measures, file="ICSMP_500.RData")

#sssurvey cran
#anesr on github




obj_vx_fun <- function(x){
  noquote(paste("obj$", noquote(d[x]),sep = ""))}

sapply(1:length(d), obj_vx_fun)

obj_vx_fun(2)

c(obj$v1,obj$v2,obj$v3,obj$v4,obj$v5)

d_vector <- sapply(1:length(d), obj_vx_fun)


inputted_variables_fun <- function(x){
  eval(parse(text=d_vector[x]))}


# eval(parse(text=d_vector[1]))



obj <- get_data()

S <- obj$data

c(obj$v1,obj$v2,obj$v3,obj$v4,obj$v5)

S <- S %>% select(c(obj$v1,obj$v2,obj$v3,obj$v4,obj$v5))
S <- cbind.data.frame(
  as.numeric(unlist(S[1]))
  ,as.numeric(unlist(S[2]))
  ,as.numeric(unlist(S[3]))
  ,as.numeric(unlist(S[4]))
  ,as.numeric(unlist(S[5]))
)
colnames(S) <- c(obj$v1,obj$v2,obj$v3,obj$v4,obj$v5)


# use [[]] to call list


#### 16th May 2023 ####


S1 <- surveygraphr::generate_survey_polarised(m=300, n=15, polarisation=0.25)
getwd()
load("surveygraph/ICSMP_500.RData")
S1

S <- COVID_measures
S[] <- lapply(S, as.numeric)

S <- na.omit(S)

(S[,1])

names1 <- data.frame(id=c(1:length(S[,1])), group=S[,1])
names2 <- data.frame(id=c(1:length(S)))

edgelists <- surveygraphr::graph_edgelists(S)

g1 <- igraph::graph.data.frame(edgelists[[1]], vertices=names1, directed=FALSE)
g2 <- igraph::graph.data.frame(edgelists[[2]], vertices=names2, directed=FALSE)


V(g1)$group
V(g1)$color <- ifelse(V(g1)$group == 10, "blue", "red")
V(g1)$color

isolated_nodes1 <- which(degree(g1)==0)
isolated_nodes2 <- which(degree(g2)==0)

g1c <- delete.vertices(g1, isolated_nodes1)
g2c <- delete.vertices(g2, isolated_nodes2)

E(g2c)$label= E(g2c)$weight

par(mfrow=c(1,2), mar=c(1,1,1,1))
plot(g1c, vertex.size=2, vertex.label=NA, edge.width=0.2
     #, layout=layout.fruchterman.reingold
     , main="respondents")
plot(g2c, vertex.size=10, edge.width=1.0, layout=layout.fruchterman.reingold, main="items")








# https://cran.r-project.org/web/packages/essurvey/vignettes/intro_ess.html

# install.packages("devtools")
devtools::install_github("ropensci/essurvey")
install.packages("essurvey")

library(essurvey)
set_email("cillian.mchugh@ul.ie")
#set_email("cillian.miltown@gmail.com")
show_countries()
import_all_cntrounds()
show
show_country_rounds("Turkey")

show_rounds()
one_round <- import_rounds(1)



df <- read.csv("../SurveyGraph/ESS10.csv")

df <- dplyr::sample_n(df,500)

head(df)

table(df$cntry)

ESS_500

place_holder <- sample(1:7, replace = T, length(ESS_500[,1]))

ESS_500 <- cbind(place_holder,ESS_500)
ESS_500


place_holder <- sample(1:7, replace = T, length(ESS_GB_500[,1]))

ESS_GB_500 <- cbind(place_holder,ESS_GB_500)
ESS_GB_500


ESS_GB_500 <- dplyr::sample_n(
  df[which(df$cntry=="GB"),],
  500)

ESS_500 <- df

save(ESS_500,ESS_GB_500, file="ESS_500.RData")






rm(list = ls())
load(url("https://github.com/cillianmiltown/surveygraph_shiny/blob/0c32f2b57afabde8467fd6f12076ba06cee5af47/surveygraph/ESS_500.RData?raw=true"))


df <- ESS_500

skimr::skim(df)

sample(500,100)

df1 <- df[c(sample(500,100)),]
skimr::skim(df1)
df1[,c(1:4)]
df1[[c("essround","proddate")]]
df1[[c("essround","proddate")]]

skimr::skim(df1[,c("essround","proddate")])

summary(df1[,c("essround","proddate")])

skimr::skim(df[c("trstprl","trstgl","trstplc","trstplt","trstprt","trstep","trstun","trstsci"),])


test <- df[which(is.na(df)==FALSE),]

df[which(is.na(df$trstprl)==FALSE),]

sum(complete.cases(df))
library(dplyr)
df %>% mutate(trstprl = recode(trstprl, "88" = "NA"))

df %>% mutate(trstprl = if_else(trstprl == 88, NA, trstprl))

df %>% mutate(trstprl = if_else(trstprl > 10, NA, trstprl))



mutate(result=recode(result, 'Win'='1', 'Loss'='0'))

remove_missing_fun <- 
df <- ESS_GB_500

df <- df[c("trstprl","trstlgl","trstplc","trstplt","trstprt","trstep","trstun","trstsci")]

df["trstprl","trstlgl","trstplc","trstplt","trstprt","trstep","trstun","trstsci"]
df$trstlgl
df[c("trstprl","trstlgl")]

df <- ESS_500


df <- df[,c("trstprl","trstgl")]
filter()
df$cntry

dplyr::filter(df, cntry == "GB")

Filter(df,cntry=="GB")
df[which(df$cntry=="GB"),]




val_repl <- c(77,88,99)
val_repl 
df_temp <- sapply(df,function(x) replace(x, x %in% val_repl, NA))
df_temp <- as.data.frame(df_temp)
sum(complete.cases(df_temp))

df_no_missing <- df_temp[which(complete.cases(df_temp)),]
class(df_no_missing)


library(igraph)
g1 <- graph( edges=c(1,2, 2,3, 3,1, 4,2), n=4, directed=F )
plot(g1)


install.packages("pacman")
library(pacman)
pacman::p_install("igraph")

library(igraph)
g1 <- graph( edges=c(1,2, 2,3, 3,1, 4,2), n=4, directed=F )
plot(g1)

# Color a node 
V(g1)$color <- '#66ff33'  
g1
V(g1)==c(1,2)
V(g1)$color <- ifelse(V(g1)==c(1,2), "lightblue", "pink")
plot(g1)

E(g1)$color <- 'orange'



plot(g1)
V(g1)[which()]

load(url("https://github.com/cillianmiltown/surveygraph_shiny/blob/0c32f2b57afabde8467fd6f12076ba06cee5af47/surveygraph/ESS_500.RData?raw=true"))

df <- df[c("trstprl","trstlgl","trstplc","trstplt","trstprt","trstep","trstun","trstsci")]


plot(x, y, type = "l", lty = 1)
plot(df$trstep,df$trstlgl)


# use sapply and replace to replace these 'missing' values with NAs
df_temp <- sapply(df,function(x) replace(x, x %in% val_repl, NA))
df_temp <- as.data.frame(df_temp)

# remove NAs
df_no_missing <- df_temp[which(complete.cases(df_temp)),]

df <- df_no_missing
plot(df$trstep,df$trstlgl,type = "l", lty = 1)


df1 <- df_no_missing

df1

variable.names(df1)
interaction_fun <- function(x){
  dplyr::case_when(x > mean(x,na.rm=T)+sd(x,na.rm=T) ~ "1high",
                   x < mean(x,na.rm=T)+sd(x,na.rm=T) & x > mean(x,na.rm=T)-sd(x,na.rm=T) ~ "2moderate",
                   x < mean(x,na.rm=T)-sd(x,na.rm=T) ~ "3low")
}

library(dplyr)
library(ggplot2)


interaction_fun <- function(x){
  dplyr::case_when(x > mean(x,na.rm=T)+sd(x,na.rm=T) ~ "1high",
                   x < mean(x,na.rm=T)+sd(x,na.rm=T) & x > mean(x,na.rm=T)-sd(x,na.rm=T) ~ "2moderate",
                   x < mean(x,na.rm=T)-sd(x,na.rm=T) ~ "3low")
}



df1 <-
  df %>%
  dplyr::mutate(trust_polt_3groups = interaction_fun(trstplt)
                , national_pi_mean = mean(trstplt)
                , national_pi_sd = sd(trstplt))

g <- ggplot(df1, aes(trstprl, trstep, linetype =trust_polt_3groups))+
  geom_jitter(height = .1, width = .1, size=.0001, color="gray") + 
  #ylim(0,12) +
  geom_smooth(method=lm, size = .5, color="black")+
  labs(y = "Trust in European Parlimant",
       x = "Trust in country's parliament",
       linetype = "Trust in politicians"
  )+
  scale_linetype_manual(values = c("longdash", "solid", "dotted"),
                        labels=c("Low",
                                 "Moderate",
                                 "High"))
g



df3 <- df

df1 <- df %>% select(trstprl)

df <- S
val_repl <- c(77,88,99)
val_repl 
df_temp <- sapply(df,function(x) replace(x, x %in% val_repl, NA))
df_temp <- as.data.frame(df_temp)
sum(complete.cases(df_temp))
S <- as.data.frame(df_temp[which(complete.cases(df_temp)),])
colnames(df)
S <- `colnames<-`(S, colnames(df))
S
S[] <- lapply(S, as.numeric)
#a1 <- make_a()
#S <- sample_n(S,a1)
# head(S)
# obj[2]
# get_d()
head(S)


#### testing updated package June 2023 ####

rm(list = ls())

library("surveygraph")
library("igraph")




S <- make_synthetic_data(nrow=400, ncol=15, polarisation=1.25)

names <- data.frame(id=c(1:length(S$X1)), group=S$X1)

edgelist <- make_projection(S, layer="agent")

g <- graph.data.frame(edgelist, vertices=names, directed=FALSE)

V(g)$color <- ifelse(V(g)$group == 1, "blue", "red")

g <- delete.vertices(g, which(degree(g)==0))

plot(g, vertex.size=2, vertex.label=NA, edge.width=0.2, layout=layout.fruchterman.reingold, main="agent layer")






polarization <- 0 #1.25 #make_polarization()



S1 <- surveygraph::make_synthetic_data(nrow=400, ncol=15, polarisation=polarization, minority =0.5)

names1 <- data.frame(id=c(1:length(S$X1)), group=S$X1)
names2 <- data.frame(id=c(1:length(S)))

names1 <- data.frame(id=c(1:length(S1$X1)), group=S1$X1)
names2 <- data.frame(id=c(1:length(S1)))

edgelists <- surveygraph::make_projection(S1)

g1 <- igraph::graph.data.frame(edgelists[[1]], vertices=names1, directed=FALSE)
g2 <- igraph::graph.data.frame(edgelists[[2]], vertices=names2, directed=FALSE)

V(g1)$color <- ifelse(V(g1)$group == 1, "blue", "red")

isolated_nodes1 <- which(degree(g1)==0)
isolated_nodes2 <- which(degree(g2)==0)

g1c <- delete.vertices(g1, isolated_nodes1)
g2c <- delete.vertices(g2, isolated_nodes2)

E(g2c)$label= E(g2c)$weight

par(mfrow=c(1,2), mar=c(1,1,1,1))
plot(g1c, vertex.size=2, vertex.label=NA, edge.width=0.2, layout=layout.fruchterman.reingold, main="respondents")
plot(g2c, vertex.size=10, edge.width=1.0, layout=layout.fruchterman.reingold, main="items")



#### for app ####
rm(list=ls())

polarization <- 2.1

S1 <- surveygraph::make_synthetic_data(nrow=100, ncol=15, polarisation=polarization, minority =0.5)

S1_agent <- make_projection(S1[,-1],
                            layer = "agent", 
                            threshold_method = "target_lcc", 
                            method_value= .75, 
                            centre = FALSE)
S1_agent_graph <- graph.data.frame(S1_agent , directed=FALSE)

girvan_newman <- cluster_edge_betweenness(S1_agent_graph)

#communities<-cluster_edge_betweenness(S1_agent_graph) 

V(S1_agent_graph)$color <- membership(girvan_newman)

plot(S1_agent_graph, vertex.size = 5, vertex.label = NA)





S1_symbolic <- make_projection(S1[,-1],
                            layer = "symbolic", 
                            threshold_method = "target_lcc", 
                            method_value= .75, 
                            centre = FALSE)

S1_symbolic_graph <- graph.data.frame(S1_symbolic , directed=FALSE)

plot(S1_symbolic_graph, vertex.size = 5, vertex.label = NA)

#Extract the communities from the Girvan-Newman result and assign them to the node colour attribute in the graph
V(S1_agent_graph)$color <- membership(girvan_newman)

#plot it 
plot(moderately_polarised_agent_graph, vertex.size = 5, vertex.label = NA)

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


###############



ESS_500 <- read.csv("https://www.dropbox.com/s/kotimfih8v0ys2t/ESS10_500.csv?dl=1")

S <- ESS_500

S <- sample_n(S, 100)

inputted_variables <- c("ppltrst","rlgatnd")

S <- S %>% select(inputted_variables)


S[] <- lapply(S, as.numeric)



S <- na.omit(S)

S1 <- S
S1_agent <- make_projection(S1, #S1[,-1],
                            layer = "agent", 
                            threshold_method = "target_lcc", 
                            method_value = .85,#threshold_up, 
                            centre = FALSE)
S1_agent_graph <- graph.data.frame(S1_agent , directed=FALSE)

girvan_newman <- cluster_edge_betweenness(S1_agent_graph)

#communities<-cluster_edge_betweenness(S1_agent_graph) 

V(S1_agent_graph)$color <- membership(girvan_newman)

plot(#communities,
  S1_agent_graph, vertex.size = 5, vertex.label = NA, main="respondents"
)

place_holder <- sample(1:7)

length(ESS_500[,1])

sample(1:7, replace = T, )
