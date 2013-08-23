library(RCurl)
library(rjson)
library(igraph)
library(RColorBrewer)

# Generic function that collects data from facebook through the facebook graph API
facebook <-  function( path = "me", access_token = token, options){
  if( !missing(options) ){
    options <- sprintf( "?%s", paste( names(options), "=",
                                      unlist(options), collapse = "&", sep = "" ) )
  } else {
    options <- ""
  }
  data <- getURL( sprintf(
    "https://graph.facebook.com/%s%s/?access_token=%s", path, options,
    access_token ) )
  print(sprintf( "https://graph.facebook.com/%s%s/?access_token=%s",
                 path, options, access_token ))
  fromJSON( data )
}

# Shiny ouputs calculation
shinyServer(function(input, output) {
  
  # Collect information about the friends on facebook
  fbFriends <- reactive({
    if (input$token=="") return(NULL)
    
    friends <- facebook( path="me/friends" , access_token=input$token)
    friends.id <- sapply(friends$data, function(x) x$id)
    friends.name <- sapply(friends$data,
                           function(x) iconv(x$name,"UTF-8","ASCII//TRANSLIT"))
    initials <- function(x) paste(substr(x,1,1), collapse=" ")
    friends.initial <- sapply(strsplit(friends.name," "), initials)
    first.name <- sapply(strsplit(friends.name," "), function(x) x[1])
    
    list("initial"=friends.initial,"name"=friends.name,"fbid"=friends.id,
         "firstname"=first.name)
  })
  
  # Collect mutual friendship information on facebook
  mainNet <- reactive({
    the.friends <- fbFriends()
    if (is.null(the.friends)) return(NULL)
    
    N <- length(the.friends$initial)
    friendship.matrix <- matrix(0,N,N)
    for (i in 1:N) {
      tmp <- facebook(paste("me/mutualfriends", the.friends$fbid[i], sep="/") ,
                      access_token=input$token)
      mutualfriends <- sapply(tmp$data, function(x) x$id)
      friendship.matrix[i,the.friends$fbid %in% mutualfriends] <- 1
    }
    
    the.full.graph <- graph.adjacency(friendship.matrix,mode="undirected")
    V(the.full.graph)$initial <- the.friends$initial
    V(the.full.graph)$fbid <- the.friends$fbid
    V(the.full.graph)$fullname <- the.friends$name
    V(the.full.graph)$firstname <- the.friends$firstname
    if (input$lcc) {
      the.clusters <- clusters(the.full.graph)
      the.graph <- induced.subgraph(the.full.graph,
                                    which(the.clusters$membership==
                                            which.max(the.clusters$csize)))
    } else the.graph <- the.full.graph
    
    the.graph
  })
  
  community.color <- function(the.graph) {
    all.cc <- clusters(the.graph)
    lcc <- induced.subgraph(the.graph,
                            which(all.cc$membership==which.max(all.cc$csize)))
    clust <- multilevel.community(lcc)
    out.color <- rep("yellow",vcount(the.graph))
    out.color[match(clust$names,V(the.graph)$name)] <-
      brewer.pal(12,"Paired")[clust$membership]
    out.color
  }
  # Display the network
  coloredGraph <- reactive({
    the.friends <- fbFriends()
    the.graph <- mainNet()
    if ((is.null(the.friends))||(is.null(the.graph))) return(NULL)
    
    V(the.graph)$color <- switch(input$ncol,
                                 "uniform"=rep("seagreen",vcount(the.graph)),
                                 "degree"=brewer.pal(9,"YlOrRd")[
                                   cut(degree(the.graph),9,label=FALSE)],
                                 "betweenness"=brewer.pal(9,"YlOrRd")[
                                   cut(betweenness(the.graph),9,label=FALSE)],
                                 "community"=community.color(the.graph))
    
    V(the.graph)$size <- switch(input$nsize,
                                "degree"=degree(the.graph)/
                                  max(degree(the.graph))*8,
                                "betweenness"=betweenness(the.graph)/
                                  max(betweenness(the.graph))*8,
                                "uniform"=rep(5,vcount(the.graph)))
    
    V(the.graph)$name <- switch(input$nname,
                                "initials"=V(the.graph)$initial,
                                "facebook id"=V(the.graph)$fbid,
                                "full names"=V(the.graph)$fullname,
                                "first names"=V(the.graph)$firstname)
    
    set.seed(input$seed)
    the.graph$layout <- switch(input$layout,
                               "auto"=layout.auto(the.graph),
                               "random"=layout.random(the.graph),
                               "circle"=layout.circle(the.graph),
                               "sphere"=layout.sphere(the.graph),
                               "Fruchterman-Reingold"=
                                 layout.fruchterman.reingold(the.graph),
                               "Kamada-Kawai"=layout.kamada.kawai(the.graph),
                               "Reingold-Tilford"=
                                 layout.reingold.tilford(the.graph),
                               "Fruchterman-Reingold grid"=
                                 layout.fruchterman.reingold.grid(the.graph),
                               "LGL"=layout.lgl(the.graph),
                               "opt"=layout.graphopt(the.graph),
                               "svd"=layout.svd(the.graph),
                               "norm"=layout.norm(the.graph))
    
    the.graph
  })
  
  output$chart <- renderPlot({
    the.graph <- coloredGraph()
    par(mar=rep(0,4))
    plot(the.graph, vertex.label.font=1, vertex.label.color="black",
         vertex.frame.color=V(the.graph)$color, vertex.label.font=2)
  })
  
  output$stats <- renderTable({
    the.friends <- fbFriends()
    the.graph <- coloredGraph()
    deg <- degree(the.graph)
    the.bet <- betweenness(the.graph)
    data.frame(
      Statistics = c("Number of friends", "Density", "Transitivity",
                     "Average degree", "Max degree", "Average betweenness",
                     "Max betweenness", "Best degree", "Best betweenness"),
      Values = as.character(c(vcount(the.graph),
                              paste(round(graph.density(the.graph)*100,0),"%",
                                    collapse=" "),
                              paste(round(transitivity(the.graph)*100,0),"%",
                                    collapse=" "), round(mean(deg),1), max(deg),
                              round(mean(the.bet),0), round(max(the.bet),0),
                              names(which.max(deg)),
                              names(which.max(the.bet)))),
      stringsAsFactors=FALSE)
  })
  
  output$downloadRda <- downloadHandler(
    filename = "fbnet.rda",
    content = function(file) {
      the.graph <- coloredGraph()
      save(the.graph, file=file)
    }
  )
  
  output$downloadText <- downloadHandler(
    filename = "fbnet.txt",
    content = function(file) {
      the.graph <- coloredGraph()
      write.table(get.edgelist(the.graph), file)
    }
  )
  
  output$downloadGraphml <- downloadHandler(
    filename = "fbnet.graphml",
    content = function(file) {
      the.graph <- coloredGraph()
      write.graph(the.graph, file=file, format="graphml")
    }
  )
  
})
