# Define UI for dataset viewer application
shinyUI(pageWithSidebar(
  # Application title
  headerPanel("Visualize your facebook network..."),
  
  sidebarPanel(
    textInput("token",strong("Copy your access token here:"), ""),
    checkboxInput("lcc"," Only the largest connected component is displayed.",
                  TRUE),
    selectInput("ncol", "Node color represents:",
                choices = c("community","betweenness","degree","uniform")),
    selectInput("nsize", "Node size represents:",
                choices = c("degree","betweenness","uniform")),
    selectInput("nname","Node names are:",
                choices=c("initials","facebook id","full names","first names")),
    numericInput("seed",
                 HTML("Set a random seed for reproducible layouts
                                 <a href='#pseudor'><sup>(1)</sup></a>:"),
                 as.numeric(format(Sys.time(), "%M%S"))),
    selectInput("layout", "Layout generation method",
                c("Fruchterman-Reingold", "auto", "random", "circle", "sphere",
                  "Kamada-Kawai", "Reingold-Tilford", 
                  "Fruchterman-Reingold grid", "LGL", "opt", "svd", "norm")),
    p(HTML("<a name='pseudor'><sup>(1)</sup></a> Most layout generations are
           based on methods that use randomness. Setting a seed results in
           fixing the random procedure in order to obtain reproducible results
           (runing several times the process with the same random seed will give
           the same layout). More information on pseudo-random generators at <a 
           href='http://en.wikipedia.org/wiki/Pseudorandom_number_generator'>
           this link</a>."))
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("User Guide",
               h3("Basic user guide"),
               p("To visualize your facebook network, please go to ", 
                 a("the facebook graph API Explorer", 
                   href="https://developers.facebook.com/tools/explorer"),
                 " to generate a token and copy it in the field on the left hand
                 side panel. Do not forget to select proper permissions to give 
                 you access to your friends' data. Then use the 'Chart' panel to
                 see the network, the 'Statistics' panel to obtain basic
                 statistics on the network and the 'Download' panel to download
                 the dataset in 'text', 'rda' or 'graphml' formats (readable,
                 for instance, by the free software ",
                 a("Gephi",href="https://gephi.org"),
                 "."),
               p(HTML("You may wait for a <strong>long time</strong> for the
                      first network to be displayed (until all your data are
                      collected from facebook) and then you can change the way
                      the network is displayed by selecting various options for
                      the node colors, size and label.")),
               p(HTML("Script sources and explanations can be found on 
                      <a href='http://tuxette.nathalievilla.org/?p=997&lang=en'>
                      my blog</a>.")),
               p(HTML("This application is kindly provided by
                      <a href='http://tuxette.nathalievilla.org'>
                      <font color='#DF01A5'><b>Natty</b></font></a>. The 
                      application scripts are available on GitHub:<br> <code>
                      <font color='#870500'><b>git clone
                      https://github.com/tuxette/fbs.git</b></font></code><br>
                      It is distributed without any guarantee under the licence
                      <a href='http://www.wtfpl.net/'>WTFPL</a>."))
      ),
      tabPanel("Chart",plotOutput("chart")),
      tabPanel("Statistics",tableOutput("stats")),
      tabPanel("Downloads",
               p(HTML("Download data in 'rda' format (for <a href='
                              http://cran.univ-paris1.fr'>R</a>):")),
               downloadButton('downloadRda',"rda"),
               p(HTML("Download the edge list in 'text' format:")),
               downloadButton('downloadText',"text"),
               p(HTML("Download data in 'graphml' format (for <a href='
                              https://gephi.org'>Gephi</a>):")),
               downloadButton('downloadGraphml',"Graphml"))
    )
  )
))
    