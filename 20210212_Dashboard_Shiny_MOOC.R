# fbrisadelamontaña()

library(mongolite)
library(syuzhet)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(tidyverse)
library(plotly)
library(dplyr)
library(NLP)
library(tm)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(fresh)

mytheme <- create_theme(
  adminlte_color(
    light_blue = "#008080"
  )
)

#####################
#Fonction pour faire l'association mot-sentiments sur les dfs
#####################
trait_sent <- function (df_t) {
  df2 <- str_replace_all(df_t, "\n"," ")
  char_v <- get_sentences(df2)
  method <- "nrc"
  lang <- "french"
  mtv <- get_nrc_sentiment(char_v, language=lang)
  mtv2 <- as.data.frame(cbind(sort(colSums(prop.table(mtv[,1:8]))))) %>%
    mutate(sent=row.names(.)) %>%
    rename(per=V1)
  mtv3 <- as.data.frame(cbind(sort(colSums(prop.table(mtv[,9:10]))))) %>%
    mutate(sent=row.names(.)) %>%
    rename(per=V1)
  return(list(char_v,mtv,mtv2,mtv3))
}
##################################

#édition de la librairie wordcloud2 afin que le nuage de mots et d'autres graphiques puissent être affichés en même temps
#--------------------------------------------------------------------------------------------

wordcloud2a <- function (data, size = 1, minSize = 0, gridSize = 0, fontFamily = "Segoe UI", 
                         fontWeight = "bold", color = "random-dark", backgroundColor = "white", 
                         minRotation = -pi/4, maxRotation = pi/4, shuffle = TRUE, 
                         rotateRatio = 0.4, shape = "circle", ellipticity = 0.65, 
                         widgetsize = NULL, figPath = NULL, hoverFunction = NULL) 
{
  if ("table" %in% class(data)) {
    dataOut = data.frame(name = names(data), freq = as.vector(data))
  }
  else {
    data = as.data.frame(data)
    dataOut = data[, 1:2]
    names(dataOut) = c("name", "freq")
  }
  if (!is.null(figPath)) {
    if (!file.exists(figPath)) {
      stop("cannot find fig in the figPath")
    }
    spPath = strsplit(figPath, "\\.")[[1]]
    len = length(spPath)
    figClass = spPath[len]
    if (!figClass %in% c("jpeg", "jpg", "png", "bmp", "gif")) {
      stop("file should be a jpeg, jpg, png, bmp or gif file!")
    }
    base64 = base64enc::base64encode(figPath)
    base64 = paste0("data:image/", figClass, ";base64,", 
                    base64)
  }
  else {
    base64 = NULL
  }
  weightFactor = size * 180/max(dataOut$freq)
  settings <- list(word = dataOut$name, freq = dataOut$freq, 
                   fontFamily = fontFamily, fontWeight = fontWeight, color = color, 
                   minSize = minSize, weightFactor = weightFactor, backgroundColor = backgroundColor, 
                   gridSize = gridSize, minRotation = minRotation, maxRotation = maxRotation, 
                   shuffle = shuffle, rotateRatio = rotateRatio, shape = shape, 
                   ellipticity = ellipticity, figBase64 = base64, hover = htmlwidgets::JS(hoverFunction))
  chart = htmlwidgets::createWidget("wordcloud2", settings, 
                                    width = widgetsize[1], height = widgetsize[2], sizingPolicy = htmlwidgets::sizingPolicy(viewer.padding = 0, 
                                                                                                                            browser.padding = 0, browser.fill = TRUE))
  chart
}

#-----------------------------fin de l'édition de libreirie--------------------------------------------------------------------------------------------------------------


ui <- dashboardPage(
  
  dashboardHeader(title = "FUN MOOC"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Général", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Sentiment Analysis", tabName = "donnees", icon = icon("file-code-o"))
    )
  ),
  ## Body content
  dashboardBody(
    use_theme(mytheme),
    tags$head(tags$style(HTML('
      .my-class { 
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 24px;
      }
      }
    '))),
    
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                pickerInput(
                  inputId = "Mooc",      label = "Les Moocs",
                  choices = c(
                    "Introduction à la statistique avec R"="messages_R",
                    "Apprendre à coder avec Python"="messages_Python_vf",
                    "L'Intelligence Artificielle… avec intelligence !"="messages_IA",
                    "Les mots du pouvoir"="messages_Les_mots_du_pouvoir",
                    "Introduction à la physique quantique"="messages_Physique_Quantique"
                  )
                )
              ),
              fluidRow(
                # Dynamic infoBoxes
                infoBoxOutput("progressBox_p", width = 3),
                infoBoxOutput("approvalBox_p", width = 3),
                infoBoxOutput("progressBox2_p", width = 3),
                infoBoxOutput("approvalBox2_p", width = 3)
              ),
              
              fluidRow(
                # Clicking this will increment the progress amount
                
                box(
                  title = "Nombre de commentaires par mois", status = "primary",solidHeader = TRUE,
                  collapsible = TRUE,width = 4,
                  plotlyOutput("plot1_p")),
                box(
                  title = "Top 20 des utilisateurs les plus actifs", status = "primary",solidHeader = TRUE,
                  collapsible = TRUE,width = 4,
                  plotlyOutput("plot2_p")),
                
                box(
                  title = "Word cloud", status = "primary",solidHeader = TRUE,
                  collapsible = TRUE,width = 4,
                  wordcloud2Output("cloud")
                )
              )
        ),
      
      tabItem(tabName = "donnees",
              fluidRow(
                pickerInput(
                  inputId = "sentid",
                  label = "Les Moocs",
                  choices = c(
                    "Introduction à la physique quantique"="messages_Physique_Quantique",
                    "Elles font l'art"="messages_Art_feminin",
                    "Les mots du pouvoir"="messages_Les_mots_du_pouvoir",
                    "Introduction à la statistique avec R"="messages_R",
                    "L'Intelligence Artificielle… avec intelligence !"="messages_IA",
                    "Probabilités pour l'ingénieur"="messages_Proba"
                  )
                )
              ),
              fluidRow(
                box(title = "La jauge",status = "primary", solidHeader = TRUE, closable = FALSE, collapsible = TRUE,plotlyOutput("jauge")),
                box(title = "La roue",status = "primary", solidHeader = TRUE, closable = FALSE, collapsible = TRUE, plotOutput("plot1")),
                box(title = "Le choix",status = "primary", solidHeader = TRUE, closable = FALSE, collapsible = TRUE, 
                        radioGroupButtons(
                          inputId = "sentbutton",
                          label = "Label",
                          choices = c("anger", "trust", "anticipation", "disgust","fear","joy","sadness","surprise","trust"),
                          individual = TRUE,
                          checkIcon = list(
                            yes = tags$i(class = "fa fa-circle", 
                                         style = "color: steelblue"),
                            no = tags$i(class = "fa fa-circle-o", 
                                        style = "color: steelblue"))
                        )
                ),
                box(title = "Les réactions",status = "primary", solidHeader = TRUE, closable = FALSE, collapsible = TRUE, collapsed = TRUE,  tableOutput("table"))
              )
      )
      
    )
  )
)

server <- function(input, output) {

###############
# Partie server du second tab
###############
  # fonction reactive de l'onglet "Les Moocs"
  reac_dfsent <- reactive({ 
    config <- yaml::yaml.load_file("config.yml")
    id <- input$sentid
    url <- paste("mongodb://", config$mongo$user, ":", config$mongo$password,"@127.0.0.1/bdd_grp4?authSource=admin", sep="")
    o <- mongo(id, url = url)
    plop <- o$aggregate('[{"$project": {"_id":0,"body":"$body"}}]')
    trait_sent(plop$body)
  })
  
  # fonction reactive permettant de changer la table suivant les sentiments
  reac_sentbutton <- reactive ({
    sent_items <- which(reac_dfsent()[[2]][input$sentbutton] > 2)
    res_sent <- reac_dfsent()[[1]][sent_items]
    head(res_sent)
  })
  
  # fonction reactive de la jauge en fonction de reac_dfsent
  reac_jauge <- reactive ({
    fig <- plot_ly(
      domain = list(x=c(0,1, y =c(0,1))),
      value = (reac_dfsent()[4][[1]][2,1])*100,
      title = list(text="Degré de satisfaction (en %)"),
      delta = list(reference = 400, increasing = list(color = "RebeccaPurple")),
      gauge = list(
        axis = list(range = list(0, 100), tickwidth = 1, tickcolor = "darkblue"),
        bar = list(color = "darkblue"),
        bgcolor = "white",
        borderwidth = 2,
        bordercolor = "gray",
        steps = list(
          list(range = c(0, 50), color = "cyan"),
          list(range = c(50, 100), color = "royalblue"))),
      type = "indicator",
      mode = "gauge+number")
    
    fig <- fig %>%
      layout(margin = list(l=20,r=30), paper_bgcolor="lavender", font = list(color="darkblue", family = "Arial"))
    
    fig
  })
  
  # Table des sentiments sortie
  output$table <- renderTable({
    reac_sentbutton()
  })
  
  # Jauge sortie
  output$jauge <- renderPlotly(reac_jauge())
  
  # Barplot coordonnées polaires sortie
  output$plot1 <- renderPlot({
    plot <- ggplot(reac_dfsent()[[3]],
                   aes(
                     x = sent,
                     y = per,
                     fill = sent,
                     text="sent"
                   )) +
      geom_col(width = 1, color = "white") +
      coord_polar()+ labs(
        x = "",
        y = ""#,
        #title = "Your Title",
        #subtitle = "Your Subtitle", 
        #caption = "Your Caption"
      ) +
      theme_gray()+
      theme(
        panel.background = element_rect(fill = "grey", colour ="grey"),
        panel.grid = element_line(size=0.5, linetype = "solid",color = "darkgrey"),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(face = "bold", colour = "white", size = "30pts"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 12)
      )
    plot
  })
  
  ################ 
  
  reac_dfval <- reactive({ 
    
    config <- yaml::yaml.load_file("config.yml")
    url <- paste("mongodb://", config$mongo$user, ":", config$mongo$password,"@127.0.0.1/bdd_grp4?authSource=admin", sep="")
    m <- mongo(input$Mooc, url = url)
    
    mooc_python <- function() {
      
      # Combien de publications chaque utilisateur a-t-il faites
      pub_par_ut_python <- m$aggregate('[
            {"$group": {"_id":"$username","publications": {"$sum": 1 } } }, 
            {"$sort": {"publications": -1 } },
            {"$limit": 20}
            ]')
      
      pub_par_ut_2_python <- pub_par_ut_python[-1,]  # nous éliminons le premier endroit qui est le formateur
      
      # Top 20 des utilisateurs les plus actifs
      plot_top_20_utilisateurs_python <- ggplot(pub_par_ut_2_python) +
        aes(x = reorder(`_id`, publications), weight = publications) +
        geom_bar(fill = "#0c4c8a") +
        coord_flip() +
        labs(title = "Top 20 des utilisateurs les plus actifs") +
        labs(x = "Utilisateurs") +
        theme_minimal()
      
      plot_top_20_utilisateurs_plotly_python <- ggplotly(plot_top_20_utilisateurs_python)
      
      
      
      
      # utilisateur qui a publié le plus grand nombre de messages
      ut_plus_actif_python <- pub_par_ut_python[1,1]
      
      # Combien de messages a-t-il postés
      num_max_pub_python <- pub_par_ut_python[1,2]
      
      
      # Combien de publications au total y a-t-il dans le MOOC?
      tot_publications_python <- m$aggregate('[
            {"$group": {"_id":"$username","publications": {"$sum": 1 } } }, 
            {"$sort": {"publications": -1 } },
            {"$group":{"_id":"", "total":{"$sum":"$publications"}}}
            ]')
      
      
      # Combien d'utilisateurs sont dans le MOOC
      list_user_python <- m$distinct("username")
      nombre_d_utilisateurs_python <- length(list_user_python)
      
      # dans MongoDB: db.countries.distinct('country').length
      
      
      #--------------------------------------------------------------
      
      # Combien de messages ont été publiés au total par mois
      m$aggregate('[{
              "$project":
              {
                "updated_at":1,
                "username":1,
                "date": { "$dateFromString": {"dateString": "$updated_at"} }
                
              }
},
{"$group":{"_id":{ "annee":{"$year":"$date"}, "mois":{"$month":"$date"}}, "subtotal":{"$sum":1}}},
            {"$sort":{"_id":1}}
]')
      
      
      
      # On ajoute le résultat précédent dans une variable
      
      df_par_mois_python <- m$aggregate('[{
              "$project":
              {
                "updated_at":1,
                "username":1,
                "date": { "$dateFromString": {"dateString": "$updated_at"} }
                
              }
},
{"$group":{"_id":{ "annee":{"$year":"$date"}, "mois":{"$month":"$date"}}, "subtotal":{"$sum":1}}},
            {"$sort":{"_id":1}}
]')
      
      #nous mettons les données à plat, convertissant les colonnes id en colonnes normales avec flatten
      #puis avec #mutate nous faisons la concaténation des colonnes du mois et de l'année 
      #et le forçons à être une date en ajoutant 1 comme jour de chaque mois et un séparateur
      
      df_votes_par_mois_python <- jsonlite::flatten(df_par_mois_python) %>%
        mutate(mois_annee=as.Date(paste(`_id.annee`,`_id.mois`,'01',sep='-')))
      
      # On ajoute une nouvelle colonne au df avec le nom du mooc
      df_votes_par_mois_python$MOOC <- "Python"
      
      # ici on fait le graphique "Nombre de commentaires par mois"
      posts_par_mois_python <- ggplot(df_votes_par_mois_python) +
        aes(x = mois_annee, y = subtotal) +
        geom_line(size = 1L, colour = "#4292c6") +
        labs(x = "mois (2020/2021)", y = "Nombre de commentaires", title = "Nombre de commentaires par mois") +
        theme_classic()
      
      posts_par_mois_plotly_python <- ggplotly(posts_par_mois_python)
      
      # ------------------ wordcloud --------------------------------------------
      text <- m$aggregate('[{"$project": {"_id":0,"body":"$body"}}]')
      
      # Create a corpus  
      docs <- Corpus(VectorSource(text$body))
      
      toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
      docs <- tm_map(docs, toSpace, "/") 
      docs <- tm_map(docs, content_transformer(tolower))
      docs <- tm_map(docs, removeNumbers)
      docs <- tm_map(docs, removeWords, stopwords("french"))
      docs <- tm_map(docs, removePunctuation)
      docs <- tm_map(docs, stripWhitespace)
      
      dtm <- TermDocumentMatrix(docs) 
      matrix <- as.matrix(dtm) 
      words <- sort(rowSums(matrix),decreasing=TRUE) 
      df_p <- data.frame(word = names(words),freq=words)
      
      # ----------------------fin de wordcloud ----------------------------------
      
      return(list(utilisateurs = nombre_d_utilisateurs_python, 
                  plus_actif = ut_plus_actif_python, 
                  publications = tot_publications_python, 
                  pub_plus_actif = num_max_pub_python,
                  votes_par_mois = df_votes_par_mois_python,
                  pub_par_ut = pub_par_ut_2_python,
                  df_p = df_p))
      
    }
    
    result_python <- mooc_python()
  })
  
  #output$value <- renderPrint(input$Mooc)
  #output$table <- renderTable(reac_dfval())
  
  
  
#----------- server de Python --------------------------------------------------------------------
  
  #  Début infoBoxes
  
  # result_python$votes_par_mois -- est dans le fichier 20210208_MOOC_python.R
  output$plot1_p <-  renderPlotly({ 
    ggplotly(ggplot(reac_dfval()$votes_par_mois) +     
      aes(x = mois_annee, y = subtotal) +
      geom_line(size = 1L, colour = "#4292c6") +
      labs(x = "mois", y = "Nombre de commentaires") +
      theme_classic())
  })
  
  
  # result_python$pub_par_ut -- est dans le fichier 20210208_MOOC_python.R
  output$plot2_p <-  renderPlotly({ 
    ggplotly(ggplot(reac_dfval()$pub_par_ut) +
      aes(x = reorder(`_id`, publications), weight = publications) +
      geom_bar(fill = "#0c4c8a") +
      coord_flip() +
      labs(x = "Utilisateurs") +
      theme_minimal())
  })
  
  
  # result_W_python  --  est dans le fichier wordcloud_cesar_python.r
  output$cloud <- renderWordcloud2({
    wordcloud2a(data=reac_dfval()$df_p, size=0.4, color='random-dark', shape = 'diamond')
  })
  
  
  # result_python$utilisateurs -- est dans le fichier 20210208_MOOC_python.R
  output$progressBox_p <- renderInfoBox({
    infoBox(
      "Nombre d'utilisateurs", reac_dfval()$utilisateurs, icon = icon("users"),
      color = "green"
    )
  })
  
  # result_python$plus_actif -- est dans le fichier 20210208_MOOC_python.R
  output$approvalBox_p <- renderInfoBox({
    infoBox(
      "Utilisateur le plus actif", reac_dfval()$plus_actif, icon = icon("send", lib = "glyphicon"), 
      color = "olive"
    )
  })
  
  # Same as above, but with fill=TRUE
  # result_python$publications -- est dans le fichier 20210208_MOOC_python.R
  output$progressBox2_p <- renderInfoBox({
    infoBox(
      "Nombre total de publications", reac_dfval()$publications, icon = icon("comments"),
      color = "green", fill = TRUE
    )
  })
  
  # result_python$pub_plus_actif -- est dans le fichier 20210208_MOOC_python.R
  output$approvalBox2_p <- renderInfoBox({
    infoBox(
      "Nombre de posts", reac_dfval()$pub_plus_actif, icon = icon("thumbs-up", lib = "glyphicon"),
      color = "olive", fill = TRUE
    )
  })
  
  #  fin infoBoxes
}


shinyApp(ui, server)















