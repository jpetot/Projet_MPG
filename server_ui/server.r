#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$mercato_table <- renderTable({
        input$update_mercato
        isolate({
          row_jpref = as.numeric(rownames(note_mpg[note_mpg$Joueur %in% input$jpref ,]))
          
          perf = scale(note_mpg$performance_beta)
          perf[row_jpref,] = 100
          cote = note_mpg$cote_alpha
          poste = as.character(note_mpg$Poste)
          note_mpg$Buts[is.na(note_mpg$Buts)] <- 0
          buts = scale(note_mpg$Buts)
          buts[row_jpref,] = 100
            
            results= MIPModel() %>%
                add_variable(z[i], i = 1:n, type = "binary") %>%
                set_objective(sum_expr((perf[i] + buts[i]) * z[i], i = 1:n), "max") %>%
                add_constraint(sum_expr(z[i], i = 1:n) == nb_joueurs) %>%
                add_constraint(sum_expr(cote[i] * z[i], i = 1:n) <= 500)  %>%
                add_constraint( sum_expr(z[i], i = 1:n, poste[i] == "G") == as.numeric(input$gardiens)) %>%
                add_constraint( sum_expr(z[i], i = 1:n, poste[i] == "D") == as.numeric(input$def)) %>%
                add_constraint( sum_expr(z[i], i = 1:n, poste[i] == "M") == as.numeric(input$mil)) %>%
                add_constraint( sum_expr(z[i], i = 1:n, poste[i] == "A") == as.numeric(input$att))
            
            contraint3 = as.expression(sum_expr(z[i], i = 1:n, poste[i] == "G"))
            contraint4 = as.expression(sum_expr(z[i], i = 1:n, poste[i] == "D"))
            contraint5 = as.expression(sum_expr(z[i], i = 1:n, poste[i] == "M"))
            contraint6 = as.expression(sum_expr(z[i], i = 1:n, poste[i] == "A"))
            
            results$constraints[[3]]$lhs =contraint3
            results$constraints[[4]]$lhs =contraint4
            results$constraints[[5]]$lhs =contraint5
            results$constraints[[6]]$lhs =contraint6
            
            results = solve_model(results , with_ROI(solver = "glpk"))
            results = get_solution(results, z[i])
            results = filter(results, value > 0)
            
            
            mercato = note_mpg[results$i,c("Poste", "Joueur", "Club", "performance_beta", "Cote", "cote_alpha", "Buts")]
            mercato = mercato[order(mercato$Poste),]
        })
    })
    
    output$effectif <- renderTable({
        input$update_team
        isolate({
            effectif = note_mpg[c(note_mpg$Club==input$team),c("Poste","Joueur","Club", "Cote")]
            if(input$tri == "Cote"){
                effectif = effectif[order(-effectif$Cote),]
            } else {
                effectif = effectif[order(effectif$Poste),]
            }
            effectif
        })
    })
    
    output$Top <- renderTable({
        if(input$top == "Buteurs"){
            table <- top_buteur
        } else if (input$top=="Attaquants"){
            table <- top_att
        } else if (input$top=="Defenseurs"){
            table <- top_def
        } else if (input$top=="Milieux"){
            table <- top_mil
        } else if (input$top=="Gardiens"){
            table <- top_G
        } else if (input$top=="Joueurs les plus chers"){
            table <- top_cote
        } else if (input$top=="Joueurs les plus reguliers"){
            table <- top_perf
        } else if (input$top=="Perles rares"){
            table <- top_perle
        } else if (input$top=="Supersub"){
            table <- top_supersub
        } else if (input$top=="Remplacants les plus utilises"){
            table <- top_entrees
        } else {
            table <- top_prolifique
        } 
        table    
    })
    
    output$note_mpg = DT::renderDataTable({
        note_mpg
    })
    
    output$beta = renderPlot({
        plot(jour,beta, main = "Coefficient beta en fonction de la journée de ligue 1")
    })
    
    output$perf_beta_summary = renderPrint({
        summary(note_mpg[,c("performance","performance_beta")])
    }) 
    
    output$alpha = renderPlot({
        plot(x_cote,alpha, main = "Coefficient Alpha en fonction de la cote initial du joueur")
    })
    
    output$text <- renderText({
        paste("n = dim(note_mpg)[1]", "nb_joueurs = 18","perf = scale(note_mpg$performance_beta)",
              "cote = note_mpg$cote_alpha",
              "poste = as.character(note_mpg$Poste)",
              "note_mpg$Buts[is.na(note_mpg$Buts)] <- 0",
              "buts = scale(note_mpg$Buts)",
              " ",
              "results= MIPModel() %>%",
              "  add_variable(z[i], i = 1:n, type = 'binary') %>%",
              "  set_objective(sum_expr((perf[i] + buts[i]) * z[i], i = 1:n), 'max') %>%",
              "  add_constraint(sum_expr(z[i], i = 1:n) == 18) %>%",
              "  add_constraint(sum_expr(cote[i] * z[i], i = 1:n) <= 500)  %>%",
              "  add_constraint( sum_expr(z[i], i = 1:n, poste[i] == 'G') == 2) %>%",
              "  add_constraint( sum_expr(z[i], i = 1:n, poste[i] == 'D') == 6) %>%",
              "  add_constraint( sum_expr(z[i], i = 1:n, poste[i] == 'M') == 6) %>%",
              "  add_constraint( sum_expr(z[i], i = 1:n, poste[i] == 'A') == 4)",
              "",
              "contraint3 = as.expression(sum_expr(z[i], i = 1:n, poste[i] == 'G'))",
              "contraint4 = as.expression(sum_expr(z[i], i = 1:n, poste[i] == 'D'))",
              "contraint5 = as.expression(sum_expr(z[i], i = 1:n, poste[i] == 'M'))",
              "contraint6 = as.expression(sum_expr(z[i], i = 1:n, poste[i] == 'A'))",
              "",
              "results$constraints[[3]]$lhs =contraint3",
              "results$constraints[[4]]$lhs =contraint4",
              "results$constraints[[5]]$lhs =contraint5",
              "results$constraints[[6]]$lhs =contraint6",
              "",
              "results = solve_model(results , with_ROI(solver = 'glpk'))",
              "results = get_solution(results, z[i])",
              "results = filter(results, value > 0)",
              "",
              sep="\n")
    })
    
    output$mercato_ex <- renderTable({
        mercatoEx
    })
    
    output$img_team <- renderUI({
        input$update_team
        isolate({
            if(input$team == "PSG"){
                image_team <- tags$img(src="https://www.gamesatlas.com/images/football/teams/france/psg.png")
            } else if (input$team == "ASSE"){
                image_team <- tags$img(src="https://www.gamesatlas.com/images/football/teams/france/saint-etienne.png")
            } else if (input$team == "OL"){
                image_team <- tags$img(src="https://www.custom-your-pes.com/assets/ol_logo_2004_02.png")
            } else if (input$team == "OM"){
                image_team <- tags$img(src="https://www.gamesatlas.com/images/football/teams/france/olympique-marseille.png")
            } else if (input$team == "FCN"){
                image_team <- tags$img(src="https://www.gamesatlas.com/images/football/teams/france/nantes.png")
            } else if (input$team == "SRFC"){
                image_team <- tags$img(src="https://www.gamesatlas.com/images/football/teams/france/rennes.png")
            } else if (input$team == "LOSC"){
                image_team <- tags$img(src="https://www.gamesatlas.com/images/football/teams/france/lille.png")
            } else if (input$team == "SB29"){
                image_team <- tags$img(src="https://www.gamesatlas.com/images/football/teams/france/brest.png")
            } else if (input$team == "ASM"){
                image_team <- tags$img(src="https://www.gamesatlas.com/images/football/teams/france/as-monaco.png")
            } else if (input$team == "TFC"){
                image_team <- tags$img(src="https://www.gamesatlas.com/images/football/teams/france/toulouse.png")
            } else if (input$team == "RCSA"){
                image_team <- tags$img(src="https://www.gamesatlas.com/images/football/teams/france/strasbourg.png")
            } else if (input$team == "OGCN"){
                image_team <- tags$img(src="https://www.gamesatlas.com/images/football/teams/france/nice.png")
            } else if (input$team == "MHSC"){
                image_team <- tags$img(src="https://www.gamesatlas.com/images/football/teams/france/montpellier.png")
            } else if (input$team == "SCO"){
                image_team <- tags$img(src="https://www.gamesatlas.com/images/football/teams/france/angers-sco.png")
            } else if (input$team == "DFCO"){
                image_team <- tags$img(src="https://www.gamesatlas.com/images/football/teams/france/dijon.png")
            } else if (input$team == "FCGB"){
                image_team <- tags$img(src="https://www.gamesatlas.com/images/football/teams/france/bordeaux.png")
            } else if (input$team == "ASC"){
                image_team <- tags$img(src="https://www.gamesatlas.com/images/football/teams/france/amiens-sc.png")
            } else if (input$team == "SR"){
                image_team <- tags$img(src="https://www.gamesatlas.com/images/football/teams/france/stade-de-reims.png")
            } else if (input$team == "NO"){
                image_team <- tags$img(src="https://www.gamesatlas.com/images/football/teams/france/olympique-marseille.png")
            } else {
                image_team <- tags$img(src="https://www.gamesatlas.com/images/football/teams/france/nimes.png")
            }
            image_team
        })
    })
    
    
})
