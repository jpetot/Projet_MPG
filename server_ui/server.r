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
            nb_goal = as.numeric(input$gardiens)
            nb_def = as.numeric(input$def)
            nb_mil = as.numeric(input$mil)
            nb_att = as.numeric(input$att)
            nb_joueurs = nb_goal + nb_def + nb_mil + nb_att
            poste = as.character(note_mpg$Poste)
            row_jpref = as.numeric(rownames(note_mpg[note_mpg$Joueur %in% input$jpref ,]))
            row_jdet = as.numeric(rownames(note_mpg[note_mpg$Joueur %in% input$jdet ,]))
            
            ## rajout du bruit sur la cote ##
            bruit = round(rnorm(520,0,1.5),0)
            cote = note_mpg$cote_alpha + bruit
            
            #on normalise la performance et le nombre de but, puis on modife pour les joueurs particulier.
            perf = scale(note_mpg$performance_beta)
            perf[row_jpref,] = 100
            perf[row_jdet,] = -100
            note_mpg$Buts[is.na(note_mpg$Buts)] <- 0
            buts = scale(note_mpg$Buts)
            buts[row_jpref,] = 100
            buts[row_jdet,] = -100
            
            results= MIPModel() %>%
                add_variable(z[i], i = 1:n, type = "binary") %>%
                set_objective(sum_expr((perf[i] + buts[i]) * z[i], i = 1:n), "max") %>%
                add_constraint(sum_expr(z[i], i = 1:n) == nb_joueurs) %>%
                add_constraint(sum_expr(cote[i] * z[i], i = 1:n) <= 500)  %>%
                add_constraint( sum_expr(z[i], i = 1:n, poste[i] == "G") == nb_goal) %>%
                add_constraint( sum_expr(z[i], i = 1:n, poste[i] == "D") == nb_def) %>%
                add_constraint( sum_expr(z[i], i = 1:n, poste[i] == "M") == nb_mil) %>%
                add_constraint( sum_expr(z[i], i = 1:n, poste[i] == "A") == nb_att) 
            
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
            effectif = note_mpg[c(note_mpg$Club==input$team),c("Poste","Joueur","Club", "Cote", "Moyenne_note")]
            if(input$tri == "Cote"){
                effectif = effectif[order(-effectif$Cote),]
            } else if (input$tri == "Moyenne note") {
                effectif = effectif[order(-effectif$Moyenne_note),]
            } else {
                effectif = effectif[order(effectif$Poste),]
            }
            effectif
        })
    })
    
    output$top_buteur <- renderTable({
        top_buteur        
    })
    
    output$top_att <- renderTable({
        top_att
    })
    
    output$top_mil <- renderTable({
        top_mil
    })
    output$top_def <- renderTable({
        top_def
    })
    
    output$top_G <- renderTable({
        top_G
    })
    
    output$top_perf <- renderTable({
        top_perf
    })
    
    output$top_cote <- renderTable({
        top_cote
    })
    
    output$top_perle <- renderTable({
        top_perle
    })
    
    output$top_supersub <- renderTable({
        top_supersub
    })
    
    output$top_entrees <- renderTable({
        top_entrees
    })
    
    output$top_prolifique <- renderTable({
        top_prolifique
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
        paste("nb_goal = as.numeric(input$gardiens) 
              nb_def = as.numeric(input$def)  
              nb_mil = as.numeric(input$mil)
              nb_att = as.numeric(input$att)
              nb_joueurs = nb_goal + nb_def + nb_mil + nb_att
              poste = as.character(note_mpg$Poste)
              row_jpref = as.numeric(rownames(note_mpg[note_mpg$Joueur %in% input$jpref ,]))
              row_jdet = as.numeric(rownames(note_mpg[note_mpg$Joueur %in% input$jdet ,]))
              
              ## rajout du bruit sur la cote ##
              bruit = round(rnorm(520,0,1.5),0)
              cote = note_mpg$cote_alpha + bruit
              
              #on normalise la performance et le nombre de but, puis on modife pour les joueurs particulier.
              perf = scale(note_mpg$performance_beta)
              perf[row_jpref,] = 100
              perf[row_jdet,] = -100
              note_mpg$Buts[is.na(note_mpg$Buts)] <- 0
              buts = scale(note_mpg$Buts)
              buts[row_jpref,] = 100
              buts[row_jdet,] = -100
              
              results= MIPModel() %>%
              add_variable(z[i], i = 1:n, type = 'binary') %>%
              set_objective(sum_expr((perf[i] + buts[i]) * z[i], i = 1:n), 'max') %>%
              add_constraint(sum_expr(z[i], i = 1:n) == nb_joueurs) %>%
              add_constraint(sum_expr(cote[i] * z[i], i = 1:n) <= 500)  %>%
              add_constraint( sum_expr(z[i], i = 1:n, poste[i] == 'G) == nb_goal) %>%
              add_constraint( sum_expr(z[i], i = 1:n, poste[i] == 'D) == nb_def) %>%
              add_constraint( sum_expr(z[i], i = 1:n, poste[i] == 'M) == nb_mil) %>%
              add_constraint( sum_expr(z[i], i = 1:n, poste[i] == 'A') == nb_att) 
              
              contraint3 = as.expression(sum_expr(z[i], i = 1:n, poste[i] == 'G'))
              contraint4 = as.expression(sum_expr(z[i], i = 1:n, poste[i] == 'D'))
              contraint5 = as.expression(sum_expr(z[i], i = 1:n, poste[i] == 'M'))
              contraint6 = as.expression(sum_expr(z[i], i = 1:n, poste[i] == 'A'))
              
              results$constraints[[3]]$lhs =contraint3
              results$constraints[[4]]$lhs =contraint4
              results$constraints[[5]]$lhs =contraint5
              results$constraints[[6]]$lhs =contraint6
              
              results = solve_model(results , with_ROI(solver = 'glpk'))
              results = get_solution(results, z[i])
              results = filter(results, value > 0)
              
              
              mercato = note_mpg[results$i,c('Poste', 'Joueur', 'Club', 'performance_beta', 'Cote', 'cote_alpha', 'Buts')]
              mercato = mercato[order(mercato$Poste),]")
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
                image_team <- tags$img(src="https://www.gamesatlas.com/images/football/teams/france/nimes.png")
            } else {
                image_team <- tags$img(src="https://www.gamesatlas.com/images/football/teams/france/metz.png")
            }
            image_team
        })
    })
    
})