library(shiny)
library(epiworldR)

function(input, output) {

  
    model_output <- eventReactive(input$simulate, {
        if(input$model == "SEIR"){
            model_seir <- ModelSEIR(name = input$seir_disease_name, 
                                    prevalence = input$seir_prevalence,
                                    transmission_rate = input$seir_transmission_rate, 
                                    recovery_rate = input$seir_recovery_rate, 
                                    incubation_days = input$seir_incubation_days)
            agents_smallworld(
                model_seir,
                n = input$seir_population_size,
                k = input$seir_k,
                d = as.logical(input$seir_directed),
                p = input$seir_prob_rewiring
            )
            # Running and printing
            verbose_off(model_seir)
            run(model_seir, ndays = input$seir_n_days, seed = input$seir_seed)
            # Plot
            plot_seir <- function() plot(model_seir, main = "SEIR Model")
            # Summary
            summary_seir <- function() summary(model_seir)
            # Reproductive Number
            reproductive_seir <- function() 
              plot_reproductive_number(
                model_seir,
                main = "SEIR Model Reproductive Number"
                )
            # Output list
            return(list(
              plot_epicurves         = plot_seir,
              plot_reproductive = reproductive_seir,
              print_summary      = summary_seir
              ))
         }
        if(input$model == "SIR"){
            model_sir <- ModelSIR(name = input$sir_disease_name,
                                  prevalence = input$sir_prevalence,
                                  transmission_rate = input$sir_transmission_rate,
                                  recovery_rate = input$sir_recovery_rate)
            agents_smallworld(
                model_sir,
                n = input$sir_population_size,
                k = input$sir_k,
                d = as.logical(input$sir_directed),
                p = input$sir_prob_rewiring
            )
            # Running and printing
            verbose_off(model_sir)
            run(model_sir, ndays = input$sir_n_days, seed = input$sir_seed)
            # Plot
            plot_sir <- function() plot(model_sir, main = "SIR Model")
            # Summary
            summary_sir <- function() summary(model_sir)
            # Reproductive Number
            reproductive_sir <- function()
              plot_reproductive_number(
                model_sir,
                main = "SIR Model Reproductive Number"
                )
            # Output list
            return(list(
              plot_epicurves         = plot_sir,
              plot_reproductive = reproductive_sir,
              print_summary      = summary_sir
              ))
        }
        # if(input$model == "SIS"){
        #     model_sis <- ModelSIS(name = input$sis_disease_name,
        #                           prevalence = input$sis_prevalence,
        #                           transmission_rate = input$sis_transmission_rate,
        #                           recovery_rate = input$sis_recovery_rate)
        #     agents_smallworld(
        #         model_sis,
        #         n = input$sis_population_size,
        #         k = input$sis_k,
        #         d = as.logical(input$sis_directed),
        #         p = input$sis_prob_rewiring
        #     )
        #     # Running and printing
        #     verbose_off(model_sis)
        #     run(model_sis, ndays = input$sis_n_days, seed = input$sis_seed)
        #     # Plot
        #     plot_sis <- function() plot(model_sis, main = "SIS Model")
        #     # Summary
        #     summary_sis <- function() summary(model_sis)
        #     # Reproductive Number
        #     reproductive_sis <- function()
        #       plot_reproductive_number(
        #         model_sis,
        #         main = "SIS Model Reproductive Number"
        #         )
        #     # Output list
        #     list(
        #       plot_sis         = plot_sis,
        #       reproductive_sis = reproductive_sis,
        #       summary_sis      = summary_sis
        #      
        #       )
        # }
        # if(input$model == "SEIRCONNECTED"){
        # 
        #     model_seirconn <- ModelSEIRCONN(name = input$seirconn_disease_name,
        #                           prevalence = input$seirconn_prevalence,
        #                           transmission_rate = input$seirconn_transmission_rate,
        #                           recovery_rate = input$seirconn_recovery_rate,
        #                           contact_rate = input$seirconn_contact_rate,
        #                           n = input$seirconn_population_size,
        #                           incubation_days = input$seirconn_incubation_days
        #                           )
        #     # Running and printing
        #     verbose_off(model_seirconn)
        #     run(model_seirconn, ndays = input$seirconn_n_days, seed = input$seirconn_seed)
        #     # Plot
        #     plot_seirconn <- function() plot(model_seirconn, main = "SEIRCONNECTED Model")
        #     # Summary
        #     summary_seirconn <- function() summary(model_seirconn)
        #     # Reproductive Number
        #     reproductive_seirconn <- function()
        #       plot_reproductive_number(
        #         model_seirconn,
        #         main = "SEIRCONN Model Reproductive Number"
        #         )
        #     # Output list
        #     list(
        #       plot_seirconn         = plot_seirconn,
        #       reproductive_seirconn = reproductive_seirconn,
        #       summary_seirconn      = summary_seirconn
        #       )
        # }
    })

    # Displaying Plots and Model Summary
    output$model_plot <- renderPlot({
            model_output()$plot_epicurves()
    })
    output$model_reproductive_plot <- renderPlot({
            model_output()$plot_reproductive()
    })
    output$model_summary <- renderPrint({
            model_output()$print_summary()
    })
    # output$model_table <- renderPrint({
    #       model_output()$data_table_seir()
    # })
}

