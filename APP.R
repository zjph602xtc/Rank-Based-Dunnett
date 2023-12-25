library(shiny)
library(shinydashboard)
library(DT)
library(Hmisc)
library(colourpicker)
library(shinyBS)


source("final function.R")

ui <- dashboardPage(
  dashboardHeader(title = tags$b("Rank Based Dunnett"),titleWidth=430),
  dashboardSidebar(disable=TRUE),
  dashboardBody(
    # tags$head(tags$script('
    #   // Define function to set height of smaller box
    #   setHeight = function() {
    #     var targetheight = $(main_box).height();
    #     $(other_box).height(targetheight);
    #   };
    #   $(document).on("shiny:connected", function(event) {
    #     setHeight();
    #   });
    # 
    #   $(window).on("resize", function(){
    #     setHeight();
    #   });
    # ')),
    tags$head(tags$style(HTML(
      "
      body{
      min-width: 900px;
      }
      "
    ))),
    fluidRow(
      box(title='Biomarker', width=3, id="main_box",
          column(12,
                 tabsetPanel(id='biomarker', 
                             tabPanel("Binary",textInput('bio_bin', label=HTML('Response Rate<br/>(control, g1, g2, ...)'), value = "0.50, 0.60, 0.65, 0.70"), bsTooltip('bio_bin', 'Input response rates of the biomarker')),
                             tabPanel("Cont", textInput('bio_con_mu', label=HTML('Mean Value<br/>(control, g1, g2, ...)'), value = "0.52, 0.56, 0.64, 0.70"), bsTooltip('bio_con_mu', 'Input mean response values of the biomarker'), 
                                      textInput('bio_con_sig', label=HTML('SD<br/>(control, g1, g2, ...)'), value = "0.25, 0.25, 0.25, 0.25"), bsTooltip('bio_con_sig', 'Input sd of response values of the biomarker')),
                             tabPanel("TTE", textInput('bio_tte_hr', label=HTML('Hazard Ratio<br/>(g1 vs con, g2 vs con, ...)'), value = "0.75, 0.75, 0.75"), bsTooltip('bio_tte_hr', 'Input hazard ratios of the biomarker'), 
                                      textInput('bio_tte_num', label=HTML('Number of Events<br/>(g1+con, g2+con, ...)'), value = "47, 47, 47"), bsTooltip('bio_tte_num', 'Input target numbers of events at the end of stage 1')),
                             tabPanel("SES", textInput('bio_ncp', label=HTML('Standardized Effect Size<br/>(control, g1, g2, ...)'), value = "0.40, 1.22, 1.88"), bsTooltip('bio_ncp', 'Input standardized effect sizes of the biomarker'))
                 )
          )
      ),
      box(title='Primary Endpoint', width=3, id = "other_box1",
          column(12,
                 tabsetPanel(id='primary', 
                             tabPanel("Binary",textInput('pri_bin', label=HTML('Response Rate<br/>(control, g1, g2, ...)'), value = "0.52, 0.56, 0.64, 0.7"), bsTooltip('pri_bin', 'Input response rates of the primary endpoint')),
                             tabPanel("Cont", textInput('pri_con_mu', label=HTML('Mean Value<br/>(control, g1, g2, ...)'), value = "0.52, 0.56, 0.64, 0.7"), bsTooltip('pri_con_mu', 'Input mean response values of the primary endpoint'), 
                                      textInput('pri_con_sig', label=HTML('SD<br/>(control, g1, g2, ...)'), value = "0.25, 0.25, 0.25, 0.25"), bsTooltip('pri_con_sig', 'Input sd of response values of the primary endpoint')),
                             tabPanel("TTE", textInput('pri_tte_hr', label=HTML('Hazard Ratio<br/>(g1 vs con, g2 vs con, ...)'), value = "0.70, 0.70, 0.70"), bsTooltip('pri_tte_hr', 'Input hazard ratios of the primary endpoint'), 
                                      textInput('pri_tte_num', label=HTML('Number of Events<br/>(g1+con, g2+con, ...)'), value = "47, 47, 47"), bsTooltip('pri_tte_num', 'Input target numbers of events at the end of stage 1'),
                                      textInput('pri_tte_num_final', label=HTML('Number of Events at final<br/>(g1+con, g2+con, ...)'), value = "331, 331, 331"), bsTooltip('pri_tte_num_final', 'Input target numbers of TOTAL events at the end of stage 2')),
                             tabPanel("SES", textInput('pri_ncp', label=HTML('Standardized Effect Size<br/>(control, g1, g2, ...)'), value = "0.40, 1.22, 1.88"), bsTooltip('pri_ncp', 'Input standardized effect sizes of the primary endpoint'))
                 )
          )
      ),
      box(title="Sample Size", width=3, id = "other_box2",
          column(12,
                 textInput('nums1',label=HTML('Stage 1<br/>(control, g1, g2, ...)'),value = "50, 50, 50, 50"), bsTooltip('nums1', 'Input the sample sizes of each group for stage 1'),
                 textInput('nums2',label=HTML('Stage 2<br/>(control, g1, g2, ...)'),value = "300, 300, 300, 300")), bsTooltip('nums2', 'Input the sample sizes of each group for stage 2 ONLY'),
          conditionalPanel(condition = 'input.primary==\'TTE\'', column(12,checkboxInput("cum_event", "Subjects in stage 1 can occur events in stage 2", value = FALSE)))
      ),
      box(title="Simulation Setting", width=3, id = "other_box3",
          column(12,
                 numericInput('alpha','One sided alpha',value = 0.025), bsTooltip('alpha', 'Input the one sided alpha'),
                 textInput('rholist',label='Rho',value = "0, 0.2, 0.4, 0.6, 0.8, 1.0"), bsTooltip('rholist', 'Input a list of rhos (correlation between the biomarker and the primary endpoint'),
                 numericInput('numsim','Number of Simulations',value = 2000),
                 # numericInput('numseed','Seed',value = 1818),
                 actionButton('model', tags$b('RUN!'))),
      )
    ),
    box(width=13,
        column(3,radioButtons("restype", label = "Output",
                              choices = list("Figure"=1, "Table"=2),inline=TRUE, selected = character(0))
        ),
        # column(4,radioButtons("restype", label = "Output",
        # choices = list("Table"=2),inline=TRUE,selected = character(0))
        # ),
        conditionalPanel(condition = "input.restype==1",
                         column(8,
                                fixedRow(
                                  column(4,selectInput('select_rho', label='Select Rho', choices = c(0, 0.2, 0.4, 0.6, 0.8, 1.0), selected = 0), bsTooltip('select_rho', 'Select a Rho (correlation b/w the biomarker and the primary endpoints) to plot')), column(4, tags$div(style='margin-top:25px',actionButton('advanced', 'Advanced settings')))),
                                column(10,plotOutput('plot_res', height='500px'))
                         ),
        ),
        conditionalPanel(condition = "input.restype==2",
                         column(12, dataTableOutput('dat_table_res'),offset = 0)),
    )
  )
)
server <- function(input, output, session) {
  
  # Rong's paper didn't use cumulative events
  cumulative_event <- FALSE
  observeEvent(input$cum_event, {
    cumulative_event <<- ifelse(input$cum_event, TRUE, FALSE)
  })
  
  # cumulative_event <- FALSE
  
  
  observeEvent(input$model, {
    showModal(
      modalDialog(
        title = "Running Simulation.... Kindly wait.",
        easyClose = F,
        footer = NULL
      )
    )
    
    check_input <- tryCatch({
      # S1N: in stage 1, sample size of control, g1, g2, ....
      S1N <- as.numeric(strsplit(input$nums1, split = ',')[[1]])
      if (any(is.na(S1N))) stop('Please check \'sample size\' for stage 1.')
      if (length(S1N) < 2) stop('\'Sample size\' for stage 1 only contains one control group (NO treatment group).')
      if (any(S1N < 1)) stop('Please check the value of \'sample size\' for stage 1.')
      # number of trt groups
      m <- length(S1N)-1
      # S2N: in stage 2, additional sample size of control, selected group
      S2N <- as.numeric(strsplit(input$nums2, split = ',')[[1]])
      if (any(is.na(S2N))) stop('Please check \'sample size\' for stage 2.')
      if (length(S2N) != (m+1)) stop('\'Sample size\' for stage 2 has a wrong number of groups.')
      if (any(S2N < 1)) stop('Please check the value of \'sample size\' for stage 2.')
      # rep
      rep <- input$numsim
      if (is.na(rep) | rep < 1) stop('Please check the number of simulations.')
      # alpha
      alpha <- input$alpha
      if (is.na(alpha) | alpha > 1 | alpha < 0) stop('Please check alpha.')
      if (length(alpha) > 1) stop('Please put a single alpha value.')
      # rho list
      Rho_list <- as.numeric(strsplit(input$rholist, split = ',')[[1]])
      if (any(is.na(Rho_list))) stop('Please check Rho list.')
      if (length(Rho_list) == 0) stop('Please check Rho list.')
      if (any(Rho_list < 0) | any(Rho_list > 1)) stop('Please check Rho list.')
      
      if (input$biomarker=='Binary'){
        p <- as.numeric(strsplit(input$bio_bin, split = ',')[[1]])
        if (any(is.na(p))) stop('Please check \'response rate\' for the biomarker.')
        if (length(p) != (m+1)) stop('\'Response rate\' for the biomarker has a wrong number of groups.')
        if (any(p<0) | any(p>1)) stop('Please check the value of \'response rate\' for the biomarker.')
        NCP_Bio <- mapply(function(N1, p1)NCP_binary(S1N[1], N1, p[1], p1), S1N[2:(m+1)], p[2:(m+1)])
      }
      if (input$primary=='Binary'){
        p <- as.numeric(strsplit(input$pri_bin, split = ',')[[1]])
        if (any(is.na(p))) stop('Please check \'response rate\' for the primary endpoint')
        if (length(p) != (m+1)) stop('\'Response rate\' for the primary endpoint has a wrong number of groups.')
        if (any(p<0) | any(p>1)) stop('Please check the value of \'response rate\' for the primary endpoint')
        NCP_Pri <- mapply(function(N1, p1)NCP_binary(S1N[1], N1, p[1], p1), S1N[2:(m+1)], p[2:(m+1)])
        NCP_Pri_Final <- mapply(function(N1, p1)NCP_binary(S2N[1], N1, p[1], p1), S2N[2:(m+1)], p[2:(m+1)])
      }
      
      if (input$biomarker=='Cont'){
        mu <- as.numeric(strsplit(input$bio_con_mu, split = ',')[[1]])
        if (any(is.na(mu))) stop('Please check \'mean response value\' for the biomarker')
        if (length(mu) != (m+1)) stop('\'Mean response value\' for the biomarker has a wrong number of groups.')
        sig <- as.numeric(strsplit(input$bio_con_sig, split = ',')[[1]])
        if (any(is.na(sig))) stop('Please check \'sd of response values\' for the biomarker')
        if (length(sig) != (m+1)) stop('\'Sd of response values\' for the biomarker has a wrong number of groups.')
        if (any(sig<0)) stop('Please check the value of \'sd of response values\' for the biomarker.')
        NCP_Bio <- mapply(function(N1, mu1, sig1)NCP_normal(S1N[1], N1, mu[1], sig[1], mu1, sig1), S1N[2:(m+1)], mu[2:(m+1)], sig[2:(m+1)])
      }
      if (input$primary=='Cont'){
        mu <- as.numeric(strsplit(input$pri_con_mu, split = ',')[[1]])
        if (any(is.na(mu))) stop('Please check \'mean response value\' for the primary endpoint')
        if (length(mu) != (m+1)) stop('\'Mean response value\' for the primary endpoint has a wrong number of groups.')
        sig <- as.numeric(strsplit(input$pri_con_sig, split = ',')[[1]])
        if (any(is.na(sig))) stop('Please check\'sd of response values\' for the primary endpoint')
        if (length(sig) != (m+1)) stop('\'Sd of response values\' for the primary endpoint has a wrong number of groups.')
        if (any(sig<0)) stop('Please check the value of \'sd of response values\' for the primary endpoint.')
        NCP_Pri <- mapply(function(N1, mu1, sig1)NCP_normal(S1N[1], N1, mu[1], sig[1], mu1, sig1), S1N[2:(m+1)], mu[2:(m+1)], sig[2:(m+1)])
        NCP_Pri_Final <- mapply(function(N1, mu1, sig1)NCP_normal(S2N[1], N1, mu[1], sig[1], mu1, sig1), S2N[2:(m+1)], mu[2:(m+1)], sig[2:(m+1)])
      }
      
      if (input$biomarker=='TTE'){
        hr <- as.numeric(strsplit(input$bio_tte_hr, split = ',')[[1]])
        if (any(is.na(hr))) stop('Please check \'hazard ratio\' for the biomarker.')
        if (length(hr) != (m)) stop('\'Hazard ratio\' for the biomarker has a wrong length.')
        if (any(hr<0)) stop('Please check the value of \'hazard ratio\' for the biomarker.')
        num_event <- as.numeric(strsplit(input$bio_tte_num, split = ',')[[1]])
        if (any(is.na(num_event))) stop('Please check \'target number of events\' for the biomarker.')
        if (length(num_event) != (m)) stop('\'Target number of events\' for the biomarker has a wrong length')
        if (any(num_event > (S1N[1]+S1N[2:(1+m)]))) stop('Please check \'target number of events\' for the biomarker. They cannot be larger than the sample sizes at stage 1')
        NCP_Bio <- mapply(function(Num_event, HR)NCP_TTE(Num_event, HR), num_event, hr)
      }
      if (input$primary=='TTE'){
        hr <- as.numeric(strsplit(input$pri_tte_hr, split = ',')[[1]])
        if (any(is.na(hr))) stop('Please check \'hazard ratio\' for the primary endpoint')
        if (length(hr) != (m)) stop('\'Hazard ratio\' for the primary endpoint has a wrong length.')
        if (any(hr<0)) stop('Please check \'hazard ratio\' for the primary endpoint.')
        num_event <- as.numeric(strsplit(input$pri_tte_num, split = ',')[[1]]) # number of total events at IA
        if (any(is.na(num_event))) stop('Please check \'target number of events\' for the primary endpoint at stage 1')
        if (length(num_event) != (m)) stop('\'Target number of events\' for the primary endpoint at stage 1 has a wrong length')
        if (any(num_event > (S1N[1]+S1N[2:(1+m)]))) stop('Please check \'target number of events\' for the primary endpoint at stage 1. They cannot be larger than the sample sizes at stage 1')
        num_event_final <- as.numeric(strsplit(input$pri_tte_num_final, split = ',')[[1]]) # cumulative number of total events at FA
        if (any(is.na(num_event_final))) stop('Please check \'target number of total events\' for the primary endpoint.')
        if (length(num_event_final) != (m)) stop('\'Target number of total events\' for the primary endpoint has a wrong length')
        if (any(num_event_final > (S1N[1]+S1N[2:(m+1)]+S2N[1]+S2N[2:(m+1)]))) stop('Please check \'target number of total events\' for the primary endpoint. They cannot be larger than sum of sample sizes at stage 1 and stage 2')
        if (any(num_event_final < num_event)) stop('Please check \'target number of events at stage 1\' and \'target number of total events\' for the primary endpoint.The target numbers of total events should always be larger.')
        NCP_Pri <- mapply(function(Num_event, HR)NCP_TTE(Num_event, HR), num_event, hr)
        if (cumulative_event){
          NCP_Pri_Final <- mapply(function(Num_event, HR)NCP_TTE(Num_event, HR), num_event_final, hr) # this is cumulative!!
        }else{
          NCP_Pri_Final <- mapply(function(Num_event, HR)NCP_TTE(Num_event, HR), num_event_final - num_event, hr) # this is incremental!!
          # NCP_Pri_Final <- mapply(function(Num_event, HR)NCP_TTE(Num_event, HR), num_event*S2N[2:(m+1)]/S1N[2:(m+1)], hr) # previous approach in the paper, the number of final events is based on the sample size only. 
        }
      }
      
      if (input$biomarker=='SES'){
        NCP_Bio <- as.numeric(strsplit(input$bio_ncp , split = ',')[[1]])
        if (any(is.na(NCP_Bio))) stop('Please check SES for the biomarker.')
        if (length(NCP_Bio) != (m)) stop('SES for the biomarker has a wrong length.')
      }
      if (input$primary=='SES'){
        NCP_Pri <- as.numeric(strsplit(input$pri_ncp, split = ',')[[1]])
        if (any(is.na(NCP_Pri))) stop('Please check SES for the primary endpoint')
        if (length(NCP_Pri) != (m)) stop('SES for the primary endpoint has a wrong length.')
        NCP_Pri_Final <- NCP_Pri/sqrt(S1N[2:(m+1)])*sqrt(S2N[2:(m+1)])
      }
      
      
      withProgress(message = 'Starting...', value = 0, {
        if (input$primary=='TTE' & cumulative_event){
          result <<- rank_based_Dunnett(S1N, S2N, NCP_Bio, NCP_Pri, NCP_Pri_Final, rep, alpha, Rho_list, prog=TRUE, seed=1818, 
                                        TTE_Primary = TRUE, num_events_s1 = num_event, num_events_final = num_event_final)
        }else{
          result <<- rank_based_Dunnett(S1N, S2N, NCP_Bio, NCP_Pri, NCP_Pri_Final, rep, alpha, Rho_list, prog=TRUE, seed=1818)
        }
        
      })
      updateRadioButtons(session, 'restype', selected = "1")
      removeModal()
      updateSelectInput(session, 'select_rho', choices = Rho_list, selected=Rho_list[1])
      updateRadioButtons(session, 'restype', selected = "2")
    }, error = function(e){
      removeModal()
      showModal(
        modalDialog(
          title = paste0("Check Error: ", e$message),
          easyClose = T,
          footer = NULL
        )
      )
    })
    
  })
  
  
  
  # line plot setting ------------------------------------------------------------
  plot_par <- list(pri_s1_D=list(plot=F,col='#ac92eb',lwd=2), pri_s1_N=list(plot=F,col='#ffce54',lwd=2),
                   pri_s2=list(plot=F,col='#4fc1e8',lwd=2), D=list(plot=T,col='#a0d568',lwd=2), 
                   N=list(plot=T,col='#ed5564',lwd=2))
  observeEvent(input$advanced, {
    showModal(modalDialog(
      title = "Plot Z stat for the primary endpoint",
      div(
        fluidRow(
          column(12,
                 checkboxInput("shows1D", "Plot Z stat at 1st stage (with Dunnet adjustment)", value = FALSE))),
        fluidRow(
          column(6,
                 colourInput("s1Dcol", "Line Color", "#ac92eb")),
          column(6,
                 sliderInput("s1Dlwd", "Line Width", min = 0.1, max = 5, value = 1.5))
        ),
        tags$hr(style = "border-top: 1px dashed #000; margin: 10px 0;"),
        fluidRow(
          column(12,
                 checkboxInput("shows1N", "Plot Z stat at 1st stage (without adjustment)", value = FALSE))),
        fluidRow(
          column(6,
                 colourInput("s1Ncol", "Line Color", "#ffce54")),
          column(6,
                 sliderInput("s1Nlwd", "Line Width", min = 0.1, max = 5, value = 1.5))
        ),
        tags$hr(style = "border-top: 1px dashed #000; margin: 10px 0;"),
        if (input$primary=='TTE'){
          fluidRow(
            column(12,
                   checkboxInput("shows2", "Plot incremental Z stat at 2nd stage", value = FALSE)))
        }else{
          fluidRow(
            column(12,
                   checkboxInput("shows2", "Plot Z stat at 2nd stage", value = FALSE)))
        },
        fluidRow(
          column(6,
                 colourInput("s2col", "Line Color", "#4fc1e8")),
          column(6,
                 sliderInput("s2lwd", "Line Width", min = 0.1, max = 5, value = 1.5))
        ),
        tags$hr(style = "border-top: 1px dashed #000; margin: 10px 0;"),
        fluidRow(
          column(12,
                 checkboxInput("showD", "Plot combined Z stat (with Dunnet adjustment)", value = FALSE))),
        fluidRow(
          column(6,
                 colourInput("Dcol", "Line Color", "#a0d568")),
          column(6,
                 sliderInput("Dlwd", "Line Width", min = 0.1, max = 5, value = 1.5))
        ),
        tags$hr(style = "border-top: 1px dashed #000; margin: 10px 0;"),
        fluidRow(
          column(12,
                 checkboxInput("showN", "Plot combined Z stat (without adjustment)", value = FALSE))),
        fluidRow(
          column(6,
                 colourInput("Ncol", "Line Color", "#ed5564")),
          column(6,
                 sliderInput("Nlwd", "Line Width", min = 0.1, max = 5, value = 1.5))
        )
      ),
      footer = actionButton("Submit","Submit")
    ))
    
    updateCheckboxInput(session, "shows1D", value=plot_par$pri_s1_D$plot)
    updateColourInput(session, "s1Dcol", value=plot_par$pri_s1_D$col)
    updateSliderInput(session, "s1Dlwd", value=plot_par$pri_s1_D$lwd)
    updateCheckboxInput(session,"shows1N", value=plot_par$pri_s1_N$plot)
    updateColourInput(session, "s1Ncol", value=plot_par$pri_s1_N$col)
    updateSliderInput(session, "s1Nlwd", value=plot_par$pri_s1_N$lwd)
    updateCheckboxInput(session, "shows2", value=plot_par$pri_s2$plot)
    updateColourInput(session, "s2col", value=plot_par$pri_s2$col)
    updateSliderInput(session, "s2lwd", value=plot_par$pri_s2$lwd)
    updateCheckboxInput(session, "showD", value=plot_par$D$plot)
    updateColourInput(session, "Dcol", value=plot_par$D$col)
    updateSliderInput(session, "Dlwd", value=plot_par$D$lwd)
    updateCheckboxInput(session, "showN", value=plot_par$N$plot)
    updateColourInput(session, "Ncol", value=plot_par$N$col)
    updateSliderInput(session, "Nlwd", value=plot_par$N$lwd)
  })
  
  observeEvent(input$Submit, {
    updateRadioButtons(session, 'restype', selected = "2")
    plot_par$pri_s1_D$plot <<- input$shows1D
    plot_par$pri_s1_D$col <<- input$s1Dcol
    plot_par$pri_s1_D$lwd <<- input$s1Dlwd
    plot_par$pri_s1_N$plot <<- input$shows1N
    plot_par$pri_s1_N$col <<- input$s1Ncol
    plot_par$pri_s1_N$lwd <<- input$s1Nlwd
    plot_par$pri_s2$plot <<- input$shows2
    plot_par$pri_s2$col <<- input$s2col
    plot_par$pri_s2$lwd <<- input$s2lwd
    plot_par$D$plot <<- input$showD
    plot_par$D$col <<- input$Dcol
    plot_par$D$lwd <<- input$Dlwd
    plot_par$N$plot <<- input$showN
    plot_par$N$col <<- input$Ncol
    plot_par$N$lwd <<- input$Nlwd
    removeModal()
    updateRadioButtons(session, 'restype', selected = "1")
  })
  
  
  observeEvent(input$restype, {
    if (input$restype=="1"){
      output$plot_res <- renderPlot(bg="#F5F8FF",{
        if (exists('result')){
          s1Ddata <- result$Z_stat[[paste0('rho=', input$select_rho)]]$Z.primary_s1.D
          s1Ndata <- result$Z_stat[[paste0('rho=', input$select_rho)]]$Z.primary_s1.N
          s2data <- result$Z_stat[[paste0('rho=', input$select_rho)]]$Z.primary_s2
          Ddata <- result$Z_stat[[paste0('rho=', input$select_rho)]]$Z.D
          Ndata <- result$Z_stat[[paste0('rho=', input$select_rho)]]$Z.N
          
          s1Drange <- quantile(s1Ddata, c(0.025, 0.975))
          s1Nrange <- quantile(s1Ndata, c(0.025, 0.975))
          s2range <- quantile(s2data, c(0.025, 0.975))
          Drange <- quantile(Ddata, c(0.025, 0.975))
          Nrange <- quantile(Ndata, c(0.025, 0.975))
          
          plot_which <- which(c(plot_par$pri_s1_D$plot, plot_par$pri_s1_N$plot, plot_par$pri_s2$plot, plot_par$D$plot, plot_par$N$plot))
          plot_range <- range(unlist(list(s1Drange, s1Nrange, s2range, Drange, Nrange)[plot_which]))
          
          lty <- rep(1:6,100)
          par(mar = c(3.3, 3.3, 3.5, 1.5))
          par(mgp = c(2,0.8,0))
          m <- length(result$Power)/2
          
          legend_name <- NULL
          legend_lwd <- NULL
          legend_col <- NULL
          legend_lty <- NULL
          if (plot_par$pri_s1_D$plot){
            for (kk in 1:m){
              Ecdf(s1Ddata[,kk], lwd = plot_par$pri_s1_D$lwd, col = plot_par$pri_s1_D$col, xlab = "Z statistics", ylab = "Empirical CDF", lty = lty[kk], 
                   add = (kk!=1)|min(plot_which)!=1, xlim=plot_range, subtitles = F, main = 'Empirical CDF of selected Z statistics')
              legend_name <- c(legend_name, paste0('Rank=',kk,' (1st stage) with Dunnet Adj'))
              legend_lwd <- c(legend_lwd, plot_par$pri_s1_D$lwd)
              legend_col <- c(legend_col, plot_par$pri_s1_D$col)
              legend_lty <- c(legend_lty, lty[kk])
            }
          }
          if (plot_par$pri_s1_N$plot){
            for (kk in 1:m){
              Ecdf(s1Ndata[,kk], lwd = plot_par$pri_s1_N$lwd, col = plot_par$pri_s1_N$col, xlab = "Z statistics", ylab = "Empirical CDF", lty = lty[kk], 
                   add = (kk!=1)|min(plot_which)!=2, xlim=plot_range, subtitles = F, main = 'Empirical CDF of selected Z statistics')
              legend_name <- c(legend_name, paste0('Rank=',kk,' (1st stage) without Adj'))
              legend_lwd <- c(legend_lwd, plot_par$pri_s1_N$lwd)
              legend_col <- c(legend_col, plot_par$pri_s1_N$col)
              legend_lty <- c(legend_lty, lty[kk])
            }
          }
          if (plot_par$pri_s2$plot){
            for (kk in 1:m){
              Ecdf(s2data[,kk], lwd = plot_par$pri_s2$lwd, col = plot_par$pri_s2$col, xlab = "Z statistics", ylab = "Empirical CDF", lty = lty[kk], 
                   add = (kk!=1)|min(plot_which)!=3, xlim=plot_range, subtitles = F, main = 'Empirical CDF of selected Z statistics')
              legend_name <- c(legend_name, paste0('Rank=',kk,' (2st stage)'))
              legend_lwd <- c(legend_lwd, plot_par$pri_s2$lwd)
              legend_col <- c(legend_col, plot_par$pri_s2$col)
              legend_lty <- c(legend_lty, lty[kk])
            }
          }
          if (plot_par$D$plot){
            for (kk in 1:m){
              Ecdf(Ddata[,kk], lwd = plot_par$D$lwd, col = plot_par$D$col, xlab = "Z statistics", ylab = "Empirical CDF", lty = lty[kk], 
                   add = (kk!=1)|min(plot_which)!=4, xlim=plot_range, subtitles = F, main = 'Empirical CDF of selected Z statistics')
              legend_name <- c(legend_name, paste0('Rank=',kk,' (combined stages) with Dunnet Adj'))
              legend_lwd <- c(legend_lwd, plot_par$D$lwd)
              legend_col <- c(legend_col, plot_par$D$col)
              legend_lty <- c(legend_lty, lty[kk])
            }
          }
          if (plot_par$N$plot){
            for (kk in 1:m){
              Ecdf(Ndata[,kk], lwd = plot_par$N$lwd, col = plot_par$N$col, xlab = "Z statistics", ylab = "Empirical CDF", lty = lty[kk], 
                   add = (kk!=1)|min(plot_which)!=5, xlim=plot_range, subtitles = F, main = 'Empirical CDF of selected Z statistics')
              legend_name <- c(legend_name, paste0('Rank=',kk,' (combined stages) without Adj'))
              legend_lwd <- c(legend_lwd, plot_par$N$lwd)
              legend_col <- c(legend_col, plot_par$N$col)
              legend_lty <- c(legend_lty, lty[kk])
            }
          }
          legend('topleft', paste0('Biomarker-Efficacy Correlation=', input$select_rho))
          
          legend('bottomright', legend = legend_name, col = legend_col, lty = legend_lty, lwd = legend_lwd)
        }else{
          NULL
        }
      })
    }else if (input$restype=="2"){
      output$dat_table_res <- DT::renderDataTable({
        if (exists('result')){
          formatRound(datatable(result$Power,extensions = c("Scroller"),
                                options = list(
                                  scroller = TRUE,
                                  scrollX = TRUE,
                                  scrollY = "400px",
                                  searching=FALSE,
                                  columnDefs=list(list(orderable=FALSE,targets='_all'))
                                ), colnames = colnames(result$Power)), digits=4, columns = colnames(result$Power))
        }else
          NULL
      })
    }
  })
  
  
}

# runGadget(ui, server, viewer = dialogViewer("Test Demo", width = 1400))

shinyApp(ui = ui, server = server)