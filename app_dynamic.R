# tagal hutan Shiny results demonstrator
# Written by Mairin Deith (mdeith@zoology.ubc.ca)
# First created October 9, 2020

# NOTE: FINISH MS BEFORE WORKING ON THIS DOOFUS IT'S OCT 4
# okay I did it's Oct 9 now.

# Load libraries ----------------------------------------------------------
library(shiny)
library(shinysky)
library(shinyjs)
library(dplyr)
library(ggplot2)
library(rlang)

# Input data --------------------------------------------------------------
res <- readRDS("res_shiny.rds")
# Rename things for prettyness' sake
# levels(res$frequency) <- c(levels(res$frequency),"1 year", "5 years", "10 years", "No rotation")
# res$frequency[res$frequency == "26"] <- "No rotation"
# res$frequency[res$frequency == "1"] <- "1 year"
# res$frequency[res$frequency == "5"] <- "5 years"
# res$frequency[res$frequency == "10"] <- "10 years"

# levels(res$habitatQuality) <-c(levels(res$habitatQuality), "Quality:Good", "Quality:Poor")
# res$habitatQuality[res$habitatQuality == "Poor"] <- "Quality:Poor"
# res$habitatQuality[res$habitatQuality == "Good"] <- "Quality:Good"

# levels(res$habitatQuality) <-c(levels(res$habitatQuality), "Good", "Poor")
# res$habitatQuality[res$habitatQuality == "p"] <- "Poor"
# res$habitatQuality[res$habitatQuality == "g"] <- "Good"

# levels(res$cheatingProp) <-c(levels(res$cheatingProp), "max.NC:0%", "max.NC:15%")
# res$cheatingProp[res$cheatingProp == "max. 0%"] <- "max.NC:0%"
# res$cheatingProp[res$cheatingProp == "max. 15%"] <- "max.NC:15%"

res$averageAnnualHarvestBiomass <- exp(res$log_averageAnnualHarvestBiomass)
# unc_list <- list("none", "huntingHours", "cheatingProp", "habitatQuality")
# names(unc_list) <- c("None", "Hunting hours", "Proportion of non-compliant hunting effort", "Habitat quality")

# uncertainty caps - can only have 2 checkmarks clicked
unc_max <- 2

# UI ----------------------------------------------------------------------
ui <- fluidPage(
    titlePanel("In-silico tagal hutan results"),
    sidebarLayout(
      sidebarPanel(
        helpText("Choose management decisions to consider:"),
        checkboxGroupInput("nres", "Number of no-take reserves", choiceValues = c("0","1","2","3","4"), choiceNames = c("No reserves", "1 (of 4)","2 (of 4)","3 (of 4)","4 (of 4)")),
        checkboxGroupInput("freq", "Frequency of rotation", choiceValues = c("1 year","5 years","10 years","No rotation"), choiceNames = c("1 year", "5 years", "10 years", "No rotation")),
        selectInput("obj", "Objective (for comparison)",
        choices = c(
          "averageAnnualHarvestBiomass",
          "averageNK_Reserve",
          "averageNK_NonReserve",
          "averageAnnualHarvestCV",
          # Species specific criteria:
          "muntHarvestReserve_averageAnnual",
          "muntHarvestNonReserve_averageAnnual",
          "muntHarvestAllPixels_varAnnual",
          "susHarvestReserve_averageAnnual",
          "susHarvestNonReserve_averageAnnual",
          "tragHarvestReserve_averageAnnual",
          "tragHarvestNonReserve_averageAnnual",
          "muntNKReserve_averageAnnual",
          "muntNKNonReserve_averageAnnual",
          "susNKReserve_averageAnnual",
          "susNKNonReserve_averageAnnual",
          "tragNKReserve_averageAnnual",
          "tragNKNonReserve_averageAnnual"
        ),
        selected =  "averageAnnualHarvestBiomass"
        ),
        helpText("(Optional) Which alternative scenarios should be considered? You may only choose up to two - one for columns of plots, another to separate rows of plots. Each row-column combination will represent a combination of system states (e.g. low hunting effort AND high habitat quality). Plotted results will have been averaged across the unselected uncertainty."),
        # checkboxGroupInput(),
        checkboxGroupInput("unc", "Explore alternatives for ... ", choiceValues = c("huntingHours","habitatQuality","cheatingProp"), choiceNames = c("Hunting effort (hours)", "Habitat quality", "Hunter non-compliance (NC)")),
        # selectInput("unc1", "Uncertainty axis 1", choices = unc_list, selected = "none"),
        # selectInput("unc2", "Uncertainty axis 2", choices = unc_list, selected = "none"),
        # checkboxGroupInput("unc", "Uncertainties", choiceNames = c("None","Habitat quality", "Annual hunting hours", "Proportion of cheating hunters"), choiceValues = c("none","habitatQuality", "huntingHours", "cheatingProp"), selected = "none"),
        # actionButton("go", "Plot scenarios"),
        hr(),
        checkboxInput("dectabs", "Print expected value and decision tables?"),
        hr(),
        helpText("(Optional) Plotting options"),
        checkboxInput("medians", "Plot median lines?"),
        checkboxInput("means", "Plot mean lines?"),
        checkboxInput("log", "Plot objective on log scale?")
      ),
      mainPanel(
        busyIndicator(wait = 1000, text = "Please wait"),
        # Distribution outputs
        # textOutput("title"),
        h3("Density plot of simulation outcomes"),
        plotOutput("distPlot", 
          hover = "plot_hover",
          dblclick = "plot1_dblclick",
          brush = brushOpts(
            id = "plot1_brush",
            resetOnNew = TRUE
          )
        ),
        h4("Zoom in/out"),
        h6("Drag a rectangle over the portion of the plot you want to zoom to. Then, double click inside of the rectangle to zoom in."),
        h6("Clear the rectangle by clicking once outside of it. To zoom out, double click with no rectangle selected"),
        h4("Cursor info"),
        h6("Hover your mouse over the plot - the textbox below will tell you the location of your mouse cursor."),
        verbatimTextOutput("info"),
        # Decision table
        hr(),
        h4("Decision tables"),
        h6("Expected value (mean) of possible decisions, and ranking of decisions based on risk averse, risk neutral, and risk prone frameworks.\nIf one or more uncertainties are provided, decision tables for each axis of uncertainty are shown."),
        
        uiOutput("input.dectabs")
        # DT::DT::dataTableOutput("evpitab")
        # DT::dataTableOutput("dectab")
      )
    )
  )


# Server ------------------------------------------------------------------
# Define functions

colMax <- function (colData) {
    apply(colData, MARGIN=c(2), max)
}

rowMax <- function (rowData) {
    apply(rowData, MARGIN=c(1), max)
}

rowMin <- function (rowData) {
    apply(rowData, MARGIN=c(1), min)
}

regretTable <- function (df) {
    colm <- apply(df, MARGIN=c(2), max)
    abs(sweep(df, 2, colm))
}

# expectedUtility <- function(df, decisions, uncertainties, objectives)

decisionTables <- function(df, decisions, uncertainties, unc_names = NULL, objectives, goal = "max", full = F){
  if(is.null(unc_names)){unc_names = uncertainties}
  if(!(goal %in% c("max", "min"))){
      stop("goal must be one of 'max' or 'min'")
  }
  res_list <- list()
  for(o_idx in 1:length(objectives)){
    obj <- objectives[o_idx]
    res_list[[obj]] <- list()
    for(u_idx in 1:length(uncertainties)){
      unc <- uncertainties[u_idx]
      name <- unc_names[u_idx]
      res_list[[obj]][[unc]] <- list()
      unc_props <- df %>% 
        group_by_at(unc) %>%
        summarize(n = length(!!sym(unc))) %>%
        ungroup %>%
        mutate(proportion = n/sum(n)) %>%
        select(-n)
      # EXPECTED UTILITY OF EACH DECISION/UNCERTAINTY COMBINATION
      tmp_df <- data.frame(df %>% 
        dplyr::group_by_at(c(unc, decisions)) %>%
        dplyr::summarize(exp_value = ifelse(goal == "max", 
                                            mean(!!sym(obj), na.rm = T),
                                           -1*mean(!!sym(obj), na.rm = T))) %>%
        tidyr::pivot_wider(names_from = !!sym(unc), values_from = exp_value))
      regret_df <- regretTable(tmp_df[,(length(decisions)+1):ncol(tmp_df)])
      regret_df$maxRegret <- # ifelse(goal == "max", 
                                    rowMax(regret_df) #, 
                             #        rowMin(regret_df))
      regret_df <- cbind(tmp_df[,1:length(decisions)], regret_df)
      regret_df$miniMaxRegret <- ifelse(regret_df$maxRegret == min(regret_df$maxRegret), "*", "")
      
      
      # tmp_df$expectedUtility <- rowSums(tmp_df[,(length(decisions)+1):ncol(tmp_df)]*unc_props$proportion)
      tmp_df$`Expected Utility` <- rowSums(tmp_df[,(length(decisions)+1):ncol(tmp_df)]*unc_props$proportion)
      # Neutral
      min_max_reg <- ifelse(regret_df$miniMaxRegret == "*", "✓", "")
      # Averse
      min_max <- rowMin(tmp_df[,(length(decisions)+1):ncol(tmp_df)])
      # Prone
      max_max <- rowMax(tmp_df[,(length(decisions)+1):ncol(tmp_df)])
      
      tmp_df$`Max. Regret` <- regret_df$maxRegret
      tmp_df$`Risk Averse` <- ifelse(min_max == max(min_max), "✓", "")
      tmp_df$`Risk Neutral` <- min_max_reg
      tmp_df$`Risk Prone` <- ifelse(max_max == max(max_max), "✓", "")
#       evpi$`Risk Averse` <- ifelse(evpi$`Max. Regret`==rowMin(tmp_df[,(length(decisions)+1):ncol(tmp_df)]), "N")        
         
      res_list[[obj]][[unc]][["Expected Value & Decision Table"]] <- tmp_df
      if(full){
        # CALCULATE EXPECTED VALUE OF PERFECT INFORMATION (EVPI)
        evpi_tmp <- colMax(tmp_df[,(length(decisions)+1):(ncol(tmp_df)-1)])*unc_props$proportion
        evpi <- sum(evpi_tmp)-max(tmp_df$expectedUtility)
        names(evpi) <- "Expected Value of Perfect Information"
        names(evpi_tmp) <- paste0("Exp.Value ", names(evpi_tmp))
        evpi <- c(evpi_tmp, evpi)
        # minimax <- rowMin(tmp_df[,(length(decisions)+1):ncol(tmp_df)])
      # minimax <- ifelse(minimax == max(minimax), paste0(signif(minimax, digits=4), "*"), signif(minimax, digits=4))
        #regret_df$minimax <- ifelse(minimax == max(minimax), "*", "")
        #maximax <- rowMax(tmp_df[,(length(decisions)+1):ncol(tmp_df)])
        #regret_df$maximax <- ifelse(maximax == max(maximax), "*", "")
      # maximax <- ifelse(maximax == max(maximax), paste0(signif(maximax, digits=4), "*"), signif(maximax, digits=4))
      # res_list[[obj]][[unc]][["MaxiMax"]] <- cbind(tmp_df[,1:length(decisions)], maximax)
        res_list[[obj]][[unc]][["Expected value of perfect information"]] <- data.frame(evpi)
        res_list[[obj]][[unc]][["Regret Table"]] <- regret_df
      # print(evpi_table)
      # reslist[o_idx][[u_idx]] <- evpi_table
      }
    }
    # Repeat for all uncertainties combined
  }
  return(res_list)
  }

server = function(input, output, session) {
  ranges <- reactiveValues(x = NULL, y = NULL)

    # Keep track of uncertainties 
    observe({
      if(length(input$unc) > unc_max){
        updateCheckboxGroupInput(session, "unc", selected = tail(input$unc,unc_max))
        }
    })
    
    # If 0 or 4 are selected, by default select "26" in rotation
    observe({
      if("0" %in% input$nres & !("No rotation" %in% input$freq)){
        print("zero res")
        updateCheckboxGroupInput(session, "freq", selected = c(input$freq, "No rotation"))
      }
      if("4" %in% input$nres & !("No rotation" %in% input$freq)){
        print("four res")
        updateCheckboxGroupInput(session, "freq", selected = c(input$freq, "No rotation"))
      }
    })
    
    observe({
      if(grepl("_Reserve", input$obj, fixed = T) & "0" %in% input$nres){
        print(input$nres)
        updateCheckboxGroupInput(session, "nres", selected = input$nres[-which(input$nres == "0")])
      }
    })
    
    # Textbox showing where the mouse is hovering
    output$info <- renderText({
      xy_str <- function(e) {
        if(is.null(e)) return("NULL\n")
        paste0(input$obj, " = ", round(e$x, 2), " (% simulations = ", round(e$y, 4), ")")
      }
      xy_str(input$plot_hover)
    })

    dat <- reactive({
      res_tmp <- res[which(res$nreserves %in% input$nres),]
      res_tmp <-  res_tmp[which(res_tmp$frequency %in% input$freq),]
      include_cols <- c("nreserves", "frequency", input$obj, input$unc)
      # if(input$unc1 != "none"){print("inc1");include_cols <- c(include_cols, input$unc1)}
      # if(input$unc2 != "none"){print("inc2");include_cols <- c(include_cols, input$unc2)}#  input$obj, ifelse(input$unc2 == "none", c(input$unc1, input$obj), c(input$unc1, input$unc2, input$obj))))
      print(include_cols)
      res_tmp <- res_tmp[, which(colnames(res_tmp) %in% include_cols)]
      rename_idx <-  which(colnames(res_tmp) == input$obj)
      # print(rename_idx)
      # cat(input$unc)
      colnames(res_tmp)[rename_idx] <- "objective"
      # cat(colnames(res_tmp))
      if(length(input$unc) == 1){
        rn_i <- which(colnames(res_tmp) == input$unc)
        colnames(res_tmp)[rn_i] <- "unc1"
      }
      if(length(input$unc) == 2){
  	rn_i1 <- which(colnames(res_tmp) == input$unc[1])
        colnames(res_tmp)[rn_i1] <- "unc1"
        rn_i2 <- which(colnames(res_tmp) == input$unc[2])
        colnames(res_tmp)[rn_i2] <- "unc2"
      }
      cat(head(res_tmp$unc1))
      cat(head(res_tmp$unc2))
      # cat(paste0("\n\n", colnames(res_tmp)))
      res_tmp
    })

    meds <- reactive({
      dat() %>% group_by(across(where(is.factor))) %>% summarize(median = median(objective, na.rm=T))
    })
    
    means <- reactive({
      dat() %>% group_by(across(where(is.factor))) %>% summarize(meanz = mean(objective, na.rm=T))
    })

    output$distPlot <- renderPlot({
      # print(class(input$unc1))
      # print(paste0("unc1: ", input$unc1, ", unc2: ", input$unc2))
      dat_tmp <- dat()
      cat(paste0("Length unc:", length(input$unc)))
      if(length(input$unc) == 1){
        gp <- ggplot(data = dat_tmp, aes(x = objective, fill = nreserves, colour = nreserves, linetype =  frequency)) +
	  geom_density(alpha = 0.3) + #data = dat_tmp, x = dat_tmp$objective) +
      	  facet_grid(~ unc1, scales = "free_y") +
      	  theme_classic()
      } else if(length(input$unc) == 2){
        print("gp3")
	if(length(unique(dat_tmp$unc1)) == 3){
	  gp <- ggplot(data = dat_tmp, aes(x = objective, colour = nreserves, linetype =  frequency)) +
            geom_density(alpha = 0.3) + 
#            facet_grid(aes(cols = vars(unc1), rows = vars(unc2)), scales = "free_y") +
            facet_grid(unc2 ~ unc1, scales = "free_y") +
            theme_classic()
        } else {
          gp <- ggplot(data = dat_tmp, aes(x = objective, colour = nreserves, linetype =  frequency)) +
            geom_density(alpha = 0.3) + 
            # facet_grid(aes(cols = vars(unc2), rows = vars(unc1)), scales = "free_y") +
            facet_grid(unc1 ~ unc2, scales = "free_y") +
            theme_classic()
        }
      } else {
        print("gp1")
        gp <- ggplot(data = dat_tmp, aes(x = objective, fill = nreserves, linetype =  frequency)) +
          geom_density(alpha = 0.3) + #position = "fill") + # data = dat_tmp, x = dat_tmp$objective) +
          theme_classic()
          }
      if(input$log){
        gp <- gp + scale_x_continuous(trans = "log10")
      }
      if(input$means){
        gp <- gp + geom_vline(data = means(), aes(xintercept = meanz, color = nreserves, linetype =  frequency))
      }
      if(input$medians){
        gp <- gp + geom_vline(data = meds(), aes(xintercept = median, color = nreserves, linetype =  frequency))
        }
        # Finally, implement zooming feature:
      gp + coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) +
        xlab(label = input$obj) + 
        ylab(label = "Density of simulations")
    })
    
    observeEvent(input$plot1_dblclick, {
      brush <- input$plot1_brush
      if (!is.null(brush)) {
        ranges$x <- c(brush$xmin, brush$xmax)
        ranges$y <- c(brush$ymin, brush$ymax)
      } else {
        ranges$x <- NULL
        ranges$y <- NULL
      }
    })
    
# Decision tables -----------------------------------------------------------------------------------
    # output$input.dectabs <- renderUI({})
    
    observe({
      if(input$dectabs == T){
        if(length(input$unc) == 1){
          output$input.dectabs <- renderUI({
            tagList(
              DT::dataTableOutput("dectab")
            )
          })
          if(grepl("CV", input$obj, fixed = TRUE)){
            res <- decisionTables(dat(), decisions = c("nreserves", "frequency"), uncertainties = "unc1", unc_names = paste0(input$unc[1],"\n"), objectives = "objective", goal = "min")
          } else {
            res <- decisionTables(dat(), decisions = c("nreserves", "frequency"), uncertainties = "unc1", unc_names = paste0(input$unc[1],"\n"), objectives = "objective")
          }
          cat(paste0("Length res: ", length(res)))
          colnames(res[[1]][[1]][["Expected Value & Decision Table"]])[1:2] <- c("n Reserves", "Rotation\nfrequency")
          output$dectab <- DT::renderDataTable(res[[1]][[1]][["Expected Value & Decision Table"]])
        } else if(length(input$unc) == 2){
          output$input.dectabs <- renderUI({
            tagList(
              DT::dataTableOutput("dectab1"),
              DT::dataTableOutput("dectab2")
              )
          })
          if(grepl("CV", input$obj, fixed = TRUE)){
            res <- decisionTables(dat(), decisions = c("nreserves", "frequency"), uncertainties = c("unc1", "unc2"), unc_names = c(paste0(input$unc[1],"\n"), paste0(input$unc[2],"\n")), objectives = "objective", goal = "min")
          } else {
            res <- decisionTables(dat(), decisions = c("nreserves", "frequency"), uncertainties = c("unc1", "unc2"), unc_names = c(paste0(input$unc[1],"\n"), paste0(input$unc[2],"\n")), objectives = "objective")
          }
          cat(paste0("Length res: ", length(res)))
          colnames(res[[1]][[1]][["Expected Value & Decision Table"]])[1:2] <- c("n Reserves", "Rotation\nfrequency")
          output$dectab1 <- DT::renderDataTable(res[[1]][[1]][["Expected Value & Decision Table"]])
          output$dectab2 <- DT::renderDataTable(res[[1]][[2]][["Expected Value & Decision Table"]])
           }
        }
    })
  }

shinyApp(ui = ui, server = server)
