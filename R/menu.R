#' @title Menu selection des variables
#' @description
#' Menu de selection des variables éléments gardés pour une analyse.
#'
#' @param df Un dataframe.
#'
#' @return Un dataframe sans les éléments déselectionnés.
#' @export
#' @import shiny
#' @importFrom shiny fluidPage titlePanel sidebarLayout sidebarPanel checkboxGroupInput actionButton mainPanel tableOutput renderTable
#' @importFrom shiny observeEvent showNotification shinyApp
#' @importFrom utils data
#' @examples
#' ##Not run:
#' #data('rock')
#' #selection_menu(rock)
#' ##End(Not run)
#'
#'
selection_menu <- function(df) {
  garde_columns <- c("CaO",	"Fe2O3",	"TiO2",	"K2O",	"SiO2",	"Al2O3",	"MgO",	"MnO","Zr",	"Sr",	"Rb",	"Zn",	"Cr",	"Ni",	"La",	"Ba","V",	"Ce")
  preselection <- setdiff(colnames(df), garde_columns)

  if ("Groupe" %in% colnames(df)){preselection<-c("Groupe",preselection)}
  # Interface utilisateur
  ui <- fluidPage(
    titlePanel("Cochez les cases correspondant a ceux que vous ne souhaitez PAS garder"),
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput("columns", "Colonnes a supprimer :",
                           choices = colnames(df),
                           selected = preselection ), # Colonnes pre-selectionnees

        actionButton("apply_changes", "Valider") # Bouton de validation
      ),
      mainPanel(
        tableOutput("table")
      )
    )
  )

  server <- function(input, output, session) {

    # Tableau initial (sans modification)
    output$table <- renderTable({
      df
    })

    # Mettre a jour le tableau apres avoir clique sur "Valider"
    observeEvent(input$apply_changes, {
      non_filtered_df <- df[, (colnames(df) %in% input$columns)]
      filtered_df <- df[, !(colnames(df) %in% input$columns)]  # Filtrer les colonnes
      output$table <- renderTable({
        filtered_df
      })
     # res <- list(selection=filtered_df,exclusion=non_filtered_df)
      #assign("df_transformed", filtered_df, envir = .GlobalEnv)  # Sauvegarder le data frame transforme
      showNotification("Fichier transforme sauvegarde en tant que df_transformed", type = "message")
      stopApp(returnValue =filtered_df)
    })
  }
  shiny::runApp(shinyApp(ui = ui, server = server))
  #return(data)
}
