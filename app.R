#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(DBI)
library(bslib)
library(RSQLite)
library(reactable)
# renv::install("reactable")
# renv::install("RSQLite")
# renv::install("tippy")

kahler <- DBI::dbConnect(RSQLite::SQLite(), "kahler.sqlite")

# Define UI for application that draws a histogram
ui <- page_navbar(
  
  id = "tabs",
  
  # Application title
  title = "Retro-digitised Enggano-German dictionary",
  
  # include your js script
  # tags$head(includeScript("returnClick.js")),
  
  theme = bs_theme(
    version = bslib::version_default(),
    bootswatch = "cosmo",
    bg = "#f9f8f5", 
    fg = "#002147", 
    primary = "#193658",
    # secondary="#003947",
    secondary = "#E4F0EF",
    "link-hover-color" = "#be0f34",
    "link-color" = "#3277ae"
    # source from Oxford colour parameters: https://communications.web.ox.ac.uk/communications-resources/visual-identity/identity-guidelines/colours
  ),
  # collapsible = TRUE,
  underline = TRUE,
  
  nav_panel(title = "main",
            card(
              card_body(
                tags$figure(img(src = "file-oxweb-logo.gif", align = "left", width = 80, style = "margin-right: 5px; margin-top: 10px", display = "inline-block"), 
                            img(src = "file-lingphil.png", align = "left", width = 80, style = "margin-right: 5px; margin-top: 10px", display = "inline-block"),
                            img(src = "file-ahrc.png", align = "left", width = 280, style = "margin-right: 5px; margin-top: 10px", display = "inline-block")),
                tags$figcaption(em(a("This research", href="https://enggano.ling-phil.ox.ac.uk", target="_blank"), "is funded by the Arts and Humanities Research Council (AHRC) Grant ID ", a("AH/S011064/1", href="https://gtr.ukri.org/projects?ref=AH%2FS011064%2F1", target="_blank"), " and ", a("AH/W007290/1", href="https://gtr.ukri.org/projects?ref=AH%2FW007290%2F1", target="_blank"), ".")),
                HTML('<p xmlns:cc="http://creativecommons.org/ns#" xmlns:dct="http://purl.org/dc/terms/"><a property="dct:title" rel="cc:attributionURL" href="https://doi.org/10.25446/oxford.28057742">Retro-digitised Enggano-German dictionary derived from Kähler’s (1987) “Enggano-Deutsches Wörterbuch”</a> by <span property="cc:attributionName">Gede Primahadi Wijaya Rajeg, Cokorda Rai Adi Pramartha, Ida Bagus Gede Sarasvananda, Putu Wahyu Widiatmika, Ida Bagus Made Ari Segara, Yul Fulgensia Rusman Pita, Fitri Koemba, I Gede Semara Dharma Putra, Putu Dea Indah Kartini, Ni Putu Wulan Lestari, and Barnaby Burleigh</span> is licensed under <a href="https://creativecommons.org/licenses/by-nc-sa/4.0/?ref=chooser-v1" target="_blank" rel="license noopener noreferrer" style="display:inline-block;">CC BY-NC-SA 4.0<img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/cc.svg?ref=chooser-v1" alt=""><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/by.svg?ref=chooser-v1" alt=""><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/nc.svg?ref=chooser-v1" alt=""><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/sa.svg?ref=chooser-v1" alt=""></a></p>'),
                h2("Overview"),
                div(p("Welcome to the ", a("Shiny", href = "https://shiny.posit.co/", target = "_blank"), "web application serving the selected data from the retro-digitised Enggano-German dictionary ", a("(Rajeg et al. 2024)", href = "https://doi.org/10.25446/oxford.28057742.v1", target = "_blank"), "by ", a("Hans Kähler (1987).", href = "https://search.worldcat.org/title/18191699", target = "_blank"), "At the moment, users can browse the", actionLink("headword", "headword"), "represented in the original dictionary or the", actionLink("subentry", "sub-entry"), "for a given headword (if any). In the ", em("sub-entry"), "panel, information about the headword or root of the sub-entry is provided. The German, English, and Indonesian translations for the sub-entries are marked with '(sub)' in the column names.")),
                
                h2("How to cite"),
                p("Please cite the original source and the digitised dictionary database as follows:"),
                div(tags$li("Kähler, Hans. (1987).", em("Enggano-Deutsches Wörterbuch"), "(Veröffentlichungen Des Seminars Für Indonesische Und Südseesprachen Der Universität Hamburg 14). Berlin; Hamburg: Dietrich Reimer Verlag.", a( "https://search.worldcat.org/title/18191699", href="https://search.worldcat.org/title/18191699", target="_blank"))),
                div(tags$li("Rajeg, Gede Primahadi Wijaya; Pramartha, Cokorda Rai Adi; Sarasvananda, Ida Bagus Gede; Widiatmika, Putu Wahyu; Segara, Ida Bagus Made Ari; Pita, Yul Fulgensia Rusman; et al. (2024). Retro-digitised Enggano-German dictionary derived from Kähler’s (1987) “Enggano-Deutsches Wörterbuch”. University of Oxford. Dataset.", a("https://doi.org/10.25446/oxford.28057742", href = "https://doi.org/10.25446/oxford.28057742", target = "_blank")))
              )
            )),
  nav_panel(title = "headword",
            # layout_columns(
              
              # card for main entry description
              # card(
                # height = 150,
                # full_screen = FALSE,
                # card_header("Description"),
                # card_body(
                  # textInput(inputId = "search_main", label = "Type to search")
                  # markdown("Some texts with a [link](https://doi.org/10.25446/oxford.28057742.v1).")
                # )
              #)),
              
              # card for main entry output
              card(
                
                # height = 400,
                #layout_sidebar(
                #  sidebar = sidebar(
                    
                #  ),
                  div(reactable::reactableOutput(outputId = "main_entry_output"),
                      style = "font-size: 90%")
                #)
              )
            ),
  nav_panel(title = "sub-entry",
            # layout_columns(
              
              # card for sub-entry description
              # card(
                # full_screen = FALSE,
                # card_header("Description"),
                # card_body(
                #  markdown("Some texts with a [link](https://doi.org/10.25446/oxford.28057742.v1).")
                # )
              #)),
              
              # card for sub-entry output
              card(
                # height = 400,
                # layout_sidebar(
                  # sidebar = sidebar(
                    
                  #),
                  div(reactable::reactableOutput(outputId = "subentry_output"),
                      style = "font-size: 90%")
                #)
              )
            )

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    k_full <- tbl(kahler, "full")
    # k_stem_out <- dbGetQuery(kahler, "SELECT * FROM stem WHERE (stem_form LIKE '%$textInput$SearchEntry%') OR (stem_EN LIKE '%textInput$SearchEntry%') OR (stem_IDN LIKE '%textInput$SearchEntry%') OR (stem_DE LIKE '%textInput$SearchEntry%')") |> 
    #   collect()
    k_stem <- k_full |> 
      select(matches("^stem|kms_page|kms_Alphabet"))
    
    k_subentry <- k_full |> 
      select(matches("^ex|stem_form$|stem_homonymID$|stem_ID|stem_(DE|EN|IDN)|kms_page|kms_Alphabet"), -matches("tokenised")) |> 
      filter(!is.na(example_id))
    
    # k_stem_out <- k_stem |> 
    #   select(-stem_id, -stem_form_comm_untokenised, -stem_form_comm_tokenised, -matches("tokenised")) |> 
    #   collect()
    k_stem_out <- k_stem |> 
      select(entry = kms_Alphabet, page = kms_page, stem_id, form = stem_form, stem_homonymID, German = stem_DE, English = stem_EN, Indonesian = stem_IDN) |> 
      mutate(form = if_else(!is.na(stem_homonymID),
                            paste(form, "<sup><i>", stem_homonymID, "</i></sup>", sep = ""),
                            form),
             entry = toupper(entry)) |> 
      distinct() |> 
      filter(!is.na(German)) |>
      select(-stem_homonymID) |> 
      collect() |> 
      mutate(across(where(is.character), ~replace_na(., "")))
    k_stem_out <- reactable::reactable(k_stem_out,
      filterable = TRUE,
      searchable = TRUE,
      showPagination = TRUE,
      highlight = TRUE,
      resizable = TRUE,
      height = 520,
      minRows = 5,
      defaultPageSize = 100,
      columns = list(
        entry = reactable::colDef(show = TRUE, maxWidth = 80),
        form = reactable::colDef(html = TRUE, maxWidth = 185),
        stem_id = reactable::colDef(show = FALSE),
        page = reactable::colDef(show = FALSE)
      )
    )
    
    k_subentry_out <- k_subentry |> 
      select(entry = kms_Alphabet, page = kms_page, stem_id, subentry = example_form, `German (sub)` = ex_DE,
             `English (sub)` = ex_EN,
             `Indonesian (sub)` = ex_IDN,
             `headword/root` = stem_form, stem_homonymID, German = stem_DE, English = stem_EN, Indonesian = stem_IDN) |> 
      mutate(`headword/root` = if_else(!is.na(stem_homonymID),
                            paste(`headword/root`, "<sup><i>", stem_homonymID, "</i></sup>", sep = ""),
                            `headword/root`),
             entry = toupper(entry)) |> 
      distinct() |> 
      # filter(!is.na(German)) |> 
      select(-stem_homonymID) |> 
      collect() |> 
      mutate(across(where(is.character), ~replace_na(., "")))
    
    k_subentry_out <- reactable::reactable(k_subentry_out,
                                       filterable = TRUE,
                                       searchable = TRUE,
                                       showPagination = TRUE,
                                       resizable = TRUE,
                                       highlight = TRUE,
                                       height = 520,
                                       minRows = 5,
                                       defaultPageSize = 100,
                                       columns = list(
                                         entry = reactable::colDef(show = TRUE, maxWidth = 80),
                                         `headword/root` = reactable::colDef(html = TRUE),
                                         stem_id = reactable::colDef(show = FALSE),
                                         page = reactable::colDef(show = FALSE)
                                       )
    )
    
    output$main_entry_output <- reactable::renderReactable(k_stem_out)
    output$subentry_output <- reactable::renderReactable(k_subentry_out)
    
    # the following code run the clicking on the hyperlink of the main panel/page
    observeEvent(input$headword, {
      updateTabsetPanel(session = session, "tabs", "headword")
    })
    
    observeEvent(input$subentry, {
      updateTabsetPanel(session = session, "tabs", "sub-entry")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
