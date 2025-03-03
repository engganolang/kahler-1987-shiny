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

# column filtering method ====
## source: https://github.com/rjake/one-off-projects/blob/main/R/posit-table-contest-2022/posit-table-contest-2022.qmd

# each fragment is searched for separately with match size >= # of fragments
# size > length happens when something like city(_id)? returns two positive matches
js_filter <-  JS(
  "function(rows, columnId, filterValue) {
      // rows = [{'table': 1, 'field': 'city'}, {'table': 2, 'field': 'city city_id'}] 
      // columnId = 'field'
      // filterValue = 'city(_id)?'
      try {
        const pattern = filterValue.split(' ')
        const find = RegExp(`(${pattern.join('|')})+`, 'gi')
        return rows.filter(
          row => new Set(row.values[columnId].match(find)).size >= pattern.length
        )
      } catch(e) {
          return rows
      }
       
  }"
)

# highlight all fragment occurrences string matches
js_match_style <- JS(
  "function(cellInfo) {
    try {
      let filterValue = cellInfo.filterValue
      let pattern = filterValue.split(' ').filter(n => n).sort().join('|')
      let regexPattern = new RegExp('(' + pattern + ')', 'gi')
      let replacement = '<span style=\"color:#be0f34;font-weight:bold;\">$1</span>'
      return cellInfo.value.replace(regexPattern, replacement)
    } catch(e) {
        return cellInfo.value
    }
  }"
)

# custom Searching method ====
## adapted from: https://glin.github.io/reactable/articles/custom-filtering.html#basic-custom-search-method
## and from: https://stackoverflow.com/a/78691006/27514095
regex_search_method <-  JS("function(rows, columnIds, searchValue) {
    return rows.filter(function(row) {
      return columnIds.some(function(columnId) {
        return new RegExp(searchValue, 'gi').test(row.values[columnId])
      })
    })
  }")

# UI: Define UI for application =====
ui <- page_navbar(
  tags$head(
    tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "ox_brand1_rev.png")),
  id = "tabs",
  fillable = TRUE,
  fillable_mobile = TRUE,
  
  # Application title
  title = "The Enggano-German Dictionary online",
  window_title = "Enggano-German Dictionary online",
  
  # include your js script
  # tags$head(includeScript("returnClick.js")),
  
  theme = bs_theme(
    version = bslib::version_default(),
    bootswatch = "cosmo",
    bg = "#f9f8f5", 
    fg = "#002147", 
    primary = "#193658",
    # primary = "#1D42A6",
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
                HTML('<p xmlns:cc="http://creativecommons.org/ns#" xmlns:dct="http://purl.org/dc/terms/"><a property="dct:title" rel="cc:attributionURL" href="https://doi.org/10.25446/oxford.28057742">The Enggano-German Dictionary online derived from Kähler’s (1987) “Enggano-Deutsches Wörterbuch”</a> by <span property="cc:attributionName">Gede Primahadi Wijaya Rajeg, Cokorda Rai Adi Pramartha, Ida Bagus Gede Sarasvananda, Putu Wahyu Widiatmika, Ida Bagus Made Ari Segara, Yul Fulgensia Rusman Pita, Fitri Koemba, I Gede Semara Dharma Putra, Putu Dea Indah Kartini, Ni Putu Wulan Lestari, and Barnaby Burleigh</span> is licensed under <a href="https://creativecommons.org/licenses/by-nc-sa/4.0/?ref=chooser-v1" target="_blank" rel="license noopener noreferrer" style="display:inline-block;">CC BY-NC-SA 4.0<img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/cc.svg?ref=chooser-v1" alt=""><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/by.svg?ref=chooser-v1" alt=""><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/nc.svg?ref=chooser-v1" alt=""><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/sa.svg?ref=chooser-v1" alt=""></a></p>'),
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
                
                # height = 300,
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
                # height = 300,
                # layout_sidebar(
                  # sidebar = sidebar(
                    
                  #),
                  div(reactable::reactableOutput(outputId = "subentry_output"),
                      style = "font-size: 90%")
                #)
              )
            )

)

# SERVER: Define server logic required =====
server <- function(input, output, session) {
    
    k_full <- tbl(kahler, "full")
    
    k_stem <- k_full |> 
      select(matches("^stem|kms_page|kms_Alphabet"))
    
    k_subentry <- k_full |> 
      select(matches("^ex|stem_form|stem_homonymID$|stem_ID|stem_(DE|EN|IDN)|kms_page|kms_Alphabet")) |> 
      filter(!is.na(example_id))
    
    k_stem_out <- k_stem |> 
      select(entry = kms_Alphabet, page = kms_page, stem_id, form = stem_form_comm_untokenised, stem_homonymID, German = stem_DE, English = stem_EN, Indonesian = stem_IDN) |> 
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
      searchMethod = regex_search_method,
      showPagination = TRUE,
      highlight = TRUE,
      resizable = TRUE,
      height = 495,
      minRows = 5,
      defaultPageSize = 100,
      elementId = "alphabet-select",
      columns = list(
        entry = reactable::colDef(show = TRUE, maxWidth = 80,
                                  filterInput = function(values, name) {
                                    tags$select(
                                      # Set to undefined to clear the filter
                                      onchange = sprintf("Reactable.setFilter('alphabet-select', '%s', event.target.value || undefined)", name),
                                      # "All" has an empty value to clear the filter, and is the default option
                                      tags$option(value = "", "All"),
                                      lapply(unique(values), tags$option),
                                      "aria-label" = sprintf("Filter %s", name),
                                      style = "width: 100%; height: 28px;"
                                    )
                                  }),
        form = reactable::colDef(html = TRUE, maxWidth = 185,
                                 filterMethod = js_filter,
                                 cell = js_match_style),
        German = reactable::colDef(html = TRUE,filterMethod = js_filter,
                                   cell = js_match_style),
        English = reactable::colDef(html = TRUE, filterMethod = js_filter,
                                   cell = js_match_style),
        Indonesian = reactable::colDef(html = TRUE, filterMethod = js_filter,
                                   cell = js_match_style),
        stem_id = reactable::colDef(show = FALSE),
        page = reactable::colDef(show = FALSE)
      )
    )
    
    k_subentry_out <- k_subentry |> 
      select(entry = kms_Alphabet, page = kms_page, stem_id, form = example_form_comm_untokenised, `German (sub)` = ex_DE,
             `English (sub)` = ex_EN,
             `Indonesian (sub)` = ex_IDN,
             `headword/root` = stem_form_comm_untokenised, stem_homonymID, German = stem_DE, English = stem_EN, Indonesian = stem_IDN) |> 
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
                                       searchMethod = regex_search_method,
                                       showPagination = TRUE,
                                       resizable = TRUE,
                                       highlight = TRUE,
                                       height = 495,
                                       minRows = 5,
                                       defaultPageSize = 100,
                                       elementId = "alphabet-select",
                                       columns = list(
                                         entry = reactable::colDef(show = TRUE, maxWidth = 80,
                                                                   filterInput = function(values, name) {
                                                                     tags$select(
                                                                       # Set to undefined to clear the filter
                                                                       onchange = sprintf("Reactable.setFilter('alphabet-select', '%s', event.target.value || undefined)", name),
                                                                       # "All" has an empty value to clear the filter, and is the default option
                                                                       tags$option(value = "", "All"),
                                                                       lapply(unique(values), tags$option),
                                                                       "aria-label" = sprintf("Filter %s", name),
                                                                       style = "width: 100%; height: 28px;"
                                                                     )
                                                                   }),
                                         `headword/root` = reactable::colDef(html = TRUE,
                                                                             filterMethod = js_filter,
                                                                             cell = js_match_style),
                                         form = reactable::colDef(html = TRUE,
                                                                             filterMethod = js_filter,
                                                                             cell = js_match_style),
                                         German = reactable::colDef(html = TRUE, filterMethod = js_filter,
                                                                    cell = js_match_style),
                                         English = reactable::colDef(html = TRUE, filterMethod = js_filter,
                                                                     cell = js_match_style),
                                         Indonesian = reactable::colDef(html = TRUE, filterMethod = js_filter,
                                                                        cell = js_match_style),
                                         `German (sub)` = reactable::colDef(html = TRUE, filterMethod = js_filter,
                                                                    cell = js_match_style),
                                         `English (sub)` = reactable::colDef(html = TRUE, filterMethod = js_filter,
                                                                     cell = js_match_style),
                                         `Indonesian (sub)` = reactable::colDef(html = TRUE,
                                                                                filterMethod = js_filter,
                                                                        cell = js_match_style),
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
