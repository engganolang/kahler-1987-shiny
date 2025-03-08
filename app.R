#
# This is a source R code for Shiny web application for the Enggano-German Dictionary online
# Programmed by Gede Primahadi Wijaya Rajeg (2025)
# University of Oxford/CIRHSS and CompLexico research group, Udayana University
# 
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
library(htmltools)
# renv::install("reactable")
# renv::install("RSQLite")
# renv::install("tippy")

kahler <- DBI::dbConnect(RSQLite::SQLite(), "kahler.sqlite")

k_stem <- tbl(kahler, "full") |> 
  select(matches("^stem|kms_page|kms_entry|kms_Alphabet"))

k_stem_interim <- k_stem |> 
  
  select(entry = kms_Alphabet, 
         page = kms_page, 
         stem_id, 
         form = stem_form_comm_untokenised, 
         stem_homonymID, 
         German = stem_DE, 
         English = stem_EN, 
         Indonesian = stem_IDN) |> 
  mutate(# form = if_else(!is.na(stem_homonymID),
                        # paste(form, 
                        #       "<sup><i>", 
                        #       stem_homonymID, 
                        #       "</i></sup>", 
                        #       sep = ""),
                        # form),
         # form = str_c("<a href=#", stem_id, ">", form, "</a>",
         #                      sep = ""),
         entry = toupper(entry),
         details = NA) |> 
  relocate(details, .before = form) |> 
  distinct() |> 
  filter(!is.na(German)) |>
  # select(-stem_homonymID) |> 
  collect() |> 
  mutate(across(where(is.character), ~replace_na(., "")))

subentry_cols <- "^ex|stem_form|stem_homonymID$|stem_ID|stem_(DE|EN|IDN)|kms_page|kms_Alphabet"

k_subentry <- tbl(kahler, "full") |> 
  select(matches(subentry_cols)) |> 
  filter(!is.na(example_id))

k_subentry_interim <- k_subentry |> 
  select(entry = kms_Alphabet, 
         page = kms_page, 
         example_id, 
         form = example_form_comm_untokenised, 
         `German (sub)` = ex_DE,
         `English (sub)` = ex_EN,
         `Indonesian (sub)` = ex_IDN, 
         stem_id,
         `main entry` = stem_form_comm_untokenised, 
         stem_homonymID, 
         German = stem_DE, 
         English = stem_EN, 
         Indonesian = stem_IDN) |> 
  mutate(#`main entry` = if_else(!is.na(stem_homonymID),
                                # paste(`main entry`, 
                                #       "<sup><i>", 
                                #       stem_homonymID, 
                                #       "</i></sup>", 
                                #       sep = ""),
                                # `main entry`),
         entry = toupper(entry)) |> 
  distinct() |> 
  # filter(!is.na(German)) |> 
  # select(-stem_homonymID) |>
  collect() |> 
  mutate(across(where(is.character), ~replace_na(., "")))

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

# List of "Links" panel ====
link_kahler_github <- tags$a(shiny::icon("github"), "Source codes", 
                             href="https://github.com/engganolang/kahler-1987-shiny", 
                             target="_blank")

link_enggano_web <- tags$a(shiny::icon("globe", lib = "glyphicon"), "Enggano webpage", 
                           href="https://enggano.ling-phil.ox.ac.uk/", 
                           target="_blank")

link_contemporary_enggano <- tags$a(shiny::icon("globe", lib = "glyphicon"), "Contemporary Enggano Dictionary",
                                    href="https://portal.sds.ox.ac.uk/projects/Contemporary_Enggano_Dictionary/238013",
                                    target="_blank")

link_kahler <- tags$a(shiny::icon("globe", lib = "glyphicon"), "Digitised Enggano-German dictionary",
                      href="https://portal.sds.ox.ac.uk/projects/Retro-digitisation_of_the_Enggano-German_Dictionary/237998",
                      target="_blank")

link_enolex <- tags$a(shiny::icon("globe", lib = "glyphicon"), "EnoLEX",
                          href="https://doi.org/10.25446/oxford.28282169.v1",
                          target="_blank")

# UI: Define UI for application =====
ui <- page_navbar(
  tags$head(
    tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "ox_brand1_rev.png")),
  id = "tabs",
  fillable = TRUE,
  fillable_mobile = TRUE,
  
  # Application title
  title = "Enggano-German Dictionary online",
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
    "link-color" = "#3277ae",
    info = "#00AAB4"
    # source from Oxford colour parameters: https://communications.web.ox.ac.uk/communications-resources/visual-identity/identity-guidelines/colours
  ),
  # collapsible = TRUE,
  underline = TRUE,
  nav_panel(title = "Home",
            card(
              card_body(
                tags$figure(img(src = "file-oxweb-logo.gif", align = "left", width = 80, style = "margin-right: 5px; margin-top: 10px", display = "inline-block"), 
                            img(src = "file-lingphil.png", align = "left", width = 80, style = "margin-right: 5px; margin-top: 10px", display = "inline-block"),
                            img(src = "file-ahrc.png", align = "left", width = 280, style = "margin-right: 5px; margin-top: 10px", display = "inline-block")),
                tags$figcaption(em(a("This research", href="https://enggano.ling-phil.ox.ac.uk", target="_blank"), "is funded by the Arts and Humanities Research Council (AHRC) Grant ID ", a("AH/S011064/1", href="https://gtr.ukri.org/projects?ref=AH%2FS011064%2F1", target="_blank"), " and ", a("AH/W007290/1", href="https://gtr.ukri.org/projects?ref=AH%2FW007290%2F1", target="_blank"), ".")),
                HTML('<p xmlns:cc="http://creativecommons.org/ns#" xmlns:dct="http://purl.org/dc/terms/"><a property="dct:title" rel="cc:attributionURL" href="https://doi.org/10.25446/oxford.28057742">The Enggano-German Dictionary online derived from Kähler’s (1987) “Enggano-Deutsches Wörterbuch”</a> by <span property="cc:attributionName">Gede Primahadi Wijaya Rajeg, Cokorda Rai Adi Pramartha, Ida Bagus Gede Sarasvananda, Putu Wahyu Widiatmika, Ida Bagus Made Ari Segara, Yul Fulgensia Rusman Pita, Fitri Koemba, I Gede Semara Dharma Putra, Putu Dea Indah Kartini, Ni Putu Wulan Lestari, Barnaby Burleigh, Charlotte Hemmings, I Wayan Arka, Sarah Ogilvie, Mary Dalrymple, Daniel Krauße, and Bernd Nothofer</span> is licensed under <a href="https://creativecommons.org/licenses/by-nc-sa/4.0/?ref=chooser-v1" target="_blank" rel="license noopener noreferrer" style="display:inline-block;">CC BY-NC-SA 4.0<img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/cc.svg?ref=chooser-v1" alt=""><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/by.svg?ref=chooser-v1" alt=""><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/nc.svg?ref=chooser-v1" alt=""><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/sa.svg?ref=chooser-v1" alt=""></a></p>'),
                h2("Overview"),
                div(p("Welcome to the ", a("Shiny", href = "https://shiny.posit.co/", target = "_blank"), "web application serving the selected content from the retro-digitised Enggano-German dictionary dataset ", a("(Rajeg et al. 2024)", href = "https://doi.org/10.25446/oxford.28057742.v1", target = "_blank"), "by ", a("Hans Kähler (1987).", href = "https://search.worldcat.org/title/18191699", target = "_blank"))),
                div(p("At the moment, users can browse (i) the ", strong(actionLink("headword", "main entry")), " represented in the original dictionary or (ii) the ", strong(actionLink("subentry", "sub-entry")), " (if any) for a given main entry (i.e., a headword/a root form). In the ", strong("Sub-entry"), "panel, we also provide information about the main entry that the sub-entry belongs to (under the", tags$code("main entry"), "column). The German, English, and Indonesian translations for the sub-entries are marked with", tags$code("(sub)"), "in the column names.")),
                div(p("The search field for each column accepts plain search pattern and regular expressions; for these two flavours of the search patterns, each match will be highlighted in red.")),
                div(p("Related materials for this sub-project are available ", a("here", href = "https://portal.sds.ox.ac.uk/projects/Retro-digitisation_of_the_Enggano-German_Dictionary/237998", target = "_blank"), ".")),
                
                h2("How to cite"),
                p("Please cite the original source dictionary, the digitised dictionary dataset, and this online web application as follows (if in ", em("DataCite"), " style):"),
                div(tags$blockquote("Kähler, Hans. (1987).", em("Enggano-Deutsches Wörterbuch"), "(Veröffentlichungen Des Seminars Für Indonesische Und Südseesprachen Der Universität Hamburg 14). Berlin; Hamburg: Dietrich Reimer Verlag.", a( "https://search.worldcat.org/title/18191699", href="https://search.worldcat.org/title/18191699", target="_blank"))),
                div(tags$blockquote("Rajeg, Gede Primahadi Wijaya; Pramartha, Cokorda Rai Adi; Sarasvananda, Ida Bagus Gede; Widiatmika, Putu Wahyu; Segara, Ida Bagus Made Ari; Pita, Yul Fulgensia Rusman; et al. (2024). Retro-digitised Enggano-German dictionary derived from Kähler’s (1987) “Enggano-Deutsches Wörterbuch”. University of Oxford. Dataset.", a("https://doi.org/10.25446/oxford.28057742", href = "https://doi.org/10.25446/oxford.28057742", target = "_blank"))),
                div(tags$blockquote("Rajeg, Gede Primahadi Wijaya; Pramartha, Cokorda Rai Adi; Sarasvananda, Ida Bagus Gede; Widiatmika, Putu Wahyu; Segara, Ida Bagus Made Ari; Pita, Yul Fulgensia Rusman; et al. (2025). The Enggano-German Dictionary online derived from Kähler’s (1987) “Enggano-Deutsches Wörterbuch”. University of Oxford. Online Resource.", a("https://doi.org/10.25446/oxford.28532666", href = "https://doi.org/10.25446/oxford.28532666", target = "_blank")))
              )
            )),
  nav_panel(title = "Main entry",
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
                
                id = "main_entry",
                # height = 300,
                #layout_sidebar(
                #  sidebar = sidebar(
                    
                #  ),
                  # div(tags$h3("Main entry table")),
                  div(reactable::reactableOutput(outputId = "main_entry_output"),
                      style = "font-size: 90%")
                #)
              )
            ),
  nav_panel(title = "Sub-entry",
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
            ),
  nav_menu(title = "Links",
           nav_item(link_kahler_github),
           nav_item(link_enggano_web),
           nav_item(link_contemporary_enggano),
           nav_item(link_kahler),
           nav_item(link_enolex))

)

# SERVER: Define server logic required =====
server <- function(input, output, session) {
  
  k_stem_out <- reactive({
    
    k_stem_interim |> 
      
      reactable::reactable(filterable = TRUE,
                           searchable = TRUE,
                           searchMethod = regex_search_method,
                           showPagination = TRUE,
                           highlight = TRUE,
                           resizable = TRUE,
                           # height = 495,
                           minRows = 5,
                           language = reactableLang(
                             filterPlaceholder = "Search"
                           ),
                           defaultPageSize = 10,
                           onClick = JS(
                             "function(rowInfo, column) {
                           if(column.id === \"details\")
                           Shiny.setInputValue('main_entry_details', { index: rowInfo.index + 1 }, { priority: 'event' })
                           }
                           "
                           ),
                           elementId = "alphabet-select",
                           columns = list(
                             details = colDef(
                               name = "",
                               sortable = FALSE,
                               cell = function() htmltools::tags$button("more", class="btn btn-info btn-sm btn-border-radius-lg"),
                               filterable = FALSE,
                               maxWidth = 83
                             ),
                             
                             entry = reactable::colDef(show = TRUE, 
                                                       maxWidth = 70,
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
                             form = reactable::colDef(html = TRUE, 
                                                      maxWidth = 185,
                                                      filterMethod = js_filter,
                                                      cell = js_match_style),
                             German = reactable::colDef(html = TRUE,
                                                        filterMethod = js_filter,
                                                        cell = js_match_style),
                             English = reactable::colDef(html = TRUE, 
                                                         filterMethod = js_filter,
                                                         cell = js_match_style),
                             Indonesian = reactable::colDef(html = TRUE, 
                                                            filterMethod = js_filter,
                                                            cell = js_match_style),
                             stem_id = reactable::colDef(show = FALSE),
                             stem_homonymID = reactable::colDef(show = FALSE),
                             page = reactable::colDef(show = FALSE)
                           )
      )
    
  })
  
  k_subentry_out <- reactive({
    
    k_subentry_interim |> 
    
    reactable::reactable(filterable = TRUE,
                         searchable = TRUE,
                         searchMethod = regex_search_method,
                         showPagination = TRUE,
                         resizable = TRUE,
                         highlight = TRUE,
                         # height = 495,
                         minRows = 5,
                         language = reactableLang(
                           filterPlaceholder = "Search"
                         ),
                         defaultPageSize = 10,
                         elementId = "alphabet-select",
                         columns = list(
                           entry = reactable::colDef(show = TRUE, 
                                                     maxWidth = 80,
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
                           `main entry` = reactable::colDef(html = TRUE,
                                                            filterMethod = js_filter,
                                                            cell = js_match_style),
                           form = reactable::colDef(html = TRUE,
                                                    filterMethod = js_filter,
                                                    cell = js_match_style),
                           German = reactable::colDef(html = TRUE, 
                                                      filterMethod = js_filter,
                                                      cell = js_match_style),
                           English = reactable::colDef(html = TRUE, 
                                                       filterMethod = js_filter,
                                                       cell = js_match_style),
                           Indonesian = reactable::colDef(html = TRUE, 
                                                          filterMethod = js_filter,
                                                          cell = js_match_style),
                           `German (sub)` = reactable::colDef(html = TRUE, 
                                                              filterMethod = js_filter,
                                                              cell = js_match_style),
                           `English (sub)` = reactable::colDef(html = TRUE, 
                                                               filterMethod = js_filter,
                                                               cell = js_match_style),
                           `Indonesian (sub)` = reactable::colDef(html = TRUE,
                                                                  filterMethod = js_filter,
                                                                  cell = js_match_style),
                           stem_id = reactable::colDef(show = FALSE),
                           example_id = reactable::colDef(show = FALSE),
                           page = reactable::colDef(show = FALSE),
                           stem_homonymID = reactable::colDef(show = FALSE)
                         )
    )
  
  })
  
    output$main_entry_output <- reactable::renderReactable(k_stem_out())
    output$subentry_output <- reactable::renderReactable(k_subentry_out())
    
    # the following code run the clicking on the hyperlink of the main panel/page
    observeEvent(input$headword, {
      updateTabsetPanel(session = session, "tabs", "Main entry")
    })
    
    observeEvent(input$subentry, {
      updateTabsetPanel(session = session, "tabs", "Sub-entry")
    })
    
    observeEvent(input$main_entry_details, {
      row_num <- as.numeric(input$main_entry_details$index)
      main_entry_id <- k_stem_interim$stem_id[row_num]
      main_entry_form <- k_stem_interim$form[row_num]
      main_entry_homonym_id <- k_stem_interim$stem_homonymID[row_num]
      main_entry_form <- if_else(is.na(main_entry_homonym_id), 
                                 str_c("<i>", main_entry_form, "</i>", sep = ""),
                                 str_c("<i>", main_entry_form, "<sup>", main_entry_homonym_id, "</sup></i>", sep = ""))
      k_stem_filtered <- filter(k_stem, stem_id == main_entry_id) |> 
        distinct()
      # page number
      pagenum <- pull(k_stem_filtered, kms_page)
      entrynum <- pull(k_stem_filtered, kms_entry_no)
      # variant forms
      variant_forms <- pull(filter(k_stem_filtered, is.na(stem_dialectVariant)), stem_formVarian_untokenised)
      # dialect variants
      dialect_forms <- pull(filter(k_stem_filtered, !is.na(stem_dialectVariant)), stem_dialectVariant)
      # etymological information
      stem_etym_form <- pull(filter(k_stem_filtered, !is.na(stem_etymological_form)), stem_etymological_form)
      stem_etym_lang <- pull(filter(k_stem_filtered, !is.na(stem_etymological_language_donor)), stem_etymological_language_donor)
      stem_etym_lang_abbrev <- pull(filter(k_stem, 
                                           !is.na(stem_etymological_language_donor), 
                                           !stem_etymological_language_donor %in% c("PAN", "SMtw")), 
                                    stem_etymological_language_donor) |> 
        unique()
      stem_etym_lang_abbrev <- sort(c(stem_etym_lang_abbrev, "Sim (Simalur)", "Bugi (Buginesisch)", "Si(ch) (Sichule)"))
      # loanword information
      stem_loan_form <- pull(filter(k_stem_filtered, !is.na(stem_loan_form)),
                             stem_loan_form)
      stem_loan_lang <- pull(filter(k_stem_filtered, !is.na(stem_loan_lang)),
                             stem_loan_lang)
      stem_loan_lang <- if_else(is.na(stem_loan_lang), "-", stem_loan_lang)
      # remarks info
      stem_remark <- k_stem_filtered |> 
        filter(!is.na(stem_remark_DE)) |> 
        mutate(myremarks = str_c("<li><sub><i>DE</i></sub>", stem_remark_DE, "</li><li><sub><i>EN</i></sub>", 
                                 stem_remark_EN, "</li><li><sub><i>ID</i></sub>", 
                                 stem_remark_IDN, "</li>", sep = "")) |> 
        pull(myremarks)
      
      showModal(
        modalDialog(
          size = "l",
          easyClose = TRUE,
          title = "Details",
          
          tags$h2(HTML(main_entry_form)),
          tags$p(HTML("<li>"), k_stem_interim$German[row_num], HTML("<sub><i>DE</i></sub></li><li>"), k_stem_interim$English[row_num], HTML("<sub><i>EN</i></sub></li><li>"), k_stem_interim$Indonesian[row_num], HTML("<sub><i>ID</i></sub></li>")),
          if (any(!is.na(variant_forms))) {
            div(p(HTML("<b>variant form(s)</b>:</br><li>", variant_forms, "</li>")))
          },
          if (any(!is.na(dialect_forms))) {
            div(p(HTML("<b>variant form(s) marked with <em>DIA</em>(lectal) in the source</b>:</br><li>", dialect_forms, "</li>")))
          },
          if (any(!is.na(stem_etym_form)) | any(!is.na(stem_etym_lang))) {
            div(p(HTML("<b>Reconstruction info</b>:<li>form: <em>", stem_etym_form, "</em></li><li>source language: ", stem_etym_lang, "</li>")))
            
          },
          # if (any(!is.na(stem_etym_form)) | any(!is.na(stem_etym_lang))) {
          #   div(p(HTML("<b>List of the etymological source language abreviation</b>:"), HTML(str_c(str_c("<li>", stem_etym_lang_abbrev, "</li>", sep = ""), collapse = ""))))
          # },
          if (any(!is.na(stem_loan_form))) {
            div(p(HTML("<b>Loanword info (marked with ̊   in the dictionary)</b>:<li>form: <em>", stem_loan_form, "</em></li><li>source language: ", stem_loan_lang, "</li>") ))
          },
          if (any(!is.na(stem_remark))) {
            div(p(HTML("<b>Notes/remarks</b>:", stem_remark)))
          },
          tags$p(HTML("<em>Kähler (1987:", pagenum, "; entry no.", entrynum, ")")),
          
        )
      )
    }
      
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
