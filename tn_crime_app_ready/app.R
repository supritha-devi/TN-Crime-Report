# =============================================================
# Tamil Nadu Crime Insight -- Shiny Application (Final)
# =============================================================
# Requires, in a "data/" subfolder next to this file:
#   tn_crime_master.csv         (from data_prep/01_clean_and_merge.R)
#   tn_crime_map_ready.geojson  (from data_prep/01_clean_and_merge.R)
#   tn_taluks.geojson           (from data_prep/02_prepare_taluk_boundaries.R)
#
# First-time package install (minimal set -- no bcrypt, no rmarkdown/tinytex/
# pandoc, chosen specifically to avoid common renv/deployment failures):
#   install.packages(c("shiny","shinyjs","leaflet","sf","dplyr","readr",
#                       "DT","RSQLite","DBI","digest","jsonlite","stringr",
#                       "ggplot2","base64enc"))
#
# Run with: shiny::runApp()
# =============================================================

library(shiny)
library(shinyjs)
library(leaflet)
library(sf)
library(dplyr)
library(readr)
library(DT)
library(RSQLite)
library(DBI)
library(digest)
library(jsonlite)
library(stringr)

source("R/chatbot_engine.R")
source("R/report_builder.R")

# ---- Password hashing (lightweight: digest, not bcrypt) --------------------
# bcrypt needs a compiled libsodium system library, which is a common source
# of renv/deployment failures. digest is pure-C and ships reliably on every
# platform. We add a per-user random salt so two identical passwords don't
# produce identical hashes -- a reasonable, low-dependency tradeoff for a
# class project (not a production-grade auth system).
hash_password <- function(password, salt) digest::digest(paste0(salt, password), algo = "sha256")
make_salt <- function() digest::digest(paste0(Sys.time(), sample(1e9, 1)), algo = "sha1")

# ---- 1. LOAD DATA ------------------------------------------------------
master_df <- read_csv("data/tn_crime_master.csv", show_col_types = FALSE)
map_sf    <- st_read("data/tn_crime_map_ready.geojson", quiet = TRUE)
taluk_sf  <- st_read("data/tn_taluks.geojson", quiet = TRUE)

state_avg_rate <- mean(master_df$total_crime_rate_2023, na.rm = TRUE)
all_districts  <- sort(master_df$district)

category_choices <- c(
  "Total (IPC+SLL)" = "total_crime_rate_2023",
  "Murder" = "murder_rate",
  "Crimes against women" = "women_crime_rate",
  "Road accidents" = "road_accident_rate",
  "Suicides" = "suicide_rate"
)

role_choices <- c(
  "General Citizen", "Builder / Property Developer", "Police / Law Enforcement",
  "Government Official", "Journalist / Media", "NGO / Social Worker",
  "Student / Researcher", "Other"
)

chat_role_choices <- c("Government Official", "Student/Researcher", "Journalist", "NGO", "Common Citizen")

avatar_bg_choices <- c(
  "Lavender" = "#E9D5FF", "Pink" = "#FBCFE8", "Sky" = "#CFFAFE",
  "Mint" = "#D1FAE5", "Cream" = "#FEF3C7", "Amber" = "#FDE68A",
  "Blue" = "#DBEAFE", "Slate" = "#F1F5F9"
)
avatar_face_choices <- c("\U0001F642","\U0001F600","\U0001F60E","\U0001F913",
                          "\U0001F9D1","\U0001F469","\U0001F468","\U0001F9D4")

# color-by-category helper, shared by both maps
color_for_value <- function(field, value) {
  vals <- sort(master_df[[field]])
  b1 <- quantile(vals, 0.25, names = FALSE)
  b2 <- quantile(vals, 0.50, names = FALSE)
  b3 <- quantile(vals, 0.75, names = FALSE)
  case_when(
    value <= b1 ~ "#FFFFFF",
    value <= b2 ~ "#FCA5A5",
    value <= b3 ~ "#EF4444",
    TRUE ~ "#991B1B"
  )
}

highest_category <- function(row) {
  rates <- c("Murder" = row$murder_rate, "Crimes against women" = row$women_crime_rate,
             "Road accidents" = row$road_accident_rate, "Suicides" = row$suicide_rate)
  names(rates)[which.max(rates)]
}

role_notes <- c(
  "Police / Law Enforcement" = "Consider whether current resource allocation matches this category's share of the total.",
  "Builder / Property Developer" = "Security features addressing this category (lighting, CCTV, access control) are worth prioritizing in new developments here.",
  "Journalist / Media" = "District-level figures like these cannot support claims about specific neighborhoods or streets.",
  "Student / Researcher" = "Consider testing this pattern statistically against literacy and urbanization rather than relying on this summary alone.",
  "NGO / Social Worker" = "Where relevant, consider cross-referencing this with categories affecting vulnerable groups specifically.",
  "Government Official" = "Useful context for prioritizing resource allocation and policy review in this district.",
  "General Citizen" = "This is one factor among many worth weighing, not a verdict on the district as a whole."
)
build_role_note <- function(role, row) {
  top_cat <- highest_category(row)
  diff_pct <- round((row$total_crime_rate_2023 - state_avg_rate) / state_avg_rate * 100, 1)
  dir <- if (diff_pct >= 0) "above" else "below"
  base <- sprintf("%s's overall crime rate is %.1f%% %s the state average, with %s as the largest contributing category.",
                   row$district, abs(diff_pct), dir, top_cat)
  extra <- role_notes[[role]]
  if (is.null(extra)) extra <- role_notes[["General Citizen"]]
  paste(base, extra)
}


# ---- 2. DATABASE SETUP ---------------------------------------------------
dir.create("db", showWarnings = FALSE)
db_con <- dbConnect(RSQLite::SQLite(), "db/tn_app.db")

dbExecute(db_con, "
  CREATE TABLE IF NOT EXISTS users (
    username TEXT PRIMARY KEY, name TEXT, email TEXT, password_hash TEXT, salt TEXT,
    role TEXT, avatar_bg TEXT, avatar_face TEXT, created_at TEXT, last_login TEXT
  )")
dbExecute(db_con, "
  CREATE TABLE IF NOT EXISTS history (
    id INTEGER PRIMARY KEY AUTOINCREMENT, username TEXT, district TEXT,
    category TEXT, source TEXT, viewed_at TEXT
  )")
onStop(function() dbDisconnect(db_con))


# =============================================================
# UI
# =============================================================
ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  titlePanel("Tamil Nadu Crime Insight"),

  div(id = "login_page",
    h3("Sign In"),
    textInput("login_username", "Username", value = "admin"),
    passwordInput("login_password", "Password", value = "admin123"),
    actionButton("login_btn", "Log In", class = "btn-primary", style = "width:100%; margin-bottom:10px;"),
    actionButton("demo_btn", "1-Click Demo / Guest Sign In", class = "btn-success", style = "width:100%; margin-bottom:15px;"),
    actionButton("show_register", "New user? Create an account", class = "btn-default", style = "width:100%;"),
    div(style = "color:#DC2626; font-weight:600; margin-top:10px;", textOutput("login_error"))
  ),

  hidden(div(id = "register_page",
    h3("Create Account"),
    textInput("reg_name", "Full Name", placeholder = "Your name"),
    textInput("reg_username", "Choose a Username", placeholder = "username"),
    textInput("reg_email", "Email", placeholder = "email@example.com"),
    passwordInput("reg_password", "Password (at least 6 characters)"),
    passwordInput("reg_password2", "Confirm Password"),
    selectInput("reg_role", "Role in Society", choices = role_choices),
    h4("Create your avatar"),
    fluidRow(
      column(6, selectInput("reg_avatar_bg", "Background", choices = avatar_bg_choices)),
      column(6, selectInput("reg_avatar_face", "Face", choices = avatar_face_choices))
    ),
    uiOutput("reg_avatar_preview"),
    br(),
    actionButton("register_btn", "Create Account & Enter", class = "btn-primary", style = "width:100%; margin-bottom:10px;"),
    actionButton("back_to_login", "Already have an account? Log in", class = "btn-default", style = "width:100%;"),
    div(style = "color:#DC2626; font-weight:600; margin-top:10px;", textOutput("register_error"))
  )),

  hidden(div(id = "main_app",
    fluidRow(
      column(8, h4(textOutput("welcome_text"))),
      column(4,
        actionButton("theme_btn", "Dark mode"),
        actionButton("logout_btn", "Logout")
      )
    ),
    hr(),
    tabsetPanel(id = "main_tabs",

      tabPanel("Explore Map", value = "map_tab",
        br(),
        selectInput("state_crime_type", "Crime type", choices = category_choices, width = "300px"),
        p("Click any district on the map, or use the dropdown below, to open its detailed map.",
          style = "color:#64748B;font-size:13px;"),
        leafletOutput("state_map", height = 560),
        br(),
        selectInput("state_district_select", "Select district", choices = c("-- choose --" = "", all_districts), width = "280px")
      ),

      tabPanel("District Map", value = "districtmap_tab",
        br(),
        selectInput("district_crime_type", "Crime type", choices = category_choices, width = "300px"),
        fluidRow(
          column(7, h4(textOutput("districtmap_title")), leafletOutput("district_zoom_map", height = 380)),
          column(5, h5("Location within Tamil Nadu"), leafletOutput("district_locator_map", height = 300))
        ),
        textOutput("districtmap_value"),
        p(textOutput("taluk_note"), style = "color:#64748B;font-size:12.5px;"),
        actionButton("goto_report_btn", "View Full Report \u2192", class = "btn-primary")
      ),

      tabPanel("Dashboard", value = "dash_tab",
        br(),
        fluidRow(
          column(3, wellPanel(h4("38"), p("Districts"))),
          column(3, wellPanel(h4(textOutput("dash_avg")), p("State avg rate / lakh"))),
          column(3, wellPanel(h4(textOutput("dash_high")), p("Highest crime rate"))),
          column(3, wellPanel(h4(textOutput("dash_low")), p("Lowest crime rate")))
        ),
        wellPanel(
          strong("Welcome to TN Crime Insight."),
          " Use the Explore Map tab to click any district, or jump straight to the District Report tab."
        ),
        wellPanel(
          h4("Data currency note"),
          p("All maps, reports, and the chatbot use ", strong("2023 district-level data"), " -- the most recent full breakdown TN-SCRB has published. A 2024/2025 district-wise release doesn't exist yet."),
          p("State-wide (not district-level) 2024 figures, from TN Police press statements:"),
          tags$ul(
            tags$li("Murders: 1,563 in 2024 -- a 12-year low"),
            tags$li("Bodily offences: 31,497 in 2024, down from 49,286 in 2023 (~36% drop)"),
            tags$li("Theft: 15,892 in 2024, down from 17,788 in 2023")
          ),
          p(em("Source: Tamil Nadu Police press statements, reported via PTI/IANS (2025). State totals only -- not merged into district figures above."),
            style = "font-size:11.5px;color:#64748B;")
        )
      ),

      tabPanel("My History", value = "history_tab",
        br(), DTOutput("history_table")
      ),

      tabPanel("Profile", value = "profile_tab",
        br(), uiOutput("profile_ui")
      ),

      tabPanel("District Report", value = "report_tab",
        br(),
        selectInput("district_select", "District", choices = all_districts, width = "280px"),
        uiOutput("district_report_ui"),
        hr(),
        h4("Download"),
        fluidRow(
          column(4, downloadButton("download_report_html", "Download Full Report (HTML)")),
          column(4, radioButtons("data_format", "Data format:", choices = c("CSV" = "csv", "JSON" = "json"), inline = TRUE)),
          column(4, downloadButton("download_data", "Download Data"))
        ),
        br(), actionButton("save_history_btn", "Save this analysis to history")
      ),

      tabPanel("Chatbot", value = "chatbot_tab",
        br(),
        uiOutput("chat_role_picker"),
        div(class = "chat-window", id = "chat_window", uiOutput("chat_messages")),
        fluidRow(
          column(9, textInput("chat_input", NULL, placeholder = "Ask about a district, e.g. 'crime in Chennai'")),
          column(3, actionButton("chat_send", "Send", class = "btn-primary"))
        )
      )
    )
  ))
)


# =============================================================
# SERVER
# =============================================================
server <- function(input, output, session) {

  current_user <- reactiveVal(NULL)
  current_role <- reactiveVal(NULL)
  current_district <- reactiveVal(NULL)
  chat_role <- reactiveVal(NULL)
  chat_context <- reactiveVal(list(district = NULL, category = "total_crime_rate_2023", category_label = "overall crime"))
  chat_log <- reactiveVal(list())

  # ---- Avatar preview (registration) ----
  output$reg_avatar_preview <- renderUI({
    div(class = "avatar-preview", style = paste0("background:", input$reg_avatar_bg, ";"), input$reg_avatar_face)
  })

  # ---- Theme toggle ----
  observeEvent(input$theme_btn, {
    shinyjs::toggleClass(selector = "body", class = "dark-theme")
    new_label <- if (isTRUE(input$theme_btn %% 2 == 1)) "Light mode" else "Dark mode"
    updateActionButton(session, "theme_btn", label = new_label)
  })

  # ---- Login / Register / Logout ----
  observeEvent(input$show_register, {
    shinyjs::hide("login_page")
    shinyjs::show("register_page")
    output$register_error <- renderText("")
  })

  observeEvent(input$back_to_login, {
    shinyjs::hide("register_page")
    shinyjs::show("login_page")
    output$login_error <- renderText("")
  })

  # Instant 1-Click Demo Login
  observeEvent(input$demo_btn, {
    current_user("Demo User")
    current_role("General Citizen")
    shinyjs::hide("login_page")
    shinyjs::hide("register_page")
    shinyjs::show("main_app")
    updateTabsetPanel(session, "main_tabs", selected = "map_tab")
  })

  observeEvent(input$login_btn, {
    u <- trimws(input$login_username)
    p <- input$login_password
    if (is.null(u) || u == "" || is.null(p) || p == "") {
      output$login_error <- renderText("Please enter both username and password.")
      return()
    }
    user_row <- dbGetQuery(db_con, "SELECT * FROM users WHERE username = ?", params = list(u))
    login_hash <- if (nrow(user_row) == 1) hash_password(p, user_row$salt[1]) else NA
    if (nrow(user_row) == 1 && identical(login_hash, user_row$password_hash[1])) {
      current_user(user_row$username[1])
      current_role(user_row$role[1])
      dbExecute(db_con, "UPDATE users SET last_login = ? WHERE username = ?",
                params = list(as.character(Sys.time()), user_row$username[1]))
      output$login_error <- renderText("")
      shinyjs::hide("login_page")
      shinyjs::hide("register_page")
      shinyjs::show("main_app")
      updateTabsetPanel(session, "main_tabs", selected = "map_tab")
    } else {
      output$login_error <- renderText("Incorrect username or password.")
    }
  })

  observeEvent(input$register_btn, {
    u <- trimws(input$reg_username)
    p <- input$reg_password
    p2 <- input$reg_password2
    nm <- trimws(input$reg_name)
    em <- trimws(input$reg_email)

    if (is.null(u) || u == "") {
      output$register_error <- renderText("Please enter a username.")
      return()
    }
    if (is.null(p) || p == "") {
      output$register_error <- renderText("Please enter a password.")
      return()
    }
    if (p != p2) {
      output$register_error <- renderText("Passwords do not match.")
      return()
    }
    if (nchar(p) < 6) {
      output$register_error <- renderText("Password must be at least 6 characters.")
      return()
    }

    existing <- dbGetQuery(db_con, "SELECT username FROM users WHERE username = ?", params = list(u))
    if (nrow(existing) > 0) {
      output$register_error <- renderText("That username is already taken. Please choose another.")
      return()
    }

    salt <- make_salt()
    hashed <- hash_password(p, salt)
    role_val <- if (is.null(input$reg_role) || input$reg_role == "") "General Citizen" else input$reg_role
    bg_val <- if (is.null(input$reg_avatar_bg)) "#E9D5FF" else input$reg_avatar_bg
    face_val <- if (is.null(input$reg_avatar_face)) "\U0001F642" else input$reg_avatar_face
    name_val <- if (nm == "") u else nm

    dbExecute(db_con, "
      INSERT INTO users (username, name, email, password_hash, salt, role, avatar_bg, avatar_face, created_at, last_login)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    ", params = list(
      u, name_val, em, hashed, salt, role_val,
      bg_val, face_val, as.character(Sys.time()), as.character(Sys.time())
    ))

    # Log in immediately!
    current_user(u)
    current_role(role_val)
    output$register_error <- renderText("")
    output$login_error <- renderText("")
    shinyjs::hide("register_page")
    shinyjs::hide("login_page")
    shinyjs::show("main_app")
    updateTabsetPanel(session, "main_tabs", selected = "map_tab")
  })

  observeEvent(input$logout_btn, {
    current_user(NULL); current_role(NULL)
    hide("main_app"); show("login_page")
  })

  output$welcome_text <- renderText({
    req(current_user())
    paste0("Welcome, ", current_user(), " (", current_role(), ")")
  })

  # ---- Activity logging (shared by report views + chatbot) ----
  log_activity <- function(district, category, source) {
    req(current_user())
    dbExecute(db_con, "INSERT INTO history (username, district, category, source, viewed_at) VALUES (?, ?, ?, ?, ?)",
              params = list(current_user(), district, category, source, as.character(Sys.time())))
  }

  # ---- Dashboard ----
  output$dash_avg <- renderText(round(state_avg_rate, 1))
  output$dash_high <- renderText({
    r <- master_df[which.max(master_df$total_crime_rate_2023), ]
    paste0(r$district, " (", r$total_crime_rate_2023, ")")
  })
  output$dash_low <- renderText({
    r <- master_df[which.min(master_df$total_crime_rate_2023), ]
    paste0(r$district, " (", r$total_crime_rate_2023, ")")
  })

  # ---- State Map ----
  output$state_map <- renderLeaflet({
    field <- input$state_crime_type
    field_label <- names(category_choices)[category_choices == field]
    map_data <- map_sf %>%
      mutate(
        fill_color = color_for_value(field, .data[[field]]),
        display_value = round(.data[[field]], 2),
        popup_text = paste0("<b>", district, "</b><br>", field_label, ": ", display_value, " per lakh")
      )
    leaflet(map_data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~fill_color, weight = 1, color = "white", fillOpacity = 0.8,
        layerId = ~district,
        label = ~district,
        labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, direction = "center",
                                     style = list("font-weight" = "600", "font-size" = "9px", "text-shadow" = "0 0 2px white")),
        highlightOptions = highlightOptions(weight = 3, color = "#4F46E5", bringToFront = TRUE),
        popup = ~popup_text
      )
  })

  observeEvent(input$state_map_shape_click, {
    click <- input$state_map_shape_click
    req(click$id)
    current_district(click$id)
    updateSelectInput(session, "state_district_select", selected = click$id)
    updateSelectInput(session, "district_select", selected = click$id)
    updateTabsetPanel(session, "main_tabs", selected = "districtmap_tab")
  })

  observeEvent(input$state_district_select, {
    req(input$state_district_select != "")
    current_district(input$state_district_select)
    updateSelectInput(session, "district_select", selected = input$state_district_select)
    updateTabsetPanel(session, "main_tabs", selected = "districtmap_tab")
  })

  # ---- District Map ----
  output$districtmap_title <- renderText({ req(current_district()); current_district() })

  # Shared reactive: figures out which geometry to show for the current
  # district + category, once, so both the map and the note text read
  # from the same source instead of one output writing to another.
  district_map_info <- reactive({
    req(current_district())
    name <- current_district()
    field <- input$district_crime_type
    val <- master_df[[field]][master_df$district == name]
    color <- color_for_value(field, val)
    taluks_here <- taluk_sf %>% filter(district == name)

    if (nrow(taluks_here) > 0) {
      list(mode = "taluk", data = taluks_here, color = color,
           note = paste0(nrow(taluks_here), " taluks shown with place names (real sub-district boundaries). Color reflects the district-level rate."))
    } else {
      single <- map_sf %>% filter(district == name)
      if (nrow(single) == 0) {
        list(mode = "none", data = NULL, color = color,
             note = "No boundary polygon is available for this district (it has no separate shape in the current boundary file -- see the project's known limitations).")
      } else {
        list(mode = "outline", data = single, color = color,
             note = "Taluk-level detail is not available for this district in the current boundary file (created after the file's administrative vintage) -- showing the district outline only.")
      }
    }
  })

  output$district_zoom_map <- renderLeaflet({
    info <- district_map_info()
    if (info$mode == "taluk") {
      leaflet(info$data) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(fillColor = info$color, weight = 1, color = "white", fillOpacity = 0.8,
                    label = ~taluk,
                    labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, direction = "center",
                                                 style = list("font-weight" = "600", "font-size" = "9px")))
    } else if (info$mode == "outline") {
      leaflet(info$data) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(fillColor = info$color, weight = 1.4, color = "#4F46E5", fillOpacity = 0.8,
                    label = ~district,
                    labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, direction = "center",
                                                 style = list("font-weight" = "600", "font-size" = "11px")))
    } else {
      leaflet() %>% addProviderTiles("CartoDB.Positron") %>% setView(lng = 78.6, lat = 11, zoom = 6)
    }
  })

  output$taluk_note <- renderText({ district_map_info()$note })

  output$district_locator_map <- renderLeaflet({
    req(current_district())
    name <- current_district()
    loc_data <- map_sf %>% mutate(hl = if_else(district == name, "#4F46E5", "#CBD5E1"))
    leaflet(loc_data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(fillColor = ~hl, weight = 0.5, color = "white", fillOpacity = 0.9)
  })

  output$districtmap_value <- renderText({
    req(current_district())
    name <- current_district(); field <- input$district_crime_type
    val <- master_df[[field]][master_df$district == name]
    avg <- mean(master_df[[field]], na.rm = TRUE)
    paste0(names(category_choices)[category_choices == field], ": ", val,
           " per lakh (state avg for this category: ", round(avg, 2), ")")
  })

  observeEvent(input$goto_report_btn, { updateTabsetPanel(session, "main_tabs", selected = "report_tab") })

  # ---- District Report ----
  selected_row <- reactive({
    req(input$district_select)
    master_df %>% filter(district == input$district_select)
  })

  observeEvent(input$district_select, { current_district(input$district_select) })

  output$category_plot_ui <- renderUI({ plotOutput("category_plot", height = "220px") })

  output$category_plot <- renderPlot({
    d <- selected_row(); req(nrow(d) == 1)
    cats <- data.frame(
      category = c("Murder", "Crimes against\nwomen", "Road\naccidents", "Suicides"),
      rate = c(d$murder_rate, d$women_crime_rate, d$road_accident_rate, d$suicide_rate)
    )
    barplot(cats$rate, names.arg = cats$category,
            col = c("#7C3AED", "#EC4899", "#EA580C", "#0284C7"),
            ylab = "Rate per lakh population", border = NA)
  })

  comparable_districts <- reactive({
    d <- selected_row(); req(nrow(d) == 1)
    others <- master_df %>% filter(district != d$district)
    others %>%
      mutate(diff = abs(population_lakhs_2023 - d$population_lakhs_2023)) %>%
      arrange(diff) %>%
      head(3) %>%
      select(district, total_crime_rate_2023, population_lakhs_2023)
  })

  output$district_report_ui <- renderUI({
    d <- selected_row(); req(nrow(d) == 1)
    role <- if (is.null(current_role())) "General Citizen" else current_role()
    note <- build_role_note(role, d)
    log_activity(d$district, "report", "report")

    tagList(
      h3(d$district),
      p(strong("Rank: "), d$rank, " of 38 districts"),
      p(strong("Crime rate: "), d$total_crime_rate_2023, " per lakh  |  ", strong("State average: "), round(state_avg_rate, 1)),
      h4("District Profile"),
      p(strong("Area: "), d$area_sqkm, " sq.km | ", strong("Population (2011): "), format(d$population_2011, big.mark = ","),
        " | ", strong("Literacy: "), d$literacy_rate_pct, "% | ", strong("Urban: "), d$urban_pct, "%"),
      if (isTRUE(d$demographics_is_borrowed))
        p(em("This district was created after the 2011 Census; demographic figures are borrowed from its parent district.")),
      uiOutput("category_plot_ui"),
      h4("Comparable Districts (by population)"),
      tableOutput("compare_table"),
      div(class = "sticky-note", h4("Interpretation"), p(note))
    )
  })

  output$compare_table <- renderTable(comparable_districts())

  # ---- Downloads ----
  output$download_report_html <- downloadHandler(
    filename = function() paste0(input$district_select, "_report_2023.html"),
    content = function(file) {
      d <- as.list(selected_row())
      role <- if (is.null(current_role())) "General Citizen" else current_role()
      note <- build_role_note(role, selected_row())
      cmp <- comparable_districts()
      html <- build_report_html(d, round(state_avg_rate, 1), note, cmp)
      writeLines(html, file)
    }
  )
  output$download_data <- downloadHandler(
    filename = function() paste0(input$district_select, "_data.", input$data_format),
    content = function(file) {
      d <- selected_row()
      if (input$data_format == "csv") write_csv(d, file) else write_json(as.list(d), file, pretty = TRUE, auto_unbox = TRUE)
    }
  )

  observeEvent(input$save_history_btn, {
    req(current_user(), input$district_select)
    log_activity(input$district_select, "report", "saved")
    showNotification("Saved to your history.", type = "message")
  })

  # ---- History / Profile ----
  output$history_table <- renderDT({
    req(current_user())
    dbGetQuery(db_con, "SELECT district AS District, category AS Category, source AS Source, viewed_at AS Viewed
                         FROM history WHERE username = ? ORDER BY viewed_at DESC", params = list(current_user()))
  })

  output$profile_ui <- renderUI({
    req(current_user())
    u <- dbGetQuery(db_con, "SELECT * FROM users WHERE username = ?", params = list(current_user()))
    req(nrow(u) == 1)
    hist <- dbGetQuery(db_con, "SELECT district, source FROM history WHERE username = ?", params = list(current_user()))
    chat_count <- sum(hist$source == "chat")
    distinct_districts <- length(unique(hist$district))
    most_viewed <- if (nrow(hist) > 0) names(sort(table(hist$district), decreasing = TRUE))[1] else "--"

    tagList(
      div(class = "avatar-preview", style = paste0("background:", u$avatar_bg[1], ";"), u$avatar_face[1]),
      p(strong("Username: "), u$username[1]),
      p(strong("Role: "), u$role[1]),
      p(strong("Joined: "), u$created_at[1]),
      p(strong("Last login: "), u$last_login[1]),
      actionButton("edit_avatar_btn", "Edit Avatar"),
      uiOutput("avatar_edit_ui"),
      h4("Your Activity"),
      p(strong("Districts explored: "), distinct_districts),
      p(strong("Chatbot questions asked: "), chat_count),
      p(strong("Most-viewed district: "), most_viewed),
      p(em("Stored in your account's database -- visible only to you."), style = "font-size:11px;color:#64748B;")
    )
  })

  observeEvent(input$edit_avatar_btn, {
    u <- dbGetQuery(db_con, "SELECT * FROM users WHERE username = ?", params = list(current_user()))
    output$avatar_edit_ui <- renderUI({
      tagList(
        fluidRow(
          column(6, selectInput("edit_avatar_bg", "Background", choices = avatar_bg_choices, selected = u$avatar_bg[1])),
          column(6, selectInput("edit_avatar_face", "Face", choices = avatar_face_choices, selected = u$avatar_face[1]))
        ),
        actionButton("save_avatar_btn", "Save Avatar", class = "btn-primary")
      )
    })
  })

  observeEvent(input$save_avatar_btn, {
    dbExecute(db_con, "UPDATE users SET avatar_bg = ?, avatar_face = ? WHERE username = ?",
              params = list(input$edit_avatar_bg, input$edit_avatar_face, current_user()))
    output$avatar_edit_ui <- renderUI(NULL)
    showNotification("Avatar updated.", type = "message")
  })

  # ---- Chatbot ----
  output$chat_role_picker <- renderUI({
    if (is.null(chat_role())) {
      tagList(
        p("Hi! I'm the TN Crime Insight assistant. To tailor my answers, what's your role?"),
        div(lapply(chat_role_choices, function(r) actionButton(paste0("chatrole_", gsub("[^A-Za-z]", "", r)), r)))
      )
    } else {
      NULL
    }
  })

  lapply(chat_role_choices, function(r) {
    btn_id <- paste0("chatrole_", gsub("[^A-Za-z]", "", r))
    observeEvent(input[[btn_id]], {
      chat_role(r)
      chat_log(list(list(who = "bot", text = paste0("Got it -- I'll answer as if you're a ", r, ". Ask me things like \"crime in Chennai\", \"which district is highest\", or \"compare Salem and Madurai\"."))))
    }, ignoreInit = TRUE)
  })

  output$chat_messages <- renderUI({
    msgs <- chat_log()
    tagList(lapply(msgs, function(m) {
      div(class = paste("chat-msg", m$who), m$text)
    }))
  })

  observeEvent(input$chat_send, {
    req(input$chat_input, chat_role())
    user_text <- input$chat_input
    log <- chat_log()
    log <- append(log, list(list(who = "user", text = user_text)))
    chat_log(log)
    updateTextInput(session, "chat_input", value = "")

    result <- chatbot_respond(user_text, chat_role(), chat_context(), master_df, all_districts, state_avg_rate)
    chat_context(result$context)
    if (!is.null(result$log)) {
      log_activity(result$log$district, result$log$category, "chat")
    }
    log <- append(chat_log(), list(list(who = "bot", text = result$reply)))
    chat_log(log)
  })
}

shinyApp(ui, server)
