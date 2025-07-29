# User Management System - GDPR Compliant
# Version: 0.0.3
# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(digest)
library(jsonlite)
library(lubridate)
library(shinyWidgets)

# Configuration
SYSTEM_NAME <- "Political Campaign Management System"
DATA_CONTROLLER <- "Your Organization Name"
DPO_EMAIL <- "dpo@yourorganization.com"
VERSION <- "0.0.3"

# International party configuration
JURISDICTIONS <- list(
  "UK" = c("", "Conservative", "Green", "Labour", "Liberal Democrat", "Reform", "Plaid Cymru", "SNP", "Other"),
  "USA" = c("", "Democrat", "Republican", "Green", "Libertarian", "Other"),
  "France" = c("", "La France Insoumise", "Parti Socialiste", "Europe Écologie", "Les Républicains", "Ensemble", "Rassemblement National", "Other"),
  "Germany" = c("", "SPD", "CDU/CSU", "FDP", "Bündnis 90/Die Grünen", "AfD", "BSW", "Other"),
  "Australia" = c("", "Labor", "Liberal", "Greens", "Nationals", "One Nation", "Other"),
  "Canada" = c("", "Liberal", "Conservative", "NDP", "Green", "Bloc Québécois", "Other"),
  "Japan" = c("", "LDP", "CDP", "Komeito", "Ishin", "JCP", "DPJ", "Other"),
  "Spain" = c("", "PSOE", "PP", "Vox", "Sumar", "Other"),
  "India" = c("", "BJP", "Congress", "AAP", "CPI", "Other")
)

# Party CSS class mapping for color coding
PARTY_CSS_CLASSES <- list(
  "UK" = list(
    "Conservative" = "uk-conservative-color",
    "Labour" = "uk-labour-color", 
    "Liberal Democrat" = "uk-libdem-color",
    "Reform" = "uk-reform-color",
    "SNP" = "uk-snp-color",
    "Plaid Cymru" = "uk-plaid-color"
  ),
  "USA" = list(
    "Democrat" = "usa-democrat-color",
    "Republican" = "usa-republican-color"
  ),
  "France" = list(
    "La France Insoumise" = "france-lfi-color",
    "Parti Socialiste" = "france-ps-color",
    "Europe Écologie" = "france-eelv-color",
    "Les Républicains" = "france-lr-color",
    "Ensemble" = "france-ensemble-color",
    "Rassemblement National" = "france-rn-color"
  ),
  "Germany" = list(
    "SPD" = "germany-spd-color",
    "CDU/CSU" = "germany-cdu-color",
    "FDP" = "germany-fdp-color",
    "Bündnis 90/Die Grünen" = "germany-greens-color",
    "AfD" = "germany-afd-color",
    "BSW" = "germany-bsw-color"
  ),
  "Australia" = list(
    "Labor" = "australia-labor-color",
    "Liberal" = "australia-liberal-color",
    "Greens" = "australia-greens-color",
    "Nationals" = "australia-nationals-color"
  ),
  "Canada" = list(
    "Liberal" = "canada-liberal-color",
    "Conservative" = "canada-conservative-color",
    "NDP" = "canada-ndp-color",
    "Green" = "canada-green-color",
    "Bloc Québécois" = "canada-bloc-color"
  ),
  "Japan" = list(
    "LDP" = "japan-ldp-color",
    "CDP" = "japan-cdp-color",
    "Komeito" = "japan-komeito-color",
    "Ishin" = "japan-ishin-color",
    "JCP" = "japan-jcp-color",
    "DPJ" = "japan-dpj-color"
  ),
  "Spain" = list(
    "PSOE" = "spain-psoe-color",
    "PP" = "spain-pp-color",
    "Vox" = "spain-vox-color",
    "Sumar" = "spain-sumar-color"
  ),
  "India" = list(
    "BJP" = "india-bjp-color",
    "Congress" = "india-congress-color",
    "AAP" = "india-aap-color",
    "CPI" = "india-cpi-color"
  )
)

# Initialize data structures
init_data <- function() {
  # Users table
  users_file <- "data/users.rds"
  audit_file <- "data/audit_log.rds"
  
  # Create data directory if it doesn't exist
  if (!dir.exists("data")) dir.create("data")
  
  # Initialize users file if it doesn't exist
  if (!file.exists(users_file)) {
    users <- data.frame(
      id = character(0),
      name = character(0),
      email = character(0),
      phone = character(0),
      jurisdiction = character(0),
      party = character(0),
      role = character(0),
      constituencies = character(0),
      password_hash = character(0),
      status = character(0),
      created_date = as.POSIXct(character(0)),
      last_login = as.POSIXct(character(0)),
      email_verified = logical(0),
      gdpr_consent = logical(0),
      gdpr_consent_date = as.POSIXct(character(0)),
      data_retention_until = as.POSIXct(character(0)),
      stringsAsFactors = FALSE
    )
    saveRDS(users, users_file)
  }
  
  # Initialize audit log
  if (!file.exists(audit_file)) {
    audit_log <- data.frame(
      timestamp = as.POSIXct(character(0)),
      user_id = character(0),
      action = character(0),
      details = character(0),
      ip_address = character(0),
      success = logical(0),
      stringsAsFactors = FALSE
    )
    saveRDS(audit_log, audit_file)
  }
}

# Audit logging function
log_action <- function(user_id, action, details, success = TRUE, ip_address = "127.0.0.1") {
  audit_log <- readRDS("data/audit_log.rds")
  new_entry <- data.frame(
    timestamp = Sys.time(),
    user_id = user_id,
    action = action,
    details = details,
    ip_address = ip_address,
    success = success,
    stringsAsFactors = FALSE
  )
  audit_log <- rbind(audit_log, new_entry)
  saveRDS(audit_log, "data/audit_log.rds")
}

# Password hashing
hash_password <- function(password, salt = NULL) {
  if (is.null(salt)) salt <- as.character(Sys.time())
  paste0(salt, ":", digest(paste0(password, salt), algo = "sha256"))
}

verify_password <- function(password, hash) {
  parts <- strsplit(hash, ":")[[1]]
  if (length(parts) != 2) return(FALSE)
  salt <- parts[1]
  stored_hash <- parts[2]
  test_hash <- digest(paste0(password, salt), algo = "sha256")
  return(test_hash == stored_hash)
}

# Data access functions with party segregation
get_users <- function(requesting_jurisdiction = NULL, requesting_party = NULL, requesting_role = NULL) {
  users <- readRDS("data/users.rds")
  
  # Sysadmin can see all users
  if (!is.null(requesting_role) && requesting_role == "sysadmin") {
    return(users)
  }
  
  # Others can only see users from their jurisdiction and party
  if (!is.null(requesting_jurisdiction)) {
    users <- users[users$jurisdiction == requesting_jurisdiction, ]
  }
  
  if (!is.null(requesting_party)) {
    users <- users[users$party == requesting_party, ]
  }
  
  return(users)
}

# Function to get party CSS class
get_party_css_class <- function(jurisdiction, party) {
  if (jurisdiction %in% names(PARTY_CSS_CLASSES) && 
      party %in% names(PARTY_CSS_CLASSES[[jurisdiction]])) {
    return(PARTY_CSS_CLASSES[[jurisdiction]][[party]])
  }
  return("")
}

# GDPR functions
generate_data_export <- function(user_id) {
  users <- readRDS("data/users.rds")
  audit_log <- readRDS("data/audit_log.rds")
  
  user_data <- users[users$id == user_id, ]
  user_audit <- audit_log[audit_log$user_id == user_id, ]
  
  export_data <- list(
    personal_data = user_data,
    activity_log = user_audit,
    export_date = Sys.time(),
    data_controller = DATA_CONTROLLER
  )
  
  return(export_data)
}

delete_user_data <- function(user_id, reason) {
  users <- readRDS("data/users.rds")
  
  # Log the deletion
  log_action("system", "user_deletion", paste("User", user_id, "deleted. Reason:", reason))
  
  # Remove user data (GDPR right to be forgotten)
  users <- users[users$id != user_id, ]
  saveRDS(users, "data/users.rds")
  
  # Anonymize audit logs (keep for legal compliance but remove personal identifiers)
  audit_log <- readRDS("data/audit_log.rds")
  audit_log$user_id[audit_log$user_id == user_id] <- "DELETED_USER"
  saveRDS(audit_log, "data/audit_log.rds")
}

# UI
ui <- dashboardPage(
  dashboardHeader(
    title = tagList(
      "User Management System",
      tags$small(style = "font-style: italic; font-size: 12px; margin-left: 10px;", paste("Version", VERSION))
    )
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("User Management", tabName = "users", icon = icon("users")),
      menuItem("Audit Log", tabName = "audit", icon = icon("list")),
      menuItem("GDPR Compliance", tabName = "gdpr", icon = icon("shield-alt")),
      menuItem("System Settings", tabName = "settings", icon = icon("cog"))
    )
  ),
  
  dashboardBody(
    # Include custom CSS from css directory
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/styles.css"),
      tags$style(HTML("
        .gdpr-notice {
          background-color: #f8f9fa;
          border: 1px solid #dee2e6;
          border-radius: 0.25rem;
          padding: 1rem;
          margin: 1rem 0;
        }
        .party-colored {
          font-weight: bold;
        }
      "))
    ),
    
    tabItems(
      # User Management Tab
      tabItem(tabName = "users",
              fluidRow(
                box(
                  title = "Add New User", status = "primary", solidHeader = TRUE,
                  width = 12,
                  
                  # GDPR Notice
                  div(class = "gdpr-notice",
                      h4("Data Protection Notice"),
                      p("By creating a user account, you confirm that you have obtained explicit consent from the individual to process their personal data in accordance with GDPR. All data will be processed lawfully and securely."),
                      p(paste("Data Controller:", DATA_CONTROLLER, "| DPO Contact:", DPO_EMAIL))
                  ),
                  
                  fluidRow(
                    column(4,
                           textInput("new_name", "Full Name", placeholder = "Enter full name"),
                           textInput("new_email", "Email Address", placeholder = "user@example.com"),
                           textInput("new_phone", "Phone Number", placeholder = "+44XXXXXXXXXX")
                    ),
                    column(4,
                           selectInput("new_jurisdiction", "Jurisdiction", 
                                       choices = c("", names(JURISDICTIONS))),
                           selectInput("new_party", "Political Party", 
                                       choices = c("", "Select jurisdiction first")),
                           selectInput("new_role", "Role",
                                       choices = c("", "member", "branch", "regional", "national", "sysadmin"))
                    ),
                    column(4,
                           textAreaInput("new_constituencies", "Constituencies (JSON)", 
                                         placeholder = '["E14000123", "E14000124"]', rows = 3),
                           passwordInput("new_password", "Password"),
                           passwordInput("confirm_password", "Confirm Password")
                    )
                  ),
                  
                  fluidRow(
                    column(12,
                           checkboxInput("gdpr_consent", "User has provided GDPR consent", value = FALSE),
                           checkboxInput("email_verified", "Email verified", value = FALSE),
                           br(),
                           actionButton("add_user", "Add User", class = "btn-primary"),
                           br(), br()
                    )
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "Existing Users", status = "info", solidHeader = TRUE,
                  width = 12,
                  DT::dataTableOutput("users_table"),
                  br(),
                  fluidRow(
                    column(3, actionButton("edit_user", "Edit Selected", class = "btn-warning")),
                    column(3, actionButton("delete_user", "Delete Selected", class = "btn-danger")),
                    column(3, actionButton("export_user_data", "Export User Data", class = "btn-info")),
                    column(3, actionButton("reset_password", "Reset Password", class = "btn-secondary"))
                  )
                )
              )
      ),
      
      # Audit Log Tab
      tabItem(tabName = "audit",
              fluidRow(
                box(
                  title = "System Audit Log", status = "warning", solidHeader = TRUE,
                  width = 12,
                  p("This log maintains a record of all system activities for security and compliance purposes."),
                  DT::dataTableOutput("audit_table"),
                  br(),
                  downloadButton("download_audit", "Download Audit Log", class = "btn-primary")
                )
              )
      ),
      
      # GDPR Compliance Tab
      tabItem(tabName = "gdpr",
              fluidRow(
                box(
                  title = "GDPR Compliance Tools", status = "success", solidHeader = TRUE,
                  width = 12,
                  
                  h3("Data Subject Rights"),
                  p("Tools to comply with GDPR data subject rights:"),
                  
                  fluidRow(
                    column(6,
                           h4("Right to Access (Article 15)"),
                           textInput("access_user_id", "User ID for Data Export"),
                           actionButton("export_personal_data", "Generate Data Export", class = "btn-info"),
                           br(), br(),
                           
                           h4("Right to Rectification (Article 16)"),
                           p("Use the 'Edit Selected' button in User Management to update personal data."),
                           br(),
                           
                           h4("Right to Erasure (Article 17)"),
                           textInput("delete_user_id", "User ID for Deletion"),
                           textInput("deletion_reason", "Reason for Deletion"),
                           actionButton("gdpr_delete_user", "Delete User Data", class = "btn-danger")
                    ),
                    
                    column(6,
                           h4("Data Retention Management"),
                           p("Users are automatically flagged for review based on retention policies."),
                           DT::dataTableOutput("retention_review_table"),
                           br(),
                           
                           h4("Consent Management"),
                           p("Track and manage GDPR consent status:"),
                           DT::dataTableOutput("consent_table")
                    )
                  )
                )
              )
      ),
      
      # Settings Tab
      tabItem(tabName = "settings",
              fluidRow(
                box(
                  title = "System Configuration", status = "primary", solidHeader = TRUE,
                  width = 12,
                  
                  h4("Security Settings"),
                  numericInput("session_timeout", "Session Timeout (minutes)", value = 30, min = 5, max = 480),
                  numericInput("password_min_length", "Minimum Password Length", value = 8, min = 6, max = 20),
                  checkboxInput("require_2fa", "Require Two-Factor Authentication", value = FALSE),
                  
                  h4("Data Retention"),
                  numericInput("inactive_user_retention", "Inactive User Retention (days)", value = 365, min = 30),
                  numericInput("audit_log_retention", "Audit Log Retention (days)", value = 2555, min = 365), # 7 years
                  
                  h4("Google Cloud Migration Preparation"),
                  p("Current storage: Local files (data/ directory)"),
                  p("Migration checklist:"),
                  tags$ul(
                    tags$li("Set up Google Cloud Project"),
                    tags$li("Configure Cloud Firestore or Cloud SQL"),
                    tags$li("Set up Cloud KMS for encryption"),
                    tags$li("Configure Firebase Authentication"),
                    tags$li("Update data access functions"),
                    tags$li("Test data migration")
                  ),
                  
                  actionButton("save_settings", "Save Settings", class = "btn-success")
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Initialize data on startup
  init_data()
  
  # Reactive values
  values <- reactiveValues(
    users = NULL,
    audit_log = NULL,
    selected_user = NULL
  )
  
  # Load data
  observe({
    values$users <- get_users()
    values$audit_log <- readRDS("data/audit_log.rds")
  })
  
  # Update party choices based on jurisdiction selection
  observeEvent(input$new_jurisdiction, {
    if (input$new_jurisdiction != "" && input$new_jurisdiction %in% names(JURISDICTIONS)) {
      updateSelectInput(session, "new_party", 
                        choices = JURISDICTIONS[[input$new_jurisdiction]])
    } else {
      updateSelectInput(session, "new_party", 
                        choices = c("", "Select jurisdiction first"))
    }
  })
  
  # Users table with party color coding
  output$users_table <- DT::renderDataTable({
    req(values$users)
    
    display_users <- values$users
    
    # Check if there are any users to display
    if (nrow(display_users) == 0) {
      # Return empty table with proper column structure
      empty_df <- data.frame(
        id = character(0),
        name = character(0),
        email = character(0),
        phone = character(0),
        jurisdiction = character(0),
        party = character(0),
        role = character(0),
        constituencies = character(0),
        password_hash = character(0),
        status = character(0),
        created_date = character(0),
        last_login = character(0),
        email_verified = character(0),
        gdpr_consent = character(0),
        gdpr_consent_date = character(0),
        data_retention_until = character(0),
        stringsAsFactors = FALSE
      )
      
      return(DT::datatable(empty_df, 
                           selection = "single",
                           options = list(scrollX = TRUE,
                                          language = list(emptyTable = "No users found. Add a user to get started."))))
    }
    
    # Don't display password hashes
    display_users$password_hash <- "***"
    
    # Add party color coding for existing users
    for (i in 1:nrow(display_users)) {
      css_class <- get_party_css_class(display_users$jurisdiction[i], display_users$party[i])
      if (css_class != "") {
        display_users$party[i] <- paste0('<span class="party-colored ', css_class, '">', 
                                         display_users$party[i], '</span>')
      }
    }
    
    DT::datatable(display_users, 
                  selection = "single",
                  escape = FALSE,  # Allow HTML formatting
                  options = list(scrollX = TRUE))
  })
  
  # Add new user
  observeEvent(input$add_user, {
    req(input$new_name, input$new_email, input$new_jurisdiction, input$new_party, input$new_role, input$new_password)
    
    # Validation
    if (input$new_password != input$confirm_password) {
      showNotification("Passwords do not match", type = "error")
      return()
    }
    
    if (nchar(input$new_password) < 8) {
      showNotification("Password must be at least 8 characters", type = "error")
      return()
    }
    
    if (!input$gdpr_consent) {
      showNotification("GDPR consent is required", type = "error")
      return()
    }
    
    # Check if email already exists
    existing_users <- values$users
    if (input$new_email %in% existing_users$email) {
      showNotification("Email address already exists", type = "error")
      return()
    }
    
    # Create new user
    new_id <- paste0("USR", format(Sys.time(), "%Y%m%d%H%M%S"))
    
    new_user <- data.frame(
      id = new_id,
      name = input$new_name,
      email = input$new_email,
      phone = input$new_phone,
      jurisdiction = input$new_jurisdiction,
      party = input$new_party,
      role = input$new_role,
      constituencies = input$new_constituencies,
      password_hash = hash_password(input$new_password),
      status = "active",
      created_date = Sys.time(),
      last_login = as.POSIXct(NA),
      email_verified = input$email_verified,
      gdpr_consent = input$gdpr_consent,
      gdpr_consent_date = Sys.time(),
      data_retention_until = Sys.time() + days(365),
      stringsAsFactors = FALSE
    )
    
    # Save user
    users <- rbind(values$users, new_user)
    saveRDS(users, "data/users.rds")
    values$users <- users
    
    # Log action
    log_action("system", "user_created", paste("New user created:", new_id))
    
    # Clear form
    updateTextInput(session, "new_name", value = "")
    updateTextInput(session, "new_email", value = "")
    updateTextInput(session, "new_phone", value = "")
    updateSelectInput(session, "new_jurisdiction", selected = "")
    updateSelectInput(session, "new_party", choices = c("", "Select jurisdiction first"), selected = "")
    updateSelectInput(session, "new_role", selected = "")
    updateTextAreaInput(session, "new_constituencies", value = "")
    updateTextInput(session, "new_password", value = "")
    updateTextInput(session, "confirm_password", value = "")
    updateCheckboxInput(session, "gdpr_consent", value = FALSE)
    updateCheckboxInput(session, "email_verified", value = FALSE)
    
    showNotification("User created successfully", type = "success")
  })
  
  # Audit log table
  output$audit_table <- DT::renderDataTable({
    req(values$audit_log)
    DT::datatable(values$audit_log, options = list(scrollX = TRUE, order = list(list(0, 'desc'))))
  })
  
  # GDPR Export
  observeEvent(input$export_personal_data, {
    req(input$access_user_id)
    
    export_data <- generate_data_export(input$access_user_id)
    
    # In a real app, you'd email this or provide a secure download
    output$download_export <- downloadHandler(
      filename = function() {
        paste0("user_data_export_", input$access_user_id, "_", Sys.Date(), ".json")
      },
      content = function(file) {
        writeLines(toJSON(export_data, pretty = TRUE), file)
      }
    )
    
    log_action("system", "data_export", paste("Data export generated for user:", input$access_user_id))
    showNotification("Data export generated", type = "success")
  })
  
  # GDPR Deletion
  observeEvent(input$gdpr_delete_user, {
    req(input$delete_user_id, input$deletion_reason)
    
    delete_user_data(input$delete_user_id, input$deletion_reason)
    values$users <- get_users()
    
    showNotification("User data deleted in compliance with GDPR", type = "success")
  })
  
  # Consent tracking table with color coding
  output$consent_table <- DT::renderDataTable({
    req(values$users)
    
    consent_data <- values$users[, c("id", "name", "email", "jurisdiction", "party", "gdpr_consent", "gdpr_consent_date")]
    
    # Check if there are any users
    if (nrow(consent_data) == 0) {
      empty_consent <- data.frame(
        id = character(0),
        name = character(0),
        email = character(0),
        jurisdiction = character(0),
        party = character(0),
        gdpr_consent = character(0),
        gdpr_consent_date = character(0),
        stringsAsFactors = FALSE
      )
      
      return(DT::datatable(empty_consent, 
                           options = list(scrollX = TRUE,
                                          language = list(emptyTable = "No consent records found."))))
    }
    
    # Add party color coding for existing users
    for (i in 1:nrow(consent_data)) {
      css_class <- get_party_css_class(consent_data$jurisdiction[i], consent_data$party[i])
      if (css_class != "") {
        consent_data$party[i] <- paste0('<span class="party-colored ', css_class, '">', 
                                        consent_data$party[i], '</span>')
      }
    }
    
    DT::datatable(consent_data, escape = FALSE, options = list(scrollX = TRUE))
  })
  
  # Users requiring retention review
  output$retention_review_table <- DT::renderDataTable({
    req(values$users)
    
    # Check if users data exists
    if (nrow(values$users) == 0) {
      empty_review <- data.frame(
        Message = "No users in system for retention review",
        stringsAsFactors = FALSE
      )
      return(DT::datatable(empty_review, options = list(scrollX = TRUE)))
    }
    
    # Users approaching retention limit
    review_users <- values$users[values$users$data_retention_until <= Sys.time() + days(30), ]
    
    if (nrow(review_users) > 0) {
      review_data <- review_users[, c("id", "name", "email", "created_date", "data_retention_until")]
      DT::datatable(review_data, options = list(scrollX = TRUE))
    } else {
      no_review <- data.frame(
        Message = "No users require retention review",
        stringsAsFactors = FALSE
      )
      DT::datatable(no_review, options = list(scrollX = TRUE))
    }
  })
  
  # Download audit log
  output$download_audit <- downloadHandler(
    filename = function() {
      paste0("audit_log_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(values$audit_log, file, row.names = FALSE)
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)