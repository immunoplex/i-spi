# user_management.R -- Manage Project: load, create and join projects.
# Every query passes user input as a bound parameter ($1, $2, ...), never as
# SQL text, and all database work goes through db_pool.

# A project number typed by the user -> integer, or NA if it is not one.
.project_number <- function(x) {
  x <- trimws(as.character(x %||% ""))
  if (!grepl("^[0-9]{1,9}$", x)) return(NA_integer_)
  as.integer(x)
}

  observeEvent(input$load_project, {
    project_id <- .project_number(input$load_project_id)
    current_user <- currentuser()

    if (!nzchar(trimws(input$load_project_id %||% ""))) {
      showNotification("Please enter a project ID.", type = "error")
    } else if (is.na(project_id)) {
      showNotification("A project ID is a whole number.", type = "error")
    } else {
      tryCatch({
        pool::poolWithTransaction(db_pool, function(conn) {
          result <- dbGetQuery(conn,
            "SELECT COUNT(*) AS user_count FROM madi_lumi_users.project_users
              WHERE project_id = $1 AND user_id = $2",
            params = list(project_id, current_user))
          if (result$user_count <= 0)
            stop("no_access")
          project_name_result <- dbGetQuery(conn,
            "SELECT project_name FROM madi_lumi_users.projects WHERE project_id = $1",
            params = list(project_id))
          if (nrow(project_name_result) == 0)
            stop("not_found")
          project_name <- project_name_result$project_name[1]

          exists <- dbGetQuery(conn,
            "SELECT 1 FROM madi_results.xmap_users WHERE auth0_user = $1",
            params = list(current_user))
          if (nrow(exists) == 0) {
            dbExecute(conn,
              "INSERT INTO madi_results.xmap_users (auth0_user, project_name, workspace_id)
               VALUES ($1, $2, $3)",
              params = list(current_user, project_name, project_id))
            message("New user inserted in xmap.")
          } else {
            dbExecute(conn,
              "UPDATE madi_results.xmap_users SET project_name = $1, workspace_id = $2
                WHERE auth0_user = $3",
              params = list(project_name, project_id, current_user))
            message("User updated in xmap.")
          }
        })
        showNotification(glue::glue("Project {project_id} loaded successfully!"), type = "message")
      }, error = function(e) {
        msg <- conditionMessage(e)
        if (identical(msg, "no_access"))
          showNotification("You do not have access to the project you are trying to load.", type = "error")
        else if (identical(msg, "not_found"))
          showNotification("Project not found.", type = "error")
        else
          showNotification(glue::glue("Failed to load project '{project_id}'. Error: {msg}"), type = "error")
      })
    }
  })

observeEvent(input$create_project, {
  project_name <- trimws(input$project_name %||% "")
  current_user <- currentuser()
  user_session <- usersession()

  if (nzchar(project_name)) {
    tryCatch({
      pool::poolWithTransaction(db_pool, function(conn) {
        new_project_id <- dbGetQuery(conn,
          "INSERT INTO madi_lumi_users.projects (project_name, creation_date)
           VALUES ($1, CURRENT_TIMESTAMP) RETURNING project_id",
          params = list(project_name))$project_id[1]
        dbExecute(conn,
          "INSERT INTO madi_lumi_users.project_users (project_id, user_id, is_owner, joined_date)
           VALUES ($1, $2, TRUE, CURRENT_TIMESTAMP)",
          params = list(new_project_id, current_user))
      })
      showNotification(glue::glue("New project '{project_name}' created successfully by {current_user} in session {user_session}"), type = "message")
    }, error = function(e) {
      showNotification(glue::glue("Failed to create project '{project_name}'. Error: {conditionMessage(e)}"), type = "error")
    })
  } else {
    showNotification("Please enter a project name.", type = "error")
  }
  updateTextInput(session, "project_name", value = "")
  refreshTabUI("manage_project_tab")
})

observeEvent(input$add_project, {
  project_number <- .project_number(input$project_id)
  access_id <- trimws(input$access_id %||% "")
  current_user <- currentuser()

  uuid_regex <- "^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$"

  if (!nzchar(trimws(input$project_id %||% "")) || !nzchar(access_id)) {
    showNotification("Please enter both project number and access ID.", type = "error")
  } else if (is.na(project_number)) {
    showNotification("A project number is a whole number.", type = "error")
  } else if (!grepl(uuid_regex, access_id)) {
    showNotification("Invalid UUID format for access ID.", type = "error")
  } else {
    tryCatch({
      outcome <- pool::poolWithTransaction(db_pool, function(conn) {
        result <- dbGetQuery(conn,
          "SELECT COUNT(*) AS project_count FROM madi_lumi_users.project_access_keys
            WHERE project_id = $1 AND access_key = $2::uuid",
          params = list(project_number, access_id))
        if (result$project_count <= 0) return("invalid")
        user_check <- dbGetQuery(conn,
          "SELECT COUNT(*) AS user_count FROM madi_lumi_users.project_users
            WHERE project_id = $1 AND user_id = $2",
          params = list(project_number, current_user))
        if (user_check$user_count > 0) return("member")
        dbExecute(conn,
          "INSERT INTO madi_lumi_users.project_users (project_id, user_id, is_owner, joined_date)
           VALUES ($1, $2, FALSE, CURRENT_TIMESTAMP)",
          params = list(project_number, current_user))
        "added"
      })
      switch(outcome,
        added   = showNotification(glue::glue("Project {project_number} added successfully for user {current_user}."), type = "message"),
        member  = showNotification(glue::glue("User {current_user} is already a member of project {project_number}."), type = "warning"),
        invalid = showNotification("Invalid project number or access ID.", type = "error"))
    }, error = function(e) {
      showNotification(glue::glue("Failed to add project {project_number}. Error: {conditionMessage(e)}"), type = "error")
    })
  }
  updateTextInput(session, "project_id", value = "")
  updateTextInput(session, "access_id", value = "")
  refreshTabUI("manage_project_tab")
})
