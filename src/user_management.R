
  observeEvent(input$load_project, {
    project_id <- input$load_project_id
    current_user <- currentuser()

    if (project_id != "") {
      tryCatch({
        pool::poolWithTransaction(db_pool, function(conn) {
          result <- dbGetQuery(conn, glue::glue("SELECT COUNT(*) as user_count FROM madi_lumi_users.project_users WHERE project_id = {project_id} AND user_id = '{current_user}'"))
          if (result$user_count <= 0)
            stop("no_access")
          project_name_query <- glue::glue("SELECT project_name FROM madi_lumi_users.projects WHERE project_id = {project_id}")
          project_name_result <- dbGetQuery(conn, project_name_query)
          if (nrow(project_name_result) == 0)
            stop("not_found")
          project_name <- project_name_result$project_name[1]

          query_check <- sprintf("SELECT 1 FROM madi_results.xmap_users WHERE auth0_user = %s", dbQuoteLiteral(conn, current_user))
          exists <- dbGetQuery(conn, query_check)
          if (nrow(exists) == 0) {
            query_insert <- glue::glue("INSERT INTO madi_results.xmap_users (auth0_user, project_name, workspace_id) VALUES ({dbQuoteLiteral(conn, current_user)}, {dbQuoteLiteral(conn, project_name)}, {project_id})")
            dbExecute(conn, query_insert); message("New user inserted in xmap.")
          } else {
            query_update <- glue::glue("UPDATE madi_results.xmap_users SET project_name = {dbQuoteLiteral(conn, project_name)}, workspace_id = {project_id} WHERE auth0_user = {dbQuoteLiteral(conn, current_user)}")
            dbExecute(conn, query_update); message("User updated in xmap.")
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
    } else {
      showNotification("Please enter a project ID.", type = "error")
    }
  })

observeEvent(input$create_project, {
  project_name <- input$project_name
  current_user <- currentuser()
  user_session <- usersession()

  if (project_name != "") {
    tryCatch({
      pool::poolWithTransaction(db_pool, function(conn) {
        dbExecute(conn, glue::glue("INSERT INTO madi_lumi_users.projects (project_name, creation_date) VALUES ('{project_name}', CURRENT_TIMESTAMP)"))
        new_project_id <- dbGetQuery(conn, "SELECT currval(pg_get_serial_sequence('madi_lumi_users.projects', 'project_id')) AS project_id")[1, 'project_id']
        dbExecute(conn, glue::glue("INSERT INTO madi_lumi_users.project_users (project_id, user_id, is_owner, joined_date) VALUES ({new_project_id}, '{current_user}', TRUE, CURRENT_TIMESTAMP)"))
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
  project_number <- input$project_id
  access_id <- input$access_id
  current_user <- currentuser()

  uuid_regex <- "^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$"

  if (project_number != "" & access_id != "") {
    if (grepl(uuid_regex, access_id)) {
      result <- dbGetQuery(conn, glue::glue("SELECT COUNT(*) as project_count FROM madi_lumi_users.project_access_keys WHERE project_id = {project_number} AND access_key = {dbQuoteLiteral(conn, access_id)}"))

      if (result$project_count > 0) {
        user_check <- dbGetQuery(conn, glue::glue("SELECT COUNT(*) as user_count FROM madi_lumi_users.project_users WHERE project_id = {project_number} AND user_id = {dbQuoteLiteral(conn, current_user)}"))

        if (user_check$user_count == 0) {
          dbExecute(conn, glue::glue("INSERT INTO madi_lumi_users.project_users (project_id, user_id, is_owner, joined_date) VALUES ({project_number}, {dbQuoteLiteral(conn, current_user)}, FALSE, CURRENT_TIMESTAMP)"))
          showNotification(glue::glue("Project {project_number} added successfully for user {current_user}."), type = "message")
        } else {
          showNotification(glue::glue("User {current_user} is already a member of project {project_number}."), type = "warning")
        }
      } else {
        showNotification("Invalid project number or access ID.", type = "error")
      }
    } else {
      showNotification("Invalid UUID format for access ID.", type = "error")
    }
  } else {
    showNotification("Please enter both project number and access ID.", type = "error")
  }
  updateTextInput(session, "project_id", value = "")
  updateTextInput(session, "access_id", value = "")
  refreshTabUI("manage_project_tab")
})
