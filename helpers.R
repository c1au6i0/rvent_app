# authentication ----------

# send email ------------------------
send_email <- function(mail_text) {
  error_email <-
    gm_mime() %>%
    gm_to("claudio.zanettini@gmail.com") %>%
    gm_from("cshinyapp@gmail.com") %>%
    gm_subject("Error in rvent_app") %>%
    gm_text_body(mail_text)

  # If all is good with your draft, then you can send it
  gm_send_message(error_email)
}

# snapshot-------------

# save inputs and outputs
# others : other var to uploads
get_status <- function(others = NULL) {
  sess_outputs <- outputOptions(output)

  sess_inputs <- lapply(names(input), function(x) {
    input[[x]]
  })
  names(sess_inputs) <- names(input)

  sess_rc <- list(
    dpath(),
    reactiveValuesToList(c_comments),
    vent(),
    rc_ses(),
    rc_plots(),
    summarized_dat(),
    rc_tabs(),
    demo_imp(),
    reactiveValuesToList(p_hide)
  )

  list(
    sess_outputs = sess_outputs,
    sess_inputs = sess_inputs,
    sess_rc = sess_rc, others = others
  )
}

# new_folder <- paste("session_errors", Sys.time(), sep = "/")
# upload status and iox file
# others R objects to upload
upload_snapshot <- function(others = NULL) {
  new_folder <- paste("rvent_errors", Sys.time(), sep = "/")
  drive_mkdir(new_folder)
  iox_files <- input$iox_files$datapath
  lapply(iox_files, drive_upload, path = paste0(new_folder, "/"))

  status <- get_status()
  all_status <- list(status, others)
  save(all_status, file = "status.RDA")
  drive_upload("status.RDA", path = paste0(new_folder, "/"))
  return(new_folder)
}


# callback for alert ----------
# if the user want to send an error report...
# https://stackoverflow.com/questions/34813231/create-a-popup-dialog-box-interactive
# type = "error"

modalCallback <- function(value) {
  if (value == TRUE) {
    withProgress(
    newfolder <- upload_snapshot(),
    message = "Sending data...please wait")
    mail_text <- paste0("An error occured. Check the error report at ", newfolder)
    send_email(mail_text = mail_text)
    js$reset()
  } else {
    js$reset()
  }
  
  }








