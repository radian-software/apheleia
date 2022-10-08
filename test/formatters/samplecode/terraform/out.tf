resource "google_sql_user" "user" {
  name     = "camunda"
  instance = google_sql_database_instance.camunda-db.name

  password = "futurice"
}
