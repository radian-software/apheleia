job "mongo-jwt" {
  namespace = "default"

  group "db" {
    network {
      port "db" {
        static = 27017
      }
    }

    task "mongo" {
      driver = "docker"

      config {
        image = "mongo:7"
        ports = ["db"]
      }
    }
  }
}
