(ns hello-world.core
(:require react-dom))

(.render js/ReactDOM
(.createElement js/React "h2" nil "Hello, React!")
           (.getElementById js/document "app"))
