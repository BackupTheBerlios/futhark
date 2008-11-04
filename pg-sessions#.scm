(##namespace
 ("pg-sessions#"
  pg-session-dbname
  pg-session-user
  pg-session-password

  pg-session-driver
  ))


(include "ehwas-sessions#.scm")
(current-session-driver pg-session-driver)