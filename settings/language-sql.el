;; Postgres syntax highlighting

(add-to-list 'auto-mode-alist
             '("\\.psql$" . (lambda ()
                              (sql-mode)
                              (sql-highlight-postgres-keywords))))

(provide 'language-sql)
