(setq-local jdee-project-file-version "0.1")

(setq-local jdee-current-project-dir (file-name-directory (file-truename load-file-name)))

(setq-local jdee-global-classpath
            (list (concat jdee-current-project-dir "build")))
(setq-local jdee-sourcepath
            (list (concat jdee-current-project-dir "src")))
