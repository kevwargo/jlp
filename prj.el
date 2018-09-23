(setq jdee-project-file-version "0.1")

(setq jdee-current-project-dir
      (file-name-directory (file-truename load-file-name)))

(setq jdee-global-classpath
      (list (concat jdee-current-project-dir "build")))
(setq jdee-sourcepath
      (list (concat jdee-current-project-dir "src")))

(jdee-backend-launch)

;; (when (featurep 'flycheck)
;;   (require 'jdee-flycheck)
;;   (jdee-flycheck-mode))

;; (setq-local flycheck-display-errors-function nil)
