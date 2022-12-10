;;; other-languages-config.el --- Config for bulk languages

;;; Commentary:
;; Config file for other programming related stuff

;;; Code:

;; Mongo
(setq-default inf-mongo-command "/usr/bin/mongo")

;; Terraform
(straight-use-package 'terraform-mode)

(provide 'other-languages-config)
;;; other-languages-config.el ends here
