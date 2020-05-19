;;; erc-settings.el --- ERC settings

;;; Commentary:
;; IRC from Emacs
;;; Code:
(require 'erc-services)

;; Try to load ercpass file
(let ((ercpass "~/.ercpass"))
  (if (file-exists-p ercpass)
      (load ercpass)
    (progn
      (setq freenode-pass ""
            snoonet-pass "")
      (message "My init :: warn :: Could not load .ercpass"))))

(erc-services-mode 1)
(erc-autojoin-mode 0)

(setq erc-nickserv-identify-mode 'both
              erc-nickserv-passwords
              `((freenode (("caisah" . ,freenode-pass)))
                (Snoonet (("caisah" . ,snoonet-pass))))
              erc-prompt-for-nickserv-password nil)

(defun my-start-erc ()
  "Start ERC, Join Freenode & Snoonet."
  (interactive)

  (erc-tls :server "irc.snoonet.org" :port 6697 :nick "caisah" :full-name "Caisah")
  (erc-tls :server "irc.freenode.net" :port 6697 :nick "caisah" :full-name "Caisah"))

;; Export:
(provide 'erc-settings)
;;; erc-settings ends here
