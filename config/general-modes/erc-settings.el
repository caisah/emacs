;;; erc-settings.el --- ERC settings

;;; Commentary:
;; IRC from Emacs
;;; Code:
(require 'erc-services)

(load "~/.ercpass")

(erc-services-mode 1)
(erc-autojoin-mode 0)

(setq-default erc-nickserv-identify-mode 'both
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
