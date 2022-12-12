;;; xml.el --- XML related

;;; Commentary:
;; Settings for delaing with XML

;;; Code:
(add-hook 'nxml-mode-hook 'sgml-mode)
(add-hook 'nxml-mode-hook 'hs-minor-mode)

(provide 'xml-config)
