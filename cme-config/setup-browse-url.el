;;; -----------------------------------------------------------------------
;;;; BROWSE-URL
;;; -----------------------------------------------------------------------
(global-set-key (kbd "C-x m")         'browse-url-at-point)


(setq browse-url-browser-function
      (cond
       (win32-p 'cme-w32-browse-url)
       (t 'browse-url-firefox)))

(defun cme-w32-browse-url (url &optional new-window)
  "Open URL in the right browser."
  (interactive (browse-url-interactive-arg "URL: "))

  (let ((browser (cme-w32-get-browser url)))
    (when (not (file-exists-p browser))
      (error "Error: browser not found; bad path: %s" browser))

    (start-process (concat "url " url)
                   nil
                   browser
                   url)))

;; the chosen browser may depend on the url
(defun cme-w32-get-browser (url)
  (let ((firefox "C:\\Program Files\\Mozilla Firefox\\firefox.exe")
        (iexpl "C:\\Program Files\\Internet Explorer\\iexplore.exe")
        (chrome "C:\\Program Files\\Google\\Chrome\\Application\\chrome.exe"))

    (cond
     ;;((string-match "^https://sapjira" url) chrome)
     ;;((string-match "\\.sap\\(-ag\\|jam\\)?\\." url) chrome)
     (t chrome))))
