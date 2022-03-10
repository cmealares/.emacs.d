;;; -----------------------------------------------------------------------
;;;; JSON
;;; -----------------------------------------------------------------------

(use-package origami
  :bind (("C-c f" . origami-recursively-toggle-node)
         ("C-c F" . origami-toggle-all-nodes)))


;; js2-mode can also be used for json files
(use-package json-mode
  :hook (json-mode . origami-mode))



(defun cme-pretty-json ()
  "Pretify the content of the region or whole buffer into a temporary buffer"
  (interactive)
  (let* ((regionp (region-active-p))
         (beg (and regionp (region-beginning)))
         (end (and regionp (region-end)))
         (buf (current-buffer))
         (newbuf
          ;;(generate-new-buffer "*cme-temp-json*")))
          (get-buffer-create "*cme-temp-json*")))
    (switch-to-buffer newbuf nil t)
    (erase-buffer)
    (insert-buffer-substring buf beg end)
    (if (fboundp 'json-mode)
        (progn
          (json-mode)
          (json-mode-beautify))
      (js-mode)
      (json-pretty-print-buffer))
    (goto-char 0)))
