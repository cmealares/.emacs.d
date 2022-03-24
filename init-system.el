;;; Help on NT emacs : http://www.gnu.org/software/emacs/windows/ntemacs.html
;;;
;;; Emacs needs a HOME variable from the env or the registry. RTFM
;;;
;;; Running addpm.exe to create the registry keys is no longer recommended
;;; To have a menu, start runemacs.exe :
;;;   - In the task bar, right-click the icon and "Pin to taskbar"
;;;   - Edit this shortcut and
;;;         . change target to ...runemacs.exe
;;;         . change the "start in" directory
;;;
;;;
;;; !!! BUG !!! Ownership problem on ~/.emacs.d/server
;;;    - Waiting for Emacs server to start...
;;;    - server-ensure-safe-dir: The directory c:/Documents and Settings/user/Application Data/.emacs.d/server is unsafe
;;;
;;;  WORKAROUND 1:
;;;    Open C:\Users\cmealare\AppData\Roaming\.emacs.d
;;;    Right-click server
;;;      Properties -> Security -> Advanced -> Owner
;;;        Change ownership from Administrator to the user
;;;        Check "replace ownership on subcontainers and objects"
;;;        OK
;;;
;;;  WORKAROUND 2:
;;;    Start emacs with a .bat :
;;;    rmdir C:\Users\<user>\AppData\Roaming\.emacs.d /s /q
;;;    "C:\Emacs\bin\runemacs.exe"


;;; -----------------------------------------------------------------------
;;;; PATH & exec-path

;; (let ((paths (list
;;               "C:\\Program Files (x86)\\glo653wb\\bin"   ; gnu global
;; ))) ;              "C:\\Program Files (x86)\\ctags58")))      ; exhuberant ctags
;;   (dolist (path paths)
;;     (if (file-directory-p path)
;;         (progn
;;           (add-to-list 'exec-path (expand-file-name path) t)
;;           (setenv "PATH" (concat (getenv "PATH")
;;                                  path
;;                                  ";")))
;;       ;;    else
;;       (error "CME path not found %s" path)))

;;   ;;(message "CME UPDATED PATH %s" (getenv "PATH"))
;;   ;;(message "CME UPDATED exec-path %S" exec-path)
;;   )
