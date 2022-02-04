;;; lsp-netbeans.el --- Apache Netbeans Language Server Client settings  -*- lexical-binding: t; -*-

;; Author: Tim Felgentreff <timfelgentreff@gmail.com>
;; Package-Requires: ((emacs "25.1") (lsp-mode "6.0") (markdown-mode "2.3") (dash "2.18.0") (f "0.20.0") (ht "2.0") (request "0.3.0") (treemacs "2.5") (dap-mode "0.5"))
;; Keywords: languague tools java groovy graalvm lsp
;; URL: https://github.com/timfel/lsp-netbeans

;; Copyright (C) 2021  Tim Felgentreff
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; LSP client for the Apache Netbeans LSP server.

;; Currently missing many features that are in the VSCode extension, including, but not limited to:
;; - All custom commands, such as
;;     java.build.workspace
;;     java.generate.getters
;;     java.generate.setters
;;     java.generate.getters.setters
;;     java.generate.implementMethod
;;     java.generate.overrideMethod
;;     java.generate.equals.hashCode
;;     java.generate.delegateMethod
;;     java.generate.constructor
;;     java.generate.logger
;;     java.generate.toString
;;     graalvm.pause.script
;;     java.super.implementation
;; - Groovy support
;; - Managing/running/debugging unittest directly
;; - GraalVM Native Image launch/debug configuration
;; - Attach to JVM process debug configs
;; - Launch JVM debug config

;;; Code:

(require 'lsp-mode)

(defcustom lsp-netbeans-download-url "https://ci-builds.apache.org/job/Netbeans/job/netbeans-vscode/lastSuccessfulBuild/artifact/java/java.lsp.server/build/*zip*/build.zip"
  "URL to download the Apache Netbeans LSP server from"
  :group 'lsp-netbeans
  :type 'string)

(defcustom lsp-netbeans-jdk nil
  "JDK home to run netbeans"
  :group 'lsp-netbeans
  :type 'directory)

(defcustom lsp-netbeans-install-dir (f-join lsp-server-install-dir "asf.apache-netbeans-java")
  "Apache Netbeans language server installation dir"
  :group 'lsp-netbeans
  :type 'string)

(defcustom-lsp lsp-netbeans-javadoc-load-timeout 10
  "Load timeout for docs."
  :type 'number
  :group 'lsp-netbeans
  :package-version '(lsp-mode . "6.2")
  :lsp-path "netbeans.javadoc.load.timeout")

(defun lsp-netbeans-server-command (main-port)
  (let ((cmd (list (f-join lsp-netbeans-install-dir "run.sh"))))
    (if (not (string-empty-p lsp-netbeans-jdk))
        (progn
          (add-to-list 'cmd "--jdkhome" t)
          (add-to-list 'cmd  lsp-netbeans-jdk t)))
    (if lsp-log-io
        (add-to-list 'cmd "-J-Dnetbeans.logger.console=true" t))
    (add-to-list 'cmd "--start-java-debug-adapter-server=listen:0" t)
    (add-to-list 'cmd (format "--start-java-language-server=listen:%d" main-port) t)
    cmd))

(defun lsp-netbeans--install-server (_client callback error-callback update?)
  (let* ((backup-dir (concat lsp-netbeans-install-dir "-backup-" (format-time-string "%d-%m-%Y"))))
    (if (or update?
            (and (f-exists? lsp-netbeans-install-dir)
                 (not (f-exists? backup-dir)))
            (not (f-exists? lsp-netbeans-install-dir)))
        (progn
          (if (f-exists? lsp-netbeans-install-dir)
              (progn
                (if (f-exists? backup-dir)
                    (delete-directory backup-dir t))
                (f-move lsp-netbeans-install-dir backup-dir)))
          (delete-directory lsp-netbeans-install-dir t)
          (make-directory lsp-netbeans-install-dir t)
          (let* ((download-path (f-join lsp-netbeans-install-dir "vspackage.zip")))
            (lsp-async-start-process
             (lambda ()
               (message "Unzipping %s" download-path)
               (lsp-unzip
                download-path
                lsp-netbeans-install-dir)
               (let ((vsix (car (directory-files-recursively lsp-netbeans-install-dir ".*\\.vsix"))))
                 (message "Unzipping %s" vsix)
                 (lsp-unzip
                  vsix
                  lsp-netbeans-install-dir))
               (let ((run-script-path (f-join lsp-netbeans-install-dir "run.sh")))
                 (with-temp-file run-script-path
                   (insert "#!/bin/bash
                      cd `dirname $0`
                      cd extension
                      node out/nbcode.js $@ >&2"))
                 (shell-command (concat "chmod u+x " run-script-path)))
               (message "Done downloading Netbeans LSP server")
               (funcall callback))
             error-callback
             "sh" "-c" (format "curl -L -C - --output %s %s" download-path lsp-netbeans-download-url)))))))

(lsp-interface (ShowQuickPickParams (:placeHolder :canPickMany :items) nil)
               (QuickPickItem (:label :picked :userData) nil))

(defun lsp-netbeans--show-quick-pick (_workspace params)
  (let* ((canPickAll (ht-get params "canPickMany"))
         (selectfunc (if canPickAll #'completing-read-multiple #'completing-read))
         (msg (ht-get params "placeHolder"))
         (items (cl-map 'list
                        (lambda (item) (ht-get item "label"))
                        (ht-get params "items")))
         (result (funcall-interactively selectfunc (format "%s%s " msg (if canPickAll " (* for all)" "")) items))
         (choices (if (listp result)
                      (if (equal result '("*"))
                          items
                        result)
                    (list result))))
    (vconcat (seq-filter #'identity (cl-map 'list
                                            (lambda (item)
                                              (if (member (ht-get item "label") choices)
                                                  (progn
                                                    (ht-set! item "picked" t)
                                                    item)
                                                nil))
                                            (ht-get params "items"))))))

(defun lsp-netbeans--show-input-box (_workspace params)
  (let* ((msg (format "%s: " (ht-get params "prompt")))
         (val (or (ht-get params "value") "")))
    (read-string msg val)))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-tcp-connection 'lsp-netbeans-server-command)
  :activation-fn (lsp-activate-on "java")
  :server-id 'netbeans
  :initialization-options (list :nbcodeCapabilities
                                (list
                                 :testResultsSupport (lsp-json-bool nil)
                                 :statusBarMessageSupport (lsp-json-bool nil)
                                 :wantsGroovySupport (lsp-json-bool nil)))
  :priority 10
  :multi-root t
  :request-handlers (ht
                     ("window/showQuickPick" #'lsp-netbeans--show-quick-pick)
                     ("window/showInputBox" #'lsp-netbeans--show-input-box))
  :download-server-fn #'lsp-netbeans--install-server))

(defun lsp-netbeans-source-action ()
  "Source generators."
  (interactive)
  (lsp-execute-code-action-by-kind "source"))

(defun lsp-netbeans-refactor-action ()
  "Refactorings."
  (interactive)
  (lsp-execute-code-action-by-kind "refactor"))

(defun lsp-netbeans-build-project ()
  (interactive)
  (lsp-request-async "workspace/executeCommand"
                     (list :command "java.build.workspace" :arguments '())
                     (lambda (result)
                       (setf (lsp--workspace-status-string (cl-first (lsp-workspaces))) nil)
                       (force-mode-line-update)
                       (pcase result
                         (1 (lsp--info "Successfully build project."))
                         (2 (lsp--error "Failed to build project."))))))

(defun lsp-netbeans-get-project-packages ()
  (interactive)
  (message "%s"
           (lsp-request "workspace/executeCommand"
                        (list :command "java.get.project.packages"
                              :arguments (vector (format "file://%s" buffer-file-name))))))

(defun lsp-netbeans-resolve-project-problems ()
  (interactive)
  (message "%s"
           (lsp-request "workspace/executeCommand"
                        (list :command "java.project.resolveProjectProblems"
                              :arguments (vector (format "file://%s" buffer-file-name))))))

(defun lsp-netbeans-clear-caches ()
  (interactive)
  (lsp-request "workspace/executeCommand" (list :command "java.clear.project.caches")))

(defun lsp-netbeans--load-tests ()
  (if-let* ((project-root (lsp-find-session-folder (lsp-session) (buffer-file-name)))
            (tests (lsp-request
                    "workspace/executeCommand"
                    (list :command "java.load.workspace.tests"
                          :arguments (vector (format "file://%s" project-root))))))
      (apply #'cl-concatenate 'list
             (cl-map 'list
                     (lambda (group)
                       (cl-map 'list
                               (lambda (test)
                                 `(,(ht-get test "id")
                                   ,test))
                               (ht-get group "tests")))
                     tests))))

(defun lsp-netbeans--xref-from-file-references (file-references)
  "Create an xref buffer from FILE-REFERENCES ((file line column display-text) ...)."
  ;; Needed for xref API.
  (require 'xref)
  (let ((xrefs (list)))
    (dolist (item file-references)
      (pcase-let ((`(,file ,line ,col ,display-text) item))
        (push (xref-make display-text (xref-make-file-location file line col)) xrefs))
      (xref--show-xrefs (lambda () xrefs) nil))))


(defun lsp-netbeans-super-impl ()
  (interactive)
  (lsp-find-locations
   "workspace/executeCommand"
   (list
    :command "java.super.implementation"
    :arguments (vector
                (format "file://%s" buffer-file-name)
                (lsp--cur-position)))))

(defun lsp-netbeans-organize-imports ()
  "Organize java imports."
  (interactive)
  (lsp-execute-code-action-by-kind "source.organizeImports"))

(defun lsp-netbeans-kill-userdir ()
  (interactive)
  (f-delete (expand-file-name "~/.config/Code/User/globalStorage/asf.apache-netbeans-java") t))

(provide 'lsp-netbeans)

;;; lsp-netbeans.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
