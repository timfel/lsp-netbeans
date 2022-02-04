;;; dap-netbeans.el --- Apache Netbeans DAP Client

;; Copyright (C) 2021  Tim Felgentreff

;; Author: Tim Felgentreff <timfelgentreff@gmail.com>
;; Keywords: java groovy graalvm lsp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; DAP client for the Apache Netbeans LSP server.

;;; Code:

(require 'lsp-netbeans)
(require 'dap-mode)

(setq dap-netbeans--function-breakpoint nil)

(defun dap-netbeans--populate-attach-args (conf)
  (pcase (plist-get conf :name)
    ("Attach to Port" (progn
                        (dap--put-if-absent conf :hostName (read-string "Enter host: " "localhost"))
                        (dap--put-if-absent conf :port (read-string "Enter port: " "8000"))))
    ("Attach to PID" (progn
                       (dap--put-if-absent conf
                                           :processId
                                           (lsp-request "workspace/executeCommand"
                                                        (list :command "java.attachDebugger.pickProcess"))))))
  (dap--put-if-absent conf :host "localhost")
  (dap--put-if-absent conf :name (format "%s(%s)" (plist-get conf :host) (plist-get conf :port)))
  conf)

(defun dap-netbeans--populate-launch-args (conf)
  (dap--put-if-absent conf :mainClass (read-string "Enter mainClass: " "Main"))
  (dap--put-if-absent conf :methodName (read-string "Enter methodName: " "testA"))
  (dap--put-if-absent conf :host "localhost")
  (dap--put-if-absent conf :name (format "%s(%s)" (plist-get conf :host) (plist-get conf :port)))
  conf)

(defun dap-netbeans--populate-default-args (conf)
  (setq conf (plist-put conf :type "java8+"))

  (setq conf (pcase (plist-get conf :request)
               ("attach" (dap-netbeans--populate-attach-args conf))
               ("launch" (dap-netbeans--populate-launch-args conf))))

  (plist-put conf :debugServer (with-current-buffer (get-buffer "*netbeans::stderr*")
                                 (goto-char (point-max))
                                 (re-search-backward "Java Debug Server Adapter listening at port ")
                                 (forward-word 8)
                                 (thing-at-point 'word)))
  (plist-put conf :__sessionId (number-to-string (float-time)))
  conf)

(dap-register-debug-provider "java8+" #'dap-netbeans--populate-default-args)

(dap-register-debug-template "Java8+ Socket Attach"
                             (list :id "com.sun.jdi.SocketAttach"
                                   :type "java8+"
                                   :request "attach"
                                   :name "Attach to Port"))

(dap-register-debug-template "Java8+ Process Attach"
                             (list :id "com.sun.jdi.ProcessAttach"
                                   :type "java8+"
                                   :request "attach"
                                   :name "Attach to PID"))

(defun dap-netbeans--listen-for-finish (formatted-output)
  (if (string-equal "User program finished" formatted-output)
      (dap--mark-session-as-terminated (dap--cur-session)))
  formatted-output)

(defun dap-netbeans-kill-debug-session ()
  (interactive)
  (if (dap--cur-session)
      (dap--mark-session-as-terminated (dap--cur-session)))
  (dap-delete-all-sessions))

(lsp-defun dap-netbeans-debug-test ()
  (interactive)
  (-if-let* ((rawTests (lsp-netbeans--load-tests))
             (tests (apply #'cl-concatenate 'list
                           (cl-map 'list (-lambda ((item &as &netbeans:Tests :tests)) tests) rawTests)))
             (testIds (cl-map 'list (-lambda ((item &as &netbeans:Test :id)) id) tests))
             (chosenId (projectile-completing-read "Select test: " testIds))
             (selectedTest (cl-find-if
                            (-lambda ((item &as &netbeans:Test :id :file :name)) (equal chosenId id))
                            tests))
             ((&netbeans:Test :id :file :name) selectedTest)
             (mainClass (replace-regexp-in-string
                        "file:/." (lambda (txt) (if (string-match-p "/\\'" txt)
                                                  txt
                                                (format "%s//%s" (substring txt 0 -1) (substring txt -1))))
                        file))
             (methodName name))
      (dap-debug (list :id "com.sun.jdi.Launch"
                       :type "java8+"
                       :request "launch"
                       :name "Java Single Debug"
                       :testRun :json-false
                       :noDebug :json-false
                       :classPaths (list "any")
                       :mainClass mainClass
                       :output-filter-function #'dap-netbeans--listen-for-finish
                       :methodName methodName))))

(provide 'dap-netbeans)

;;; dap-netbeans.el ends here
