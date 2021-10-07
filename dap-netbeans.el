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

(defun dap-netbeans--populate-attach-args (conf)
  (dap--put-if-absent conf :hostName (read-string "Enter host: " "localhost"))
  (dap--put-if-absent conf :port (read-string "Enter port: " "8000"))
  (dap--put-if-absent conf :host "localhost")
  (dap--put-if-absent conf :name (format "%s(%s)" (plist-get conf :host) (plist-get conf :port)))
  conf)

(defun dap-netbeans--populate-default-args (conf)
  (setq conf (plist-put conf :type "java8+"))

  (setq conf (pcase (plist-get conf :request)
               ("attach" (dap-netbeans--populate-attach-args conf))
               (_ (dap-netbeans--populate-attach-args conf))))

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
                                   :name "Attach to Port"
                                   :hostName "localhost"
                                   :port nil))
(provide 'dap-netbeans)

;;; dap-netbeans.el ends here
