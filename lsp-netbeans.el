;;; lsp-netbeans.el --- Apache Netbeans Language Server Client settings         -*- lexical-binding: t; -*-

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

;; LSP and DAP client for the Apache Netbeans LSP server.

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

(defcustom lsp-netbeans-install-dir (f-join lsp-server-install-dir "asf.apache-netbeans-java")
  "Apache Netbeans language server installation dir"
  :group 'lsp-netbeans
  :type 'string)

(defun lsp-netbeans-server-command (main-port)
  `(,(f-join lsp-netbeans-install-dir "run.sh")
    "--start-java-debug-adapter-server=listen:0"
    ,(format "--start-java-language-server=listen:%d" main-port)))

(defun lsp-netbeans--install-server (client callback error-callback update?)
  (let* ((install-dir lsp-netbeans-install-dir)
         (backup-dir (concat install-dir "-backup-" (time-stamp-string "%d-%m-%Y"))))
    (if (or update?
            (and (f-exists? install-dir)
                 (not (f-exists? backup-dir))))
        (progn
          (if (f-exists? install-dir)
              (progn
                (if (f-exists? backup-dir)
                    (delete-directory backup-dir t))
                (f-move install-dir backup-dir)))
          (delete-directory install-dir t)
          (make-directory install-dir t)
          (let ((download-path (f-join install-dir "vspackage")))
            (lsp-download-install
             (lambda ()
               (lsp-unzip
                (car (directory-files-recursively install-dir ".*\\.vsix"))
                install-dir)
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
             :url lsp-netbeans-download-url
             :store-path download-path
             :decompress :zip))))))

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
  :download-server-fn #'lsp-netbeans--install-server))

(provide 'lsp-netbeans)

