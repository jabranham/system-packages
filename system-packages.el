;;; system-packages.el --- functions to manage system packages

;; Copyright (C) 2016 J. Alexander Branham

;; Author: J. Alexander Branham <branham@utexas.edu>
;; Maintainer: J. Alexander Branham <branham@utexas.edu>
;; URL: https://github.com/jabranham/system-packages
;; Version: 0.1

;; This file is not part of GNU Emacs.

;;; License:
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
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/>

;;; Commentary:

;; This is a package to manage installed system packages. Useful
;; functions include installing packages, removing packages, listing
;; installed packages, and others.

;; Usage:

;; (require 'system-packages)
;;

;;; Code:

(eval-when-compile
  (require 'cl))

(defgroup system-packages nil
  "Manages system packages"
  :tag "System Packages"
  :prefix "system-packages"
  :group 'packages)

(defconst system-packages-supported-package-managers
  '(
    ;; Arch-based systems
    (pacaur .
            ((default-sudo . nil)
             (install . "pacaur -S")
             (search . "pacaur -Ss")
             (uninstall . "pacaur -Rs")
             (update . "pacaur -Syu")
             (remove-orphaned . "pacman -Rns $(pacman -Qtdq)")
             (list-installed-packages . "pacman -Qe")
             (list-installed-packages-all . "pacman -Q")))
    (pacman .
            ((default-sudo . t)
             (install . "pacman -S")
             (search . "pacman -Ss")
             (uninstall . "pacman -Rs")
             (update . "pacman -Syu")
             (remove-orphaned . "pacman -Rns $(pacman -Qtdq)")
             (list-installed-packages . "pacman -Qe")
             (list-installed-packages-all . "pacman -Q")))
    ;; Debian (and Ubuntu) based systems
    (apt .
         ((default-sudo . t)
          (install . "apt-get install")
          (search . "apt-cache search")
          (uninstall . "apt-get remove")
          (update . ("apt-get update" "apt-get upgrade"))
          (remove-orphaned . "apt-get autoremove")
          (list-installed-packages . nil)
          (list-installed-packages-all . nil)))
    ;; Fedora
    (dnf .
         ((default-sudo . t)
          (install . "dnf install")
          (search . "dnf search")
          (uninstall . "dnf remove")
          (update . ("dnf upgrade"))
          (remove-orphaned . "dnf autoremove")
          (list-installed-packages . "dnf list --installed")
          (list-installed-packages-all . nil)))
          (list-installed-packages-all . nil)))
    ;; Mac
    (brew .
          ((default-sudo . nil)
           (install . "brew install")
           (search . "brew search")
           (uninstall . "brew uninstall")
           (update . ("brew update" "brew upgrade --all"))
           (remove-orphaned . nil)
           (list-installed-packages . "brew list")
           (list-installed-packages-all . nil)))))

(defvar system-packages-packagemanager
  (cl-loop for (name . prop) in system-packages-supported-package-managers
           for path = (executable-find (symbol-name name))
           when path
           return name)
  "String containing the package manager to use. Currently
    system-packages supports pacman, pacaur, apt, and
    homebrew. Tries to be smart about selecting the default.")

(defvar system-packages-usesudo
  (cdr (assoc 'default-sudo (cdr (assoc system-packages-packagemanager
                                        system-packages-supported-package-managers))))
  "If non-nil, system-packages will use sudo for appropriate
  commands. Tries to be smart for selecting the default.")

(defun system-packages--run-command (action &optional pack)
  "ACTION can be `default-sudo', `install', `search',
`uninstall' etc. Run the command according to
`system-packages-supported-package-managers' and ACTION."
  (let ((command
         (cdr (assoc action (cdr (assoc system-packages-packagemanager
                                        system-packages-supported-package-managers))))))
    (unless command
      (error (format "%S not supported in %S" action system-packages-packagemanager)))
    (unless (listp command)
      (setq command (list command)))
    (when system-packages-usesudo
      (setq command (mapcar (lambda (part) (concat "sudo " part)) command)))
    (setq command (mapconcat 'identity command " && "))
    (async-shell-command (mapconcat 'identity (list command pack) " "))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions on named packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun system-packages-install (pack)
  "Installs system packages using the package manager named in
system-packages-packagemanager."
  (interactive "sWhat package to install: ")
  (system-packages--run-command 'install pack))

(defun system-packages-search (pack)
  "Search for system packages using the package manager named in
system-packages-packagemanager."
  (interactive "sSearch string: ")
  (system-packages--run-command 'search pack))

(defun system-packages-uninstall (pack)
  "Uninstalls installed system packages using the package manager named in
system-packages-packagemanager."
  (interactive "sWhat package to uninstall: ")
  (system-packages--run-command 'uninstall pack))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions that don't take a named package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun system-packages-update ()
  "Updates installed system packages using the package manager named in
system-packages-packagemanager."
  (interactive)
  (system-packages--run-command 'update))

(defun system-packages-remove-orphaned ()
  "This function removes orphaned packages (i.e. unused packages). using the package manager named in
system-packages-packagemanager."
  (interactive)
  (system-packages--run-command 'remove-orphaned))

(defun system-packages-list-installed-packages (arg)
  "List explicitly installed packages using the package manager
named in system-packages-packagemanager. With
\\[universal-argument], list all installed packages."
  (interactive "P")
  (if arg
      (system-packages--run-command 'list-installed-packages-all)
    (system-packages--run-command 'list-installed-packages)))
               
(provide 'system-packages)
