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

(defgroup system-packages nil
  "Manages system packages"
  :tag "System Packages"
  :prefix "system-packages"
  :group 'packages)

(defvar system-packages-packagemanager
  (if (executable-find "pacman") "pacman"
    (if (executable-find "apt") "apt"
      (if (executable-find "brew") "brew")))
  "String containing the package manager to use. Currently
    system-packages supports pacman, apt, and home-brew.")

(defvar system-packages-usesudo
  (if (executable-find "pacman") t
    (if (executable-find "apt") t
      (if (executable-find "brew") nil)))
  "If non-nil, system-packages will use sudo for appropriate
  commands")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions on named packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun system-packages-install (pack)
  "Installs system packages"
  (interactive "sWhat package to install?")
  (let ((command
         (if (equal system-packages-packagemanager "pacman") "pacman -S"
           (if (equal system-packages-packagemanager "apt") "apt-get install"
             (if (equal system-packages-packagemanager "brew") "brew install")))))
    (if (equal system-packages-usesudo t)
        (async-shell-command (mapconcat 'identity (list "sudo" command pack) " "))
      (async-shell-command (mapconcat 'identity (list command pack) " ")))))

(defun system-packages-search (pack)
  "Search for system packages"
  (interactive "sSearch string?")
  (let ((command
         (if (equal system-packages-packagemanager "pacman") "pacman -Ss"
           (if (equal system-packages-packagemanager "apt") "apt-cache search"
             (if (equal system-packages-packagemanager "brew") "brew search")))))
    (async-shell-command (mapconcat 'identity (list command pack) " "))))

(defun system-packages-uninstall (pack)
  "Uninstalls installed system packages"
  (interactive "sWhat package to uninstall?")
  (let ((command
         (if (equal system-packages-packagemanager "pacman") "pacman -Rs"
           (if (equal system-packages-packagemanager "apt") "apt-get remove"
             (if (equal system-packages-packagemanager "brew") "brew uninstall")))))
    (if (equal system-packages-usesudo t)
        (async-shell-command (mapconcat 'identity (list "sudo" command pack) " "))
      (async-shell-command (mapconcat 'identity (list command pack) " ")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions that don't take a named package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun system-packages-update ()
  "Updates installed system packages"
  (interactive)
  (let ((command
         (if (equal system-packages-packagemanager "pacman") "pacman -Syu"
           (if (equal system-packages-packagemanager "apt") "apt-get update && sudo apt-get upgrade"
             (if (equal system-packages-packagemanager "brew") "brew update && brew upgrade")))))
    (if (equal system-packages-usesudo t)
        (async-shell-command (mapconcat 'identity (list "sudo" command) " "))
      (async-shell-command (mapconcat 'identity (list command) " ")))))

(defun system-packages-remove-orphaned ()
  "This function removes orphaned packages i.e. unused packages."
  (interactive)
  (if (equal system-packages-packagemanager "brew")
      (error "Not supported on homebrew"))
  (let ((command
         (if (equal system-packages-packagemanager "pacman") "pacman -Rns $(pacman -Qtdq)"
           (if (equal system-packages-packagemanager "apt") "apt-get autoremove"))))
    (if (equal system-packages-usesudo t)
        (async-shell-command (mapconcat 'identity (list "sudo" command) " "))
      (async-shell-command (mapconcat 'identity (list command) " ")))))

(defun system-packages-list-installed-packages (arg)
  "List explicitly installed packages. With \\[universal-argument],
list all installed packages."
  (interactive "P")
  (if (equal system-packages-packagemanager "apt")
      (error "Not supported on apt systems"))
  (let ((command
         (if (and arg (equal system-packages-packagemanager "pacman")) "pacman -Q"
           (if (equal system-packages-packagemanager "pacman") "pacman -Qe"
             (if (equal system-packages-packagemanager "brew") "brew list")))))
         (async-shell-command command)))
               
(provide 'system-packages)
