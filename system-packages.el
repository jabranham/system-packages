;;; system-packages.el --- functions to manage system packages -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2018 J. Alexander Branham

;; Author: J. Alexander Branham <branham@utexas.edu>
;; Maintainer: J. Alexander Branham <branham@utexas.edu>
;; URL: https://github.com/jabranham/system-packages
;; Package-Requires: ((cl-lib "0.5"))

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

;; This is a package to manage installed system packages.  Useful
;; functions include installing packages, removing packages, listing
;; installed packages, and others.

;; Usage:

;; (require 'system-packages)
;;

;;; Code:

(require 'cl-lib)

(defgroup system-packages nil
  "Manages system packages"
  :tag "System Packages"
  :prefix "system-packages"
  :group 'packages)

(defconst system-packages-supported-package-managers
  '(
    ;; nix
    (nix-env .
             ((default-sudo . nil)
              (install . "nix-env -i")
              (search . "nix-env -qaP")
              (uninstall . "nix-env -e")
              (update . ("nix-env -u" ))
              (clean-cache . nil)
              (log . nil)
              (get-info . nil)
              (get-info-remote . nil)
              (list-files-provided-by . nil)
              (verify-all-packages . nil)
              (verify-all-dependencies . nil)
              (remove-orphaned . nil)
              (list-installed-packages . "nix-env -q")
              (list-installed-packages-all . "nix-env -q")
              (list-dependencies-of . nil)
              (noconfirm . nil)))
    ;; Mac
    (brew .
          ((default-sudo . nil)
           (install . "brew install")
           (search . "brew search")
           (uninstall . "brew uninstall")
           (update . ("brew update" "brew upgrade --all"))
           (clean-cache . "brew cleanup")
           (log . nil)
           (get-info . nil)
           (get-info-remote . nil)
           (list-files-provided-by . "brew ls --verbose")
           (verify-all-packages . nil)
           (verify-all-dependencies . nil)
           (remove-orphaned . nil)
           (list-installed-packages . "brew list --installed")
           (list-installed-packages-all . "brew list")
           (list-dependencies-of . "brew deps")
           (noconfirm . nil)))
    (port .
          ((default-sudo . t)
           (install . "port install")
           (search . "port search")
           (uninstall . "port uninstall")
           (update . ("port sync" "port upgrade outdated"))
           (clean-cache . "port clean --all")
           (log . "port log")
           (get-info . "port info")
           (get-info-remote . nil)
           (list-files-provided-by . "port contents")
           (verify-all-packages . nil)
           (verify-all-dependencies . nil)
           (remove-orphaned . "port uninstall leaves")
           (list-installed-packages . "port installed")
           (list-installed-packages-all . "port installed")
           (list-dependencies-of . "port deps")
           (noconfirm . nil)))
    ;; Arch-based systems
    (pacman .
            ((default-sudo . t)
             (install . "pacman -S")
             (search . "pacman -Ss")
             (uninstall . "pacman -Rns")
             (update . "pacman -Syu")
             (clean-cache . "pacman -Sc")
             (log . "cat /var/log/pacman.log")
             (get-info . "pacman -Qi")
             (get-info-remote . "pacman -Si")
             (list-files-provided-by . "pacman -Ql")
             (verify-all-packages . "pacman -Qkk")
             (verify-all-dependencies . "pacman -Dk")
             (remove-orphaned . "pacman -Rns $(pacman -Qtdq)")
             (list-installed-packages . "pacman -Qe")
             (list-installed-packages-all . "pacman -Q")
             (list-dependencies-of . "pacman -Qi")
             (noconfirm . "--noconfirm")))
    ;; Debian (and Ubuntu) based systems
    (apt .
         ((default-sudo . t)
          (install . "apt install")
          (search . "apt search")
          (uninstall . "apt remove")
          (update . ("apt update" "apt upgrade"))
          (clean-cache . "apt-get clean")
          (log . "cat /var/log/dpkg.log")
          (get-info . "dpkg -s")
          (get-info-remote . "apt show")
          (list-files-provided-by . "dpkg -L")
          (verify-all-packages . "debsums")
          (verify-all-dependencies . "apt-get check")
          (remove-orphaned . "apt autoremove")
          (list-installed-packages . nil)
          (list-installed-packages-all . nil)
          (list-dependencies-of . "apt-cache deps")
          (noconfirm . "-y")))
    (aptitude .
              ((default-sudo . t)
               (install . "aptitude install")
               (search . "aptitude search")
               (uninstall . "aptitude remove")
               (update . ("aptitude update"))
               (clean-cache . "aptitude clean")
               (log . "cat /var/log/dpkg.log")
               (get-info . "aptitude show")
               (get-info-remote . "aptitude show")
               (list-files-provided-by . "dpkg -L")
               (verify-all-packages . "debsums")
               (verify-all-dependencies . "apt-get check")
               (remove-orphaned . nil) ; aptitude does this automatically
               (list-installed-packages . "aptitude search '~i!~M'")
               (list-installed-packages-all . "aptitude search '~i!~M'")
               (list-dependencies-of . "apt-cache deps")
               (noconfirm . "-y")))
    ;; Gentoo
    (emerge .
            ((default-sudo . t)
             (install . "emerge")
             (search . "emerge -S")
             (uninstall . "emerge -C")
             (update . "emerge -u world")
             (clean-cache . "eclean distfiles")
             (log . "cat /var/log/portage")
             (get-info . "emerge -pv")
             (get-info-remote . "emerge -S")
             (list-files-provided-by . "equery files")
             (verify-all-packages . "equery check")
             (verify-all-dependencies . "emerge -uDN world")
             (remove-orphaned . "emerge --depclean")
             (list-installed-packages . nil)
             (list-installed-packages-all . nil)
             (list-dependencies-of . "emerge -ep")
             (noconfirm . nil)))
    ;; openSUSE
    (zypper .
            ((default-sudo . t)
             (install . "zypper install")
             (search . "zypper search")
             (uninstall . "zypper remove")
             (update . "zypper update")
             (clean-cache . "zypper clean")
             (log . "cat /var/log/zypp/history")
             (get-info . "zypper info")
             (get-info-remote . "zypper info")
             (list-files-provided-by . "rpm -Ql")
             (verify-all-packages . "rpm -Va")
             (verify-all-dependencies . "zypper verify")
             (remove-orphaned . "zypper rm -u")
             (list-installed-packages . nil)
             (list-installed-packages-all . nil)
             (list-dependencies-of . "zypper info --requires")
             (noconfirm . nil)))
    ;; Fedora
    (dnf .
         ((default-sudo . t)
          (install . "dnf install")
          (search . "dnf search")
          (uninstall . "dnf remove")
          (update . ("dnf upgrade"))
          (clean-cache . "dnf clean all")
          (log . "dnf history")
          (get-info . "rpm -qi")
          (get-info-remote . "dnf info")
          (list-files-provided-by . "rpm -ql")
          (verify-all-packages . "rpm -Va")
          (verify-all-dependencies . "dnf repoquery --requires")
          (remove-orphaned . "dnf autoremove")
          (list-installed-packages . "dnf list --installed")
          (list-installed-packages-all . nil)
          (list-dependencies-of . "rpm -qR")
          (noconfirm . nil)))
    ;; Void
    ;; xbps is the name of the package manager, but that doesn't appear as an
    ;; executable, so let's just call it xbps-install:
    (xbps-install .
                  ((default-sudo . t)
                   (install . "xbps-install")
                   (search . "xbps-query -Rs")
                   (uninstall . "xbps-remove -R")
                   (update . ("xbps-install -Su"))
                   (clean-cache . "xbps-remove -O")
                   (log . nil)
                   (get-info . "xbps-query")
                   (get-info-remote . "xbps-query -R")
                   (list-files-provided-by . "xbps-query -f")
                   (verify-all-packages . nil)
                   (verify-all-dependencies . "xbps-pkgdb -a")
                   (remove-orphaned . "dnf autoremove")
                   (list-installed-packages . "xbps-query -l ")
                   (list-installed-packages-all . "xbps-query -l ")
                   (list-dependencies-of . "xbps-query -x")
                   (noconfirm . nil)))))

(defcustom system-packages-package-manager
  (cl-loop for (name . prop) in system-packages-supported-package-managers
           for path = (executable-find (symbol-name name))
           when path
           return name)
  "Symbol containing the package manager to use.

See `system-packages-supported-package-managers' for a list of
supported software.  Tries to be smart about selecting the
default."
  :group 'system-packages
  :type 'symbol)

(define-obsolete-variable-alias 'system-packages-packagemanager
  'system-packages-package-manager "2017-12-25")

(defcustom system-packages-use-sudo
  (cdr (assoc 'default-sudo (cdr (assoc system-packages-package-manager
                                        system-packages-supported-package-managers))))
  "If non-nil, system-packages uses sudo for appropriate commands.

Tries to be smart for selecting the default."
  :type 'boolean
  :group 'system-packages)

(define-obsolete-variable-alias 'system-packages-usesudo
  'system-packages-use-sudo "2017-12-25")

(defcustom system-packages-noconfirm nil
  "If non-nil, bypass prompts asking the user to confirm package upgrades."
  :group 'system-packages
  :type 'boolean)

(defun system-packages--run-command (action &optional pack args)
  "Run a command that affects system packages.

ACTION can be `default-sudo', `install', `search', `uninstall'
etc.  Run the command according to
`system-packages-supported-package-managers' and ACTION.  PACK is
used to operation on specific packages.

ARGS gets passed to the command and is useful for passing options
to the package manager."
  (let ((command
         (cdr (assoc action (cdr (assoc system-packages-package-manager
                                        system-packages-supported-package-managers)))))
        (noconfirm (when system-packages-noconfirm
                     (cdr (assoc 'noconfirm
                                 (cdr (assoc system-packages-package-manager
                                             system-packages-supported-package-managers)))))))
    (unless command
      (error (format "%S not supported in %S" action system-packages-package-manager)))
    (unless (listp command)
      (setq command (list command)))
    (when system-packages-use-sudo
      (setq command (mapcar (lambda (part) (concat "sudo " part)) command)))
    (setq command (mapconcat 'identity command " && "))
    (setq command (mapconcat 'identity (list command pack) " "))
    (setq args (concat args noconfirm))
    (when args
      (setq command (concat command args)))
    (async-shell-command command "*system-packages*")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions on named packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun system-packages-install (pack &optional args)
  "Install system packages.

Use the package manager from `system-packages-package-manager' to
install PACK.  You may use ARGS to pass options to the package
manger."
  (interactive "sPackage to install: ")
  (system-packages--run-command 'install pack args))

;;;###autoload
(defun system-packages-search (pack &optional args)
  "Search for system packages.

Use the package manager named in `system-packages-package-manager'
to search for PACK.  You may use ARGS to pass options to the
package manager."
  (interactive "sSearch string: ")
  (system-packages--run-command 'search pack args))

;;;###autoload
(defun system-packages-uninstall (pack &optional args)
  "Uninstall system packages.

Uses the package manager named in
`system-packages-package-manager' to uninstall PACK.  You may use
ARGS to pass options to the package manager."
  (interactive "sWhat package to uninstall: ")
  (system-packages--run-command 'uninstall pack args))

;;;###autoload
(defun system-packages-list-dependencies-of (pack &optional args)
  "List the dependencies of PACK.

You may use ARGS to pass options to the package manager."
  (interactive "sWhat package to list dependencies of: ")
  (system-packages--run-command 'list-dependencies-of pack args))

;;;###autoload
(defun system-packages-get-info (pack &optional args)
  "Display local package information of PACK.

With a prefix argument, display remote package information.  You
may use ARGS to pass options to the package manager."
  (interactive "sWhat package to list info for: ")
  (if current-prefix-arg
      (system-packages--run-command 'get-info-remote pack args)
    (system-packages--run-command 'get-info pack args)))

;;;###autoload
(defun system-packages-list-files-provided-by (pack &optional args)
  "List the files provided by PACK.

You may use ARGS to pass options to the package manager."
  (interactive "sWhat package to list provided files of: ")
  (system-packages--run-command 'list-files-provided-by pack args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions that don't take a named package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun system-packages-update (&optional args)
  "Update system packages.

Use the package manager `system-packages-package-manager'.  You
may use ARGS to pass options to the package manger."
  (interactive)
  (system-packages--run-command 'update nil args))

;;;###autoload
(defun system-packages-remove-orphaned (&optional args)
  "Remove orphaned packages.

Uses the package manager named in
`system-packages-package-manager'.  You may use ARGS to pass
options to the package manger."
  (interactive)
  (system-packages--run-command 'remove-orphaned nil args))

;;;###autoload
(defun system-packages-list-installed-packages (all &optional args)
  "List explicitly installed packages.

Uses the package manager named in
`system-packages-package-manager'.  With
\\[universal-argument] (for ALL), list all installed packages.
You may use ARGS to pass options to the package manger."
  (interactive "P")
  (if all
      (system-packages--run-command 'list-installed-packages-all nil args)
    (system-packages--run-command 'list-installed-packages nil args)))

;;;###autoload
(defun system-packages-clean-cache (&optional args)
  "Clean the cache of the package manager.

You may use ARGS to pass options to the package manger."
  (interactive)
  (system-packages--run-command 'clean-cache nil args))

;;;###autoload
(defun system-packages-log (&optional args)
  "Show a log from `system-packages-package-manager'.

You may use ARGS to pass options to the package manger."
  (interactive)
  (system-packages--run-command 'log args))

;;;###autoload
(defun system-packages-verify-all-packages (&optional args)
  "Check that files owned by packages are present on the system.

You may use ARGS to pass options to the package manger."
  (interactive)
  (system-packages--run-command 'verify-all-packages nil args))

;;;###autoload
(defun system-packages-verify-all-dependencies (&optional args)
  "Verify that all required dependencies are installed on the system.

You may use ARGS to pass options to the package manger."
  (interactive)
  (system-packages--run-command 'verify-all-dependencies nil args))

(provide 'system-packages)
;;; system-packages.el ends here
