;;; system-packages.el --- functions to manage system packages

;; Copyright (C) 2016-2017 J. Alexander Branham

;; Author: J. Alexander Branham <branham@utexas.edu>
;; Maintainer: J. Alexander Branham <branham@utexas.edu>
;; URL: https://github.com/jabranham/system-packages
;; Version: 0.1
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

;; This is a package to manage installed system packages. Useful
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
           (list-dependencies-of . "brew deps")))
    ;; Arch-based systems
    (pacaur .
            ((default-sudo . nil)
             (install . "pacaur -S")
             (search . "pacaur -Ss")
             (uninstall . "pacaur -Rs")
             (update . "pacaur -Syu")
             (clean-cache . "pacaur -Sc")
             (log . "cat /var/log/pacman.log")
             (get-info . "pacaur -Qi")
             (get-info-remote . "pacaur -Si")
             (list-files-provided-by . "pacaur -Ql")
             (verify-all-packages . "pacaur -Qkk")
             (verify-all-dependencies . "pacaur -Dk")
             (remove-orphaned . "pacaur -Rns $(pacman -Qtdq)")
             (list-installed-packages . "pacaur -Qe")
             (list-installed-packages-all . "pacaur -Q")
             (list-dependencies-of . "pacaur -Qi")))
    (pacman .
            ((default-sudo . t)
             (install . "pacman -S")
             (search . "pacman -Ss")
             (uninstall . "pacman -Rs")
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
             (list-dependencies-of . "pacman -Qi")))
    ;; Debian (and Ubuntu) based systems
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
               (list-dependencies-of . "apt-cache deps")))
    (apt .
         ((default-sudo . t)
          (install . "apt-get install")
          (search . "apt-cache search")
          (uninstall . "apt-get remove")
          (update . ("apt-get update" "apt-get upgrade"))
          (clean-cache . "apt-get clean")
          (log . "cat /var/log/dpkg.log")
          (get-info . "dpkg -s")
          (get-info-remote . "apt-cache show")
          (list-files-provided-by . "dpkg -L")
          (verify-all-packages . "debsums")
          (verify-all-dependencies . "apt-get check")
          (remove-orphaned . "apt-get autoremove")
          (list-installed-packages . nil)
          (list-installed-packages-all . nil)
          (list-dependencies-of . "apt-cache deps")))
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
             (list-dependencies-of . "emerge -ep")))
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
             (list-dependencies-of . "zypper info --requires")))
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
          (list-dependencies-of . "rpm -qR")))))

(defcustom system-packages-packagemanager
  (cl-loop for (name . prop) in system-packages-supported-package-managers
           for path = (executable-find (symbol-name name))
           when path
           return name)
  "String containing the package manager to use.

See `system-packages-supported-package-managers' for a list of
supported software. Tries to be smart about selecting the
default."
  :type 'symbol)

(defcustom system-packages-usesudo
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

;;;###autoload
(defun system-packages-install (pack)
  "Installs system packages using the package manager named in
system-packages-packagemanager."
  (interactive "sWhat package to install: ")
  (system-packages--run-command 'install pack))

;;;###autoload
(defun system-packages-search (pack)
  "Search for system packages using the package manager named in
system-packages-packagemanager."
  (interactive "sSearch string: ")
  (system-packages--run-command 'search pack))

;;;###autoload
(defun system-packages-uninstall (pack)
  "Uninstalls installed system packages using the package manager named in
system-packages-packagemanager."
  (interactive "sWhat package to uninstall: ")
  (system-packages--run-command 'uninstall pack))

;;;###autoload
(defun system-packages-list-dependencies-of (pack)
  "List all the dependencies of PACK."
  (interactive "sWhat package to list dependencies of: ")
  (system-packages--run-command 'list-dependencies-of pack))

;;;###autoload
(defun system-packages-get-info (pack)
  "Display local package information of PACK.

With a prefix argument, display remote package information."
  (interactive "sWhat package to list info for: ")
  (if current-prefix-arg
      (system-packages--run-command 'get-info-remote pack)
    (system-packages--run-command 'get-info pack)))

;;;###autoload
(defun system-packages-list-files-provided-by (pack)
  "List the files provided by PACK."
  (interactive "sWhat package to list provided files of: ")
  (system-packages--run-command 'list-files-provided-by pack))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions that don't take a named package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun system-packages-update ()
  "Updates installed system packages using the package manager named in
system-packages-packagemanager."
  (interactive)
  (system-packages--run-command 'update))

;;;###autoload
(defun system-packages-remove-orphaned ()
  "This function removes orphaned packages (i.e. unused packages). using the package manager named in
system-packages-packagemanager."
  (interactive)
  (system-packages--run-command 'remove-orphaned))

;;;###autoload
(defun system-packages-list-installed-packages (arg)
  "List explicitly installed packages using the package manager
named in system-packages-packagemanager. With
\\[universal-argument], list all installed packages."
  (interactive "P")
  (if arg
      (system-packages--run-command 'list-installed-packages-all)
    (system-packages--run-command 'list-installed-packages)))

;;;###autoload
(defun system-packages-clean-cache ()
  "Clean the cache of the package manager."
  (interactive)
  (system-packages--run-command 'clean-cache))

;;;###autoload
(defun system-packages-log ()
  "Show a log of the actions taken by the package manager."
  (interactive)
  (system-packages--run-command 'log))

;;;###autoload
(defun system-packages-verify-all-packages ()
  "Check that files owned by packages are present on the system."
  (interactive)
  (system-packages--run-command 'verify-all-packages))

;;;###autoload
(defun system-packagesverify-all-dependencies ()
  "Verify that all required dependencies are installed on the system."
  (interactive)
  (system-packages--run-command 'verify-all-dependencies))

(provide 'system-packages)
;;; system-packages.el ends here
