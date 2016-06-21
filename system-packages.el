  ;;; system-packages.el --- functions to manage system packages

;; Copyright

;; Author: J. Alexander Branham
;; Version: 0.1

(defvar system-packages-packagemanager
  (if (executable-find "pacman") "pacman"
    (if (executable-find "apt") "apt"
      (if (executable-find "brew") "brew")))
  "String containing the package manager to use. Currently
    system-packages supports pacman, apt, and home-brew.")

(defvar system-packages-usesudo t
  "If non-nil, system-packages will use sudo for appropriate
  commands")


(defun system-packages-install (pack)
  "Installs system packages"
  (interactive "sWhat package to install?")
  (let ((command
         (cond ((equal system-packages-packagemanager "pacman") "pacman -S")
               ((equal system-packages-packagemanager "apt") "apt-get install")
               ((equal system-packages-packagemanager "brew") "brew install"))))
    (if (equal system-packages-usesudo t)
        (async-shell-command
         (mapconcat 'identity
                    '("sudo" command pack)
                    " "))
      (async-shell-command
       (mapconcat 'identity
                  '(command pack)
                  " ")))))


(defun system-packages-search ()
  "Search for system packages"
  (interactive "sSearch string?")
  (let ((command
         (cond ((equal system-packages-packagemanager "pacman") "pacman -Ss")
               ((equal system-packages-packagemanager "apt") "apt-cache search")
               ((equal system-packages-packagemanager "brew") "brew search"))))
      (async-shell-command command)))

(defun system-packages-uninstall ()
  "Uninstalls installed system packages"
  (interactive "sWhat package to uninstall?")
  (let ((command
         (cond ((equal system-packages-packagemanager "pacman") "pacman -Rs")
               ((equal system-packages-packagemanager "apt") "apt-get remove")
               ((equal system-packages-packagemanager "brew") "brew uninstall"))))
    (if (equal system-packages-usesudo t)
        (async-shell-command (concat "sudo " command))
      (async-shell-command command))))

(defun system-packages-update ()
  "Updates installed system packages"
  (interactive)
  (let ((command
         (cond ((equal system-packages-packagemanager "pacman") "pacman -Syu")
               ((equal system-packages-packagemanager "apt") "apt-get update && sudo apt-get upgrade")
               ((equal system-packages-packagemanager "brew") "brew update && brew upgrade"))))
    (if (equal system-packages-usesudo t)
        (async-shell-command (concat "sudo " command))
      (async-shell-command command))))

