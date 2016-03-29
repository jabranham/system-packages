(when (executable-find "pacman")
  (setq system-packages-packagemanager "pacman"))
(when (executable-find "apt-get")
  (setq system-packages-packagemanager "apt-get"))
(when (executable-find "brew")
  (setq system-packages-packagemanager "brew"))

(when (equal system-packages-packagemanager "pacman")
  (setq system-packages-installcommand "pacman -S"
        system-packages-searchcommand "pacman -Ss"
        system-packages-uninstallcommand "pacman -Rs"
        system-packages-updatecommand "pacman -Syu"))

(when (equal system-packages-packagemanager "apt-get")
  (setq system-packages-installcommand "apt-get install"
        system-packages-searchcommand "apt-cache search"
        system-packages-uninstallcommand "apt-get remove"
        system-packages-updatecommand "apt-get update && sudo apt-get upgrade"))

(when (equal system-packages-packagemanager "brew")
  (setq system-packages-installcommand "brew install"
        system-packages-searchcommand "brew search"
        system-packages-uninstallcommand "brew uninstall"
        system-packages-updatecommand "brew update && brew upgrade"))

(setq system-packages-usesudo t)

(defun system-packages-install ()
  (if (equal system-packages-usesudo t)
      (async-shell-command (concat "sudo " system-packages-installcommand)))
  (async-shell-command system-packages-installcommand))
(defun system-packages-search ()
  (if (equal system-packages-usesudo t)
      (async-shell-command (concat "sudo " system-packages-searchcommand))
    (async-shell-command system-packages-searchcommand)))
(defun system-packages-uninstall ()
  (if (equal system-packages-usesudo t)
      (async-shell-command (concat "sudo " system-packages-uninstallcommand)))B
      (async-shell-command system-packages-uninstallcommand))
(defun system-packages-update ()
  (if (equal system-packages-usesudo t)
      (async-shell-command (concat "sudo " system-packages-updatecommand))
    (async-shell-command system-packages-updatecommand)))
