(defvar elpaca-installer-version 0.5)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(setq load-path (cons "~/.config/emacs/local" load-path))
(require 'buffer-line)
(require 'catppuccin-theme)

(load-theme 'catppuccin :no-confirm)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq display-line-numbers-type 'relative)
(setq ring-bell-function 'ignore)

(set-face-attribute
 'buffer-line-face-active nil
 :family "Roboto Mono"
 :background "#b4befe"
 :foreground "#181825")
(set-face-attribute
 'buffer-line-face-inactive nil
 :family "Roboto Mono"
 :background "#181825"
 :foreground "#a6adc8")
(set-face-attribute
 'buffer-line-face-status nil
 :family "Roboto Mono"
 :background "#181825"
 :foreground "#b4befe")
(set-face-attribute
 'header-line nil
 :background "#181825"
 :foreground "#a6adc8")
(set-face-attribute
 'minibuffer-prompt nil
 :background "#181825")
(set-face-attribute
 'default nil
 :family "FiraCode Nerd Font Mono")


(elpaca which-key
  (setq which-key-popup-type 'side-window
	which-key-side-window-location 'bottom
	which-key-side-window-max-height 0.10)
  (which-key-mode))
(elpaca evil
  (evil-mode 1)
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-set-leader 'visual (kbd "SPC"))
  (evil-define-key 'normal 'global (kbd "a") 'evil-insert)
  (evil-define-key 'normal 'global (kbd "A") 'evil-insert-line)
  (evil-define-key 'normal 'global (kbd "t") 'evil-append)
  (evil-define-key 'normal 'global (kbd "T") 'evil-append-line)
  (evil-define-key 'motion 'global (kbd "s") 'evil-replace)
  (evil-define-key 'motion 'global (kbd "S") 'evil-substitute)
  (evil-define-key 'motion 'global (kbd "h") 'evil-change)
  (evil-define-key 'motion 'global (kbd "H") 'evil-change-line)
  
  (evil-define-key 'motion 'global (kbd "N") 'evil-beginning-of-line)
  (evil-define-key 'normal 'global (kbd "I") 'evil-end-of-line)
  (evil-define-key 'visual 'global (kbd "I") 'evil-end-of-line)
  (evil-define-key 'motion 'global (kbd "n") 'evil-backward-char)
  (evil-define-key 'motion 'global (kbd "e") 'evil-next-visual-line)
  (evil-define-key 'normal 'global (kbd "o") 'evil-previous-visual-line)
  ;; needed because of some default binding in visual mode
  (evil-define-key 'visual 'global (kbd "o") 'evil-previous-visual-line)
  (evil-define-key 'normal 'global (kbd "i") 'evil-forward-char)
  (evil-define-key 'visual 'global (kbd "i") 'evil-forward-char)
  
  (evil-define-key 'normal 'global (kbd "<leader>wh") 'evil-window-split)
  (evil-define-key 'normal 'global (kbd "<leader>wv") 'evil-window-vsplit)
  (evil-define-key 'normal 'global (kbd "<leader>wq") 'evil-window-delete)
  (evil-define-key 'normal 'global (kbd "<leader>wo") 'delete-other-windows)

  (evil-define-key 'normal 'global (kbd "<leader>n") 'evil-window-left)
  (evil-define-key 'normal 'global (kbd "<leader>e") 'evil-window-down)
  (evil-define-key 'normal 'global (kbd "<leader>o") 'evil-window-up)
  (evil-define-key 'normal 'global (kbd "<leader>i") 'evil-window-right)
  (evil-define-key 'motion 'global (kbd "C-e") 'evil-scroll-line-down)
  (evil-define-key 'motion 'global (kbd "C-o") 'evil-scroll-line-up)

  (evil-define-key 'normal 'global (kbd "/") 'swiper)
  (evil-define-key 'normal 'global (kbd "<leader>s") 'swiper)
  (evil-define-key 'normal 'global (kbd "j") 'evil-avy-goto-word-1)

  (evil-define-key 'normal 'global (kbd "<leader>/") 'comment-line)
  (evil-define-key 'visual 'global (kbd "<leader>/") 'comment-line)
  (evil-define-key 'normal 'global (kbd "<leader>uc") 'insert-char)

  (evil-define-key 'normal 'global (kbd "<leader>f") 'find-file)
  (evil-define-key 'normal 'global (kbd "ff") 'find-file)

  (evil-define-key 'normal 'global (kbd "<leader>cm") 'olivetti-mode)
  (evil-define-key 'normal 'global (kbd "<leader>cs") 'olivetti-set-width)
  
  (evil-define-key 'motion 'global (kbd "<leader>xx") 'eval-defun)
  (evil-define-key 'motion 'global (kbd "<leader>xs") 'execute-extended-command)

  (evil-define-key 'normal 'global (kbd "<leader>bs") 'switch-to-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>bn") 'evil-next-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>bb") 'evil-previous-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>bd") 'evil-delete-buffer)

  ;(evil-define-key 'normal 'global (kbd "<leader>l") 'lsp)
  (evil-define-key 'normal 'global (kbd "<leader>ln") 'flycheck-next-error)
  (evil-define-key 'normal 'global (kbd "<leader>le") 'flycheck-list-errors)
  (evil-define-key 'normal 'global (kbd "<leader>lr") 'eglot-rename)
  (evil-define-key 'normal 'global (kbd "<leader>lf") 'eglot-format)
  (evil-define-key 'normal 'global (kbd "<leader>lc") 'eglot-code-actions)

  (evil-define-key 'normal 'global (kbd "<leader>gd") 'xref-find-definitions))

(elpaca solaire-mode
  (solaire-global-mode))

(elpaca avy)

(elpaca ivy
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t)
  (ivy-mode))
(elpaca prescient)
(elpaca ivy-prescient
  (ivy-prescient-mode))
(elpaca marginalia
  (marginalia-mode))
(elpaca swiper)

(elpaca olivetti)

(elpaca flycheck
  (add-hook 'prog-mode-hook 'flycheck-mode))
(elpaca company
  (add-hook 'prog-mode-hook 'company-mode))
(elpaca rustic
  (setq rustic-lsp-client 'eglot))
(elpaca zig-mode)
(setq-default c-basic-offset 4)

(elpaca ligature
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  (ligature-set-ligatures 'prog-mode
                          '(;; == === ==== => =| =>>=>=|=>==>> ==< =/=//=// =~
                            ;; =:= =!=
                            ("=" (rx (+ (or ">" "<" "|" "/" "~" ":" "!" "="))))
                            ;; ;; ;;;
                            (";" (rx (+ ";")))
                            ;; && &&&
                            ("&" (rx (+ "&")))
                            ;; !! !!! !. !: !!. != !== !~
                            ("!" (rx (+ (or "=" "!" "\." ":" "~"))))
                            ;; ?? ??? ?:  ?=  ?.
                            ("?" (rx (or ":" "=" "\." (+ "?"))))
                            ;; %% %%%
                            ("%" (rx (+ "%")))
                            ;; |> ||> |||> ||||> |] |} || ||| |-> ||-||
                            ;; |->>-||-<<-| |- |== ||=||
                            ;; |==>>==<<==<=>==//==/=!==:===>
                            ("|" (rx (+ (or ">" "<" "|" "/" ":" "!" "}" "\]"
                                            "-" "=" ))))
                            ;; \\ \\\ \/
                            ("\\" (rx (or "/" (+ "\\"))))
                            ;; ++ +++ ++++ +>
                            ("+" (rx (or ">" (+ "+"))))
                            ;; :: ::: :::: :> :< := :// ::=
                            (":" (rx (or ">" "<" "=" "//" ":=" (+ ":"))))
                            ;; // /// //// /\ /* /> /===:===!=//===>>==>==/
                            ("/" (rx (+ (or ">"  "<" "|" "/" "\\" "\*" ":" "!"
                                            "="))))
                            ;; .. ... .... .= .- .? ..= ..<
                            ("\." (rx (or "=" "-" "\?" "\.=" "\.<" (+ "\."))))
                            ;; -- --- ---- -~ -> ->> -| -|->-->>->--<<-|
                            ("-" (rx (+ (or ">" "<" "|" "~" "-"))))
                            ;; *> */ *)  ** *** ****
                            ("*" (rx (or ">" "/" ")" (+ "*"))))
                            ;; www wwww
                            ("w" (rx (+ "w")))
                            ;; <> <!-- <|> <: <~ <~> <~~ <+ <* <$ </  <+> <*>
                            ;; <$> </> <|  <||  <||| <|||| <- <-| <-<<-|-> <->>
                            ;; <<-> <= <=> <<==<<==>=|=>==/==//=!==:=>
                            ;; << <<< <<<<
                            ("<" (rx (+ (or "\+" "\*" "\$" "<" ">" ":" "~"  "!"
                                            "-"  "/" "|" "="))))
                            ;; >: >- >>- >--|-> >>-|-> >= >== >>== >=|=:=>>
                            ;; >> >>> >>>>
                            (">" (rx (+ (or ">" "<" "|" "/" ":" "=" "-"))))
                            ;; #: #= #! #( #? #[ #{ #_ #_( ## ### #####
                            ("#" (rx (or ":" "=" "!" "(" "\?" "\[" "{" "_(" "_"
					 (+ "#"))))
                            ;; ~~ ~~~ ~=  ~-  ~@ ~> ~~>
                            ("~" (rx (or ">" "=" "-" "@" "~>" (+ "~"))))
                            ;; __ ___ ____ _|_ __|____|_
                            ("_" (rx (+ (or "_" "|"))))
                            ;; Fira code: 0xFF 0x12
                            ("0" (rx (and "x" (+ (in "A-F" "a-f" "0-9")))))
                            ;; Fira code:
                            "Fl"  "Tl"  "fi"  "fj"  "fl"  "ft"
                            ;; The few not covered by the regexps.
                            "{|"  "[|"  "]#"  "(*"  "}#"  "$>"  "^="))
  (global-ligature-mode t))

					; Look into nano-minibuffer
