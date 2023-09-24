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
 :height 160
 :family "FiraCode Nerd Font Mono")

(defun artemis-meowing-append-end () (interactive)
       (meow-line 1) (meow-append))
(defun artemis-meowing-insert-start () (interactive)
       (meow-line 1) (meow-insert))
(defun artemis-meowing-line-start () (interactive)
       (meow-beginning-of-thing ?v))
(defun artemis-meowing-line-end () (interactive)
       (meow-end-of-thing ?v))
(defun artemis-meowing-change-line () (interactive)
       (meow-line 1) (meow-change))
(defun artemis-meowing-buffer-start () (interactive)
       (meow-beginning-of-thing ?b))
(defun artemis-meowing-buffer-end () (interactive)
       (meow-end-of-thing ?b))

(setq kbd-layout 'workman)

(elpaca meow
  (meow-global-mode)
  (if (eq kbd-layout 'workman)
      ((meow-leader-define-key
	'("/" . meow-comment)

	'("q" . kill-emacs)
	'("w" . save-buffer)

	'("a" . meow-expand-1)
	'("s" . meow-expand-2)
	'("h" . meow-expand-3)
	'("t" . meow-expand-4)
	'("y" . meow-expand-5)
	'("n" . meow-expand-6)
	'("e" . meow-expand-7)
	'("o" . meow-expand-8)
	'("i" . meow-expand-9)

	'("f" . find-file)
	'("b" switch-to-buffer)
	'("ln" . flycheck-next-error)
	'("le" . flycheck-list-error)
	'("lr" . eglot-rename)
	'("lf" . eglot-format)
	'("lc" . eglot-code-actions)
	'("ld" . xref-find-definitions)
	)
       (meow-define-keys
	   'normal
	 '("1" . meow-expand-1)
	 '("2" . meow-expand-2)
	 '("3" . meow-expand-3)
	 '("4" . meow-expand-4)
	 '("5" . meow-expand-5)
	 '("6" . meow-expand-6)
	 '("7" . meow-expand-7)
	 '("8" . meow-expand-8)
	 '("9" . meow-expand-9)
	 '("0" . meow-expand-0)
	 
	 '("n" . meow-left)
	 '("i" . meow-right)
	 '("e" . meow-next)
	 '("o" . meow-prev)
	 '("N" . artemis-meowing-line-start)
	 '("I" . artemis-meowing-line-end)
	 '("E" . meow-next-word)
	 '("O" . meow-back-word)
	 '("g" . artemis-meowing-buffer-start)
	 '("G" . artemis-meowing-buffer-end)
	 '(":" . meow-goto-line)
	 '(";" . swiper)

	 '("l" . meow-line)
	 '("," . meow-beginning-of-thing)
	 '("." . meow-end-of-thing)
	 '("/" . meow-cancel)

	 '("u" . meow-undo)
	 '("U" . undo-redo)
	 '("=" . meow-indent)
	 '("<escape>" . meow-keyboard-quit)

	 '("a" . meow-insert)
	 '("t" . meow-append)
	 '("h" . meow-change)
	 '("s" . meow-kill)
	 '("w" . meow-yank)
	 '("A" . artemis-meowing-insert-start)
	 '("T" . artemis-meowing-append-end)
	 '("H" . artemis-meowing-change-line)
	 '("S" . meow-save)
	 ))
    ((meow-leader-define-key
      '("/" . meow-comment)

      '("q" . kill-emacs)
      '("w" . save-buffer)

      '("a" . meow-expand-1)
      '("s" . meow-expand-2)
      '("d" . meow-expand-3)
      '("f" . meow-expand-4)
      '("g" . meow-expand-5)
      '("h" . meow-expand-6)
      '("j" . meow-expand-7)
      '("k" . meow-expand-8)
      '("l" . meow-expand-9)

      '("y" . find-file)
      '("t" switch-to-buffer)
      '("ln" . flycheck-next-error)
      '("le" . flycheck-list-error)
      '("lr" . eglot-rename)
      '("lf" . eglot-format)
      '("lc" . eglot-code-actions)
      '("ld" . xref-find-definitions)
      )
     (meow-define-keys
	 'normal
       '("1" . meow-expand-1)
       '("2" . meow-expand-2)
       '("3" . meow-expand-3)
       '("4" . meow-expand-4)
       '("5" . meow-expand-5)
       '("6" . meow-expand-6)
       '("7" . meow-expand-7)
       '("8" . meow-expand-8)
       '("9" . meow-expand-9)
       '("0" . meow-expand-0)
       
       '("h" . meow-left)
       '("l" . meow-right)
       '("j" . meow-next)
       '("k" . meow-prev)
       '("H" . artemis-meowing-line-start)
       '("L" . artemis-meowing-line-end)
       '("J" . meow-next-word)
       '("K" . meow-back-word)
       '("g" . artemis-meowing-buffer-start)
       '("G" . artemis-meowing-buffer-end)
       '(":" . meow-goto-line)
       '(";" . swiper)

       '("n" . meow-line)
       '("m" . meow-beginning-of-thing)
       '("," . meow-end-of-thing)
       '("/" . meow-cancel)

       '("u" . meow-undo)
       '("U" . undo-redo)
       '("=" . meow-indent)
       '("<escape>" . meow-keyboard-quit)

       '("a" . meow-insert)
       '("f" . meow-append)
       '("d" . meow-change)
       '("s" . meow-kill)
       '("w" . meow-yank)
       '("A" . artemis-meowing-insert-start)
       '("F" . artemis-meowing-append-end)
       '("D" . artemis-meowing-change-line)
       '("S" . meow-save)
       ))
    )
  (setq meow-use-cursor-position-hack t)
  (setq meow-keypad-start-keys (delq (assoc ?h meow-keypad-start-keys) meow-keypad-start-keys)))

(elpaca solaire-mode
  (solaire-global-mode))

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
