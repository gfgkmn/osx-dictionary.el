;;; osx-dictionary-gfgkmn.el --- Personal additions to osx-dictionary -*- lexical-binding: t; -*-

;; Author: yuhe <gfgkmn@gmail.com>
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, dictionary

;;; Commentary:

;; Additions to `osx-dictionary' that upstream is unlikely to want, kept in a
;; file upstream does not have, so that `git merge origin/master' never
;; conflicts.  Upstream's own osx-dictionary.el is not modified at all.
;;
;; Three features:
;;
;;   1. A two-state toggle on "s" -- one dictionary alone, or a group of them
;;      together -- re-rendering the word already on screen.
;;   2. A lemma fallback, so a dictionary with a thin inflection index (the
;;      Collins CCED bundle) stops returning blank for "books" or "ran".
;;   3. A centred child-frame display, sized to its content -- the default;
;;      set `gfgkmn/osx-dict-display-style' to `window' for the stock one.
;;
;; Personal choices -- which dictionaries, which display style -- belong in
;; your init file rather than here; see the defcustoms.

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'subr-x)
(require 'osx-dictionary)

(defgroup osx-dictionary-gfgkmn nil
  "Personal additions to `osx-dictionary'."
  :group 'osx-dictionary
  :prefix "gfgkmn/osx-dict-")

;;;; Two-state toggle

(defconst gfgkmn/osx-dict-primary-dictionary "Collins"
  "The dictionary shown alone in state 1 of `gfgkmn/osx-dict-toggle-dictionary'.
Deliberately NOT in `osx-dictionary-allowed-dictionaries': that list is
state 2's contents.  `osx-dictionary--search-dictionary-args' ignores the
allowlist entirely whenever `osx-dictionary-current-dictionary' is set, so
this name only has to be installed, not allowed.")

(defun gfgkmn/osx-dict-toggle-dictionary ()
  "Toggle the dictionary buffer between its two states, re-rendering in place.

  state 1  Collins alone            (`osx-dictionary-current-dictionary' = name)
  state 2  牛津英汉 + 汉语大词典     (nil => every `...-allowed-dictionaries')

Both fall out of `osx-dictionary--search-dictionary-args': one `-d' for the
current dictionary when set, otherwise one `-d' per allowed dictionary.  So
state 2 is simply \"no single dictionary selected\", and the allowlist is
sized to be exactly state 2.

No prompt — this is the one-key look.  Any other single dictionary (say one
reached via \"S\") counts as state 1 and toggles back into state 2."
  (interactive)
  ;; Does the setq, the persist, and the in-place re-search.
  (osx-dictionary-select-dictionary
   (if osx-dictionary-current-dictionary nil gfgkmn/osx-dict-primary-dictionary))
  ;; Upstream would report state 2 as "All allowed dictionaries", which is
  ;; true but unhelpful when the allowlist IS the state.  Name it instead.
  (message "osx-dictionary: %s"
           (or osx-dictionary-current-dictionary
               (mapconcat #'osx-dictionary--entry-display-name
                          osx-dictionary-allowed-dictionaries " + "))))

;;;; Lemma fallback — teach Collins about tenses and plurals

;; Collins (the CCED bundle) indexes inflections far more thinly than the
;; Oxford dictionaries do.  Measured, same CLI, same words:
;;
;;   runs / books / studies   Collins: nothing at all    牛津: full entry
;;   ran / went / children    Collins: a one-line stub   牛津: full base entry
;;                            ("Ran is the past tense of run.")
;;
;; That is the dictionary DATA, not the API — `DCSCopyRecordsForSearchString'
;; reports only what the bundle's index holds.  We cannot fix the index, but
;; both failures are recoverable from this side:
;;
;;   nothing -> strip the inflection by rule, look the base form up
;;   stub    -> the stub NAMES its base form; parse it and append that entry
;;
;; Deliberately conservative: `running' has its own 2238-byte Collins entry, so
;; it is neither empty nor a stub and is left completely alone.

(defcustom gfgkmn/osx-dict-lemma-fallback t
  "When non-nil, retry a failed lookup with the word's base form.
Costs one extra CLI call, and only when the first lookup came back empty or
as a cross-reference stub — an ordinary hit never pays for it."
  :type 'boolean)

(defvar gfgkmn/osx-dict--in-lemma-fallback nil
  "Recursion guard: the fallback's own lookups must not re-enter the advice.")

(defconst gfgkmn/osx-dict--stub-re
  "\\bis the +[a-z0-9 ]*?of +\\([a-z][a-z-]*\\)"
  "Matches a cross-reference stub, capturing the base form in group 1.
Covers \"is the past tense of run\", \"is the plural of child\", and
\"is the past tense and past participle of X\".")

(defconst gfgkmn/osx-dict--stub-max 300
  "A stub is short.  Past this many characters, treat a hit as a real entry.
Guards against a long definition that merely happens to contain the phrase.")

(defun gfgkmn/osx-dict--undouble (s)
  "S without a doubled final consonant (\"stopp\" -> \"stop\"), else nil."
  (let ((n (length s)))
    (and (>= n 3)
         (eq (aref s (1- n)) (aref s (- n 2)))
         (not (memq (aref s (1- n)) '(?a ?e ?i ?o ?u)))
         (substring s 0 (1- n)))))

(defun gfgkmn/osx-dict--lemma-candidates (word)
  "Plausible base forms of WORD, best first.
Rule-based and deliberately over-generous: a wrong candidate simply finds
nothing and is skipped, so `bus' -> `bu' costs a miss, never a wrong answer.
Returns nil for anything that is not a plain ASCII word, which keeps Chinese
headwords away from English suffix stripping."
  (when (string-match-p "\\`[a-zA-Z][a-zA-Z-]*\\'" word)
    (let* ((w (downcase word)) (len (length w)) cands)
      (cl-flet ((add (c)
                  (when (and c (> (length c) 1) (not (equal c w)))
                    (cl-pushnew c cands :test #'equal))))
        (when (string-suffix-p "ies" w) (add (concat (substring w 0 (- len 3)) "y")))
        (when (string-suffix-p "ied" w) (add (concat (substring w 0 (- len 3)) "y")))
        (when (string-suffix-p "es" w)  (add (substring w 0 (- len 2))))
        (when (and (string-suffix-p "s" w) (not (string-suffix-p "ss" w)))
          (add (substring w 0 (1- len))))
        (when (string-suffix-p "ed" w)
          (add (substring w 0 (- len 2)))                              ; walked -> walk
          (add (substring w 0 (1- len)))                               ; used   -> use
          (add (gfgkmn/osx-dict--undouble (substring w 0 (- len 2))))) ; stopped -> stop
        (when (string-suffix-p "ing" w)
          (add (substring w 0 (- len 3)))                              ; walking -> walk
          (add (concat (substring w 0 (- len 3)) "e"))                 ; making  -> make
          (add (gfgkmn/osx-dict--undouble (substring w 0 (- len 3))))) ; running -> run
        (when (string-suffix-p "est" w) (add (substring w 0 (- len 3))))
        (when (string-suffix-p "er" w)  (add (substring w 0 (- len 2)))))
      (nreverse cands))))

(defun gfgkmn/osx-dict--stub-base (text)
  "Base form named by TEXT if it is a cross-reference stub, else nil."
  (and (stringp text)
       (< (length text) gfgkmn/osx-dict--stub-max)
       (string-match gfgkmn/osx-dict--stub-re text)
       (match-string 1 text)))

(defun gfgkmn/osx-dict--search-a (fn word)
  "Around `osx-dictionary--search': recover from a miss or a stub."
  (let ((result (funcall fn word)))
    (if (or gfgkmn/osx-dict--in-lemma-fallback
            (not gfgkmn/osx-dict-lemma-fallback)
            (not (stringp result)))
        result
      (let ((gfgkmn/osx-dict--in-lemma-fallback t))
        (cond
         ;; nothing found: try base forms until one lands
         ((string-blank-p result)
          (or (cl-loop for c in (gfgkmn/osx-dict--lemma-candidates word)
                       for r = (funcall fn c)
                       unless (string-blank-p r) return r)
              result))
         ;; a stub that names its base: show the stub AND the real entry
         ((gfgkmn/osx-dict--stub-base result)
          (let ((full (funcall fn (gfgkmn/osx-dict--stub-base result))))
            (if (string-blank-p full) result (concat result "\n" full))))
         (t result))))))

(advice-add 'osx-dictionary--search :around #'gfgkmn/osx-dict--search-a)

;; Keep evil-snipe out of the dictionary buffer, so upstream's "S" can be
;; reached at all.  `evil-snipe-local-mode-map' binds "s"/"S" in normal+motion
;; state, and a MINOR-mode map outranks a major-mode map (including evil's
;; auxiliary map for one), so "S" resolved to `evil-snipe-S' no matter what we
;; put in `osx-dictionary-mode-map'.
;;
;; Note this must be the disabled-modes list, NOT an `osx-dictionary-mode-hook'
;; that flips `evil-snipe-local-mode' off: `evil-snipe-mode' is a globalized
;; minor mode, so it re-enables from `after-change-major-mode-hook', which runs
;; AFTER the major-mode hook.  Measured — the hook version installed fine and
;; still left `S -> evil-snipe-S'.
;;
;; Side effect, measured rather than assumed: this also gates
;; `turn-on-evil-snipe-override-mode', so in this buffer "F"/"t"/"T" drop from
;; snipe's 2-char versions to evil's 1-char `evil-find-char-backward' /
;; `evil-find-char-to' / `evil-find-char-to-backward'.  "f" is NOT affected —
;; it stays `evil-avy-goto-char', because that binding does not come from the
;; snipe override map.  Same trade `dired-mode', `Info-mode' and `magit-mode'
;; already make, all of which upstream ships in this very list.
;;
;; Bonus from freeing "s": upstream's own "s" (search another word) becomes
;; reachable here for the first time.
(with-eval-after-load 'evil-snipe
  (add-to-list 'evil-snipe-disabled-modes 'osx-dictionary-mode))

;;;; Key bindings

;; "s" is the quick look: toggle state 1 <-> state 2, no prompt.  "S" keeps
;; upstream's picker as an escape hatch for jumping straight to one.
;;
;; Both need the evil STATE map, not `osx-dictionary-mode-map\': evil\'s normal
;; state outranks a major-mode map, so unbound there they would reach
;; `evil-substitute\' and `evil-change-whole-line\'.  And both need the
;; `evil-snipe-disabled-modes\' entry above, or snipe\'s minor-mode map --
;; which outranks BOTH -- takes "s"/"S" first.
;;
;; "q" is deliberately NOT bound here: quitting is entangled with whatever
;; window / workspace machinery the user runs, so it stays their business.
(with-eval-after-load 'evil
  (evil-define-key 'normal osx-dictionary-mode-map
    (kbd "s") #'gfgkmn/osx-dict-toggle-dictionary
    (kbd "S") #'osx-dictionary-select-dictionary))

;;;; *osx-dictionary* display: centred child frame (default) or window

(defcustom gfgkmn/osx-dict-display-style 'child-frame
  "How the `*osx-dictionary*' buffer is shown.

`child-frame' (default) a centred, undecorated child frame sized to its
              content, in the style of a posframe: no mode-line, no
              header-line, no fringes, no minibuffer of its own.
`window'      the stock osx-dictionary behaviour — a window on the current
              frame.  Every code path it takes is untouched by this file, so
              it is the escape hatch if the child frame misbehaves.

Known cost of `child-frame': owning no minibuffer, \"/\" and \"S\" prompt on
the PARENT frame's echo area and leave point there.  That keeps the frame to
a single window, which is what makes the content-fit unambiguous.  \"s\" and
\"q\" need no minibuffer and are unaffected."
  :type '(choice (const :tag "Window on the current frame" window)
                 (const :tag "Centred child frame" child-frame)))

;;;; Per-parent state
;;
;; One global child frame was wrong twice over.  A lookup from frame B reused
;; the frame parented to A, so the definition appeared on the frame you were
;; not looking at; and a single `*osx-dictionary*' buffer meant two frames
;; could never show different words anyway.
;;
;; Both are keyed off the PARENT frame now, and stored as frame parameters
;; rather than in a global registry: a frame parameter dies with its frame, so
;; there is no alist to garbage-collect and no way to leak a reference to a
;; dead frame.

(defvar gfgkmn/osx-dict--frame-counter 0
  "Source of the small stable ids used in per-frame buffer names.")

(defun gfgkmn/osx-dict--host-frame (&optional frame)
  "The frame a lookup belongs to: FRAME, or its parent if it IS a popup.
`s' re-renders from inside the child frame, where `selected-frame' is the
popup itself; without this the popup would try to parent to itself."
  (let ((f (or frame (selected-frame))))
    (or (frame-parameter f 'parent-frame) f)))

(defun gfgkmn/osx-dict--frame-id (frame)
  "A small integer identifying FRAME, assigned on first use.
Frame NAMES are unusable for this -- they change as buffers change."
  (or (frame-parameter frame 'gfgkmn/osx-dict-id)
      (let ((id (cl-incf gfgkmn/osx-dict--frame-counter)))
        (set-frame-parameter frame 'gfgkmn/osx-dict-id id)
        id)))

(defun gfgkmn/osx-dict--popup-frame (&optional host)
  "The live child frame belonging to HOST, or nil."
  (let ((f (frame-parameter (or host (gfgkmn/osx-dict--host-frame))
                            'gfgkmn/osx-dict-child)))
    (and (frame-live-p f) f)))

(defun gfgkmn/osx-dict-buffer-name (_word)
  "Per-frame name for the result buffer.
Installed as `osx-dictionary-generate-buffer-name-function'.  One buffer per
host frame is what lets two frames show two different words at once; with
upstream's single fixed name they would always share one."
  (format "*osx-dictionary %d*"
          (gfgkmn/osx-dict--frame-id (gfgkmn/osx-dict--host-frame))))

(setq osx-dictionary-generate-buffer-name-function #'gfgkmn/osx-dict-buffer-name)

(defconst gfgkmn/osx-dict--margin 3
  "Columns of breathing room inside each side of the child frame.")

(defconst gfgkmn/osx-dict--max-frame-width 92
  "Widest the child frame may get, in total columns, margins included.")

(defcustom gfgkmn/osx-dict-child-frame-contrast 8
  "Percent of HSL lightness to move the child frame's background by.
Away from the ambient background, so lighter on a dark theme and darker on a
light one — an undecorated frame sharing its parent's exact background reads
as a hole in the buffer rather than a surface floating over it.  0 disables
the shift and restores the previous behaviour."
  :type 'integer)

(defcustom gfgkmn/osx-dict-child-frame-border-contrast 22
  "Like `gfgkmn/osx-dict-child-frame-contrast', but for the border.
Shifted further than the background and in the same direction, so the edge
stays visible once the two surfaces are no longer identical."
  :type 'integer)

(defun gfgkmn/osx-dict--dark-p (color)
  "Non-nil when COLOR is dark, by Rec. 601 luma."
  (let ((rgb (color-name-to-rgb color)))
    (or (null rgb)                      ; unparseable: assume a dark theme
        (< (+ (* 0.299 (nth 0 rgb))
              (* 0.587 (nth 1 rgb))
              (* 0.114 (nth 2 rgb)))
           0.5))))

(defun gfgkmn/osx-dict--shift (color percent)
  "COLOR moved PERCENT of lightness AWAY from itself.
Direction follows the colour: dark gets lighter, light gets darker, so one
setting works on both a dark and a light theme without a special case."
  (cond ((or (null color) (<= percent 0)) color)
        ((gfgkmn/osx-dict--dark-p color) (color-lighten-name color percent))
        (t (color-darken-name color percent))))

(defun gfgkmn/osx-dict--surface-colors (parent)
  "Return (BACKGROUND . BORDER) for a child frame over PARENT."
  (let ((bg (or (face-background 'default parent t) "#282c34")))
    (cons (gfgkmn/osx-dict--shift bg gfgkmn/osx-dict-child-frame-contrast)
          (gfgkmn/osx-dict--shift bg gfgkmn/osx-dict-child-frame-border-contrast))))

(defun gfgkmn/osx-dict--paint-surface (frame parent)
  "Give FRAME a background distinct from PARENT's, plus a matching border.

Setting the `background-color' frame parameter is NOT enough, twice over --
both measured rather than assumed:

  * A face carrying an explicit background beats the frame parameter, and the
    theme gives `default' one.  With only the parameter set, the frame read
    \"#3043...\" while `default' on it still reported the parent's #21242B.
  * `solaire-mode' remaps `default' to `solaire-default-face' in this buffer
    \(face-remapping-alist holds `(default solaire-default-face default)'), so
    even a corrected `default' would not be what actually paints.

Hence all three, and every one of them scoped to FRAME.  That scoping is the
part to preserve: a frame-wide `set-face-background' records an attribute
that frames created LATER inherit over the theme, which is exactly how an
earlier change left black fringes on new frames after a theme switch.
Verified — after this runs, every other live frame still reports the theme's
own background for both faces."
  (let* ((colors (gfgkmn/osx-dict--surface-colors parent))
         (bg (car colors)))
    (set-frame-parameter frame 'background-color bg)
    (set-face-background 'default bg frame)
    (when (facep 'solaire-default-face)
      (set-face-background 'solaire-default-face bg frame))
    (set-face-background 'internal-border (cdr colors) frame)))

(defun gfgkmn/osx-dict--fit-bounds ()
  "MAX-HEIGHT MIN-HEIGHT MAX-WIDTH MIN-WIDTH for `fit-frame-to-buffer'.

The width bounds apply to the window BODY, and the display margins are added
on top of them — measured: a body bound of 92 with 3+3 margins produced a
98-column frame.  So the margins are subtracted here, and
`gfgkmn/osx-dict--max-frame-width' means what it says.

Bounds are mandatory, not cosmetic: an unbounded fit on a 400-line entry
would grow past the display.  Measured under `emacs -Q' across 2/8/40/400
line buffers — the fit is idempotent at every size, and stays idempotent
after the re-centre below, so the two cannot chase each other."
  (let ((inset (* 2 gfgkmn/osx-dict--margin)))
    (list 24 4
          (- gfgkmn/osx-dict--max-frame-width inset)
          (- 50 inset))))

(defun gfgkmn/osx-dict--apply-chrome (child-p)
  "Strip chrome for CHILD-P display in the current buffer, or restore it.
Buffer-local, and the buffer outlives any one popup, so the `window' style
must actively restore what `child-frame' stripped — otherwise switching
`gfgkmn/osx-dict-display-style' back would leave a mode-line-less buffer."
  (if child-p
      (progn (setq-local mode-line-format nil)
             (setq-local header-line-format nil)
             (setq-local display-line-numbers nil)
             (setq-local left-margin-width gfgkmn/osx-dict--margin)
             (setq-local right-margin-width gfgkmn/osx-dict--margin))
    (dolist (v '(mode-line-format header-line-format display-line-numbers
                 left-margin-width right-margin-width))
      (kill-local-variable v))))

(defun gfgkmn/osx-dict--fit-and-centre (&optional host)
  "Size HOST's child frame to its content, then centre it on HOST.
Order matters: fitting changes the size, so centring must follow it."
  (let ((f (gfgkmn/osx-dict--popup-frame host)))
    (when (frame-live-p f)
      (apply #'fit-frame-to-buffer f (gfgkmn/osx-dict--fit-bounds))
      (let ((parent (frame-parameter f 'parent-frame)))
        (when (frame-live-p parent)
          (set-frame-position
           f
           (max 0 (/ (- (frame-pixel-width parent) (frame-pixel-width f)) 2))
           (max 0 (/ (- (frame-pixel-height parent) (frame-pixel-height f)) 2))))))))

(defun gfgkmn/osx-dict--make-child-frame (parent)
  "Create the dictionary child frame on PARENT.
`minibuffer' nil is deliberate — it is what keeps the frame to a single
window, which is what makes `fit-frame-to-buffer' unambiguous.  The cost is
that \\[isearch-forward] / \"S\" prompt on PARENT's echo area."
  (make-frame
   `((name . "osx-dictionary")
     (parent-frame . ,parent)
     (minibuffer . nil)
     (undecorated . t)
     (width . 74) (height . 16)          ; provisional; the fit overrides it
     (internal-border-width . 1)
     (left-fringe . 0) (right-fringe . 0)
     (vertical-scroll-bars . nil) (horizontal-scroll-bars . nil)
     (menu-bar-lines . 0) (tool-bar-lines . 0)
     (visibility . nil))))               ; shown only once sized, to avoid a jump

(defun gfgkmn/osx-dict--use-child-frame-p ()
  "Non-nil when this lookup should be shown in a child frame.
Always in `child-frame' style.  In `window' style, still when the host frame is
`unsplittable' -- a cc-delegate popup, for one.  There
`display-buffer-pop-up-window' cannot split, every window action falls through,
and Emacs's fallback borrows a window on ANOTHER frame, overwriting whatever that
frame was showing.  Measured: a lookup from an unsplittable single-window frame
landed in a different frame's root window, with or without the display rule.  A
child frame of the host keeps the definition where it was asked for."
  (or (eq gfgkmn/osx-dict-display-style 'child-frame)
      (and (frame-parameter (gfgkmn/osx-dict--host-frame) 'unsplittable) t)))

(defun gfgkmn/osx-dict-display-buffer (buffer _alist)
  "`display-buffer' action for `*osx-dictionary*'.
Returns a window when it handled BUFFER, or nil to fall through to the
ordinary window actions — which is what keeps the `window' style intact on
every frame that can actually hold a split; see
`gfgkmn/osx-dict--use-child-frame-p'."
  (if (not (gfgkmn/osx-dict--use-child-frame-p))
      (progn (with-current-buffer buffer (gfgkmn/osx-dict--apply-chrome nil))
             nil)
    (with-current-buffer buffer (gfgkmn/osx-dict--apply-chrome t))
    (let* ((parent (gfgkmn/osx-dict--host-frame))
           ;; Reuse only THIS host's popup.  Reusing whichever popup happened to
           ;; exist rendered frame B's lookup onto frame A.
           (f (or (gfgkmn/osx-dict--popup-frame parent)
                  (let ((new (gfgkmn/osx-dict--make-child-frame parent)))
                    (set-frame-parameter parent 'gfgkmn/osx-dict-child new)
                    new)))
           (win (frame-selected-window f)))
      (set-window-buffer win buffer)
      (set-window-margins win gfgkmn/osx-dict--margin gfgkmn/osx-dict--margin)
      ;; Re-derived on every display, not just at creation: the frame is reused
      ;; across lookups, and the theme can change under it.  Both are scoped to
      ;; F — `set-frame-parameter' is frame-local by definition, and passing F
      ;; to `set-face-background' keeps that attribute off every other frame.
      ;; (A frame-wide `set-face-background' would be inherited by frames
      ;; created later, over the theme.)
      (gfgkmn/osx-dict--paint-surface f parent)
      (gfgkmn/osx-dict--fit-and-centre parent)
      (make-frame-visible f)
      (select-frame-set-input-focus f)
      win)))

(defun gfgkmn/osx-dict--refit-a (&rest _)
  "Re-fit after a re-render (e.g. the \"s\" toggle).
Needed because `osx-dictionary--goto-dictionary' short-circuits to
`select-window' when the buffer already has a window in the selected frame,
so `display-buffer' — and with it the fit — never runs on that path."
  ;; Keyed on whether THIS host has a popup, not on the display style: in
  ;; `window' style an unsplittable host still gets one.
  (let ((host (gfgkmn/osx-dict--host-frame)))
    (when (gfgkmn/osx-dict--popup-frame host)
      (gfgkmn/osx-dict--fit-and-centre host))))

(advice-add 'osx-dictionary--view-result :after #'gfgkmn/osx-dict--refit-a)

(defun gfgkmn/osx-dict-kill-child-frame (&optional host)
  "Delete HOST's dictionary child frame, returning focus to HOST."
  (interactive)
  (let* ((host (or host (gfgkmn/osx-dict--host-frame)))
         (f (gfgkmn/osx-dict--popup-frame host)))
    (when f (delete-frame f))
    (when (frame-live-p host)
      (set-frame-parameter host 'gfgkmn/osx-dict-child nil)
      (when f (select-frame-set-input-focus host)))))

(defun gfgkmn/osx-dict--cleanup-on-frame-delete (frame)
  "Tear down FRAME's popup and result buffer when FRAME goes away.
Runs for every frame deletion, so it must be cheap and must not assume FRAME
is one of ours.  Deliberately does NOT touch workspaces: a child frame gets a
throwaway persp like any frame, and doom's own
`+workspaces-delete-associated-workspace-h' already reclaims it -- verified,
deleting a child frame leaves every surviving frame with a valid workspace."
  (when (frame-live-p frame)
    (let ((child (frame-parameter frame 'gfgkmn/osx-dict-child))
          (id (frame-parameter frame 'gfgkmn/osx-dict-id)))
      (when (frame-live-p child) (ignore-errors (delete-frame child)))
      (when id
        (let ((buf (get-buffer (format "*osx-dictionary %d*" id))))
          (when (buffer-live-p buf) (kill-buffer buf)))))))

(add-hook 'delete-frame-functions #'gfgkmn/osx-dict--cleanup-on-frame-delete)

;; Registering the rule is left to the CALLER, deliberately.  This package
;; provides the action function; where it goes in `display-buffer-alist' is the
;; host config's business -- and under Doom it must not be done from here at
;; all.  Doom's popup module rewrites `display-buffer-alist' during startup, so
;; a top-level `add-to-list' in a lazily-loaded package is simply lost:
;; measured, the entry was in neither `display-buffer-alist' nor
;; `+popup--old-display-buffer-alist' afterwards, while adding the identical
;; entry at runtime worked immediately.
;;
;; Put this in your init file, at top level (NOT inside the package's own
;; `:config'), so it lands after Doom has finished with that variable:
;;
;;   (add-to-list 'display-buffer-alist
;;                '("\\*osx-dictionary\\*"
;;                  (gfgkmn/osx-dict-display-buffer
;;                   display-buffer-reuse-window
;;                   display-buffer-pop-up-window
;;                   display-buffer-use-some-window)
;;                  (reusable-frames . nil)
;;                  (inhibit-switch-frame . t)))
;;
;; `gfgkmn/osx-dict-display-buffer' must go FIRST, and returns nil unless the
;; child-frame style is active, so the ordinary window actions after it remain
;; exactly the stock behaviour.


(provide 'osx-dictionary-gfgkmn)
;;; osx-dictionary-gfgkmn.el ends here
