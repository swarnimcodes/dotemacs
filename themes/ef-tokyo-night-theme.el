;;; ef-tokyo-night-theme.el --- Tokyo Night palette on the ef-themes engine -*- lexical-binding:t -*-

;;; Commentary:
;;
;; A Tokyo Night inspired dark theme built on `ef-themes' (itself built
;; on `modus-themes').  The palette follows the Tokyo Night "night"
;; variant; the face mappings follow Tokyo Night's syntax conventions
;; (purple keywords, blue functions, green strings, orange constants,
;; teal types).  All the face coverage (magit, org, vertico, ...) comes
;; from ef-themes.

;;; Code:

(require 'ef-themes)

(defconst ef-tokyo-night-palette-partial
  '((cursor "#c0caf5")
    (bg-main "#1a1b26")
    (bg-dim "#16161e")
    (bg-alt "#1f2335")
    (fg-main "#c0caf5")
    (fg-dim "#737aa2")
    (fg-alt "#a9b1d6")
    (bg-active "#292e42")
    (bg-inactive "#1f2335")
    (border "#3b4261")

    (red "#f7768e")
    (red-warmer "#ff757f")
    (red-cooler "#ff899d")
    (red-faint "#c9677a")
    (green "#9ece6a")
    (green-warmer "#b9f27c")
    (green-cooler "#73daca")
    (green-faint "#8aae68")
    (yellow "#e0af68")
    (yellow-warmer "#ff9e64")
    (yellow-cooler "#f0c58a")
    (yellow-faint "#c4a46f")
    (blue "#7aa2f7")
    (blue-warmer "#7d8cf7")
    (blue-cooler "#5fb0ff")
    (blue-faint "#7f96cf")
    (magenta "#bb9af7")
    (magenta-warmer "#d98ae6")
    (magenta-cooler "#9d7cd8")
    (magenta-faint "#a99cc8")
    (cyan "#7dcfff")
    (cyan-warmer "#89ddff")
    (cyan-cooler "#2ac3de")
    (cyan-faint "#9ab8d6")

    (bg-red-intense "#a8384c")
    (bg-green-intense "#3b7a4a")
    (bg-yellow-intense "#8a6a2c")
    (bg-blue-intense "#3d59a1")
    (bg-magenta-intense "#7a55c4")
    (bg-cyan-intense "#1f7f96")

    (bg-red-subtle "#4a2433")
    (bg-green-subtle "#25392c")
    (bg-yellow-subtle "#43382a")
    (bg-blue-subtle "#283457")
    (bg-magenta-subtle "#3b2d5c")
    (bg-cyan-subtle "#1f3a4d")

    (bg-added "#20303b")
    (bg-added-faint "#1b2832")
    (bg-added-refine "#2c4a55")
    (bg-added-fringe "#449dab")
    (fg-added "#b9f27c")

    (bg-changed "#272d43")
    (bg-changed-faint "#222639")
    (bg-changed-refine "#394b70")
    (bg-changed-fringe "#6183bb")
    (fg-changed "#a9b8ef")

    (bg-removed "#37222c")
    (bg-removed-faint "#2d1e28")
    (bg-removed-refine "#55303d")
    (bg-removed-fringe "#914c54")
    (fg-removed "#ffb3c0")

    (bg-mode-line-active "#292e42")
    (fg-mode-line-active "#a9b1d6")
    (bg-completion "#283457")
    (bg-popup "#1f2335")
    (bg-hover "#394b70")
    (bg-hover-secondary "#3b4261")
    (bg-hl-line "#292e42")
    (bg-paren-match "#3d59a1")
    (bg-err "#3a2230")
    (bg-warning "#3a3126")
    (bg-info "#1f3333")
    (bg-region "#283457")))

(defconst ef-tokyo-night-palette-mappings-partial
  '((err red)
    (warning yellow)
    (info green-cooler)

    (fg-link blue)
    (fg-link-visited magenta)
    (name blue)
    (keybind cyan)
    (identifier fg-alt)
    (fg-prompt cyan)

    (builtin cyan)
    (comment fg-dim)
    (constant yellow-warmer)
    (fnname blue)
    (fnname-call blue)
    (keyword magenta)
    (preprocessor cyan-warmer)
    (docstring green-faint)
    (string green)
    (type cyan-cooler)
    (variable fg-main)
    (variable-use fg-main)
    (rx-backslash cyan-warmer)
    (rx-construct magenta)

    (accent-0 blue)
    (accent-1 magenta)
    (accent-2 cyan-cooler)
    (accent-3 yellow-warmer)

    (date-common cyan)
    (date-deadline red)
    (date-deadline-subtle red-faint)
    (date-event fg-alt)
    (date-holiday magenta-warmer)
    (date-now fg-main)
    (date-range fg-alt)
    (date-scheduled yellow)
    (date-scheduled-subtle yellow-faint)
    (date-weekday cyan)
    (date-weekend red-faint)

    (fg-prose-code green-cooler)
    (prose-done green-cooler)
    (fg-prose-macro cyan-cooler)
    (prose-metadata fg-dim)
    (prose-metadata-value fg-alt)
    (prose-table fg-alt)
    (prose-table-formula info)
    (prose-tag fg-alt)
    (prose-todo red)
    (fg-prose-verbatim yellow-warmer)

    (mail-cite-0 blue)
    (mail-cite-1 magenta)
    (mail-cite-2 green-cooler)
    (mail-cite-3 yellow)
    (mail-part cyan)
    (mail-recipient blue)
    (mail-subject cyan-cooler)
    (mail-other cyan-warmer)

    (bg-search-static bg-warning)
    (bg-search-current bg-yellow-intense)
    (bg-search-lazy bg-blue-intense)
    (bg-search-replace bg-red-intense)

    (bg-search-rx-group-0 bg-magenta-intense)
    (bg-search-rx-group-1 bg-green-intense)
    (bg-search-rx-group-2 bg-red-subtle)
    (bg-search-rx-group-3 bg-cyan-subtle)

    (bg-space-err bg-yellow-intense)

    (rainbow-0 blue)
    (rainbow-1 magenta)
    (rainbow-2 cyan)
    (rainbow-3 yellow)
    (rainbow-4 green)
    (rainbow-5 yellow-warmer)
    (rainbow-6 cyan-cooler)
    (rainbow-7 red)
    (rainbow-8 magenta-cooler)))

(defcustom ef-tokyo-night-palette-overrides nil
  "Overrides for `ef-tokyo-night-palette'.

Mirror the elements of the aforementioned palette, overriding
their value.  For overrides shared across all Ef themes, see
`ef-themes-common-palette-overrides'."
  :group 'ef-themes
  :type '(repeat (list symbol (choice symbol string)))
  :link '(info-link "(ef-themes) Palette overrides"))

(defconst ef-tokyo-night-palette
  (modus-themes-generate-palette
   ef-tokyo-night-palette-partial
   nil
   nil
   (append ef-tokyo-night-palette-mappings-partial ef-themes-palette-common)))

;;;###theme-autoload
(modus-themes-theme
 'ef-tokyo-night
 'ef-themes
 "Dark theme with the Tokyo Night palette, built on the ef-themes engine."
 'dark
 'ef-tokyo-night-palette
 nil
 'ef-tokyo-night-palette-overrides)

(provide 'ef-tokyo-night-theme)

;;; ef-tokyo-night-theme.el ends here
