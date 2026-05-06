# CLAUDE.md

Personal Emacs configuration. Repo is git-tracked at
`github.com:honnix/dot.emacs`. Targets Emacs 29+ (currently developed
against 31.0.50).

## Layout

- `early-init.el` — runs before package.el and the GUI initializes.
  Holds anything that must happen pre-frame-creation: warning
  suppression, `custom-file` location, initial frame geometry, font.
- `init.el` — entrypoint. Sets `package-archives` and queues
  `personal/*.el` to load via `after-init-hook`.
- `personal/NN<topic>.el` — main config, loaded alphanumerically.
  - `01basic.el` — base settings, loads `custom-file`, key bindings,
    custom helper functions.
  - `02packages.el` — bulk of `use-package` declarations (general
    packages: ivy, magit, lsp, dashboard, themes, etc.).
  - `03c.el` … `30org.el` — language-/topic-specific configs.
  - `99last.el` — last-loaded tweaks.
- `3rd/` — vendored elisp not on MELPA (`ligature.el`,
  `multi-scratch.el`, `hl-line+.el`, `prolog.el`).
- `custom.el` — Custom-managed variables (don't edit by hand for
  package lists; use `M-x customize` or `package-install` and let it
  write here).
- `elpa/` — installed packages (managed by package.el; in
  `.gitignore` only for selected sub-paths).

## Load order

1. `early-init.el`
2. `init.el` top-level (sets `package-archives`, registers
   `after-init-hook`)
3. Implicit `package-initialize` / use-package activation
4. `after-init-hook` fires → `mapc 'load` over `personal/*.el`
   alphabetically
5. `personal/01basic.el` loads `custom-file`

Anything that must affect the *initial* frame (size, font, faces) has
to be in `early-init.el` — by the time `personal/*.el` runs the
initial frame already exists.

## Conventions

- New language/topic configs go in a new `personal/NN<name>.el` —
  pick a free `NN` prefix to control load order.
- Use `use-package` with `:ensure t` for MELPA packages; `:ensure nil`
  for built-ins.
- `:ensure-system-package` is wired up via the built-in
  use-package keyword and depends on the `system-packages` package
  (which is in `package-selected-packages`).
- Settings touched via `M-x customize` write to `custom.el` because
  `custom-file` is set in `early-init.el`. Don't add a second
  `custom-set-variables` block elsewhere.
- Frame geometry/font live in `early-init.el`; per-package faces
  belong wherever the package is configured.

## Common tasks

- **Install missing packages**: `M-x package-install-selected-packages`
  reads `package-selected-packages` from `custom.el`.
- **Add a package**: `M-x package-install RET name RET` (Custom auto-
  saves the name into `custom.el`'s `package-selected-packages`), then
  add a `use-package` block to the appropriate `personal/NN*.el`.
- **Spell check**: uses `aspell` (configured in `02packages.el`).
  Install via `brew install aspell` on macOS.
- **Icon fonts**: `nerd-icons` is required by dashboard/doom-modeline.
  Run `M-x nerd-icons-install-fonts` once and install the resulting
  `NFM.ttf` via Font Book.
- **Editor font**: Fira Code (`brew install --cask font-fira-code`).

## Things to know

- Suppressing the Emacs 30+ "missing lexical-binding cookie" warning
  spam is done in `early-init.el` via
  `warning-suppress-log-types` (not `-types`, which `custom.el`
  manages and would clobber).
- `cl` deprecation message is harmless — comes from old ELPA packages
  (`drag-stuff`, `gist`, `company-go`, `highlight-symbol`, etc.) still
  doing `(require 'cl)`. Not from this config.
- `undo-tree` writes `.~undo-tree~` files next to saved files for
  cross-session undo. They are gitignored.
- Auto-generated state files (`recentf.eld`, `.session`, `custom.el`'s
  ordering) will churn on each Emacs session — don't bother diffing.
