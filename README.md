# H.el  [![CI](https://github.com/NinjaTrappeur/h.el/actions/workflows/test.yml/badge.svg)](https://github.com/NinjaTrappeur/h.el/actions/workflows/test.yml)
Project checkout and navigation Emacs package heavily inspired by [zimbatm/h](https://github.com/zimbatm/h).

**It's not ready yet. Come back later.**

TODO before first release:

- [x] Replace `directory-files-recursively` with custom implemention. Support 'till Emacs 24.
- [ ] Implement GitHub fetcher.
- [ ] Implement GitLab fetcher.
- [ ] Implement sr.ht fetcher.
- [ ] Implement codeberg fetcher.
- [ ] Document how to implement a new fetcher.
- [ ] Explain what the hell this thing is about in readme.

# Setup

You'll have to setup

```elisp
(require 'h)
(setq h-code-root "~/code-root")
```

```elisp
(global-set-key (kbd "M-h") 'h-jump-to-project)
```
