# Learning Log

> Episodic memory — lessons learned, patterns discovered, mistakes to avoid.
> Append new entries; do not modify old ones.

## Format

```markdown
### [DATE] — [SHORT TITLE]

**Context:** What was happening
**Lesson:** What was learned
**Apply When:** Future situations where this applies
```

---

## Entries

### 2024-01-24 — Gatekeeper Extended Attributes

**Context:** DMG downloaded from GitHub showed "damaged" error on macOS.
**Lesson:** macOS Gatekeeper adds quarantine extended attributes to downloaded files. Users must either right-click → Open or strip attributes with `xattr -cr`.
**Apply When:** Any unsigned app distribution; document this in installation instructions.

### 2024-01-24 — Upstream Sync Pattern

**Context:** Needed to update from Steve's stats4ROI repository.
**Lesson:** One-way pull from `deployment/` directory works well. Don't modify upstream code; just package it.
**Apply When:** Any future updates from professorperegrine/stats4ROI.

---
*Append new lessons below this line.*
