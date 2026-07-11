# plugins/

Drop your project plugins here. pi++ auto-loads this directory at startup
(and any dir passed via `--plugins <dir>`).

- `foo.rkt` — **trusted** plugin: `(provide plugin)` a `(-> plugin-api void)` register
  function (loaded via `dynamic-require`, full access).
- `foo-sandbox.rkt` — **sandboxed** plugin: `#lang racket/base`, `(provide manifest tool-run)`
  (loaded via `racket/sandbox` with memory/time/fs/net limits).

Runnable examples live in [`../examples/plugins/`](../examples/plugins/); design and the
extension-point API are in [`../design-plugins.md`](../design-plugins.md).
Try them: `racket main.rkt --plugins examples/plugins`.
