# Tests.
* SBCL/2.2.4
* CCL/1.12.1
* CLISP/2.49
* ECL/21.2.1
* Allegro/10.1
* CMUCL/21D ; See below.
* ABCL/1.9.0 ; Failed. See below.

# Known issues.

## CMUCL
To maximize robustness, jingoh does not provide an issue coloring feature for
CMUCL because [`vivid-colors` is stopping to support CMUCL.](https://github.com/hyotang666/vivid-colors/tree/main/content#cmucl)

If you want to try an issue coloring feature in CMUCL, add `:vivid-colors` to `*features*`.

```lisp
(puashnew :vivid-colors *features*)
```

## ABCL
### [WITH-COMPILATION-UNIT issue in abcl](https://github.com/armedbear/abcl/issues/396)
ABCL `with-compilation-unit` tries to muffle warning when a warning is signaled.
If the restart `muffle-warning` is not achieved, a `control-error` is signaled.
This means you can not test `cl:signal` behavior via `asdf:test-system`.
(`asdf:test-system` implicitly uses `with-compilation-unit`.)

In such cases, the debugger is invoked with constructing the restart `continue`.
If you choose the `continue`, such tests are skipped.

### [Timeout issue in abcl](https://github.com/armedbear/abcl/issues/405)
You will get `java.nio.channels.ClosedByInterruptException` after testing.
(Typically, you will get it when you evaluates `asdf:load-system`.)
You can recover it by evaluating `(sleep 0)`. (I do not know why.)
For issue details, see the link.
