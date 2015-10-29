###3.1
  - make declarations open via `class Declared`; this allows to provide problem
    specific declarations in executables
    - suitably adaption of `Argument` and `TctConfig` types
  - replace `Control.Monad.Error` with `Control.Monad.Except` as former one is
    deprecated since `mtl-2.2.1`

###3.0
  - initial release
