# composite-xstep

This package provides a specific XRec pattern for [composite](https://hackage.haskell.org/package/composite) records, where the interpretation functor is isomorphic to `ReaderT r m`.

For example

```
import Path
import Composite.Record
import Composite.TH

withLensesAndProxies [d|
  type FPath          = "path"            :-> Path Rel File
  type FPath2         = "path2"           :-> Path Rel File
  |]

type A = '[FPath, FPath2]

foo :: MonadThrow m => XStep m FilePath A
foo =  parseRelDir
   ::& stripProperPrefix $(mkRelFile "foo") =<< parseRelDir
   ::& XRNil
```
