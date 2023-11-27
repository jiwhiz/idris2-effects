module Effect.Exception

import Effects
import System

public export
data Exception : Type -> Effect where
     Raise : a -> sig (Exception a) b

public export
implementation {a:_} -> Handler (Exception a) Maybe where
     handle _ (Raise err) k = Nothing

public export
implementation {a:_} -> Handler (Exception a) List where
     handle _ (Raise e) k = []

public export
implementation {a:_} -> Show a => Handler (Exception a) IO where
     handle _ (Raise e) k = do printLn e
                               exitFailure

public export
implementation {a:_} -> Handler (Exception a) (Either a) where
     handle _ (Raise e) k = Left e

public export
EXCEPTION : Type -> EFFECT
EXCEPTION t = MkEff () (Exception t)

public export
raise : {a:_} -> a -> Eff b [EXCEPTION a]
raise err = call (Raise err) {prf=Here}
