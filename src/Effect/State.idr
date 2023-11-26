module Effect.State

import Effects
import Data.List.Elem

public export
data State : Effect where
  Get :      sig State a  a
  Put : b -> sig State () a b

-- using (m : Type -> Type)
public export
implementation Handler State m where
     handle st Get     k = k st st
     handle st (Put n) k = k () n

public export
STATE : Type -> EFFECT
STATE t = MkEff t State

public export
get : {x : _} -> Eff x [STATE x]
get = call Get {prf=Here}

public export
put : {x : _} -> x -> Eff () [STATE x]
put val = call (Put val) {prf=Here}

public export
putM : {x, y : _} -> y -> Eff () [STATE x] [STATE y]
putM val = call (Put val) {prf=Here}

public export
update : {x : _} -> (x -> x) -> Eff () [STATE x]
update f = put (f !get)

public export
updateM : {x, y : _} -> (x -> y) -> Eff () [STATE x] [STATE y]
updateM f = putM (f !get)

public export
locally : {x, y : _} -> x -> (Eff t [STATE x]) -> Eff t [STATE y]
locally newst prog = do st <- get
                        putM newst
                        val <- prog
                        putM st
                        pure val

