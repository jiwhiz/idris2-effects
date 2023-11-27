module Effect.Select

import Effects

public export
data Selection : Effect where
    Select : List a -> sig Selection a

public export
implementation Handler Selection Maybe where
    handle _ (Select xs) k = tryAll xs where
        tryAll : List t -> Maybe a
        tryAll [] = Nothing
        tryAll (x :: xs) =
            case k x () of
                Nothing => tryAll xs
                Just v => Just v

public export
implementation Handler Selection List where
     handle _ (Select xs) k = concatMap (\x => k x ()) xs

public export
SELECT : EFFECT
SELECT = MkEff () Selection

public export
select : List a -> Eff a [SELECT]
select xs = call (Select xs) {prf=Z}

