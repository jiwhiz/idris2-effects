module Effect.StdIO

import Data.String
import Effects

-------------------------------------------------------------
-- IO effects internals
-------------------------------------------------------------

||| The internal representation of StdIO effects
public export
data StdIO : Effect where
     PutStr : String -> sig StdIO ()
     GetStr : sig StdIO String
     PutCh : Char -> sig StdIO ()
     GetCh : sig StdIO Char


-------------------------------------------------------------
-- IO effects handlers
-------------------------------------------------------------
public export
implementation Handler StdIO IO where
    handle () (PutStr s) k = do putStr s; k () ()
    handle () GetStr     k = do x <- getLine; k x ()
    handle () (PutCh c)  k = do putChar c; k () ()
    handle () GetCh      k = do x <- getChar; k x ()

-- public export
-- implementation Handler StdIO (IOExcept a) where
--     handle () (PutStr s) k = do ioe_lift $ putStr s; k () ()
--     handle () GetStr     k = do x <- ioe_lift $ getLine; k x ()
--     handle () (PutCh c)  k = do ioe_lift $ putChar c; k () ()
--     handle () GetCh      k = do x <- ioe_lift $ getChar; k x ()

-------------------------------------------------------------
--- The Effect and associated functions
-------------------------------------------------------------
public export
STDIO : EFFECT
STDIO = MkEff () StdIO

||| Write a string to standard output.
public export
putStr : String -> Eff () [STDIO]
putStr s = call (PutStr s) {prf=Here}

||| Write a string to standard output, terminating with a newline.
public export
putStrLn : String -> Eff () [STDIO]
putStrLn s = putStr (s ++ "\n")

||| Write a character to standard output.
public export
putChar : Char -> Eff () [STDIO]
putChar c = call (PutCh c) {prf=Here}

||| Write a character to standard output, terminating with a newline.
public export
putCharLn : Char -> Eff () [STDIO]
putCharLn c = putStrLn (singleton c)

||| Read a string from standard input.
public export
getStr : Eff String [STDIO]
getStr = call GetStr {prf=Here}

||| Read a character from standard input.
public export
getChar : Eff Char [STDIO]
getChar = call GetCh {prf=Here}

||| Given a parameter `a` 'show' `a` to standard output.
public export
print : Show a => a -> Eff () [STDIO]
print a = putStr (show a)

||| Given a parameter `a` 'show' `a` to a standard output, terminating with a newline
public export
printLn : Show a => a -> Eff () [STDIO]
printLn a = putStrLn (show a)
