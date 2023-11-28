||| Effectful file operations.
module Effect.File

import Effects
import public System.File

-- -------------------------------------------------------------- [ Predicates ]

||| A record of the file modes that can read from a file.
public export
data ValidModeRead : Mode -> Type where
  VMRRead   : ValidModeRead Read
  VMRReadW  : ValidModeRead ReadWrite
  VMRReadWT : ValidModeRead ReadWriteTruncate
  VMRReadA  : ValidModeRead ReadAppend

||| A record of the file modes that can write from a file.
public export
data ValidModeWrite : Mode -> Type where
  VMWWrite  : ValidModeWrite WriteTruncate
  VMWAppend : ValidModeWrite Append
  VMWReadW  : ValidModeWrite ReadWrite
  VMWReadWT : ValidModeWrite ReadWriteTruncate

-- -------------------------------------------------- [ Custom Error Reporting ]

namespace FileResult

    ||| A type to describe the return type of file operations.
    public export
    data ResultDesc = SUCCESS | RESULT

    ||| A custom return type for file operations that is dependent on
    ||| the type of file operation.
    |||
    ||| @desc Parameterises the constructors to describe if the function
    |||       returns a value or not.
    ||| @ty   The return type for a file operation that returns a value.
    |||
    public export
    data FileOpReturnTy : (desc : ResultDesc)
                        -> (ty : Type)
                        -> Type where

        ||| The operation completed successfully and doesn't return a
        ||| result.
        Success : FileOpReturnTy SUCCESS ty

        ||| The operation returns a result of type `ty`.
        |||
        ||| @ty The value returned.
        Result : ty -> FileOpReturnTy RESULT ty

        ||| The operation failed and the RTS produced the given error.
        |||
        ||| @err The reported error code.
        FError : (err : FileError) -> FileOpReturnTy desc ty

    ||| Type alias to describe a file operatons that returns a result.
    public export
    FileOpResult : Type -> Type
    FileOpResult ty = FileOpReturnTy RESULT ty

    ||| Type alias to describe file oeprations that indicate success.
    public export
    FileOpSuccess : Type
    FileOpSuccess = FileOpReturnTy SUCCESS ()

-- ------------------------------------------------------------ [ The Resource ]

||| The file handle associated with the effect.
|||
||| @m The `Mode` that the handle was generated under.
public export
data FileHandle : (m : Mode) -> Type where
    FH : File -> FileHandle m

-- ---------------------------------------------- [ Resource Type Construction ]

||| Calculates the type for the resource being computed over.  `Unit`
||| to describe pre-and-post file handle acquisition, and `FileHandle
||| m` when a file handle has been acquired.
|||
||| @m The mode the file handle was generated under.
||| @ty The functions return type.
public export
calcResourceTy : (md  : Mode)
              -> (ty : FileOpReturnTy fOpTy retTy)
              -> Type
calcResourceTy _ (FError e) = ()
calcResourceTy md _          = FileHandle md

-- ------------------------------------------------------- [ Effect Definition ]

||| An effect to describe operations on a file.
public export
data FileE : Effect where

    -- Open/Close

    Open : (fname : String)
        -> (m : Mode)
        -> sig FileE
                (FileOpSuccess)
                ()
                (\res => calcResourceTy m res)

    -- OpenX : (fname : String)
    --     -> (m : Mode)
    --     -> sig FileE
    --             (FileOpSuccess)
    --             ()
    --             (\res => calcResourceTy m res)

    Close : sig FileE () (FileHandle m) ()

    -- Read

    FGetC : {auto prf : ValidModeRead m}
        -> sig FileE
                (FileOpResult Char)
                (FileHandle m)
                (FileHandle m)

    FGetLine : {auto prf : ValidModeRead m}
            -> sig FileE
                    (FileOpResult String)
                    (FileHandle m)
                    (FileHandle m)

    FReadFile : (fname : String)
            -> sig FileE
                    (FileOpResult String)
                    ()
                    ()
    -- Write
    FPutStr : (str : String)
            -> {auto prf : ValidModeWrite m}
            -> sig FileE
                    (FileOpSuccess)
                    (FileHandle m)
                    (FileHandle m)

    FPutStrLn : (str : String)
            -> {auto prf : ValidModeWrite m}
            -> sig FileE
                    (FileOpSuccess)
                    (FileHandle m)
                    (FileHandle m)

    FWriteFile : (fname    : String)
                -> (contents : String)
                -> sig FileE
                    (FileOpSuccess)
                    ()

    -- Flush
    FFlush : sig FileE
                ()
                (FileHandle m)
                (FileHandle m)

    -- Query
    FEOF : {auto prf : ValidModeRead m}
        -> sig FileE
                Bool
                (FileHandle m)

-- ---------------------------------------------------------------------- [ IO ]
public export
Handler FileE IO where

    -- Open Close
    handle () (Open fname m) k = do
        res <- openFile fname m
        case res of
            Left err => k (FError err) ()
            Right fh => k Success      (FH fh)

    -- handle () (OpenX fname m) k = do
    --     res <- openFileX fname m
    --     case res of
    --         Left err => k (FError err) ()
    --         Right fh => k Success      (FH fh)

    handle (FH h) Close k = do
        closeFile h
        k () ()

    -- Read
    handle (FH h) FGetC k = do
        res <- fGetChar h
        case res of
            Left err => k (FError err) (FH h)
            Right  c => k (Result c)   (FH h)

    handle (FH h) FGetLine k = do
        res <- fGetLine h
        case res of
            Left err => k (FError err) (FH h)
            Right ln => k (Result ln)  (FH h)

    handle () (FReadFile fname) k = do
        res <- readFile fname
        case res of
            Left err  => k (FError err) ()
            Right str => k (Result str) ()

    -- Write
    handle (FH fh) (FPutStr str) k = do
        res <- fPutStr fh str
        case res of
            Left err => k (FError err) (FH fh)
            Right () => k Success      (FH fh)

    handle (FH fh) (FPutStrLn str) k = do
        res <- fPutStr fh str
        case res of
            Left err => k (FError err) (FH fh)
            Right () => k Success      (FH fh)

    handle () (FWriteFile fname str) k = do
        res <- writeFile fname str
        case res of
            Left err => k (FError err) ()
            Right () => k Success      ()

    -- Flush
    handle (FH fh) FFlush k = do
        fflush fh
        k () (FH fh)

    -- Query
    handle (FH fh) FEOF k = do
        res <- fEOF fh
        k res (FH fh)

-- ------------------------------------------------------ [ Effect and Helpers ]

||| Effectful operations for interacting with files.
|||
||| The `FILE` effect is parameterised by a file handle once a handle has been acquired, and Unit (`()`) if the file handle is expected to be released once the function has returned.
|||
public export
FILE : (ty : Type) -> EFFECT
FILE t = MkEff t FileE

||| A file has been opened for reading.
public export
R : Type
R = FileHandle Read

||| A file has been opened for writing.
public export
W : Type
W = FileHandle WriteTruncate

||| A file can only be appended to.
public export
A : Type
A = FileHandle Append

||| A file can be read and written to.
public export
RW : Type
RW = FileHandle ReadWrite

||| A file opened for reading and writing and has been truncated to
||| zero if it previously existed.
public export
RWPlus : Type
RWPlus = FileHandle ReadWriteTruncate

||| A file will read from the beginning and write at the end.
public export
APlus : Type
APlus = FileHandle ReadAppend

-- --------------------------------------------------------------------- [ API ]

-- -------------------------------------------------------- [ Open/Close/Query ]

||| Open a file.
|||
||| @ fname the filename.
||| @ m     the mode; either Read, WriteTruncate, Append, ReadWrite,
|||         ReadWriteTruncate, or ReadAppend
|||
export
openFile
     : (fname : String)
    -> (md : Mode)
    -> Eff (FileOpSuccess)
           [FILE ()]
           (\res => [FILE (calcResourceTy md res)])
openFile f md = call (Open f md) {prf=Z}

||| Close a file.
export
close : {md : Mode} -> Eff () [FILE (FileHandle md)] [FILE ()]
close = call (Close) {prf=Z}

||| Have we reached the end of the file.
export
eof  : {md : Mode}
    -> {auto prfm : ValidModeRead md}
    -> Eff Bool [FILE (FileHandle md)]
eof = call FEOF {prf=Z}

export
flush : {md : Mode} -> Eff () [FILE (FileHandle md)]
flush = call FFlush {prf=Z}

-- -------------------------------------------------------------------- [ Read ]

||| Read a `Char`.
export
readChar
     : {md : Mode}
    -> {auto prf : ValidModeRead md}
    -> Eff (FileOpResult Char) [FILE (FileHandle md)]
readChar = call FGetC {prf=Z}

||| Read a complete line.
export
readLine
     : {md : Mode}
    -> {auto prf : ValidModeRead md}
    -> Eff (FileOpResult String) [FILE (FileHandle md)]
readLine = call FGetLine {prf=Z}

-- ------------------------------------------------------------------- [ Write ]

||| Write a string to the file.
export
writeString
     : {md : Mode}
    -> (str : String)
    -> {auto prf : ValidModeWrite md}
    -> Eff (FileOpSuccess) [FILE (FileHandle md)]
writeString str = call (FPutStr str) {prf=Z}

||| Write a complete line to the file.
export
writeLine
     : {md : Mode}
    -> (str : String)
    -> {auto prf : ValidModeWrite md}
    -> Eff (FileOpSuccess) [FILE (FileHandle md)]
writeLine str = call (FPutStrLn str) {prf=Z}

-- -------------------------------------------------------------- [ Whole File ]

||| Read the contents of a file into a string.
|||
||| This checks the size of
||| the file before beginning to read, and only reads that many bytes,
||| to ensure that it remains a total function if the file is appended
||| to while being read.
|||
||| Returns an error if fname is not a normal file.
export
readFile
     : (fname : String)
    -> Eff (FileOpResult String) [FILE ()]
readFile fn = call (FReadFile fn) {prf=Z}

||| Create a file and write contents to the file.
export
writeFile
     : (fname    : String)
    -> (contents : String)
    -> Eff (FileOpSuccess) [FILE ()]
writeFile fn str = call (FWriteFile fn str) {prf=Z}

-- --------------------------------------------------------------------- [ EOF ]
