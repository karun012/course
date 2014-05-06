{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.FileIO where

import Course.Core
import Course.Applicative
import Course.Apply
import Course.Bind
import Course.Functor
import Course.List

{-

Useful Functions --

  getArgs :: IO (List Chars)
  putStrLn :: Chars -> IO ()
  readFile :: Chars -> IO Chars
  lines :: Chars -> List Chars
  void :: IO a -> IO ()

Abstractions --
  Applicative, Monad:

    <$>, <*>, >>=, =<<, pure

Problem --
  Given a single argument of a file name, read that file,
  each line of that file contains the name of another file,
  read the referenced file and print out its name and contents.

Example --
Given file files.txt, containing:
  a.txt
  b.txt
  c.txt

And a.txt, containing:
  the contents of a

And b.txt, containing:
  the contents of b

And c.txt, containing:
  the contents of c

$ runhaskell io.hs "files.txt"
============ a.txt
the contents of a

============ b.txt
the contents of b

============ c.txt
the contents of c

-}

-- /Tip:/ use @getArgs@ and @run@
main ::
  IO ()
main = do
    args <- getArgs
    run (headOr Nil args)

type FilePath =
  Chars

-- /Tip:/ Use @getFiles@ and @printFiles@.
run ::
  Chars
  -> IO ()
run args = printFiles =<< (getFiles . lines) =<< readFile args 
-- Feels imperative
-- do
--    fileNamesSeparatedByNewLine <- readFile args
--    files <- getFiles (lines fileNamesSeparatedByNewLine)
--    printFiles files

getFiles ::
  List FilePath
  -> IO (List (FilePath, Chars))
getFiles = sequence . (<$>) getFile

getFile ::
  FilePath
  -> IO (FilePath, Chars)
getFile filePath = lift2 (<$>) (,) readFile filePath
-- Feels imperative
--    do
--    contents <- readFile filePath
--    pure (filePath, contents)

printFiles ::
  List (FilePath, Chars)
  -> IO ()
printFiles (x :. y) = void (sequence ((uncurry printFile x) :. (printFiles y) :. Nil))
printFiles Nil = void (pure Nil)

printFile ::
  FilePath
  -> Chars
  -> IO ()
printFile filePath contents = putStrLn ("============ " ++ filePath ++ "\n" ++ contents)

