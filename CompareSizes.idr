module CompareSizes

import Data.List
import Data.Maybe
import Data.SortedMap
import Data.String.Parser
import Interlude.CLI
import Interlude.Control
import Interlude.SortedMap
import ReadElf
import System
import System.File

data Difference = Delta Integer | New Integer | Removed Integer

sizeOf: Difference -> Integer
sizeOf (Delta     d) = d
sizeOf (New       d) = d
sizeOf (Removed   d) = negate d

Show Difference where
   show (Delta   d) = case compare d 0 of
      GT => "Grew \{show d}"
      EQ => "Unchanged"
      LT => "Shrank \{show (negate d)}"
   show (New     d) = "New \{show d}"
   show (Removed d) = "Removed \{show d}"

difference: OneOrBoth Integer Integer -> Difference
difference (JustRight after) = New after
difference (JustLeft before) = Removed before
difference (Both before after) = Delta (after - before)

usage: Maybe String -> String
usage program =
    let defaultName = "CompareSizes"
    in "usage: \{fromMaybe defaultName program} <symbols-1> <symbols-2>\n"

loadSymbolMap: File -> IO (Either (InterpretError String) (SortedMap String Integer))
loadSymbolMap file =
   let addInts = Monoid.Additive {a = Integer}
   in  interpretLinesRight file $ \line => do
      let Right (t, _) = parse table line
         | Left e => pure (Left "\{e}\nInput: \{show line}")
      pure (Right (singleton t.name t.size))

putStdErr: String -> IO ()
putStdErr = ignore . fPutStr stderr

main: IO ()
main = do
   [program, beforeFileName, afterFileName] <- getArgs
      | args => putStr$ usage (head' args)

   Right beforeFile <- openFile beforeFileName Read
      | Left err => putStdErr "File error opening \{beforeFileName}: \{show err}\n"
   Right afterFile <- openFile afterFileName Read
      | Left err => putStdErr "File error opening \{afterFileName}: \{show err}\n"

   putStdErr "Loading symbol sizes from \{beforeFileName}...\n"
   Right symbolsBefore <- loadSymbolMap beforeFile
      | Left (FileError    e) => putStdErr "IO error processing \{beforeFileName}: \{show e}\n"
      | Left (HandlerError e) => putStdErr "Parse error processing \{beforeFileName}: \{e}\n"
   putStdErr "Loading symbol sizes from \{afterFileName}...\n"
   Right symbolsAfter <- loadSymbolMap afterFile
      | Left (FileError    e) => putStdErr "IO error processing \{afterFileName}: \{show e}\n"
      | Left (HandlerError e) => putStdErr "Parse error processing \{afterFileName}: \{e}\n"

   putStdErr "Calculating symbols size difference...\n"
   let mergedSymbols = SortedMap.zipWith difference symbolsBefore symbolsAfter
   let orderedByDelta =
      sortBy (comparing (sizeOf . Builtin.fst))$
      mapMaybe (\(name, delta) => case delta of
            Delta   0 => Nothing
            New     0 => Nothing
            Removed 0 => Nothing
            _         => Just (delta, name))
         (SortedMap.toList mergedSymbols)
   for_ orderedByDelta printLn
   let totalDelta = foldl (\acc,(d,_) => acc + sizeOf d) 0 orderedByDelta
   putStr "Total symbol size change: \{show totalDelta}\n"

