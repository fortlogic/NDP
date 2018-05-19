module Make.GHDL ( ghdlRules
                 , GHDLSpec (..)
                 , parseGHDLSpec
                 , generateIncludeFiles
                 , generateExecutableFiles ) where

import Control.Monad.IO.Class
import Data.List
import Development.Shake
import Development.Shake.Config
import Development.Shake.FilePath
import System.Directory

import Make.Clash
import Make.Config
import Make.HDL
import Make.Utils

-- ghdl -i --workdir=build/i386_Darwin/ghdl/PrimitiveTest --work=clockStrobe build/i386_Darwin/clash/vhdl/PrimitiveTest/clockStrobe/*.vhdl


-- validTestbenchOutput :: Rules (FilePath -> Bool)
-- validTestbenchOutput = do
--   ghdlDir <- getGHDLDir
--   (Just entityD) <- liftIO $ getConfigIO "HDL_ENTITIES"
--   entities <- liftIO $ getDirectoryDirsIO entityD
--   return (\ testbenchF -> let testbenchF' = makeRelative ghdlOut testbenchF in
--                       elem testbenchF' (map (\ entity -> entity </> "manifest" <.> "txt") entities))

ghdlRules :: Rules ()
ghdlRules = do
  ghdlDir <- getGHDLDir

  (ghdlDir </> "*/*-obj93.cf") %> \ incF -> do
    let path = stripPrefix ghdlDir incF
    let projectName = (takeFileName . takeDirectory) incF
    let (Just libName) = (stripSuffix "-obj93.cf" . takeFileName) incF
    generateIncludeFiles (GHDLLibrary projectName libName)


  -- (ghdlDir </> "*/work-obj93.cf") %> \ incF -> do
  --   let workD = takeDirectory incF
  --   let entityName = withPath (pure . last) $ workD
  --   (Just clashOutD) <- getConfig "CLASH_OUT"
  --   let clashVhdlD = clashOutD </> entityName
  --   clashVhdlAbsD <- liftIO $ makeAbsolute clashVhdlD
  --   need [clashVhdlD </> "ndp_topentity" <.> "vhdl"]
  --   clashVhdlFs <- liftIO $ getDirectoryFilesWithExt clashVhdlAbsD ".vhdl"
  --   cmd "ghdl" "-i" ("--workdir=" ++ workD) clashVhdlFs

  (ghdlDir </> "*/testbench") %> \ tbF -> do
    putQuiet "OOPS"
    -- TODO: instead of all the junk below, generate a GHDLSpec
    -- then pass it to generateExecutableFiles

    -- let workD = takeDirectory tbF
    -- need [workD </> "work-obj93.cf"]
    -- cmd "ghdl" ["-m",
    --             "--workdir=" ++ workD,
    --             "--ieee=synopsys",
    --             "-fexplicit",
    --             "-o", workD </> "testbench",
    --             "ndp_testbench"]

data GHDLSpec = GHDLEverything
              | GHDLProject String
              | GHDLLibrary String String
              | GHDLEntity String String String
              deriving (Read, Show, Eq)

ghdlSpecProject :: GHDLSpec -> Maybe String
ghdlSpecProject (GHDLProject p) = Just p
ghdlSpecProject (GHDLLibrary p _) = Just p
ghdlSpecProject (GHDLEntity p _ _) = Just p
ghdlSpecProject _ = Nothing

ghdlSpecLibrary :: GHDLSpec -> Maybe String
ghdlSpecLibrary (GHDLLibrary _ l) = Just l
ghdlSpecLibrary (GHDLEntity _ l _) = Just l
ghdlSpecLibrary _ = Nothing

ghdlSpecEntity :: GHDLSpec -> Maybe String
ghdlSpecEntity (GHDLEntity p _ _) = Just p
ghdlSpecEntity _ = Nothing

parseGHDLSpec :: String -> Maybe GHDLSpec
parseGHDLSpec s = case splitBy (=='-') s of
                    [] -> Just GHDLEverything
                    ["*"] -> Just GHDLEverything
                    [p] -> Just (GHDLProject p)
                    [p, "*"] -> Just (GHDLProject p)
                    [p, l] -> Just (GHDLLibrary p l)
                    [p, l, "*"] -> Just (GHDLLibrary p l)
                    [p, l, e] -> Just (GHDLEntity p l e)
                    _ -> Nothing

-- needs project and library
-- TODO: Currently can't handle GHDLEverything or GHDLProject because they aren't specific
{-# WARNING generateIncludeFiles "Need to be able to figure out what projects/librarys can exist before I can parse any wildcards" #-}
generateIncludeFiles :: GHDLSpec -> Action ()
generateIncludeFiles (GHDLLibrary p l) = generateIncludeFile p l
generateIncludeFiles (GHDLEntity p l _) = generateIncludeFile p l
generateIncludeFiles _ = putQuiet "I need more specificity" >> fail "oops"

generateIncludeFile :: String -> String -> Action ()
generateIncludeFile project library = do
  clashOut <- getClashDir
  ghdlOut <- getGHDLDir
  need [ clashOut </> "vhdl" </> project </> "manifest.txt" ]

  cmd "ghdl" [ "-i"
             , "--work=" ++ library
             , "--workdir=" ++ ghdlOut </> project
             ]

{-# WARNING generateExecutableFiles "can't do wildcards yet." #-}
generateExecutableFiles :: GHDLSpec -> Action ()
generateExecutableFiles (GHDLEntity p l e) = generateExecutableFile p l e
generateExecutableFiles _ = putQuiet "haven't written this yet"

generateExecutableFile :: String -> String -> String -> Action ()
generateExecutableFile project library entity = do
  putQuiet "NOT FINISHED WRITING THIS, needs to dispatch back to shake"
  generateIncludeFile project library
