module Make.Shortcuts (shortcutRules) where

import Data.List
import Development.Shake
import Development.Shake.Config
import Development.Shake.FilePath

import Make.Clash
import Make.Command
import Make.Config
import Make.HDL
import Make.Utils
import Make.Vagrant

shortcutRules :: Rules ()
shortcutRules = do
  installCommandTree $ commandGroup ":" [ generalCommands
                                        , fpgaCommands
                                        , clashCommands
                                        , simulateCommands
                                        ]

generalCommands :: CommandTree
generalCommands = commandGroup "" [ mkCommand' "clean" cleanCmd ]
  where cleanCmd = getBuildDir >>= (flip removeFilesAfter [ "//*" ])

fpgaCommands :: CommandTree
fpgaCommands = commandGroup "fpga:" [ mkCommand "reset:" resetCmd
                                    , mkCommand "build:" buildCmd
                                    , mkCommand "load:" loadCmd
                                    , mkCommand "burn:" burnCmd
                                    , mkCommand "stage:" stageCmd
                                    ]

clashCommands :: CommandTree
clashCommands = commandGroup "clash:" [ mkCommand "vhdl:" (buildClashCmd VHDL)
                                      , mkCommand "verilog:" (buildClashCmd Verilog)
                                      , listCommands ]
  where listCommands = commandGroup "list:" [ mkCommand "projects:" listProjectsClashCmd
                                            , mkCommand "targets:" listTargetsClashCmd
                                            ]

simulateCommands :: CommandTree
simulateCommands = commandGroup "simulate:" [ commandGroup "vhdl:" [ mkCommand "build:" ghdlBuildCmd
                                                                   , mkCommand "list:" ghdlListCmd ]]

resetCmd :: String -> Action ()
resetCmd _ = do
  (Just fpgaProg) <- getConfig "FPGAPROG"
  withVagrant $ vagrantSSH ["sudo", fpgaProg, "-vr"]

buildCmd :: String -> Action ()
buildCmd project = do
  (Just container) <- getXilinxDir
  need [container </> project -<.> "bit"]

loadCmd :: String -> Action ()
loadCmd project = do
  (Just container) <- getXilinxDir
  vmContainer <- getVMXilinxDir
  (Just fpgaProg) <- getConfig "FPGAPROG"

  need [container </> project -<.> "bit"]

  let vagrantBitfile = vmContainer </> project -<.> "bit"

  withVagrant $ vagrantSSH ["sudo", fpgaProg, "-vf", vagrantBitfile]

burnCmd :: String -> Action ()
burnCmd project = do
  (Just container) <- getXilinxDir
  vmContainer <- getVMXilinxDir
  (Just fpgaProg) <- getConfig "FPGAPROG"
  (Just burner) <- getConfig "FPGA_BURNER"

  need [container </> project -<.> "bit"]

  let vagrantBitfile = vmContainer </> project -<.> "bit"
  let burnCmdline = ["sudo", fpgaProg,
                 "-vf", vagrantBitfile,
                 "-b", burner,
                 "-sa", "-r"]

  withVagrant $ vagrantSSH burnCmdline

stageCmd :: String -> Action ()
stageCmd project = do
  (Just container) <- getXilinxDir
  need [ container </> project -<.> "prj"
       , container </> project -<.> "ucf"]

buildClashCmd :: HDL -> String -> Action ()
buildClashCmd hdl "" = (map fst <$> getProjects) >>= mapM (buildClashCmd hdl) >> return ()
buildClashCmd hdl project = do
  clashOut <- getClashDir
  putNormal $ "building " ++ project ++ " (" ++ hdlName hdl ++ ")"
  need [ clashOut  </> hdlName hdl </> project </> "manifest" <.> "txt" ]

listProjectsClashCmd :: String -> Action ()
listProjectsClashCmd _ = (map fst <$> getProjects) >>= mapM putQuiet >> return ()

listTargetsClashCmd :: String -> Action ()
listTargetsClashCmd "" = (map fst <$> getProjects) >>= mapM listTargetsClashCmd >> return ()
listTargetsClashCmd "*" = listTargetsClashCmd ""
listTargetsClashCmd project = getTargets project >>= putQuiet . mkTargetString
  where mkTargetString targets = project ++ ": " ++ intercalate ", " (map fst targets)

type GHDLSpec = (Maybe String, Maybe String, Maybe String)

-- project, library, entity
-- Nothing means wildcard
parseGHDLSpec :: String -> Maybe GHDLSpec
parseGHDLSpec spec = case splitBy (=='-') spec of
                       [] -> Just (Nothing, Nothing, Nothing)
                       [p] -> Just (parseWild p, Nothing, Nothing)
                       [p, l] -> Just (parseWild p, parseWild l, Nothing)
                       [p, l, e] -> Just (parseWild p, parseWild l, parseWild e)
                       _ -> Nothing
  where parseWild "*" = Nothing
        parseWild s = Just s

--getGHDLEntityFiles :: GHDLSpec -> Action [FilePath]

ghdlListCmd :: String -> Action ()
ghdlListCmd _ = do
  -- projects <- getProjects
  putQuiet "NOT DONE"
  -- parse the GHDL spec, build all the clash stuff that needs to be built,
  -- create the necessary vhdl libraries, and have ghdl list the contents of the
  -- libraries (ghdl --dir LIB)

ghdlBuildCmd :: String -> Action ()
ghdlBuildCmd spec = do
  let (Just spec') = parseGHDLSpec spec
  -- ghdlDir <- getGHDLDir
  putQuiet "NOT DONE"
  putQuiet ("should probably do something about " ++ show spec')
  -- depend on the entity executable
