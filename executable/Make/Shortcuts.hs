module Make.Shortcuts (shortcutRules) where

import Control.Monad
import Data.List
import Development.Shake
import Development.Shake.Config
import Development.Shake.FilePath

import Make.Clash
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
                                        , romCommands
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
romCommands :: CommandTree
romCommands = commandGroup "rom:" [ mkCommand "tileset:" romBuildTilesetCmd ]

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

romBuildTilesetCmd :: String -> Action ()
romBuildTilesetCmd tileset = do
  (Just tileOut) <- getConfigIO "TILE_OUT"
  need [ tileOut </> tileset ]








-- Represents a heiarchy of commands.
data CommandTree = Command (String -> Action ()) -- Command that takes an argument
                 | Command' (Action ())
                 | CommandSet String [CommandTree] -- a set of commands beginning with a name.

mkCommand :: String -> (String -> Action ()) -> CommandTree
mkCommand name act = CommandSet name [ Command act ]

mkCommand' :: String -> Action () -> CommandTree
mkCommand' name act = CommandSet name [ Command' act ]

commandGroup :: String -> [CommandTree] -> CommandTree
commandGroup prefix commands = CommandSet prefix commands

newtype PhonyMatcher = PM (String -> Maybe (Action ()))

instance Semigroup PhonyMatcher where
  (PM m1) <> (PM m2) = PM $ \ target ->
    case m1 target of
      -- If the first matcher doesn't match then try the second
      Nothing -> m2 target
      -- otherwise the first one is fine
      result  -> result

instance Monoid PhonyMatcher where
  mempty = PM $ \ _ -> Nothing


-- Creates a matcher that behaves like `m` when it sees a target that begins with `p`.
prefixMatcher :: String -> PhonyMatcher -> PhonyMatcher
prefixMatcher p (PM m) = PM $ \ input -> join $ m <$> stripPrefix p input

-- Turns a command tree into a matcher
commandTreeMatcher :: CommandTree -> PhonyMatcher
commandTreeMatcher (Command act) = PM $ \ arg -> Just (act arg)
commandTreeMatcher (Command' act) = PM $ \ arg -> case arg of
                                                    "" -> Just act
                                                    _ -> Nothing
commandTreeMatcher (CommandSet prefix cmds) = prefixMatcher prefix (mconcat children)
  where children = map commandTreeMatcher cmds

installCommandTree :: CommandTree -> Rules ()
installCommandTree ctree = phonys matcher
  where (PM matcher) = commandTreeMatcher ctree
