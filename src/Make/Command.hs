module Make.Command (CommandTree (),
                     mkCommand,
                     commandGroup,
                     installCommandTree) where

import Control.Monad
import Data.List
import Data.Maybe
import Development.Shake

-- Represents a heiarchy of commands.
data CommandTree = Command (String -> Action ()) -- Command that takes an argument
                 | CommandSet String [CommandTree] -- a set of commands beginning with a name.

mkCommand :: String -> (String -> Action ()) -> CommandTree
mkCommand name command = CommandSet name [Command command]

commandGroup :: String -> [CommandTree] -> CommandTree
commandGroup prefix commands = CommandSet prefix commands

newtype PhonyMatcher = PM (String -> Maybe (Action ()))

instance Monoid PhonyMatcher where
  mempty = PM $ \ _ -> Nothing
  mappend (PM m1) (PM m2) = PM $ \ target ->
    case m1 target of
      -- If the first matcher doesn't match then try the second
      Nothing -> m2 target
      -- otherwise the first one is fine
      result  -> result

-- Creates a matcher that behaves like `m` when it sees a target that begins with `p`.
prefixMatcher :: String -> PhonyMatcher -> PhonyMatcher
prefixMatcher p (PM m) = PM $ \ input -> join $ m <$> stripPrefix p input

-- Turns a command tree into a matcher
commandTreeMatcher :: CommandTree -> PhonyMatcher
commandTreeMatcher (Command action) = PM $ \ arg -> Just (action arg)
commandTreeMatcher (CommandSet prefix cmds) = prefixMatcher prefix (mconcat children)
  where children = map commandTreeMatcher cmds

installCommandTree :: CommandTree -> Rules ()
installCommandTree ctree = phonys matcher
  where (PM matcher) = commandTreeMatcher ctree
