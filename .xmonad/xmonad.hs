import           Control.Applicative
import           Control.Monad

import           System.IO                      ( hPutStrLn )

import           XMonad
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops      ( ewmh )
import           XMonad.Util.EZConfig           ( additionalKeysP )
import           XMonad.Util.Run                ( safeSpawn
                                                , spawnPipe
                                                )
import           XMonad.Hooks.ManageDocks
                                         hiding ( L )
import           XMonad.Layout.WorkspaceDir     ( changeDir
                                                , workspaceDir
                                                )
import           XMonad.Prompt.Shell            ( shellPrompt )
import           XMonad.Prompt                  ( defaultXPConfig )
import           XMonad.Layout.Spacing
import           XMonad.Layout.GridVariants     ( SplitGrid(..)
                                                , Orientation(..)
                                                )
import           XMonad.Layout.Tabbed    hiding ( L
                                                , T
                                                )
import           XMonad.Layout.NoBorders
import           XMonad.Hooks.ManageDocks       ( avoidStruts )
import           XMonad.Layout.Named            ( named )
import           XMonad.Layout.PerScreen        ( PerScreen
                                                , ifWider
                                                )
import           XMonad.Actions.GroupNavigation
import           XMonad.Layout.Fullscreen
import           XMonad.Util.Cursor

import qualified XMonad.StackSet               as W

-- https://www.reddit.com/r/xmonad/comments/e9ivu/xmonad_compositing_a_quick_howto_on_ubuntu/
-- https://xiangji.me/2018/11/19/my-xmonad-configuration/

-- need to run [xcompmgr &] or [compton &]

myTerminalCommand :: String
myTerminalCommand = "alacritty"

myHomeDir :: String
myHomeDir = "/home/scen/"

home :: String -> String
home path = myHomeDir ++ path

--per monitor layout
-- https://www.reddit.com/r/xmonad/comments/fhzw3/permonitor_layout/ or this:
-- https://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Layout-PerScreen.html
-- the [PerLayout] thing isn't exactly what I want, but it kinda works since one
-- of my monitors is vertical and the other isn't
-- can use https://github.com/xmonad/xmonad-contrib/issues/293 to do alt tab (XMonad.Actions.GroupNavigation.isOnAnyVisibleWS)
-- TODO: there is a way to change the style of the tab text
-- TODO: implement alt tab, either jumping to last window or just the other monitor (should be easier)

goldenRatio = realToFrac $ (1 + sqrt 5 :: Double) / 2

splitGrid :: Orientation -> SplitGrid a
splitGrid orientation = SplitGrid orientation
                                  masterRows
                                  slaveRows
                                  masterRatio
                                  aspect
                                  increment
 where
  masterRows  = 1
  slaveRows   = 1
  masterRatio = case orientation of
    L -> 6 / 10
    T -> 5 / 8
    B -> 5 / 8
    _ -> 1 / 2
  aspect    = goldenRatio
  increment = 3 / 100

gaps = spacingRaw smartBorder
                  screenBorder
                  screenBorderEnabled
                  windowBorder
                  windowBorderEnabled
 where
  smartBorder         = True
  screenBorder        = Border 10 10 10 10
  screenBorderEnabled = True
  windowBorder        = Border 20 20 20 20
  windowBorderEnabled = True

data MyAmbiguity = HideBorderWhenSingleWindow deriving (Read, Show)

instance SetsAmbiguous MyAmbiguity where
  hiddens _ _ _ _ wrs = case length wrs of
    1 -> map fst wrs
    _ -> []

splitGridAutoRotate =
-- gaps $
                      lessBorders HideBorderWhenSingleWindow
  $ ifWider threshold (splitGrid L) (splitGrid B)
  where
    -- differentiates between landscape & portrait 4k monitors
        threshold = 3000

myTabConfig = def { activeTextColor     = "#ebdbb2"
                  , inactiveTextColor   = "#ebdbb2"
                  , urgentTextColor     = "#ebdbb2"
  -- , activeBorderColor   = "#454948"
  -- , inactiveBorderColor = "#454948"
  -- , urgentBorderColor   = "#268BD2"
                  , activeBorderWidth   = 0
                  , inactiveBorderWidth = 0
                  , urgentBorderWidth   = 0
                  , activeColor         = "#303030"
                  , inactiveColor       = "#282828"
                  , urgentColor         = "#282828"
                  , decoHeight          = 26
                  , fontName            = "xft:Source Code Pro Semibold:size=9"
                  }

myLayoutHook =
  avoidStruts $ named "grid" splitGridAutoRotate ||| named "tabs" tabs
  where tabs = noBorders $ tabbedBottom shrinkText myTabConfig
  -- where tabs = tabbedBottom shrinkText myTabConfig

workspaceLogFile = "/tmp/.xmonad-workspace-log"

myStartupHook =
  safeSpawn "rm" ["-f", workspaceLogFile]
    <+> spawn "~/.bin/start-polybar"
    <+> setDefaultCursor xC_left_ptr

myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]


ppWorkspaces ws = io $ appendFile workspaceLogFile (ws ++ "\n")

-- myLogHook = dynamicLogWithPP xmobarPP
  --   {
--    ppOutput
--   }
--   where
--     ppOutput wsName = io $ appendFile workspaceLogFile (wsName ++ "\n")

myManageHook = manageDocks

dimText = "#a89984"

barActiveBg = "#bf616a"
barActiveFg = "#282828"

barVisibleFg = "#8ec07c"

myLogPP = dynamicLogWithPP $ def { ppTitle   = const ""
                                 , ppOutput  = ppWorkspaces
                                 , ppCurrent = current
                                 , ppVisible = visible
                                 , ppSep     = sep
                                 , ppLayout  = layout
                                 }
 where
    -- current = wrap ("%{B" ++ barActiveBg ++ "}%{F" ++ barActiveFg ++ "}[") "]%{B-}%{F-}"
  current = wrap ("%{F" ++ barActiveBg ++ "}[") "]%{F-}"
  visible = wrap ("%{F" ++ barVisibleFg ++ "}<") ">%{F-}"
  sep     = "%{F" ++ dimText ++ "} : " ++ "%{F-}"
  layout  = wrap ("%{F" ++ dimText ++ "}(") ")%{F-}"

runZsh cmd = safeSpawn "zsh" ["-i", "-c", cmd]

toggleScreen = windows
  (\ws -> case W.visible ws of
    [x] -> let (W.Screen (W.Workspace tag _ _) _ _) = x in W.view tag ws
    _   -> ws
  )

additionalBindings :: [(String, X ())]
additionalBindings =
  [ ("M-S-p c"      , spawn "google-chrome")
  , ("M-S-p m"      , safeSpawn "zsh" ["-i", "-c", "emacs"])
  , ("M-S-p e"      , safeSpawn "zsh" ["-i", "-c", "code"])
  , ("M-S-o e"      , runZsh (home ".bin/open-in-editor.sh"))
  , ("M-o"          , safeSpawn "rofi" ["-show", "run"])
  , ("M-p"          , safeSpawn "rofi" ["-show", "window"])
  , ("M-<Escape>"   , safeSpawn (home ".bin/lock.sh") [])
  , ("M1-C-<Delete>", safeSpawn (home ".bin/power.sh") [])
  , ("M-`"          , windows W.focusDown)
  , ("M-S-`"        , windows W.focusUp)
  , ("M-<Tab>"      , toggleScreen)
  , ( "M1-<Tab>"
    , nextMatch History (return True)
    ) -- "alt tab"
  , ( "<XF86AudioLowerVolume>"
    , safeSpawn "pactl" ["set-sink-volume", "@DEFAULT_SINK@", "-1%"]
    )
  , ( "<XF86AudioRaiseVolume>"
    , safeSpawn "pactl" ["set-sink-volume", "@DEFAULT_SINK@", "+1%"]
    )
  , ( "<XF86AudioMute>"
    , safeSpawn "pactl" ["set-sink-mute", "@DEFAULT_SINK@", "toggle"]
    )
  ]

myBorderWidth = 10
-- myNormalBorderColor = "#2b303b"
-- myNormalBorderColor = "#282828"
myNormalBorderColor = "#222222"
-- myNormalBorderColor = "#3c3836"
myFocusedBorderColor = "#bf616a" -- nice red


-- myFocusedBorderColor = "#ffb86c"
-- myNormalBorderColor = "#3c3836"

main :: IO ()
main =
  (xmonad . ewmh . docks . fullscreenSupport)
    $                 def { terminal           = myTerminalCommand
                          , modMask            = mod4Mask
                          , borderWidth        = myBorderWidth
                          , focusedBorderColor = myFocusedBorderColor
                          , normalBorderColor  = myNormalBorderColor
                          , focusFollowsMouse  = False
                          , layoutHook         = myLayoutHook
                          , startupHook        = myStartupHook
                          , manageHook         = myManageHook
                          , workspaces         = myWorkspaces
                          , logHook            = myLogPP >> historyHook
                          }
    `additionalKeysP` additionalBindings
