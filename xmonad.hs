-- my xmonad.hs
-- adapted from many sources, thanks to everyone!
--

-- IMPORTS {{{

import XMonad
import List
import Data.Monoid
import System.Exit
import System.IO

import Graphics.X11.ExtraTypes.XF86

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import XMonad.Util.EZConfig
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.Scratchpad

import XMonad.Actions.CycleWS
import XMonad.Actions.Promote
import XMonad.Actions.TopicSpace

import qualified XMonad.Prompt 		as P
import XMonad.Prompt
import XMonad.Prompt.AppendFile (appendFilePrompt)
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Shell
import XMonad.Prompt.Workspace

import XMonad.Layout
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed

import Dzen

import XMonad.Hooks.DynamicLog hiding (dzen)
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
-- import XMonad.Hooks.UrgencyHook


-- import XMonad.Hooks.DynamicLog
-- }}}

-- defines and variables {{{

myTerminal = "urxvt"
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False
myBorderWidth = 1
myModMask = mod4Mask

myWorkspaces = ["skrive", "web", "cli", "wiki", "fm", "6", "7", "8", "9"]

-- myWorkspaces    = ["一 巣","二 くも","三 著す","四 参照","五","六 曲","七 絵","八 映画館","九 仮想"]
-- Japanese meanings {{{
-- ws 1: su
-- 1: nest; rookery; breeding place; hive;
-- 2: den;
-- 3: haunt;
-- 4: (spider's) web
--
-- ws 2: kumo
-- = spider
--
-- ws 3: arawasu
-- = to write; to publish
--
-- ws 4: sanshou
-- = reference; bibliographical reference; consultation; browsing (e.g. when selecting a file to upload on a computer); checking out 
--
-- ws 5:
--
-- ws 6: kyoku
-- = tune; piece of music
--
-- ws 7: e (!)
-- picture; drawing; painting; sketch
--
-- ws 8: eigakan
-- cinema; movie theatre
--
-- ws 9: kasou
-- 	imagination; supposition; virtual; potential (enemy)
-- }}}

-- workspace variables
skriveWs  = (myWorkspaces !! 0)
webWs     = (myWorkspaces !! 1)
cliWs     = (myWorkspaces !! 2)
wikiWs    = (myWorkspaces !! 3)
fmWs      = (myWorkspaces !! 4)
--musicWs   = (myWorkspaces !! 5)
--gimpWs    = (myWorkspaces !! 6)
--mediaWs   = (myWorkspaces !! 7)
niWs = (myWorkspaces !! 8)

myNormalBorderColor = "#000000"
-- myFocusedBorderColor = "#306EFF"
myFocusedBorderColor = "#222222"

myJapFontName = "IPAGothic"
myJapFontSize = "10"
myJapaneseFont = myJapFontName ++ "-" ++ myJapFontSize

-- }}}

-- XPConfig {{{
myXPConfig = defaultXPConfig                                    
    { 
	XMonad.Prompt.font  = "-*-terminus-*-*-*-*-12-*-*-*-*-*-*-u" 
	--,fgColor = "#0096d1"
	, fgColor = "#D37E2C"
	, bgColor = "#000000"
	, bgHLight    = "#000000"
	, fgHLight    = "#FF0000"
	, position = Bottom
    , historySize = 512
    , showCompletionOnTab = True
    , historyFilter = deleteConsecutive
    }
-- }}}

-- TopicSpace {{{ 
myTopics :: [Topic]
myTopics =
  [ "skrive",
  "web",
  "cli",
  "wiki",
  "fm",
  "musikk",
  "7",
  "8",
  "9"
  ]

myTopicConfig :: TopicConfig
myTopicConfig = TopicConfig
  { topicDirs = M.fromList $
      [ ("dashboard", "Desktop"),
      ("skrive", "documents/skriving"),
      ("web", "/home/rolf"),
      ("cli", "/home/rolf"),
      ("wiki", "/home/rolf"),
      ("fm", "/home/rolf"),
      ("musikk", "music")
      ]
  , defaultTopicAction = const $ spawnShell >*> 3
  , defaultTopic = "dashboard"
  , maxTopicHistory = 10
  , topicActions = M.fromList $
      [ ("skrive", spawn "focuswriter")
      , ("web",  spawn "syncfox")
      , ("cli", spawn "urxvt")
      , ("wiki", spawn "urxvt -e vim -c VimwikiIndex")
      , ("fm",  spawn "urxvt -e ranger")
      , ("musikk", spawn "urxvt -e ncmpcpp")
      ]
  }

-- }}}

-- Key bindings {{{
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [
    ((modm,               xK_Return), spawn $ XMonad.terminal conf),
    ((modm .|. shiftMask, xK_Return), spawn "urxvt -e screen"),
    ((modm,               xK_f),      spawn "syncfox"),
    ((modm,               xK_w),      spawn "focuswriter"),
    ((modm,               xK_b),      spawn "abiword"),
    ((modm,               xK_u),      spawn "gnumeric"),
--    ((modm,               xK_F3),     spawn "/home/rolf/bin/tptoggle"),
--    ((modm,               xK_x),      spawn "xbmc"),

    ((modm,               xK_p),      shellPrompt myXPConfig),
    ((modm,               xK_space),  sendMessage NextLayout),
    ((modm,               xK_Tab),    windows W.focusDown),
    ((modm,               xK_j),      windows W.focusDown),
    ((modm,               xK_k),      windows W.focusUp),
    ((modm .|. shiftMask, xK_j),      windows W.swapDown),
    ((modm .|. shiftMask, xK_k),      windows W.swapUp),
    ((modm,               xK_h),      sendMessage Shrink),
    ((modm,               xK_l),      sendMessage Expand),
    ((modm,               xK_m),      windows W.focusMaster),
    ((modm .|. shiftMask, xK_m),      windows W.swapMaster),
    ((modm .|. shiftMask, xK_c),      kill),
    ((modm,               xK_n),      refresh),
    ((modm,               xK_Escape), toggleWS),
    ((modm,               xK_bar),    scratchpadSpawnActionTerminal myTerminal),

    -- TopicSpace stuff
    ((modm, xK_n), spawnShell),
    ((modm, xK_a), currentTopicAction myTopicConfig),
    ((modm, xK_g), promptedGoto),
    ((modm .|. shiftMask, xK_g), promptedShift),
    ((modm, xK_F1), goto "skrive"),
    ((modm, xK_F2), goto "web"),
    ((modm, xK_F3), goto "cli"),
    ((modm, xK_F4), goto "wiki"),
    ((modm, xK_F5), goto "fm"),
    ((modm, xK_F6), goto "musikk"),
    ((modm, xK_F7), goto "7"),
    ((modm, xK_F8), goto "8"),
    ((modm, xK_F9), goto "9"),

    -- keybindings for controlling MPD
    ((modm,               xK_Home),      spawn "mpc toggle"),
    ((modm,               xK_Page_Down), spawn "mpc next"),
    ((modm,               xK_Page_Up),   spawn "mpc prev"),
    ((modm,               xK_Insert),    spawn "mpc volume +2"),
    ((modm,               xK_Delete),    spawn "mpc volume -2"),

    ((modm,               xK_q),      spawn "killall conky dzen2; xmonad --recompile; xmonad --restart"),
    ((modm .|. shiftMask, xK_q),      io (exitWith ExitSuccess))
    ]
        ++
        [((m .|. modm, k), windows $ f i)
            | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
            , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

-- }}}

-- Mouse bindings {{{
--
myMouseBindings conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [
    ((modm, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)),
    ((modm, button2), (\w -> focus w >> windows W.shiftMaster)),
    ((modm, button3), (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)),
    ((modm, button4), (\w -> nextWS)),
    ((modm, button5), (\w -> prevWS))
    ]

-- }}}

-- Layout {{{
tiled x = Tall nmaster delta ratio
    where
        nmaster = x
--        ratio = 1/2
--        Golden ratio:
        ratio = toRational ( 2 / (1 + sqrt 5 :: Double))
        delta = 3/100

fullLayout = noBorders $ Full ||| Grid
-- gimpLayout = withIM (0.14) (Role "gimp-toolbox") $ reflectHoriz $ withIM (0.22) (Role "gimp-dock") Full
defaultLayout = Grid ||| (tiled 1) ||| Mirror (tiled 1) ||| fullLayout

myLayout = avoidStruts $ onWorkspace webWs fullLayout $ onWorkspace skriveWs fullLayout $ onWorkspace cliWs fullLayout $ onWorkspace wikiWs fullLayout $ defaultLayout

-- }}}

-- floatClickFocusHandler {{{
floatClickFocusHandler :: Event -> X All
floatClickFocusHandler ButtonEvent { ev_window = w } = do
        withWindowSet $ \s -> do
                if isFloat w s
                        then (focus w >> promote)
                        else return ()
                return (All True)
                where isFloat w ss = M.member w $ W.floating ss
floatClickFocusHandler _ = return (All True)
-- }}}

-- ManageHook (rules for programs) {{{
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat,
      className =? "Smplayer"       --> doFloat,
      className =? "Vlc"            --> doFloat,
      className =? "Firefox"        --> doShift webWs,
      className =? "Pcmanfm"        --> doShift fmWs,
      className =? "Abiword"        --> doShift skriveWs <+> doFullFloat,
      className =? "Focuswriter"    --> doShift skriveWs <+> doFullFloat,
      className =? "feh"            --> doFullFloat,
      className =? "Gnumeric"       --> doShift niWs,
      resource  =? "desktop_window" --> doIgnore
--      className =? "Chromium"       --> doShift webWs,
--      className =? "xbmc.bin"       --> doShift mediaWs,
--      className =? "Gimp"           --> doShift gimpWs,
--      className =? "Ardour"         --> doShift musicWs,
--      className =? "Gvim"           --> doShift skriveWs,
--      className =? "VirtualBox"     --> doShift virtualWs <+> doFullFloat,
    ] <+> manageDocks <+> manageScratchPad
-- }}}

-- {{{ Scratchpad

manageScratchPad :: ManageHook
manageScratchPad = scratchpadManageHook (W.RationalRect l t w h)
    where
        h = 0.2    -- terminal height, 10%
        w = 1      -- terminal width, 100%
        t = 1 - h  -- distance from top edge, 90%
        l = 1 - w  -- distance from left edge, 0% 
-- }}}

-- Other hooks {{{
myEventHook = floatClickFocusHandler

-- myLogHook = return ()

--myUrgencyHook = withUrgencyHook dzenUrgencyHook
--    { args = ["-bg", "'#000000'" , "-fg", "'#FF0000'"] } 

myStartupHook = return ()
-- }}}

-- Loghook (PP) {{{
-- 
-- 
myLogHook h = dynamicLogWithPP $ defaultPP -- the h here...
    -- display current workspace as darkgrey on light grey (opposite of default colors)
--    { ppCurrent         = wrapFont myJapaneseFont . dzenColor "#306EFF" "#202020" . pad 
    { ppCurrent         = dzenColor "#306EFF" "#202020" . pad 
--    { ppCurrent         = dzenColor "#D37E2C" "#202020" . pad 

    -- display other workspaces which contain windows as a brighter grey
    , ppHidden          = dzenColor "#909090" "" . pad . noScratchPad
--    , ppHidden          = wrapFont myJapaneseFont . dzenColor "#909090" "" . pad 

    -- display other workspaces with no windows as a normal grey
--    , ppHiddenNoWindows = wrapFont myJapaneseFont . dzenColor "#606060" "" . pad 
    , ppHiddenNoWindows = dzenColor "#606060" "" . pad 

    -- display the current layout as a brighter grey
    , ppLayout          = dzenColor "#909090" "" . pad 

    -- if a window on a hidden workspace needs my attention, color it so
    --, ppUrgent          = wrapFont myJapaneseFont . dzenColor "#ff0000" "" . pad . dzenStrip
    , ppUrgent          = dzenColor "#ff0000" "" . pad . dzenStrip

    -- shorten if it goes over 100 characters
    , ppTitle           = shorten 100  

    -- no separator between workspaces
    , ppWsSep           = ""

    -- put a few spaces between each object
    , ppSep             = "  "

    , ppOutput          = hPutStrLn h -- ... must match the h here
    }
    where
        wrapFont font = wrap ("^fn(" ++ font ++ ")") "^fn()"
        noScratchPad ws = if ws == "NSP" then "" else ws
-- }}}

-- Statusbars {{{
-- 
myTopBar :: DzenConf
myTopBar = defaultDzen
    -- use the default as a base and override width and
    -- colors
    { width       = Just 1024
    , fg_color    = Just "#909090"
    , bg_color    = Just "#202020"
--    , Dzen.font   = "-*-terminus-*-*-*-*-12-*-*-*-*-*-*-u" 
    }

--myBottomLeftBar :: DzenConf
--myBottomLeftBar = myTopBar
-- use Top Bar as base!
--    { y_position = Just 1024,
--      width      = Just 640,
--      alignment  = Just LeftAlign
--    }

myBottomRightBar :: DzenConf
myBottomRightBar = myTopBar
-- use Top Bar as base!
    { y_position = Just 588,
      x_position = Just 0,
      width      = Just 1024,
      alignment  = Just RightAlign
    }
-- }}}

-- {{{ Main
-- main = xmonad =<< dzen defaults


spawnShell :: X ()
spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn

spawnShellIn :: Dir -> X ()
spawnShellIn dir = spawn $ "urxvt '(cd ''" ++ dir ++ "'' && " ++ myTerminal ++ " )'"

goto :: Topic -> X ()
goto = switchTopic myTopicConfig

promptedGoto :: X ()
promptedGoto = workspacePrompt myXPConfig goto

promptedShift :: X ()
promptedShift = workspacePrompt myXPConfig $ windows . W.shift

main = do
    checkTopicConfig myTopics myTopicConfig
    d <- spawnDzen myTopBar
    --spawnToDzen "conky -c ~/.config/dzen2conkyrcleft" myBottomLeftBar
    spawnToDzen "conky -c ~/.config/dzen2conkyrcright" myBottomRightBar
    --xmonad $ myUrgencyHook $ ewmh defaultConfig {
    xmonad $ ewmh defaultConfig {
        terminal = myTerminal,
        focusFollowsMouse = myFocusFollowsMouse,
        borderWidth = myBorderWidth,
        modMask = myModMask,
        workspaces = myTopics,
        normalBorderColor = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
        keys = myKeys,
        mouseBindings = myMouseBindings,
        layoutHook = myLayout,
        manageHook = myManageHook,
        handleEventHook = myEventHook,
        logHook = myLogHook d,
        startupHook = myStartupHook
    }


--- old main {{{
-- main = do
--    conf <- dzen defaultConfig
--    xmonad $ conf 
--        { manageHook = manageDocks <+> manageHook defaultConfig
--        , layoutHook = avoidStruts  $  layoutHook defaultConfig
--        , logHook = dynamicLogWithPP $ xmobarPP
--                        { ppOutput = hPutStrLn xmproc
--                        , ppTitle = xmobarColor "green" "" . shorten 50
--                        }
--        } 
-- }}}
-- }}}

-- vim: fdm=marker ts=4 sw=4 sts=4 et:
