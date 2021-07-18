-- Base
import XMonad
import System.Directory
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

    -- Actions
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CycleWS (toggleWS', Direction1D(..), moveTo, shiftTo, WSType(..), nextScreen, prevScreen)
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (sinkAll, killAll)
import qualified XMonad.Actions.Search as S
import XMonad.Actions.DynamicWorkspaceGroups
import XMonad.Actions.DynamicWorkspaces

    -- Data
import Data.Char (isSpace, toUpper)
import Data.Maybe ( fromJust, isJust )
import Data.Monoid
import Data.Tree
import qualified Data.Map as M

    -- Hooks
-- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..), filterOutWsPP)
import XMonad.Hooks.EwmhDesktops  -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, doCenterFloat)
import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WorkspaceHistory

    -- Layouts
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spiral
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns

    -- Layouts modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.Magnifier
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

   -- Utilities
import XMonad.Util.Dmenu
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Actions.SpawnOn

   -- Scratchpads
import Data.Semigroup
import XMonad.Hooks.DynamicProperty

import XMonad.Layout.IndependentScreens
import qualified XMonad.Util.ExtensibleState as XS

import Control.Monad

import XMonad.Prompt
import XMonad.Prompt.Window

import XMonad.Actions.FloatKeys
import Data.List.Split ( splitOn )
import Data.Foldable (traverse_)

-- import Data.List ()
-- import Control.Lens ()
myFont :: String
myFont = "xft:SauceCodePro Nerd Font Mono:regular:size=9:antialias=true:hinting=true"

myEmojiFont :: String
myEmojiFont = "xft:JoyPixels:regular:size=9:antialias=true:hinting=true"

myModMask :: KeyMask
myModMask = mod4Mask        -- Sets modkey to super/windows key

myTerminal :: String
myTerminal = "alacritty"    -- Sets default terminal

myBorderWidth :: Dimension
myBorderWidth = 2           -- Sets border width for windows

myNormColor :: String
myNormColor   = "#282c34"   -- Border color of normal windows

myFocusColor :: String
myFocusColor  = "#46d9ff"   -- Border color of focused windows

altMask :: KeyMask
altMask = mod1Mask          -- Setting this for use in xprompts

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

newtype HasBooted = HasBooted Bool deriving (Typeable, Read, Show)
instance ExtensionClass HasBooted where
        initialValue = HasBooted False
        extensionType = PersistentExtension

getHasBooted :: HasBooted -> Bool
getHasBooted (HasBooted hb) = hb

myStartupHook :: X ()
myStartupHook = do
          setWMName "LG3D"
          (XS.get :: X HasBooted) >>= myOnceStartupHook . getHasBooted

myOnceStartupHook :: Bool -> X ()
myOnceStartupHook hasRun =
          if hasRun then
            return ()
          else do
            spawnOnce "gromit-mpx --key XF86Mail --undo-key none"
            addRawWSGroup "1:www" [(S 0, "www1"), (S 2, "www2"), (S 1, "www3")]
            addRawWSGroup "2:dev" [(S 0, "dev1"), (S 2, "dev2"), (S 1, "dev3")]
            addRawWSGroup "3:game" [(S 0, "game1"), (S 2, "game2"), (S 1, "game3")]
            viewWSGroup "1:www"
            feedPopulateWSGroupMap
            XS.put(HasBooted True)

xpconfig = def
  { font        = "xft:Source Code Pro:pixelsize=18"
  , borderColor = "#1e2320"
  , fgColor     = "#dddddd"
  , fgHLight    = "#ffffff"
  , bgColor     = "#1e2320"
  , bgHLight    = "#5f5f5f"
  , height      = 36
  , position    = CenteredAt 0.45 0.2
}


myColorizer :: Window -> Bool -> X (String, String)
myColorizer = colorRangeFromClassName
                  (0x28,0x2c,0x34) -- lowest inactive bg
                  (0x28,0x2c,0x34) -- highest inactive bg
                  (0xc7,0x92,0xea) -- active bg
                  (0xc0,0xa7,0x9a) -- inactive fg
                  (0x28,0x2c,0x34) -- active fg

mygridConfig :: p -> GSConfig Window
mygridConfig colorizer = (buildDefaultGSConfig myColorizer)
    { gs_cellheight   = 40
    , gs_cellwidth    = 200
    , gs_cellpadding  = 6
    , gs_originFractX = 0.5
    , gs_originFractY = 0.5
    , gs_font         = myFont
    }

spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = gridselect conf lst >>= flip whenJust spawn
    where conf = def
                   { gs_cellheight   = 40
                   , gs_cellwidth    = 200
                   , gs_cellpadding  = 6
                   , gs_originFractX = 0.5
                   , gs_originFractY = 0.5
                   , gs_font         = myFont
                   }

myAppGrid = [ ("Files", "nautilus")
                 , ("Text Editor", "gedit")
                 , ("OneNote", "p3x-onenote")
                 , ("IntelliJ", "idea")
                 , ("htop", "alacritty -e htop")
                 , ("MyPaint", "mypaint")
                 , ("Krita", "krita")
                 , ("OBS", "obs")
                 ]

myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "discord" "discord"                                      (className =? "discord") discordHook
                , NS "spotify" "spotify"                                      (className =? "Spotify") spotifyHook
                , NS "keepassxc" "keepassxc"                                  (className =? "keepassxc") keepassxcHook
                , NS "pia" "/opt/piavpn/bin/pia-client"                       (className =? "privateinternetaccess") piaHook
                , NS "qbittorrent" "qbittorrent"                              (className =? "qBittorrent") qbittorrentHook
                , NS "droidcam" "droidcam"                                    (className =? "Droidcam") droidcamHook
                , NS "qalculate" "qalculate"                                  (className =? "Qalculate") qalculateHook
                , NS "cellwriter" "cellwriter"                                (className =? "Cellwriter") cellwriterHook
                , NS "mc18" "/bin/bash $HOME/.local/share/multimc/MultiMC -l '1.8'"        (className =? "Minecraft 1.8.8") mc18Hook
                , NS "mc111" "/bin/bash $HOME/.local/share/multimc/MultiMC -l '1.11'"      (className =? "Minecraft 1.11.2") mc111Hook
                , NS "mc116" "/bin/bash $HOME/.local/share/multimc/MultiMC -l '1.16.5'"    (className =? "Minecraft* 1.16.5") mc116Hook
                , NS "mc117" "/bin/bash $HOME/.local/share/multimc/MultiMC -l '1.17'"      (className =? "Minecraft 1.17") mc117Hook
                , NS "mccc" "cosmicclient"                                    (className =? "Cosmic Client 2.2.11.bbb7a (MC 1.8)") mcccHook
                , NS "terminal_1" "alacritty --class terminal_1,terminal_1"   (className =? "terminal_1") terminal_1_hook
                , NS "terminal_2" "alacritty --class terminal_2,terminal_2"   (className =? "terminal_2") terminal_2_hook
                , NS "terminal_3" "alacritty --class terminal_3,terminal_3"   (className =? "terminal_3") terminal_3_hook
                , NS "terminal_4" "alacritty --class terminal_4,terminal_4"   (className =? "terminal_4") terminal_4_hook
                , NS "terminal_5" "alacritty --class terminal_5,terminal_5"   (className =? "terminal_5") terminal_5_hook
                , NS "obs" "obs"                                              (className =? "obs") obsHook
                , NS "msteams" "teams"                                        (className =? "Microsoft Teams - Preview") teamsHook
                ]
                  where discordHook = customFloating $ rr (1/10) (1/10) (8/10) (8/10)
                        spotifyHook = customFloating $ rr (1/10) (1/10) (8/10) (8/10)
                        keepassxcHook = customFloating $ rr (1/10) (1/10) (8/10) (8/10)
                        piaHook = doCenterFloat
                        qbittorrentHook = customFloating $ rr (1/10) (1/10) (8/10) (8/10)
                        droidcamHook = customFloating $ rr (4/10) (4/10) (2/10) (2/10)
                        qalculateHook = doCenterFloat
                        cellwriterHook = doCenterFloat
                        mc18Hook = doCenterFloat
                        mc111Hook = doCenterFloat
                        mc116Hook = doCenterFloat
                        mc117Hook = doCenterFloat
                        mcccHook = doCenterFloat
                        obsHook = customFloating $ rr (1/10) (1/10) (8/10) (8/10)
                        teamsHook = customFloating $ rr (1/10) (1/10) (8/10) (8/10)
                        terminal_1_hook = customFloating $ rr (1/10) (1/10) (8/10) (8/10)
                        terminal_2_hook = customFloating $ rr (1/10) (1/10) (8/10) (8/10)
                        terminal_3_hook = customFloating $ rr (1/10) (1/10) (8/10) (8/10)
                        terminal_4_hook = customFloating $ rr (1/10) (1/10) (8/10) (8/10)
                        terminal_5_hook = customFloating $ rr (1/10) (1/10) (8/10) (8/10)
                        rr = W.RationalRect

spotifyHandleEventHook :: Event -> X All
spotifyHandleEventHook = dynamicPropertyChange "WM_NAME" (title =? "Spotify" --> floating) where floating = customFloating $ W.RationalRect (1/10) (1/10) (8/10) (8/10)

--Makes setting the spacingRaw simpler to write. The spacingRaw module adds a configurable amount of space around windows.
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- Below is a variation of the above except no borders are applied
-- if fewer than two windows. So a single window has no gaps.
mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

tall     = renamed [Replace "tall"]
           $ windowNavigation
           $ smartBorders
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 12
           $ mySpacing 4
           $ ResizableTall 1 (3/100) (1/2) []
grid     = renamed [Replace "grid"]
           $ windowNavigation
           $ smartBorders
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 12
           $ mySpacing 0
           $ mkToggle (single MIRROR)
           $ Grid (16/10)
threeRow = renamed [Replace "threeRow"]
           $ windowNavigation
           $ smartBorders
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 7
           $ Mirror
           $ ThreeCol 1 (3/100) (1/2)

-- setting colors for tabs layout and tabs sublayout.
myTabTheme = def { fontName            = myFont
                 , activeColor         = "#46d9ff"
                 , inactiveColor       = "#313846"
                 , activeBorderColor   = "#46d9ff"
                 , inactiveBorderColor = "#282c34"
                 , activeTextColor     = "#282c34"
                 , inactiveTextColor   = "#d0d0d0"
                 }

-- Theme for showWName which prints current workspace when you change workspaces.
myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
    { swn_font              = "xft:Ubuntu:bold:size=60"
    , swn_fade              = 1.0
    , swn_bgcolor           = "#1c1f24"
    , swn_color             = "#ffffff"
    }

-- The layout hook
myLayoutHook = avoidStruts $ mouseResize $ windowArrange
               $ mkToggle (NBFULL XMonad.Layout.MultiToggle.?? NOBORDERS XMonad.Layout.MultiToggle.?? EOT) myDefaultLayout
             where
               myDefaultLayout =     tall
                                 ||| grid
                                 ||| threeRow

myWorkspaces = ["www1","www2","www3","dev1","dev2","dev3","game1","game2","game3"]
myWorkspaceIndices = M.fromList $ zip myWorkspaces [1..] -- (,) == \x y -> (x,y)

clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
    where i = fromJust $ M.lookup ws myWorkspaceIndices

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll [] <+> namedScratchpadManageHook myScratchPads <+> manageSpawn

newtype IsHidden = IsHidden Bool deriving (Typeable, Read, Show)
instance ExtensionClass IsHidden where
        initialValue = IsHidden False
        extensionType = PersistentExtension

getIsHidden :: IsHidden -> Bool
getIsHidden (IsHidden is) = is

toggleHide :: X ()
toggleHide =
    (XS.get :: X IsHidden) >>= toggleHideFunction . getIsHidden

toggleHideFunction :: Bool -> X ()
toggleHideFunction isHidden = do
    if isHidden
    then do
        spawn "/home/oscar/.xmonad/kill_hide.sh"
        forgetWSGroup "hide"
        removeWorkspaceByTag "hide1"
        removeWorkspaceByTag "hide2"
        removeWorkspaceByTag "hide3"
        viewWSGroup "1:www"
        XS.put (IsHidden False)
    else do
        spawn "playerctl -a pause"
        addWorkspace "hide1"
        addWorkspace "hide2"
        addWorkspace "hide3"
        spawnOn "hide1" "alacritty --class 'hide_aquarium','hide_aquarium' -e asciiquarium"
        spawnOn "hide3" "alacritty --class 'hide_cbonsai','hide_cbonsai' -e /home/oscar/.xmonad/cbonsai.sh"
        spawnOn "hide2" "alacritty --class 'hide_text','hide_text' -e /home/oscar/.xmonad/away.sh"
        addRawWSGroup "hide" [(S 0, "hide1"), (S 2, "hide2"), (S 1, "hide3")]
        viewWSGroup "hide"
        XS.put (IsHidden True)
    return ()

newtype IsCinema = IsCinema Bool deriving (Typeable, Read, Show)
instance ExtensionClass IsCinema where
        initialValue = IsCinema False
        extensionType = PersistentExtension


getIsCinema :: IsCinema -> Bool
getIsCinema (IsCinema ic) = ic

toggleCinema :: X ()
toggleCinema =
    (XS.get :: X IsCinema) >>= toggleCinemaFunction . getIsCinema

toggleCinemaFunction :: Bool -> X ()
toggleCinemaFunction isCinema = do
    if isCinema
    then do
        spawn "nitrogen --force-setter=xinerama --head=0 --set-auto /usr/share/backgrounds/nyewallpaper.png"
        spawn "nitrogen --force-setter=xinerama --head=1 --set-auto /usr/share/backgrounds/nyewallpaper.png"
        spawn "nitrogen --force-setter=xinerama --head=2 --set-auto /usr/share/backgrounds/nyewallpaper.png"
        spawn "xmobar -x 0 $HOME/.config/xmobar/xmobarrc0"
        spawn "xmobar -x 1 $HOME/.config/xmobar/xmobarrc1"
        spawn "xmobar -x 2 $HOME/.config/xmobar/xmobarrc2"
        forgetWSGroup "cinema"
        removeWorkspaceByTag "cinema1"
        removeWorkspaceByTag "cinema2"
        removeWorkspaceByTag "cinema3"
        viewWSGroup "1:www"
        XS.put (IsCinema False)
    else do
        spawn "nitrogen --force-setter=xinerama --head=0 --set-auto /usr/share/backgrounds/black.jpg"
        spawn "nitrogen --force-setter=xinerama --head=1 --set-auto /usr/share/backgrounds/black.jpg"
        spawn "nitrogen --force-setter=xinerama --head=2 --set-auto /usr/share/backgrounds/black.jpg"
        spawn "killall xmobar"
        addWorkspace "cinema1"
        addWorkspace "cinema2"
        addWorkspace "cinema3"
        addRawWSGroup "cinema" [(S 0, "cinema1"), (S 2, "cinema2"), (S 1, "cinema3")]
        viewWSGroup "cinema"
        XS.put (IsCinema True)
    return ()

newtype DWSGPrompt = DWSGPrompt String

instance XPrompt DWSGPrompt where
  showXPrompt (DWSGPrompt s) = s

addDWSGroup :: XPConfig -> String -> X ()
addDWSGroup xp s =
  mkXPrompt (DWSGPrompt s) xp (const $ return []) fixDWSGroup

fixDWSGroup :: String -> X ()
fixDWSGroup s =
  (XS.get :: X WSGroupMap) >>= createDWSGroup s . getWSGroupMap

createDWSGroup :: String -> M.Map String WorkspaceId -> X ()
createDWSGroup s map = do
  let i = nextWSId (M.keys map)
  let fullName = i ++ ":" ++ s
  let wname1 = s ++ "1"
  let wname2 = s ++ "2"
  let wname3 = s ++ "3"
  addWorkspace wname1
  addWorkspace wname2
  addWorkspace wname3
  addRawWSGroup fullName [(S 0, wname1), (S 2, wname2), (S 1, wname3)]
  viewWSGroup fullName
  let m1 = M.insert fullName wname2 map
  XS.put (WSGroupMap m1)
  return ()

nextWSId :: [String] -> String
nextWSId strings = do
  let splitArr = map (splitOn ":") strings
  let split = map head splitArr
  let splitInts = map read split
  show (minout splitInts)

minout :: [Int] -> Int
minout = minoutaux 1
    where
    minoutaux :: Int -> [Int] -> Int -- \list base -> smallest integer >= base not occuring in list
    minoutaux base [] = base
    minoutaux base [x] = base + (if x==base then 2 else 1)
    minoutaux base xs = if length smallpart == n2 then  minoutaux (base+n2) bigpart else minoutaux base smallpart
        where
        n = length xs
        n2 = n `div` 2
        smallpart = [x | x <- xs , base <= x , x < base + n2]
        bigpart = [x | x <- xs, base + n2 <= x, x < base + n]

newtype WSGroupMap = WSGroupMap (M.Map String WorkspaceId) deriving (Typeable, Read, Show)
instance ExtensionClass WSGroupMap where
        initialValue = WSGroupMap M.empty
        extensionType = PersistentExtension

getWSGroupMap :: WSGroupMap -> M.Map String WorkspaceId
getWSGroupMap (WSGroupMap wmap) = wmap

feedPopulateWSGroupMap :: X ()
feedPopulateWSGroupMap =
  (XS.get :: X WSGroupMap) >>= populateWSGroupMap . getWSGroupMap

populateWSGroupMap :: M.Map String WorkspaceId -> X ()
populateWSGroupMap map = do
  let m1 = M.insert "1:www" "www2" map
  let m2 = M.insert "2:dev" "dev2" m1
  let m3 = M.insert "3:game" "game2" m2
  XS.put (WSGroupMap m3)
  return ()

newtype SWSGPrompt = SWSGPrompt String

instance XPrompt SWSGPrompt where
  showXPrompt (SWSGPrompt s) = s

feedSendToWSGroup :: XPConfig -> String -> X ()
feedSendToWSGroup xp s =
  (XS.get :: X WSGroupMap) >>= sendToWSGroup xp s . getWSGroupMap

sendToWSGroup :: XPConfig -> String -> M.Map String WorkspaceId -> X ()
sendToWSGroup xp s map = do
  mkXPrompt (SWSGPrompt s) xp (mkComplFunFromList' xp (M.keys map)) feedShiftWindowToWSGroup
  return ()

feedShiftWindowToWSGroup :: String -> X ()
feedShiftWindowToWSGroup s =
  (XS.get :: X WSGroupMap) >>= shiftWindowToWSGroup s . getWSGroupMap

shiftWindowToWSGroup :: String -> M.Map String WorkspaceId -> X ()
shiftWindowToWSGroup s map = do
  let ws = M.lookup s map
  when (isJust ws) $ do
    let wsx = getXWorkspaceId (fromJust ws)
    wsx >>= windows . W.shift
    return ()

feedPromptDeleteWSGroup :: XPConfig -> String -> X ()
feedPromptDeleteWSGroup xp s =
  (XS.get :: X WSGroupMap) >>= promptDeleteWSGroup xp s . getWSGroupMap

promptDeleteWSGroup :: XPConfig -> String -> M.Map String WorkspaceId -> X ()
promptDeleteWSGroup xp s map =
  mkXPrompt (SWSGPrompt s) xp (mkComplFunFromList' xp (M.keys map)) feedDeleteWSGroup

feedDeleteWSGroup :: String -> X ()
feedDeleteWSGroup s = do
  forgetWSGroup s
  (XS.get :: X WSGroupMap) >>= feedDeleteWSGroupMap s . getWSGroupMap

feedDeleteWSGroupMap :: String -> M.Map String WorkspaceId -> X ()
feedDeleteWSGroupMap s map = do
  let m1 = M.delete s map
  XS.put (WSGroupMap m1)
  return ()

getXWorkspaceId :: WorkspaceId -> X WorkspaceId
getXWorkspaceId = return

{-
-- | Execute an 'X' action for each window on the current workspace.
withAllVisible :: (Window -> X ()) -> X ()
withAllVisible f = do
  withWindowSet $ \ws -> let all' = W.integrate' . W.stack . W.workspace . W.current $ ws
    in forM_ all' f
  withWindowSet $ \ws -> let all' = W.integrate' . W.stack . W.workspace . W.visible $ ws
    in forM_ all' f
  -- map (\wvis -> (withWindowSet $ \ws -> let all' = W.integrate' . W.stack . W.workspace . wvis $ ws in forM_ all' f)) (W.visible)
  -- map (forStackSet f) (W.visible (W.StackSet i l a sid sd))
  return ()
-}

{-
-- For every window on the visible workspaces, execute an 'X' action.
withAllVisible :: (Window -> X ()) -> X ()
withAllVisible f = do
  let screens = W.screens
  traverse_ screens $ \scr -> do
    let workspace = W.workspace scr
    withWindowSet $ \ws -> let all' = W.integrate' . W.stack . workspace $ ws
      in forM_ all' f
-- withWindowSet $ \ws -> let all' = W.integrate' . W.stack . W.workspace . stackSetWorkspace $ ws
-- in forM_ all' f
-}
-- forStackSet :: (Window -> X ()) -> W.StackSet (*) (*) (*) (*) (*) -> X ()
-- forStackSet = do
--   return ()
-- forStackSet :: (W.StackSet i l a sid sd -> Screen i l a sid sd) -> (Window -> X ()) -> X ()
-- forStackSet wvis f = do
--   withWindowSet $ \ws -> let all' = W.integrate' . W.stack . W.workspace . W.current $ ws
--     in forM_ all' f
--   return ()

toggleFloat w = windows (\s -> if M.member w (W.floating s)
                            then W.sink w s
                            else W.float w (W.RationalRect (1/10) (1/10) (8/10) (8/10)) s)

myKeys :: [(String, X ())]
myKeys =
    -- Xmonad
        [ ("M-S-r", spawn "killall xmobar;killall gromit-mpx;xmonad --recompile;xmonad --restart")
        , ("M-S-q", io exitSuccess)  -- Quits xmonad

    -- Run Prompt
        , ("M-S-<Return>", spawn "$HOME/.config/rofi/launchers/ribbon/launcher.sh") -- Dmenu

    -- Useful programs to have a keybinding for launch
        , ("M-<Return>", spawn myTerminal)
        , ("M-b", spawn "google-chrome")
        , ("M-S-b", spawn "google-chrome --incognito")
        , ("M-y", spawn "google-chrome --new-window https://youtube.com/")
        , ("M-M1-h", spawn (myTerminal ++ " -e htop"))

    -- Kill windows
        , ("M-c", kill1)     -- Kill the currently focused client
        , ("M-S-a", killAll)   -- Kill all windows on current workspace
        , ("M-t", feedSendToWSGroup xpconfig "")

    -- Workspaces
        , ("M-/", prevScreen)  -- Switch focus to next monitor (somehow backwards idk)
        , ("M-S-f", shiftTo Next nonNSP >> prevScreen)
        , ("M-S-d", shiftTo Prev nonNSP >> nextScreen) -- My XServer & monitor setup is kinda fucked so my monitors go different directions to my workspaces. Looks cucked but it works for me.
        , ("M-S-h", toggleHide)
        , ("M-S-c", toggleCinema)

    -- Floating windows
        , ("M-f", withFocused toggleFloat)
        , ("M-S-t", sinkAll)                       -- Push ALL floating windows to tile

    -- Grid Select (CTR-g followed by a key)
        , ("M-g g", spawnSelected' myAppGrid)                 -- grid select favorite apps
        , ("M-g t", goToSelected $ mygridConfig myColorizer)  -- goto selected window
        , ("M-g b", bringSelected $ mygridConfig myColorizer) -- bring selected window
--        , ("M-S-l", withFocused sendToWorkspace)

    -- Windows navigation
--        , ("M-m", windows W.focusMaster)  -- Move focus to the master window
        , ("M-.", windows W.focusDown)    -- Move focus to the next window
        , ("M-,", windows W.focusUp)      -- Move focus to the prev window
--        , ("M-S-m", windows W.swapMaster) -- Swap the focused window and the master window
        , ("M-S-.", windows W.swapDown)   -- Swap focused window with next window
        , ("M-S-,", windows W.swapUp)     -- Swap focused window with prev window
        , ("M-<Backspace>", promote)      -- Moves focused window to master, others maintain order
        , ("M-S-<Tab>", rotSlavesDown)    -- Rotate all windows except master and keep focus in place
        , ("M-C-<Tab>", rotAllDown)       -- Rotate all the windows in the current stack

    -- Layouts
        , ("M-<Tab>", sendMessage NextLayout)           -- Switch to next layout
        , ("M-C-M1-<Up>", sendMessage Arrange)
        , ("M-C-M1-<Down>", sendMessage DeArrange)
        , ("M-<Space>", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggles noborder/full
        , ("M-S-<Space>", promptWSGroupView xpconfig "")
        , ("M-C-<Space>", addDWSGroup xpconfig "")
        , ("M-M1-<Space>", feedPromptDeleteWSGroup xpconfig "")

    -- Increase/decrease windows in the master pane or the stack
        , ("M-S-<Up>", sendMessage (IncMasterN 1))      -- Increase number of clients in master pane
        , ("M-S-<Down>", sendMessage (IncMasterN (-1))) -- Decrease number of clients in master pane
        , ("M-C-<Up>", increaseLimit)                   -- Increase number of windows
        , ("M-C-<Down>", decreaseLimit)                 -- Decrease number of windows

    -- Window resizing
        , ("M-;", sendMessage Shrink)                   -- Shrink horiz window width
        , ("M-'", sendMessage Expand)                   -- Expand horiz window width
        , ("M-M1-j", sendMessage MirrorShrink)          -- Shrink vert window width
        , ("M-M1-k", sendMessage MirrorExpand)          -- Exoand vert window width

    -- Sublayouts
    -- This is used to push windows to tabbed sublayouts, or pull them out of it.
        , ("M-C-h", sendMessage $ pullGroup L)
        , ("M-C-l", sendMessage $ pullGroup R)
        , ("M-C-k", sendMessage $ pullGroup U)
        , ("M-C-j", sendMessage $ pullGroup D)
        , ("M-C-m", withFocused (sendMessage . MergeAll))
        , ("M-C-u", withFocused (sendMessage . UnMerge))
        , ("M-C-/", withFocused (sendMessage . UnMergeAll))
        , ("M-C-.", onGroup W.focusUp')    -- Switch focus to next tab
        , ("M-C-,", onGroup W.focusDown')  -- Switch focus to prev tab

    -- Scratchpads
        , ("M-d", namedScratchpadAction myScratchPads "discord")
        , ("M-s", namedScratchpadAction myScratchPads "spotify")
        , ("M-k", namedScratchpadAction myScratchPads "keepassxc")
        , ("M-p p", namedScratchpadAction myScratchPads "pia")
        , ("M-q", namedScratchpadAction myScratchPads "qbittorrent")
        , ("M-a", namedScratchpadAction myScratchPads "droidcam")
        , ("<XF86Calculator>", namedScratchpadAction myScratchPads "qalculate")
        , ("<F9>", namedScratchpadAction myScratchPads "cellwriter")
        , ("M-m", namedScratchpadAction myScratchPads "msteams")
        , ("M-S-m 8", namedScratchpadAction myScratchPads "mc18")
        , ("M-S-m 1", namedScratchpadAction myScratchPads "mc111")
        , ("M-S-m 6", namedScratchpadAction myScratchPads "mc116")
        , ("M-S-m 7", namedScratchpadAction myScratchPads "mc117")
        , ("M-S-m c", namedScratchpadAction myScratchPads "mccc")
        , ("M-o", namedScratchpadAction myScratchPads "obs")
        , ("M-z 1", namedScratchpadAction myScratchPads "terminal_1")
        , ("M-z 2", namedScratchpadAction myScratchPads "terminal_2")
        , ("M-z 3", namedScratchpadAction myScratchPads "terminal_3")
        , ("M-z 4", namedScratchpadAction myScratchPads "terminal_4")
        , ("M-z 5", namedScratchpadAction myScratchPads "terminal_5")

    -- Misc
        , ("M-S-s", spawn "flameshot gui")
        , ("M-p <Space>", spawn "piactl-toggle")

    -- Controls for mocp music player (SUPER-u followed by a key)
        , ("M-u p", spawn "playerctl play")
        , ("M-u s", spawn "playerctl -a pause")
        , ("M-u l", spawn "playerctl next")
        , ("M-u h", spawn "playerctl previous")
        , ("M-u <Space>", spawn "playerctl play-pause")

    -- Multimedia Keys
        , ("<XF86AudioMute>",   spawn "~/.xmonad/volume_change.sh toggle")
        , ("<XF86AudioLowerVolume>", spawn "~/.xmonad/volume_change.sh down")
        , ("<XF86AudioRaiseVolume>", spawn "~/.xmonad/volume_change.sh up")

        ]
    -- The following lines are needed for named scratchpads.
          where nonNSP          = WSIs (return (\ws -> W.tag ws /= "NSP"))
                nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "NSP"))

main :: IO ()
main = do
    -- Launching three instances of xmobar on their monitors.
    spawn "xmobar -x 0 $HOME/.config/xmobar/xmobarrc0"
    spawn "xmobar -x 1 $HOME/.config/xmobar/xmobarrc1"
    spawn "xmobar -x 2 $HOME/.config/xmobar/xmobarrc2"
    -- the xmonad, ya know...what the WM is named after!
    xmonad $ ewmh def
        { manageHook = ( isFullscreen --> doFullFloat ) <+> myManageHook <+> manageDocks
        -- Run xmonad commands from command line with "xmonadctl command". Commands include:
        -- shrink, expand, next-layout, default-layout, restart-wm, xterm, kill, refresh, run,
        -- focus-up, focus-down, swap-up, swap-down, swap-master, sink, quit-wm. You can run
        -- "xmonadctl 0" to generate full list of commands written to ~/.xsession-errors.
        -- To compile xmonadctl: ghc -dynamic xmonadctl.hs
        , handleEventHook    = serverModeEventHookCmd
                               <+> spotifyHandleEventHook
                               <+> serverModeEventHook
                               <+> serverModeEventHookF "XMONAD_PRINT" (io . putStrLn)
                               <+> docksEventHook
                               <+> fullscreenEventHook  -- this does NOT work right if using multi-monitors! -- LIES IT WORKS FINE
        , modMask            = myModMask
        , terminal           = myTerminal
        , startupHook        = myStartupHook
        , layoutHook         = myLayoutHook
        , workspaces         = myWorkspaces
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormColor
        , focusedBorderColor = myFocusColor
        , logHook = dynamicLogWithPP $ filterOutWsPP ["NSP"] $ xmobarPP
                        { ppCurrent = xmobarColor "#98be65" "" . wrap "[" "]"           -- Current workspace in xmobar
                        , ppVisible = xmobarColor "#98be65" "" . clickable              -- Visible but not current workspace
                        , ppHidden = xmobarColor "#82AAFF" "" . wrap "*" "" . clickable -- Hidden workspaces in xmobar
                        , ppHiddenNoWindows = xmobarColor "#c792ea" ""  . clickable     -- Hidden workspaces (no windows)
                        , ppTitle = xmobarColor "#b3afc2" "" . shorten 60               -- Title of active window in xmobar
                        , ppSep =  "<fc=#666666> <fn=1>|</fn> </fc>"                    -- Separators in xmobar
                        , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"            -- Urgent workspace
                        , ppExtras  = [windowCount]                                     -- # of windows current workspace
                        , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
                        }
        } `additionalKeysP` myKeys