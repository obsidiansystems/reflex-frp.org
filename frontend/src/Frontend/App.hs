{-# LANGUAGE RecursiveDo, TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Frontend.App where

import Reflex
import Reflex.Dom
import Reflex.Dom.Path

import qualified Data.Map as Map
import Data.Monoid
import Control.Monad
import Data.Map (Map)
import Data.Text (Text)
import Common.Route -- ^ used for navBar's Route data type 
import Focus.JS.Prerender (Prerender, prerender)
import Control.Monad.Fix

import Focus.JS.FontAwesome as FA
import Web.FontAwesomeType

------------------- <head></head> ----------------------------------------
siteHead :: DomBuilder t m => m ()
siteHead = do
  el "title" $ text "Reflex FRP"      -- ^ add Page title
  elAttr "meta" metaDesc blank        -- ^ add meta-data description
  elAttr "meta" metaKeywords blank    -- ^ add meta-data keywords
  elAttr "meta" viewport blank        -- ^ add meta-data viewport
  fontAwesomeCDN                      -- ^ link MaxCDN FontAwesome
  -- | add various favIcon links
  faviconLinker "icon" "image/png" "16x16" "img/favicon-16x16.png"
  faviconLinker "icon" "image/png" "32x32" "img/favicon-32x32.png" 
  faviconLinker "apple-touch-icon" "/" "57x57" "img/apple-touch-icon-57x57.png"
  faviconLinker "apple-touch-icon" "/" "60x60" "img/apple-touch-icon-60x60.png"
  faviconLinker "apple-touch-icon" "/" "72x72" "img/apple-touch-icon-72x72.png"
  faviconLinker "apple-touch-icon" "/" "76x76" "img/apple-touch-icon-76x76.png"
  faviconLinker "apple-touch-icon" "/" "114x114" "img/apple-touch-icon-114x114.png"
  faviconLinker "apple-touch-icon" "/" "120x120" "img/apple-touch-icon-120x120.png"
  faviconLinker "apple-touch-icon" "/" "144x144" "img/apple-touch-icon-144x144.png"
  faviconLinker "apple-touch-icon" "/" "152x152" "img/apple-touch-icon-152x152.png"
  faviconLinker "icon" "image/png" "img/favicon-196x196.png" "196x196"
  styleSheet "style.css"              -- ^ link css stylesheet
  styleSheet "font.css"               -- ^ link css fonts
  return ()

------------------- <body></body> ----------------------------------------
-- | takes the initial Route of the website and creates a widget
siteBody :: ( DomBuilder t m, MonadHold t m, MonadFix m, TriggerEvent t m, PostBuild t m
            , PerformEvent t m, Prerender x m)
         => Route -> m ()
siteBody initRoute = do 
  -- | List of tupled Haskell community links
  let links = [ ("Hackage", "https://hackage.haskell.org/package/reflex")
              , ("irc.freenode.net #reflex-frp", "http://webchat.freenode.net/?channels=%23reflex-frp&uio=d4")
              ]

  rec
    pageSwitch <- elClass "div" "header" $ do
      elAttr "img" logo blank                 -- ^ add site logo header
      mobileNavMenu (navMenu active) active   -- ^ add nav bar nested within responsive mobile nav bar funciton

    -- | show active widget until a different route is chosen to be
    -- prerendered, server-side, and shown 
    active <- prerender (routeToWidget initRoute >> return (constDyn initRoute))
                 (pathWidget $ \r -> do  
                    routeToWidget r
                    return (pageSwitch, r))

  -- | Create a list of Haskell community links from a list of tuples
  elClass "div" "main" $ do 
    el "p" $ text "Check us out on Hackage or join the community IRC chat!"
    forM_ links $ \pair -> do
      elAttr "a" ("href" =: (snd pair)) $ text (fst pair)
      el "br" $ return ()
  el "br" blank

  -- | Place Font Awesome Icons in footer <div> 
  elClass "div" "footer" $ do
    elAttr "a" rdirTwitter $ do
      FA.faIcon FaTwitter def
    elAttr "a" rdirGithub $ do
      FA.faIcon FaGithub def
    elAttr "a" rdirReddit $ do
      FA.faIcon FaReddit def
  return ()
  
----------------------Helper Functions-------------------------------

-- | styleSheet are functions to add links to html <head>
styleSheet :: DomBuilder t m => Text -> m ()
styleSheet myLink = elAttr "link" (Map.fromList [
    ("rel", "stylesheet"),
    ("type", "text/css"),
    ("href", myLink)
  ]) $ return ()

-- TODO: make this function more type safe.
-- The 4 arguments are as follows: rel type size href
-- turn the second argument into a Maybe Text
faviconLinker :: DomBuilder t m => Text -> Text -> Text -> Text -> m ()
faviconLinker r t s h = elAttr "link" attribs blank 
    where 
      attribs = "rel" =: r
             <> "type" =: t
             <> "size" =: s
             <> "href" =: h

  
-- | Nav Bar generator produces click-able Widget Events
navMenu :: (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m) => Dynamic t Route -> m (Event t Route)
navMenu currentTab = do
  let currentTabDemux = demux currentTab      -- ^ change type (Dynamic t Route) to (Demux t Route)
  rec events <- forM sections $ \route -> do
        let selected = demuxed currentTabDemux route -- ^ compare currentTab and section
        let highlight = zipDynWith isActive currentTab selected -- ^ if selected is True, highlight currentTab
        el "li" $ do
          -- | Get anchor tag element with Route name and corresponding "active:" styling
          (linkEl, _) <- elDynAttr' "a" (highlight) $ text (routeToTitle route)
          return (route <$ domEvent Click linkEl)  -- ^ get Event t Route anchor element
  -- | send clicked Route to \route function
  return $ leftmost events
  where sections = [ Route_Home
                   , Route_Tutorials
                   , Route_Examples
                   , Route_Documentation
                   , Route_FAQ
                   ]

--TODO some of the style changes that are within the style.css file may want
--to be integrated into the function somehow in order to avoid fingering
--through css code to figure out how to get this function to be useful
--straight out of the box.
-- | Make the mobile app Menu become the parent ul of the li generated from
-- 'navMenu'
mobileNavMenu :: (DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m)=> m (Event t Route) -> Dynamic t Route -> m (Event t Route)
mobileNavMenu items activeTab = do
  rec
    isOpen <- toggle False onClick                      -- ^ add toggle-able Boolean Event when clicked
    let toggleOpen = section <$> isOpen                 -- ^ fmap Boolean to 'section'
    let onClick = domEvent Click mobileNav              -- ^ add Event target
    (mobileNav,widg) <- elDynAttr' "ul" toggleOpen $ do -- ^ get a tuple with (EventResult, m())
      let selectedTitle = routeToTitle <$> activeTab    -- ^ set Title for Responsive Menu
      el "h3" $ dynText selectedTitle                   -- ^ add h3 with Dynmically changing title
      FA.faIcon' FaBars $ def {_faConfig_size = Size_Large} -- ^ add FontAwsome Menu Icon with Large size configs added
      items                                             -- ^ add contents of whatever widget is passed as an arg
  return (widg)

-- | helper function for mobileNavMenu
section :: Bool -> Map Text Text 
section True = "class" =: "sections"
section False = "class" =: "noshow"

-- | helper function for navMenu, underlines active Route
isActive :: Route -> Bool -> Map Text Text
isActive ia isit = "id" =: (routeToTitle ia) 
           <> "style" =: ("border-bottom: " <> active isit)
  where 
    active True = "4px solid #d4272a;"
    active False = "none;"
    

-- | Produces Text for navMenu, takes a route as an arguement
routeToTitle :: Route -> Text
routeToTitle r = case r of  
     Route_Home -> "Home"
     Route_Tutorials -> "Tutorials"
     Route_Examples -> "Examples"
     Route_Documentation -> "Documentation" 
     Route_FAQ -> "FAQ"

-- | Receives a route and returns it's corresponding widget
routeToWidget :: DomBuilder t m => Route -> m ()  
routeToWidget r = case r of
     Route_Home -> home
     Route_Tutorials -> tutorials
     Route_Examples -> examples
     Route_Documentation -> documentation
     Route_FAQ -> faq

----------------------Element Attributes------------------------------
metaDesc :: Map Text Text
metaDesc = "name" =: "description" 
        <> "content" =: "Reflex Functional Reactive Programming"

metaKeywords :: Map Text Text
metaKeywords = "name" =: "keywords"
            <> "content" =: "reflex, reflex frp, functional reactive programming, haskell, framework, reflex dom" 

viewport :: Map Text Text
viewport = "name" =: "viewport"
        <> "content" =: "width=device-width, initial-scale=1"

logo :: Map Text Text
logo = "class" =: "logo" 
        <> "src" =: "img/REFLEX.png" 

icon :: Map Text Text
icon = "rel" =: "shortcut icon"
        <> "href" =: "img/apple-touch-icon-57x57.png"

rdirTwitter :: Map Text Text
rdirTwitter = "href" =: "https://twitter.com/search?q=%23reflexfrp"
           <> "title" =: "twitter"
rdirGithub :: Map Text Text
rdirGithub = "href" =: "http://github.com/reflex-frp"
           <> "title" =: "github"
rdirReddit :: Map Text Text
rdirReddit = "href" =: "http://reddit.com/r/reflexfrp"
           <> "title" =: "reddit"


---------------------Widgets to switch to/from-----------------------
home :: (DomBuilder t m) => m ()
home = elClass "div" "main" $ do
         elClass "h3" "title" $ text "Practical Functional Reactive Programming"
         elClass "p" "class" $ text "Reflex is an fully-deterministic, higher-order Functional Reactive Programming (FRP) interface and an engine that efficiently implements that interface."


tutorials :: (DomBuilder t m) => m ()
tutorials = elClass "div" "main" $ do
    elClass "h3" "title" $ text "Tutorials"
    el "ol" $ do
      el "li" $ do 
        el "label" $ text "Installation: "
        elAttr "a" ("href" =: "https://github.com/reflex-frp/reflex-platform/blob/develop/README.md") $ text "setup-instructions" 
      el "li" $ do 
        el "label" $ text "Beginner Friendly Tutorial: "
        elAttr "a" ("href" =: "https://github.com/hansroland/reflex-dom-inbits/blob/master/tutorial.md") $ text "reflex-dom-inbits" 



examples :: (DomBuilder t m) => m ()
examples = elClass "div" "main" $ do
     elClass "h3" "title" $ text "Check Out Some Example Code"
     el "ul" $ do
      el "li" $ do 
        el "label" $ text "Basic ToDo List: "
        elAttr "a" ("href" =: "https://github.com/reflex-frp/reflex-examples/blob/master/BasicTodo/BasicTodo.hs") $ text "See Code Here" 
      el "li" $ do 
        el "label" $ text "JSON API - NASA Pic of the Day: "
        elAttr "a" ("href" =: "https://github.com/reflex-frp/reflex-examples/blob/master/nasa-pod/workshop.hs") $ text "See Code Here" 


documentation :: (DomBuilder t m) => m ()
documentation = elClass "div" "main" $ do
    elClass "h3" "title" $ text "Refreshing Reflex Documentation"
    el "ul" $ do
      el "li" $ do 
        el "label" $ text "Reflex Basic Documentation: "
        elAttr "a" ("href" =: "http://reflex-frp.readthedocs.io/en/latest/architecture.html#overview-of-reflex-basics") $ text "View Here" 
      el "li" $ do 
        el "label" $ text "Quick Reference: " 
        elAttr "a" ("href" =: "https://github.com/reflex-frp/reflex-dom/blob/develop/Quickref.md") $ text "View Here" 


faq :: (DomBuilder t m) => m ()
faq = elClass "div" "main" $ do
            elClass "h3" "title" $ text "FAQ"
            el "p" $ text "FAQ questions coming soon! For now, feel free to ask questions within the Reflex-FRP IRC chat provided below. Thank you!"
