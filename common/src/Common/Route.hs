{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecursiveDo, TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}


module Common.Route where

import Reflex
import Reflex.Dom

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Control.Monad.Fix
import Control.Monad

import Focus.JS.FontAwesome as FA
import Web.FontAwesomeType
import Reflex.Dom.Path

-- This class will have to go elsewhere...
class WebRoute a where 
  routeToTitle :: a -> Text
  routeToUrl :: a -> Text
  routeToWidget :: (DomBuilder t m) => a -> m ()
  urlToRoute :: Text -> Maybe a

-- | Custom data type that corresponds to a site's navBar
data Route = Route_Home | Route_Tutorials | Route_Examples | Route_Documentation | Route_FAQ
  deriving (Show, Eq, Ord)

--  | instance of class from Frontend.Route
instance IsPath Route where 
  pathToText = routeToUrl 
  textToPath = fromMaybe Route_Home . urlToRoute

instance WebRoute Route where 
  -- | Outputs text to be appended to url
  routeToUrl :: Route -> Text
  routeToUrl r = case r of
   Route_Home -> "/home"
   Route_Tutorials -> "/tutorials"
   Route_Examples -> "/examples"
   Route_Documentation -> "/documentation" 
   Route_FAQ -> "/faq"

  -- | Look up if a given url extension exists within a list of routes
  urlToRoute :: Text -> Maybe Route
  urlToRoute path = Map.lookup path routes
    where routes = Map.fromList $ fmap (\r ->(routeToUrl r, r)) [Route_Home , Route_Tutorials , Route_Examples , Route_Documentation , Route_FAQ]

  routeToTitle :: Route -> Text
  routeToTitle r = case r of
   Route_Home -> "Home"
   Route_Tutorials -> "Tutorials"
   Route_Examples -> "Examples"
   Route_Documentation -> "Documentation" 
   Route_FAQ -> "Faq"

  routeToWidget :: DomBuilder t m => Route -> m ()
  routeToWidget r = case r of
   Route_Home -> home
   Route_Tutorials -> tutorials
   Route_Examples -> examples
   Route_Documentation -> documentation
   Route_FAQ -> faq

bodyGen :: (DomBuilder t m, PostBuild t m, Prerender js m, MonadHold t m, MonadFix m, PerformEvent t m, TriggerEvent t m, WebRoute a, IsPath a, Ord a) 
              => Text -> [a] -> a ->  m ()
bodyGen theLogo pageTabs ir = do
  rec
    pageSwitch <- elClass "div" "header" $ do
      rec
        (homeEvent,_) <- elAttr' "img" ("class" =: "logo" <> "src" =: theLogo) blank
      goHome <- return $ (head pageTabs) <$ domEvent Click homeEvent -- ^ go Home if site logo is clicked
      goNavi <- mobileNavMenu (navMenu active pageTabs) active
      return (leftmost [goHome, goNavi])

    active <- prerender (routeToWidget ir >> return (constDyn ir))
      (pathWidget $ \r -> do
        routeToWidget r
        return (pageSwitch, r))
  return ()

----------------------------------------------------NAV MENU BUILDER ---------------------------------------------------
-- | Nav Bar generator produces click-able Widget Events
navMenu :: (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m, WebRoute a, IsPath a, Ord a) => Dynamic t a -> [a] -> m (Event t a)
navMenu currentTab tabList = do
  let currentTabDemux = demux currentTab      -- ^ change type (Dynamic t a) to (Demux t a)
  rec events <- forM tabList $ \route -> do
        let selected = demuxed currentTabDemux route -- ^ compare currentTab and section
        let highlight = zipDynWith isActive currentTab selected -- ^ if selected is True, highlight currentTab
        el "li" $ do
          -- | Get anchor tag element with Route name and corresponding "active:" styling
          (linkEl, _) <- elDynAttr' "a" (highlight) $ text (routeToTitle route)
          return (route <$ domEvent Click linkEl)  -- ^ get Event t Route anchor element
  -- | send clicked Route to \route function
  return $ leftmost events

isActive :: (WebRoute a) => a -> Bool -> Map Text Text
isActive ia isit = "id" =: (routeToTitle ia)
           <> "style" =: ("border-bottom: " <> active isit)
  where
    active True = "4px solid #D92323"
    active False = "none;"

-------------MOBILE NAV MENU BUILDER ----------------------------------
--TODO some of the style changes that are within the style.css file may want
--to be integrated into the function somehow in order to avoid fingering
--through css code to figure out how to get this function to be useful
--straight out of the box.
-- | Make the mobile app Menu become the parent ul of the li generated from
-- 'navMenu'
mobileNavMenu :: (DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m, WebRoute a, IsPath a, Ord a)=> m (Event t a) -> Dynamic t a -> m (Event t a)
mobileNavMenu items activeTab = do
  rec
    isOpen <- toggle False onClick                      -- ^ add toggle-able Boolean Event when clicked
    let toggleOpen = section <$> isOpen                 -- ^ fmap Boolean to 'section'
    let onClick = domEvent Click modalDiv                -- ^ add Event target
    (modalDiv,mWidg) <- elDynAttr' "div" toggleOpen $ do -- ^ Bootleg modal div (used to close dropdown if user clicks elsewhere)
      (_,widg) <- elDynAttr' "ul" toggleOpen $ do        -- ^ get a tuple with (EventResult, m())
        let selectedTitle = Text.toUpper . routeToTitle <$> activeTab    -- ^ set Title for Responsive Menu
        el "div" $ text " "                              -- ^ added this div for flexbox fix (temp fix)
        el "p" $ dynText selectedTitle                   -- ^ add h3 with Dynmically changing title
        _ <- FA.faIcon' FaBars $ def {_faConfig_size = Size_Large} -- ^ add FontAwsome Menu Icon with Large size configs added
        items                                             -- ^ add contents of whatever widget is passed as an arg
      return widg
  return (mWidg)

-- | helper function for mobileNavMenu
section :: Bool -> Map Text Text
section True = "class" =: "sections"
section False = "class" =: "noshow"

-----------------------------------------WIDGET BODIES------------------------------------
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
