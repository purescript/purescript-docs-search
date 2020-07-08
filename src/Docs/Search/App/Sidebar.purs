module Docs.Search.App.Sidebar where

import Docs.Search.Config (config)
import Docs.Search.ModuleIndex (ModuleIndex)
import Docs.Search.Types (ModuleName, PackageName)

import Prelude
import Data.Array as Array
import Data.Lens ((.~))
import Data.Lens.Record (prop)
import Data.List (foldr)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (wrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Symbol (SProxy(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.HTML as HTML
import Web.HTML.Window as Window
import Web.Storage.Storage as Storage


data Action = ToggleGrouping Mode

data Query a = UpdateModuleGrouping a

data Mode = GroupByPackage | DontGroup

derive instance modeEq :: Eq Mode


type State = { moduleIndex :: Map PackageName (Set ModuleName)
             , mode :: Mode
             , moduleNames :: Array ModuleName
             }


mkComponent
  :: forall i
  .  ModuleIndex
  -> Aff (H.Component HH.HTML Query i Action Aff)
mkComponent moduleIndex = do
  mode <- H.liftEffect loadModeFromLocalStorage
  pure $
    H.mkComponent
      { initialState: const { moduleIndex
                            , mode
                            , moduleNames
                            }
      , render
      , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                       , handleQuery = handleQuery
                                       }
      }
  where
    moduleNames = Array.sort $ Array.fromFoldable $ foldr Set.union mempty moduleIndex


handleAction
 :: forall o
 .  Action
 -> H.HalogenM State Action () o Aff Unit
handleAction (ToggleGrouping mode) = do
  H.modify_ (_mode .~ mode)

  H.liftEffect do
    window <- HTML.window
    localStorage <- Window.localStorage window

    if mode == GroupByPackage
    then Storage.setItem    config.groupModulesItem "true" localStorage
    else Storage.removeItem config.groupModulesItem        localStorage


handleQuery
  :: forall a i
  .  Query a
  -> H.HalogenM State i () Action Aff (Maybe a)
handleQuery (UpdateModuleGrouping next) = do
  oldMode <- H.get <#> _.mode
  newMode <- H.liftEffect loadModeFromLocalStorage
  when (oldMode /= newMode) do
    H.modify_ (_mode .~ newMode)
  pure Nothing


render
  :: forall m
  .  State
  -> H.ComponentHTML Action () m
render { moduleIndex, mode, moduleNames } =

  HH.div [ HP.classes [ wrap "col", wrap "col--aside" ] ]

  [ HH.h3_ [ HH.text "Modules" ]
  , HH.input [ HP.id_ "group-modules__input"
             , HP.type_ HP.InputCheckbox
             , HP.checked (mode == GroupByPackage)
             , HE.onChecked $ Just <<< ToggleGrouping <<< isCheckedToMode
             ]

  , HH.text " "
  , HH.label [ HP.for "group-modules__input"
             , HP.id_ "group-modules__label"
             ]
    [ HH.text " GROUP BY PACKAGE" ]

  , HH.ul_ $ if mode == GroupByPackage
             then renderPackageEntry <$> packageList
             else renderModuleName <$> moduleNames
  ]
  where

    renderPackageEntry (packageName /\ modules) =
      HH.li [ HP.classes [ wrap "li-package" ] ]
      [ HH.details_
        [ HH.summary_ [ HH.text packageName ]
        , HH.ul_ $ Set.toUnfoldable modules <#> renderModuleName
        ]
      ]

    renderModuleName moduleName =
      HH.li_
      [ HH.a [ HP.href (moduleName <> ".html") ]
        [ HH.text moduleName ]
      ]

    packageList :: Array (String /\ Set ModuleName)
    packageList = Map.toUnfoldable moduleIndex


-- | Decide whether to group modules by package in the sidebar, using localStorage.
loadModeFromLocalStorage :: Effect Mode
loadModeFromLocalStorage = do
  window <- HTML.window
  localStorage <- Window.localStorage window
  mbGroupModules <- Storage.getItem config.groupModulesItem localStorage
  pure $ if isJust mbGroupModules then GroupByPackage else DontGroup


-- | Convert checkbox status to sidebar mode
isCheckedToMode :: Boolean -> Mode
isCheckedToMode = if _ then GroupByPackage else DontGroup


-- Some optics:

_mode :: forall a b rest.  (a -> b) -> { mode :: a | rest } -> { mode :: b | rest }
_mode = prop (SProxy :: SProxy "mode")
