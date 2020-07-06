module Docs.Search.App.Sidebar where

import Docs.Search.ModuleIndex (ModuleIndex)
import Docs.Search.Types (ModuleName)

import Prelude
import Data.Array as Array
import Data.Lens (Setter', _2, (%~), (.~))
import Data.Lens.Record (prop)
import Data.List (List, foldr)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Profunctor.Strong (first, (***))
import Data.Search.Trie (Trie)
import Data.Search.Trie as Trie
import Data.Set (Set)
import Data.Set as Set
import Data.String.CodeUnits (singleton, toCharArray) as String
import Data.Symbol (SProxy(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (stopPropagation)
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as ME


data Action = ToggleCollapse String | ToggleGrouping Boolean | ModuleClicked MouseEvent


data Mode = GroupByPackage | DontGroup

derive instance modeEq :: Eq Mode


data PackageEntryState = Expanded | Collapsed

derive instance packageEntryStateEq :: Eq PackageEntryState

type State = { expansions :: Trie Char (Set ModuleName /\ PackageEntryState)
             , mode :: Mode
             , moduleNames :: Array ModuleName
             }


mkComponent
  :: forall o i q
  .  ModuleIndex
  -> H.Component HH.HTML q i o Aff
mkComponent moduleIndex =
  H.mkComponent
    { initialState: const { expansions
                          , mode: GroupByPackage
                          , moduleNames
                          }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
    where
      moduleNames = Array.sort $ Array.fromFoldable $ foldr Set.union mempty moduleIndex

      -- Convert `ModuleIndex` to the appropriate format.
      expansions :: Trie Char (Set ModuleName /\ PackageEntryState)
      expansions =
        moduleIndex #
        Map.toUnfoldable <#>
        (String.toCharArray >>> Array.toUnfoldable) *** (_ /\ Collapsed) #
        Trie.fromList


handleAction
  :: forall o
  .  Action
  -> H.HalogenM State Action () o Aff Unit
handleAction (ModuleClicked event) = do
  -- Do not allow the module listing to collapse before the page unloads.
  H.liftEffect $ stopPropagation $ ME.toEvent event
handleAction (ToggleCollapse packageName) = do
  H.modify_ (
    _expansions %~ trieKey packageName %~ _2 %~
    case _ of
      Expanded -> Collapsed
      Collapsed -> Expanded
  )
handleAction (ToggleGrouping flag) =
  H.modify_ (_mode .~ if flag then GroupByPackage else DontGroup)


render
  :: forall m
  .  State
  -> H.ComponentHTML Action () m
render { expansions, mode, moduleNames } =

  HH.div [ HP.classes [ wrap "col", wrap "col--aside" ] ]

  [ HH.h3_ [ HH.text "Modules" ]
  , HH.input [ HP.id_ "group-modules__input"
             , HP.type_ HP.InputCheckbox
             , HP.checked (mode == GroupByPackage)
             , HE.onChecked $ Just <<< ToggleGrouping
             ]

  , HH.text " "
  , HH.label [ HP.for "group-modules__input"
             , HP.id_ "group-modules__label"
             ]
    [ HH.text " GROUP BY PACKAGE" ]

  , if mode == GroupByPackage
    then HH.ul_ $ renderPackageEntry <$> packageList
    else HH.ul_ $ renderModuleName <$> moduleNames
  ]
  where

    renderPackageEntry (packageName /\ modules /\ status) =

      HH.li [ HE.onClick $ const $ Just $ ToggleCollapse packageName
            , HP.classes [ wrap $ if status == Expanded
                                  then "li-expanded-package"
                                  else "li-collapsed-package"
                         , wrap "li-package" ]
            ]

      if status == Expanded
      then [ HH.text packageName
           , HH.ul_ $ Set.toUnfoldable modules <#> renderModuleName ]
      else [ HH.text packageName ]

    renderModuleName moduleName =
      HH.li_
      [ HH.a [ HP.href (moduleName <> ".html")
             , HE.onClick $ Just <<< ModuleClicked
             ]
        [ HH.text moduleName ]
      ]

    packageList :: Array (String /\ Set ModuleName /\ PackageEntryState)
    packageList =
      first (Array.foldMap String.singleton) <$> (
        Trie.toUnfoldable expansions :: Array (Array Char /\ Set ModuleName /\ PackageEntryState)
      )


-- Some optics:

_expansions :: forall a b rest.  (a -> b) -> { expansions :: a | rest } -> { expansions :: b | rest }
_expansions = prop (SProxy :: SProxy "expansions")

_mode :: forall a b rest.  (a -> b) -> { mode :: a | rest } -> { mode :: b | rest }
_mode = prop (SProxy :: SProxy "mode")

trieKey :: forall a. String -> Setter' (Trie Char a) a
trieKey key f = Trie.update f path
  where
    path :: List Char
    path = Array.toUnfoldable $ String.toCharArray key
