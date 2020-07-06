module Docs.Search.App.Sidebar where

import Docs.Search.ModuleIndex (ModuleIndex)
import Docs.Search.Types (ModuleName)

import Prelude
import Data.Array as Array
import Data.Lens (Setter', (.~))
import Data.Lens.Record (prop)
import Data.List (List, foldr)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Profunctor.Strong (first)
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


data Action = ToggleGrouping Boolean


data Mode = GroupByPackage | DontGroup

derive instance modeEq :: Eq Mode


type State = { expansions :: Trie Char (Set ModuleName)
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
      expansions :: Trie Char (Set ModuleName)
      expansions =
        moduleIndex #
        Map.toUnfoldable <#>
        first (String.toCharArray >>> Array.toUnfoldable) #
        Trie.fromList


handleAction
 :: forall o
 .  Action
 -> H.HalogenM State Action () o Aff Unit
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
    packageList =
      first (Array.foldMap String.singleton) <$> (
        Trie.toUnfoldable expansions :: Array (Array Char /\ Set ModuleName)
      )


-- Some optics:

_mode :: forall a b rest.  (a -> b) -> { mode :: a | rest } -> { mode :: b | rest }
_mode = prop (SProxy :: SProxy "mode")

trieKey :: forall a. String -> Setter' (Trie Char a) a
trieKey key f = Trie.update f path
  where
    path :: List Char
    path = Array.toUnfoldable $ String.toCharArray key
