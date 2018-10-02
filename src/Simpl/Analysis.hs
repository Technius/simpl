{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module Simpl.Analysis where

import Data.Functor.Foldable (Fix(Fix))
import Data.List (find)
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Simpl.Ast

data SymbolTable expr = MkSymbolTable
  { symTabAdts :: Map Text (Type, [Constructor])
  , symTabFuns :: Map Text (Type, expr) }
  deriving (Show, Functor, Foldable, Traversable)

buildSymbolTable :: SourceFile e -> SymbolTable e
buildSymbolTable (SourceFile _ decls) =
  let adts = Map.fromList $ mapMaybe
        (\case
          DeclAdt name ctors -> Just (name, (Fix (TyAdt name), ctors))
          _ -> Nothing) decls
      funs = Map.fromList $ mapMaybe
        (\case
          DeclFun name ty body -> Just (name, (ty, body))
          _ -> Nothing) decls
  in MkSymbolTable
     { symTabAdts = adts
     , symTabFuns = funs }

symTabModifyAdts :: (Map Text (Type, [Constructor]) -> Map Text (Type, [Constructor]))
                 -> SymbolTable e
                 -> SymbolTable e
symTabModifyAdts f t = t { symTabAdts = f (symTabAdts t) }

symTabMapFuns :: ((Type, e) -> (Type, e')) -> SymbolTable e -> SymbolTable e'
symTabMapFuns f t = t { symTabFuns = Map.map f (symTabFuns t) }

-- | Searches for the given constructor, returning the name of the ADT, the
-- constructor, and the index of the constructor.
symTabLookupCtor :: Text -> SymbolTable e -> Maybe (Type, Constructor, Int)
symTabLookupCtor name t = listToMaybe . mapMaybe (find isTheCtor) $ getCtors
  where
    getCtors = (\(ty, cs) -> zip3 (repeat ty) cs [0..]) <$> Map.elems (symTabAdts t)
    isTheCtor (_, Ctor ctorName _, _) = ctorName == name

symTabLookupAdt :: Text -> SymbolTable e -> Maybe (Type, [Constructor])
symTabLookupAdt name = Map.lookup name . symTabAdts
