{-# LANGUAGE LambdaCase #-}
module Simpl.Analysis where

import Data.Functor.Foldable (Fix(Fix))
import Data.Bifunctor (second)
import Data.List (find)
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Simpl.Ast

data SymbolTable expr = MkSymbolTable
  { symTabAdts :: Map Text (Type, [Constructor])
  , symTabFuns :: Map Text (Type, expr) }

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

symTabMapFuns :: (e -> e') -> SymbolTable e -> SymbolTable e'
symTabMapFuns f t = t { symTabFuns = Map.map (second f) (symTabFuns t) }

symTabLookupCtor :: Text -> SymbolTable e -> Maybe (Type, Constructor)
symTabLookupCtor name t = listToMaybe . mapMaybe (find isTheCtor) $ getCtors
  where
    getCtors = (\(ty, cs) -> repeat ty `zip` cs) <$> Map.elems (symTabAdts t)
    isTheCtor (_, Ctor ctorName _) = ctorName == name
