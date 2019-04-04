{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module Simpl.SymbolTable where

import Data.Functor.Foldable (Fix(Fix))
import Data.List (find)
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Simpl.Ast

data SymbolTable expr = MkSymbolTable
  { symTabAdts :: Map Text (Type, [Constructor])
  , symTabFuns :: Map Text ([(Text, Type)], Type, expr)
  , symTabVars :: Map Text Type }
  deriving (Show, Functor, Foldable, Traversable)

buildSymbolTable :: SourceFile e -> SymbolTable e
buildSymbolTable (SourceFile _ decls) =
  let adts = Map.fromList $ mapMaybe
        (\case
          DeclAdt name ctors -> Just (name, (Fix (TyAdt name), ctors))
          _ -> Nothing) decls
      funs = Map.fromList $ mapMaybe
        (\case
          DeclFun name params ty body -> Just (name, (params, ty, body))
          _ -> Nothing) decls
  in MkSymbolTable
     { symTabAdts = adts
     , symTabFuns = funs
     , symTabVars = Map.empty }

symTabModifyAdts :: (Map Text (Type, [Constructor]) -> Map Text (Type, [Constructor]))
                 -> SymbolTable e
                 -> SymbolTable e
symTabModifyAdts f t = t { symTabAdts = f (symTabAdts t) }

symTabMapExprs :: (([(Text, Type)], Type, e) -> ([(Text, Type)], Type, e')) -- ^ Map over functions
               -> SymbolTable e
               -> SymbolTable e'
symTabMapExprs f t = t { symTabFuns = Map.map f (symTabFuns t) }

symTabTraverseExprs
  :: Monad m
  => (([(Text, Type)], Type, e) -> ([(Text, Type)], Type, m e')) -- ^ Map over functions
  -> SymbolTable e
  -> m (SymbolTable e')
symTabTraverseExprs f t = do
  upd <- traverse ((\(args, ty, me) -> (args, ty,) <$> me) . f) (symTabFuns t)
  pure $ t { symTabFuns = upd }

-- | Searches for the given constructor, returning the name of the ADT, the
-- constructor, and the index of the constructor.
symTabLookupCtor :: Text -> SymbolTable e -> Maybe (Type, Constructor, Int)
symTabLookupCtor name t = listToMaybe . mapMaybe (find isTheCtor) $ getCtors
  where
    getCtors = (\(ty, cs) -> zip3 (repeat ty) cs [0..]) <$> Map.elems (symTabAdts t)
    isTheCtor (_, Ctor ctorName _, _) = ctorName == name

symTabLookupAdt :: Text -> SymbolTable e -> Maybe (Type, [Constructor])
symTabLookupAdt name = Map.lookup name . symTabAdts

symTabLookupVar :: Text -> SymbolTable e -> Maybe Type
symTabLookupVar name = Map.lookup name . symTabVars

symTabInsertVar :: Text -> Type -> SymbolTable e -> SymbolTable e
symTabInsertVar name ty t = t { symTabVars = Map.insert name ty (symTabVars t) }

symTabInsertVars :: [(Text, Type)] -> SymbolTable e -> SymbolTable e
symTabInsertVars vars t = t { symTabVars = Map.union (Map.fromList vars) (symTabVars t) }

symTabLookupFun :: Text -> SymbolTable e -> Maybe ([(Text, Type)], Type, e)
symTabLookupFun name = Map.lookup name . symTabFuns
