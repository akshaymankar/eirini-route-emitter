module Data.JSONPath.Execute
  (executeJSONPath, executeJSONPath', ExecutionResult(..))
where

import Data.Aeson
import Data.Bifunctor          (second)
import Data.Either             (rights)
import Data.Either.Combinators
import Data.Function           ((&))
import Data.HashMap.Strict     as Map
import Data.JSONPath.Types
import Data.Text               (Text, unpack)

import qualified Data.ByteString.Lazy      as LBS
import qualified Data.ByteString.Lazy.UTF8 as LBS
import qualified Data.Vector               as V

notFoundErr key o = "expected key " <> unpack key <> " in object " <> (LBS.toString $ encode o)
invalidIndexErr i a = "index " <> show i <> " invalid for array " <> (LBS.toString $ encode a)
expectedObjectErr val = "expected object, found " <> (LBS.toString $ encode val)
expectedArrayErr val = "expected array, found " <> (LBS.toString $ encode val)

executeJSONPath :: [JSONPathElement] -> Value -> Either String [Value]
executeJSONPath js val = resultToEither $ executeJSONPath' js val

executeJSONPath' :: [JSONPathElement] -> Value -> ExecutionResult Value
executeJSONPath' [] val = ResultError "empty json path"
executeJSONPath' (j:[]) val = executeJSONPathElement j val
executeJSONPath' (j:js) val = executeJSONPath' js =<< executeJSONPathElement j val

data ExecutionResult a = ResultList [a]
                       | ResultValue a
                       | ResultError String

instance Functor ExecutionResult where
  fmap f (ResultList xs)   = ResultList $ Prelude.map f xs
  fmap f (ResultValue x)   = ResultValue $ f x
  fmap f (ResultError err) = ResultError err

instance Applicative ExecutionResult where
  pure = ResultValue
  (<*>) (ResultList fs) (ResultList xs) = ResultList $ fs <*> xs
  (<*>) (ResultList fs) (ResultValue x) = ResultList $ Prelude.map (\f -> f x) fs
  (<*>) (ResultValue f) (ResultList xs) = ResultList $ Prelude.map f xs
  (<*>) (ResultValue f) (ResultValue x) = ResultValue $ f x
  (<*>) (ResultError e) _               = ResultError e
  (<*>) _ (ResultError e)               = ResultError e

instance Monad ExecutionResult where
  (>>=) (ResultValue x) f = f x
  (>>=) (ResultList xs) f = concatResults $ Prelude.map f xs
  (>>=) (ResultError e) f = ResultError e

concatResults :: [ExecutionResult a] -> ExecutionResult a
concatResults [] = ResultList []
concatResults (ResultList xs:rs) = case concatResults rs of
                                     ResultList ys -> ResultList (xs ++ ys)
                                     ResultValue y -> ResultList (y:xs)
                                     e -> e
concatResults (ResultValue x:[]) = ResultValue x
concatResults (ResultValue x:rs) = case concatResults rs of
                                     ResultList ys -> ResultList (x:ys)
                                     ResultValue y -> ResultList [x,y]
                                     e -> e
concatResults (e:_) = e

executeJSONPathElement :: JSONPathElement -> Value -> ExecutionResult Value
executeJSONPathElement (KeyChild key) val =
  case val of
    Object o -> Map.lookup key o
                & (maybeToResult (notFoundErr key o))
    _ -> ResultError $ expectedObjectErr val
executeJSONPathElement (AnyChild) val =
  case val of
    Object o -> ResultList $ Map.elems o
    Array a  -> ResultList $ V.toList a
    _        -> ResultError $ expectedObjectErr val
executeJSONPathElement (Slice slice) val =
  case val of
    Array a -> executeSliceElement slice a
    _       -> ResultError $ expectedArrayErr val
executeJSONPathElement (SliceUnion first second) val =
  case val of
    Array a -> appendResults (executeSliceElement first a) (executeSliceElement second a)
    _ -> ResultError $ expectedArrayErr val
executeJSONPathElement (Filter _ jsonPath cond lit) val =
  case val of
    Array a -> do
      let l = V.toList a
      ResultList $ Prelude.map (executeJSONPath' jsonPath) l
        & zip l
        & excludeSndErrors
        & Prelude.foldr (\(x,ys) acc -> if length ys == 1 then (x, head ys):acc else acc) []
        & Prelude.filter (\(origVal, exprVal) -> executeCondition exprVal cond lit)
        & Prelude.map fst
    _ -> ResultError $ expectedArrayErr val
executeJSONPathElement s@(Search js) val =
  let x = either (const []) id $ executeJSONPath js val
      y = excludeErrors $ valMap (executeJSONPathElement s) val
  in if Prelude.null x && Prelude.null y
     then ResultError "Search failed"
     else ResultList $ x ++ y

valMap :: ToJSON b => (Value -> ExecutionResult b) -> Value -> [ExecutionResult b]
valMap f v@(Object o) = elems $ Map.map f o
valMap f (Array a) = V.toList $ V.map f a
valMap _ v = pure $ ResultError $ "Expected object or array, found " <> (LBS.toString $ encode v)

executeCondition :: Value -> Condition -> Literal -> Bool
executeCondition (Number n1) Equal (LitNumber n2) = n1 == (fromInteger $ toInteger n2)
executeCondition (String s1) Equal (LitString s2) = s1 == s2

executeSliceElement :: SliceElement -> V.Vector Value -> ExecutionResult Value
executeSliceElement (SingleIndex i) v                = if i < 0
                                                          then maybeToResult (invalidIndexErr i v) $ (V.!?) v (V.length v + i)
                                                          else maybeToResult (invalidIndexErr i v) $ (V.!?) v i
executeSliceElement (SimpleSlice start end) v        = sliceEither v start end 1
executeSliceElement (SliceWithStep start end step) v = sliceEither v start end step
executeSliceElement (SliceTo end) v                  = sliceEither v 0 end 1
executeSliceElement (SliceToWithStep end step) v     = sliceEither v 0 end step
executeSliceElement (SliceFrom start) v              = sliceEither v start (-1) 1
executeSliceElement (SliceFromWithStep start step) v = sliceEither v start (-1) step
executeSliceElement (SliceWithOnlyStep step) v       = sliceEither v 0 (-1) step

sliceEither :: ToJSON a
    => V.Vector a -> Int -> Int -> Int -> ExecutionResult a
sliceEither v start end step = let len = V.length v
                                   realStart = if start < 0 then len + start else start
                                   realEnd = if end < 0 then len + end + 1 else end
                               in if realStart < realEnd
                                  then appendResults (indexEither v realStart) (sliceEither v (realStart + step) realEnd step)
                                  else ResultList []

indexEither :: ToJSON a => V.Vector a -> Int -> ExecutionResult a
indexEither v i = (V.!?) v i
                  & maybeToResult (invalidIndexErr i v)

appendResults :: ExecutionResult a -> ExecutionResult a -> ExecutionResult a
appendResults (ResultValue x) (ResultValue y) = ResultList [x,y]
appendResults (ResultValue x) (ResultList ys) = ResultList $ x:ys
appendResults (ResultList xs) (ResultValue y) = ResultList $ y:xs
appendResults (ResultList xs) (ResultList ys) = ResultList $ xs ++ ys
appendResults _ e                             = e

maybeToResult :: String -> Maybe a ->ExecutionResult a
maybeToResult _ (Just x) = ResultValue x
maybeToResult err _      = ResultError err

resultToEither :: ExecutionResult a -> Either String [a]
resultToEither (ResultList xs) = return xs
resultToEither (ResultValue x) = return [x]
resultToEither (ResultError e) = Left e

excludeErrors :: [ExecutionResult a] -> [a]
excludeErrors []                 = []
excludeErrors (ResultError _:rs) = excludeErrors rs
excludeErrors (ResultList xs:rs) = xs ++ excludeErrors rs
excludeErrors (ResultValue x:rs) = x:(excludeErrors rs)

excludeSndErrors :: [(c, ExecutionResult a)] -> [(c, [a])]
excludeSndErrors xs = Prelude.foldr accumulateFn ([] :: [(c, b)]) xs where
  accumulateFn (x, ResultList ys) acc = (x, ys):acc
  accumulateFn (x, ResultValue y) acc = (x, [y]):acc
  accumulateFn (x, _) acc             = acc

