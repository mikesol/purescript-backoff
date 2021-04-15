module Klank.Dev.Util where

import Prelude

import Control.Comonad.Cofree (Cofree, head, tail)
import Data.Either (Either(..), either)
import Data.Filterable (filter)
import Data.Lens (over, _2)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst)
import Effect (Effect)
import Effect.Aff (Aff, Error, launchAff_, makeAff, parallel, sequential, try)
import Effect.Class (liftEffect)
import Foreign.Object (Object, fromHomogeneous)
import Foreign.Object as O
import Type.Row.Homogeneous (class Homogeneous)

loopDownload :: forall a b. (b -> Aff a) -> Cofree ((->) Unit) (Aff Unit) -> b -> Aff a
loopDownload f cfcm' str = go 0 cfcm'
  where
  go nt cfcm =
    res
      >>= either
          ( \e ->
              do
                head cfcm
                go (nt + 1) (tail cfcm unit)
          )
          pure
    where
    res = try $ f str

backoffWithCache :: forall a b. (b -> Aff a) -> Cofree ((->) Unit) (Aff Unit) -> (O.Object a -> Tuple (Array (Tuple String b)) (O.Object a)) -> O.Object a -> (O.Object a -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit
backoffWithCache looper cfcm bf prev' =
  affable
    $ sequential
        ( O.union <$> (pure prev)
            <*> ( sequence
                  $ O.fromFoldable
                      ( map
                          ( over _2
                              (parallel <<< loopDownload looper cfcm)
                          )
                          (filter (not <<< flip O.member prev <<< fst) newB)
                      )
              )
        )
  where
  (Tuple newB prev) = bf prev'

backoffWithCacheSync :: forall a b. (b -> Aff a) -> (O.Object a -> Tuple (Array (Tuple String b)) (O.Object a)) -> O.Object a -> (O.Object a -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit
backoffWithCacheSync funk bf prev' =
  affable
    ( O.union <$> (pure prev)
        <*> ( sequence
              $ O.fromFoldable
                  ( map
                      (over _2 funk)
                      (filter (not <<< flip O.member prev <<< fst) newB)
                  )
          )
    )
  where
  (Tuple newB prev) = bf prev'

type CacheFunction a b
  = O.Object a -> Tuple (Array (Tuple String b)) (O.Object a)

affableRec ::
  forall (a :: Row Type) b.
  Homogeneous a b =>
  Aff (Record a) ->
  (Object b -> Effect Unit) ->
  (Error -> Effect Unit) ->
  Effect Unit
affableRec aff res rej =
  launchAff_ do
    result <- try $ aff
    case result of
      Left err -> liftEffect $ rej err
      Right resp -> liftEffect $ res (fromHomogeneous resp)

affable :: forall a. Aff a -> (a -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit
affable aff res rej =
  launchAff_ do
    result <- try $ aff
    case result of
      Left err -> liftEffect $ rej err
      Right resp -> liftEffect $ res resp

affize :: forall a. ((a -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit) -> Aff a
affize f =
  makeAff \cb -> do
    f (cb <<< Right) (cb <<< Left)
    pure mempty

