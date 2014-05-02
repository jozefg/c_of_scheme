{-# LANGUAGE RecordWildCards, FlexibleContexts #-}
module Error where
import Text.Printf
import Control.Monad.Error

data Stage = Parser | CPS | ClosConv | CodeGen

type Loc  = String
type Info = String
data Failure = Failure { state    :: Stage
                       , location :: Loc
                       , summary  :: Info }

presentError :: Failure -> String
presentError Failure{..} =
  printf "Error! failure while %s at %s. \n    %s" stateS location summary
  where stateS = case state of
          CPS      -> "transforming to CPS"
          ClosConv -> "converting closures and lambda lifting"
          CodeGen  -> "generating C code"
          Parser   -> "parsing"


failCPS :: MonadError Failure m => Loc -> Info -> m a
failCPS loc info = throwError $ Failure CPS loc info

failClos :: MonadError Failure m => Loc -> Info -> m a
failClos loc info = throwError $ Failure ClosConv loc info

failGen :: MonadError Failure m => Loc -> Info -> m a
failGen loc info = throwError $ Failure CodeGen loc info
