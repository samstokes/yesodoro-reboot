module Handler.Plans where

import Import
import Util
import Util.Angular

postPlansR :: Handler Value
postPlansR = do
  userId <- requireAuthIdPreventingXsrf
  newPlan <- parseJsonBody_ -- TODO error page is HTML, not friendly!
  planEntity <- runDB $ createPlan userId newPlan
  returnJson $ JsonSanityWrapper planEntity


postCompletePlanR :: PlanId -> Handler Value
postCompletePlanR planId = do
  Entity _ plan <- authedPlan planId
  time <- now
  runDB $ update planId [PlanDoneAt =. Just time]
  returnJson $ JsonSanityWrapper $ Entity planId plan { planDoneAt = Just time }


putPlanR :: PlanId -> Handler Value
putPlanR planId = do
  _ <- authedPlan planId
  editedPlan <- parseJsonBody_ -- TODO error page is HTML, not friendly!
  (_, mCurrentPlan) <- runDB $ updatePlan editedPlan planId
  case mCurrentPlan of
    Just plan -> returnJson $ JsonSanityWrapper $ Entity planId plan
    Nothing -> notFound


deletePlanR :: PlanId -> Handler Value
deletePlanR planId = do
  _ <- authedPlan planId
  runDB $ delete planId
  returnJson $ object ["deleted" .= True]



authedPlan :: PlanId -> Handler (Entity Plan)
authedPlan planId = do
  userId <- requireAuthIdPreventingXsrf
  maybeAuthedPlan <- runDB $ selectFirst [PlanId ==. planId, PlanUser ==. userId] []
  case maybeAuthedPlan of
    Just plan -> return plan
    Nothing -> notFound
