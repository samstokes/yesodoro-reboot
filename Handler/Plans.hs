module Handler.Plans where

import Handler
import Import
import Util
import Util.Angular

getPlansR :: Handler Value
getPlansR = do
  userId <- requireNgAuthId
  horizon <- horizonFromParams
  plans <- userPlansSince userId horizon
  returnJson plans

postPlansR :: Handler Value
postPlansR = do
  userId <- requireNgAuthId
  newPlan <- requireJsonBody -- TODO error page is HTML, not friendly!
  planEntity <- runDB $ createPlan userId newPlan
  returnJson planEntity


postCompletePlanR :: PlanId -> Handler Value
postCompletePlanR planId = do
  Entity _ plan <- authedPlan planId
  time <- now
  runDB $ update planId [PlanDoneAt =. Just time]
  returnJson $ Entity planId plan { planDoneAt = Just time }


putPlanR :: PlanId -> Handler Value
putPlanR planId = do
  _ <- authedPlan planId
  editedPlan <- requireJsonBody -- TODO error page is HTML, not friendly!
  (_, mCurrentPlan) <- runDB $ updatePlan editedPlan planId
  case mCurrentPlan of
    Just plan -> returnJson $ Entity planId plan
    Nothing -> notFound


deletePlanR :: PlanId -> Handler Value
deletePlanR planId = do
  _ <- authedPlan planId
  runDB $ delete planId
  returnJson $ object ["deleted" .= True]



userPlansSince :: UserId -> UTCTime -> Handler [Entity Plan]
userPlansSince userId horizon = runDB $ selectUserPlansSince userId horizon [Desc PlanCreatedAt, Desc PlanDoneAt]


authedPlan :: PlanId -> Handler (Entity Plan)
authedPlan planId = do
  userId <- requireNgAuthId
  maybeAuthedPlan <- runDB $ selectFirst [PlanId ==. planId, PlanUser ==. userId] []
  case maybeAuthedPlan of
    Just plan -> return plan
    Nothing -> notFound
