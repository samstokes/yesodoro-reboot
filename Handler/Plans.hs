module Handler.Plans where

import Handler
import Import
import Util
import Util.Angular

getPlansR :: Handler RepJson
getPlansR = do
  userId <- requireNgAuthId
  horizon <- horizonFromParams
  plans <- userPlansSince userId horizon
  jsonToRepJson plans

postPlansR :: Handler RepJson
postPlansR = do
  userId <- requireNgAuthId
  newPlan <- parseJsonBody_ -- TODO error page is HTML, not friendly!
  planEntity <- runDB $ createPlan userId newPlan
  jsonToRepJson planEntity


postCompletePlanR :: PlanId -> Handler RepJson
postCompletePlanR planId = do
  Entity _ plan <- authedPlan planId
  time <- now
  runDB $ update planId [PlanDoneAt =. Just time]
  jsonToRepJson $ Entity planId plan { planDoneAt = Just time }


putPlanR :: PlanId -> Handler RepJson
putPlanR planId = do
  _ <- authedPlan planId
  editedPlan <- parseJsonBody_ -- TODO error page is HTML, not friendly!
  (_, mCurrentPlan) <- runDB $ updatePlan editedPlan planId
  case mCurrentPlan of
    Just plan -> jsonToRepJson $ Entity planId plan
    Nothing -> notFound


deletePlanR :: PlanId -> Handler RepJson
deletePlanR planId = do
  _ <- authedPlan planId
  runDB $ delete planId
  jsonToRepJson $ object ["deleted" .= True]



userPlansSince :: UserId -> UTCTime -> Handler [Entity Plan]
userPlansSince userId horizon = runDB $ selectUserPlansSince userId horizon [Desc PlanCreatedAt, Desc PlanDoneAt]


authedPlan :: PlanId -> Handler (Entity Plan)
authedPlan planId = do
  userId <- requireNgAuthId
  maybeAuthedPlan <- runDB $ selectFirst [PlanId ==. planId, PlanUser ==. userId] []
  case maybeAuthedPlan of
    Just plan -> return plan
    Nothing -> notFound
