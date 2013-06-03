module Handler.Plans where

import Import
import Util
import Util.Angular

postPlansR :: Handler RepJson
postPlansR = do
  userId <- requireAuthIdPreventingXsrf
  newPlan <- parseJsonBody_ -- TODO error page is HTML, not friendly!
  planEntity <- runDB $ createPlan userId newPlan
  jsonToRepJson planEntity


postCompletePlanR :: PlanId -> Handler RepJson
postCompletePlanR planId = do
  Entity _ plan <- authedPlan planId
  time <- now
  runDB $ update planId [PlanDoneAt =. Just time]
  jsonToRepJson $ Entity planId plan { planDoneAt = Just time }



authedPlan :: PlanId -> Handler (Entity Plan)
authedPlan planId = do
  userId <- requireAuthIdPreventingXsrf
  maybeAuthedPlan <- runDB $ selectFirst [PlanId ==. planId, PlanUser ==. userId] []
  case maybeAuthedPlan of
    Just plan -> return plan
    Nothing -> notFound
