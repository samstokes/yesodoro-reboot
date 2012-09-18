module Handler.Plans where

import Forms
import Import
import Util
import Yesod.Auth (requireAuthId)

postPlansR :: Handler RepHtml
postPlansR = do
  userId <- requireAuthId

  ((result, _), _) <- runFormPost newPlanForm
  case result of
    FormSuccess plan -> do
      _ <- runDB $ createPlan userId plan
      redirect TasksR
    _ -> undefined -- TODO

postCompletePlanR :: PlanId -> Handler RepHtml
postCompletePlanR planId = do
  _ <- authedPlan planId
  time <- now
  runDB $ update planId [PlanDoneAt =. Just time]
  redirect TasksR



authedPlan :: PlanId -> Handler Plan
authedPlan planId = do
  userId <- requireAuthId
  maybeAuthedPlan <- runDB $ selectFirst [PlanId ==. planId, PlanUser ==. userId] []
  case maybeAuthedPlan of
    Just plan -> return $ entityVal plan
    Nothing -> redirect TasksR
