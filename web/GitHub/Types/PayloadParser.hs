{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module GitHub.Types.PayloadParser where

import           Data.Aeson          (FromJSON (..), ToJSON (..))
import           Data.Aeson.Types    (Parser, Value (..))
import qualified Data.List           as List
import           Data.Monoid         ((<>))
import           Data.Text           (Text)
import qualified Data.Text           as Text

import           GitHub.Types.Events


data Payload
    = CommitCommentEventPayload            CommitCommentEvent
    | CreateEventPayload                   CreateEvent
    | DeleteEventPayload                   DeleteEvent
    | DeploymentEventPayload               DeploymentEvent
    | DeploymentStatusEventPayload         DeploymentStatusEvent
    | ForkEventPayload                     ForkEvent
    | GollumEventPayload                   GollumEvent
    | IssueCommentEventPayload             IssueCommentEvent
    | IssuesEventPayload                   IssuesEvent
    | MemberEventPayload                   MemberEvent
    | MembershipEventPayload               MembershipEvent
    | MilestoneEventPayload                MilestoneEvent
    | OrganizationEventPayload             OrganizationEvent
    | PingEventPayload                     PingEvent
    | PullRequestEventPayload              PullRequestEvent
    | PullRequestReviewCommentEventPayload PullRequestReviewCommentEvent
    | PullRequestReviewEventPayload        PullRequestReviewEvent
    | PushEventPayload                     PushEvent
    | ReleaseEventPayload                  ReleaseEvent
    | StatusEventPayload                   StatusEvent
    | WatchEventPayload                    WatchEvent
    deriving (Eq, Show, Read)


instance ToJSON Payload where
    toJSON (CommitCommentEventPayload            x) = toJSON x
    toJSON (CreateEventPayload                   x) = toJSON x
    toJSON (DeleteEventPayload                   x) = toJSON x
    toJSON (DeploymentEventPayload               x) = toJSON x
    toJSON (DeploymentStatusEventPayload         x) = toJSON x
    toJSON (ForkEventPayload                     x) = toJSON x
    toJSON (GollumEventPayload                   x) = toJSON x
    toJSON (IssueCommentEventPayload             x) = toJSON x
    toJSON (IssuesEventPayload                   x) = toJSON x
    toJSON (MemberEventPayload                   x) = toJSON x
    toJSON (MembershipEventPayload               x) = toJSON x
    toJSON (MilestoneEventPayload                x) = toJSON x
    toJSON (OrganizationEventPayload             x) = toJSON x
    toJSON (PingEventPayload                     x) = toJSON x
    toJSON (PullRequestEventPayload              x) = toJSON x
    toJSON (PullRequestReviewCommentEventPayload x) = toJSON x
    toJSON (PullRequestReviewEventPayload        x) = toJSON x
    toJSON (PushEventPayload                     x) = toJSON x
    toJSON (ReleaseEventPayload                  x) = toJSON x
    toJSON (StatusEventPayload                   x) = toJSON x
    toJSON (WatchEventPayload                    x) = toJSON x


data PayloadParser = PayloadParser
    { payloadParserEventName   :: Text
    , payloadParserWebhookName :: Text
    , payloadParser            :: Value -> Parser Payload
    }


payloadParsers :: [PayloadParser]
payloadParsers = map (\(e, w, p) -> PayloadParser e w p)
    [ ( "CommitCommentEvent"           , "commit_comment"             , fmap CommitCommentEventPayload            . parseJSON)
    , ( "CreateEvent"                  , "create"                     , fmap CreateEventPayload                   . parseJSON)
    , ( "DeleteEvent"                  , "delete"                     , fmap DeleteEventPayload                   . parseJSON)
    , ( "DeploymentEvent"              , "deployment"                 , fmap DeploymentEventPayload               . parseJSON)
    , ( "DeploymentStatusEvent"        , "deployment_status"          , fmap DeploymentStatusEventPayload         . parseJSON)
    , ( "ForkEvent"                    , "fork"                       , fmap ForkEventPayload                     . parseJSON)
    , ( "GollumEvent"                  , "gollum"                     , fmap GollumEventPayload                   . parseJSON)
    , ( "IssueCommentEvent"            , "issue_comment"              , fmap IssueCommentEventPayload             . parseJSON)
    , ( "IssuesEvent"                  , "issues"                     , fmap IssuesEventPayload                   . parseJSON)
    , ( "MemberEvent"                  , "member"                     , fmap MemberEventPayload                   . parseJSON)
    , ( "MembershipEvent"              , "membership"                 , fmap MembershipEventPayload               . parseJSON)
    , ( "MilestoneEvent"               , "milestone"                  , fmap MilestoneEventPayload                . parseJSON)
    , ( "OrganizationEvent"            , "organization"               , fmap OrganizationEventPayload             . parseJSON)
    , ( "PingEvent"                    , "ping"                       , fmap PingEventPayload                     . parseJSON)
    , ( "PullRequestEvent"             , "pull_request"               , fmap PullRequestEventPayload              . parseJSON)
    , ( "PullRequestReviewCommentEvent", "pull_request_review_comment", fmap PullRequestReviewCommentEventPayload . parseJSON)
    , ( "PullRequestReviewEvent"       , "pull_request_review"        , fmap PullRequestReviewEventPayload        . parseJSON)
    , ( "PushEvent"                    , "push"                       , fmap PushEventPayload                     . parseJSON)
    , ( "ReleaseEvent"                 , "release"                    , fmap ReleaseEventPayload                  . parseJSON)
    , ( "StatusEvent"                  , "status"                     , fmap StatusEventPayload                   . parseJSON)
    , ( "WatchEvent"                   , "watch"                      , fmap WatchEventPayload                    . parseJSON)
    ]



eventPayloadParser :: Text -> Value -> Parser Payload
eventPayloadParser eventType x =
  case List.find ((== eventType) . payloadParserEventName) payloadParsers of
    Nothing -> fail $ "eventPayloadParser: unknown event type: " <> Text.unpack eventType
    Just p  -> payloadParser p x

-- | Since the event type is included through different means (X-GitHub-Event
-- header, or inline in the JSON object), it's not possible to make 'Event'
-- an instance of 'FromJSON'. But if you know the type, you can use this
-- parser.
webhookPayloadParser :: Text -> Value -> Parser Payload
webhookPayloadParser eventType x =
  case List.find ((== eventType) . payloadParserWebhookName) payloadParsers of
    Nothing -> fail $ "webhookPayloadParser: unknown event type: " <> Text.unpack eventType
    Just p  -> payloadParser p x
