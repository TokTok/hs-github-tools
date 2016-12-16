{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module GitHub.Types.Events where


import           Control.Applicative ((<$>), (<*>))

import           Data.Aeson          (FromJSON (..), ToJSON (..), object)
import           Data.Aeson.Types    (Parser, Value (..), (.:), (.:?), (.=))
import qualified Data.List           as List
import           Data.Monoid         ((<>))
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Data.Time           (UTCTime)

import           GitHub.Types.Base



-- | All events which can be produced by GitHub.
--
-- See https://developer.github.com/v3/activity/events/
data Event = Event
    { eventId        :: !Text
    , eventActor     :: !Actor
    , eventRepo      :: !Repo
    , eventCreatedAt :: !UTCTime
    , eventPublic    :: !Bool
    , eventPayload   :: !Payload
    } deriving (Eq, Show, Read)

instance FromJSON Event where
    parseJSON (Object o) = do
        eventType <- o .: "type"

        Event
            <$> o .: "id"
            <*> o .: "actor"
            <*> o .: "repo"
            <*> o .: "created_at"
            <*> o .: "public"
            <*> (eventPayloadParser eventType =<< o .: "payload")

    parseJSON _ = fail "Event"


data Actor = Actor
    { actorId    :: !Integer
    , actorLogin :: !Text
    } deriving (Eq, Show, Read)

instance FromJSON Actor where
    parseJSON (Object o) = Actor
        <$> o .: "id"
        <*> o .: "login"

    parseJSON _ = fail "Actor"


data Repo = Repo
    { repoId   :: !Integer
    , repoName :: !Text
    } deriving (Eq, Show, Read)

instance FromJSON Repo where
    parseJSON (Object o) = Repo
        <$> o .: "id"
        <*> o .: "name"

    parseJSON _ = fail "Repo"


data Payload
    = CommitCommentEventPayload            CommitCommentEvent
    | DeploymentEventPayload               DeploymentEvent
    | DeploymentStatusEventPayload         DeploymentStatusEvent
    | PushEventPayload                     PushEvent
    | IssuesEventPayload                   IssuesEvent
    | IssueCommentEventPayload             IssueCommentEvent
    | CreateEventPayload                   CreateEvent
    | PullRequestEventPayload              PullRequestEvent
    | PullRequestReviewEventPayload        PullRequestReviewEvent
    | PullRequestReviewCommentEventPayload PullRequestReviewCommentEvent
    | WatchEventPayload                    WatchEvent
    | DeleteEventPayload                   DeleteEvent
    | ForkEventPayload                     ForkEvent
    | ReleaseEventPayload                  ReleaseEvent
    | GollumEventPayload                   GollumEvent
    | MemberEventPayload                   MemberEvent
    | PublicEventPayload                   Value
    | StatusEventPayload                   StatusEvent
    deriving (Eq, Show, Read)


instance ToJSON Payload where
    toJSON (CommitCommentEventPayload            x) = toJSON x
    toJSON (DeploymentEventPayload               x) = toJSON x
    toJSON (DeploymentStatusEventPayload         x) = toJSON x
    toJSON (PushEventPayload                     x) = toJSON x
    toJSON (IssuesEventPayload                   x) = toJSON x
    toJSON (IssueCommentEventPayload             x) = toJSON x
    toJSON (CreateEventPayload                   x) = toJSON x
    toJSON (PullRequestEventPayload              x) = toJSON x
    toJSON (PullRequestReviewEventPayload        x) = toJSON x
    toJSON (PullRequestReviewCommentEventPayload x) = toJSON x
    toJSON (WatchEventPayload                    x) = toJSON x
    toJSON (DeleteEventPayload                   x) = toJSON x
    toJSON (ForkEventPayload                     x) = toJSON x
    toJSON (ReleaseEventPayload                  x) = toJSON x
    toJSON (GollumEventPayload                   x) = toJSON x
    toJSON (MemberEventPayload                   x) = toJSON x
    toJSON (PublicEventPayload                   x) = toJSON x
    toJSON (StatusEventPayload                   x) = toJSON x


eventPayloadParsers :: [(Text, Text, Value -> Parser Payload)]
eventPayloadParsers =
    [ ( "CommitCommentEvent", "commit_comment"
      , fmap CommitCommentEventPayload . parseJSON)

    , ( "DeploymentEvent", "deployment"
      , fmap DeploymentEventPayload . parseJSON)

    , ( "DeploymentStatusEvent", "deployment_status"
      , fmap DeploymentStatusEventPayload . parseJSON)

    , ( "PushEvent", "push"
      , fmap PushEventPayload . parseJSON)

    , ( "IssuesEvent", "issues"
      , fmap IssuesEventPayload . parseJSON)

    , ( "IssueCommentEvent", "issue_comment"
      , fmap IssueCommentEventPayload . parseJSON)

    , ( "CreateEvent", "create"
      , fmap CreateEventPayload . parseJSON)

    , ( "PullRequestEvent", "pull_request"
      , fmap PullRequestEventPayload . parseJSON)

    , ( "PullRequestReviewEvent", "pull_request_review"
      , fmap PullRequestReviewEventPayload . parseJSON)

    , ( "PullRequestReviewCommentEvent", "pull_request_review_comment"
      , fmap PullRequestReviewCommentEventPayload . parseJSON)

    , ( "WatchEvent", "watch"
      , fmap WatchEventPayload . parseJSON)

    , ( "DeleteEvent", "delete"
      , fmap DeleteEventPayload . parseJSON)

    , ( "ForkEvent", "fork"
      , fmap ForkEventPayload . parseJSON)

    , ( "ReleaseEvent", "release"
      , fmap ReleaseEventPayload . parseJSON)

    , ( "GollumEvent", "gollum"
      , fmap GollumEventPayload . parseJSON)

    , ( "MemberEvent", "member"
      , fmap MemberEventPayload . parseJSON)

    , ( "PublicEvent", "public"
      , fmap PublicEventPayload . parseJSON)

    , ( "StatusEvent", "status"
      , fmap StatusEventPayload . parseJSON)
    ]



eventPayloadParser :: Text -> Value -> Parser Payload
eventPayloadParser eventType x =
  case List.find (\(t, _, _) -> t == eventType) eventPayloadParsers of
    Nothing -> fail $ "eventPayloadParser: Unknown event type: " <> Text.unpack eventType
    Just (_, _, p) -> p x

-- | Since the event type is included through different means (X-GitHub-Event
-- header, or inline in the JSON object), it's not possible to make 'Event'
-- an instance of 'FromJSON'. But if you know the type, you can use this
-- parser.
webhookPayloadParser :: Text -> Value -> Parser Payload
webhookPayloadParser eventType x =
  case List.find (\(_, t, _) -> t == eventType) eventPayloadParsers of
    Nothing -> fail $ "webhookPayloadParser: Unknown event type: " <> Text.unpack eventType
    Just (_, _, p) -> p x


------------------------------------------------------------------------------
-- CommitCommentEvent

data CommitCommentEvent = CommitCommentEvent
    { commitCommentEventAction       :: Text
    , commitCommentEventComment      :: CommitComment
    , commitCommentEventRepository   :: Repository
    , commitCommentEventSender       :: User
    , commitCommentEventOrganization :: Organization
    } deriving (Eq, Show, Read)

instance FromJSON CommitCommentEvent where
    parseJSON (Object x) = CommitCommentEvent
        <$> x .: "action"
        <*> x .: "comment"
        <*> x .: "repository"
        <*> x .: "sender"
        <*> x .: "organization"

    parseJSON _ = fail "CommitCommentEvent"

instance ToJSON CommitCommentEvent where
    toJSON CommitCommentEvent{..} = object
        [ "action"       .= commitCommentEventAction
        , "comment"      .= commitCommentEventComment
        , "repository"   .= commitCommentEventRepository
        , "sender"       .= commitCommentEventSender
        , "organization" .= commitCommentEventOrganization
        ]


------------------------------------------------------------------------------
-- DeploymentEvent

data DeploymentEvent = DeploymentEvent
    { deploymentEventDeployment   :: Deployment
    , deploymentEventRepository   :: Repository
    , deploymentEventSender       :: User
    , deploymentEventOrganization :: Organization
    } deriving (Eq, Show, Read)

instance FromJSON DeploymentEvent where
    parseJSON (Object x) = DeploymentEvent
        <$> x .: "deployment"
        <*> x .: "repository"
        <*> x .: "sender"
        <*> x .: "organization"

    parseJSON _ = fail "DeploymentEvent"

instance ToJSON DeploymentEvent where
    toJSON DeploymentEvent{..} = object
        [ "deployment"   .= deploymentEventDeployment
        , "repository"   .= deploymentEventRepository
        , "sender"       .= deploymentEventSender
        , "organization" .= deploymentEventOrganization
        ]


------------------------------------------------------------------------------
-- DeploymentStatusEvent

data DeploymentStatusEvent = DeploymentStatusEvent
    { deploymentStatusEventDeploymentStatus :: DeploymentStatus
    , deploymentStatusEventDeployment       :: Deployment
    , deploymentStatusEventRepository       :: Repository
    , deploymentStatusEventOrganization     :: Organization
    , deploymentStatusEventSender           :: User
    } deriving (Eq, Show, Read)

instance FromJSON DeploymentStatusEvent where
    parseJSON (Object x) = DeploymentStatusEvent
        <$> x .: "deployment_status"
        <*> x .: "deployment"
        <*> x .: "repository"
        <*> x .: "organization"
        <*> x .: "sender"

    parseJSON _ = fail "DeploymentStatusEvent"

instance ToJSON DeploymentStatusEvent where
    toJSON DeploymentStatusEvent{..} = object
        [ "deployment_status"  .= deploymentStatusEventDeploymentStatus
        , "deployment"         .= deploymentStatusEventDeployment
        , "repository"         .= deploymentStatusEventRepository
        , "organization"       .= deploymentStatusEventOrganization
        , "sender"             .= deploymentStatusEventSender
        ]


------------------------------------------------------------------------------
-- PushEvent

data PushEvent = PushEvent
    { pushEventRef             :: Text
    , pushEventBefore          :: Text
    , pushEventAfter           :: Text
    , pushEventCreated         :: Bool
    , pushEventDeleted         :: Bool
    , pushEventForced          :: Bool
    , pushEventRefName         :: Maybe Text
    , pushEventBaseRef         :: Maybe Text
    , pushEventCompare         :: Text
    , pushEventDistinctCommits :: Maybe [PushCommit]
    , pushEventCommits         :: [PushCommit]
    , pushEventHeadCommit      :: Maybe PushCommit
    , pushEventRepository      :: Repository
    , pushEventPusher          :: Pusher
    , pushEventOrganization    :: Organization
    , pushEventSender          :: User
    } deriving (Eq, Show, Read)

instance FromJSON PushEvent where
    parseJSON (Object x) = PushEvent
        <$> x .: "ref"
        <*> x .: "before"
        <*> x .: "after"
        <*> x .: "created"
        <*> x .: "deleted"
        <*> x .: "forced"
        <*> x .:? "ref_name"
        <*> x .: "base_ref"
        <*> x .: "compare"
        <*> x .:? "distinct_commits"
        <*> x .: "commits"
        <*> x .: "head_commit"
        <*> x .: "repository"
        <*> x .: "pusher"
        <*> x .: "organization"
        <*> x .: "sender"

    parseJSON _ = fail "PushEvent"

instance ToJSON PushEvent where
    toJSON PushEvent{..} = object
        [ "ref"              .= pushEventRef
        , "before"           .= pushEventBefore
        , "after"            .= pushEventAfter
        , "created"          .= pushEventCreated
        , "deleted"          .= pushEventDeleted
        , "forced"           .= pushEventForced
        , "ref_name"         .= pushEventRefName
        , "base_ref"         .= pushEventBaseRef
        , "compare"          .= pushEventCompare
        , "distinct_commits" .= pushEventDistinctCommits
        , "commits"          .= pushEventCommits
        , "head_commit"      .= pushEventHeadCommit
        , "repository"       .= pushEventRepository
        , "pusher"           .= pushEventPusher
        , "organization"     .= pushEventOrganization
        , "sender"           .= pushEventSender
        ]


------------------------------------------------------------------------------
-- IssuesEvent

data IssuesEvent = IssuesEvent
    { issuesEventRepository   :: Repository
    , issuesEventSender       :: User
    , issuesEventAction       :: Text
    , issuesEventOrganization :: Organization
    , issuesEventIssue        :: Issue
    } deriving (Eq, Show, Read)

instance FromJSON IssuesEvent where
    parseJSON (Object x) = IssuesEvent
        <$> x .: "repository"
        <*> x .: "sender"
        <*> x .: "action"
        <*> x .: "organization"
        <*> x .: "issue"

    parseJSON _ = fail "IssuesEvent"

instance ToJSON IssuesEvent where
    toJSON IssuesEvent{..} = object
        [ "repository"   .= issuesEventRepository
        , "sender"       .= issuesEventSender
        , "action"       .= issuesEventAction
        , "organization" .= issuesEventOrganization
        , "issue"        .= issuesEventIssue
        ]


------------------------------------------------------------------------------
-- IssueCommentEvent

data IssueCommentEvent = IssueCommentEvent
    { issueCommentEventRepository   :: Repository
    , issueCommentEventSender       :: User
    , issueCommentEventAction       :: Text
    , issueCommentEventComment      :: IssueComment
    , issueCommentEventOrganization :: Organization
    , issueCommentEventIssue        :: Issue
    } deriving (Eq, Show, Read)

instance FromJSON IssueCommentEvent where
    parseJSON (Object x) = IssueCommentEvent
        <$> x .: "repository"
        <*> x .: "sender"
        <*> x .: "action"
        <*> x .: "comment"
        <*> x .: "organization"
        <*> x .: "issue"

    parseJSON _ = fail "IssueCommentEvent"

instance ToJSON IssueCommentEvent where
    toJSON IssueCommentEvent{..} = object
        [ "repository"   .= issueCommentEventRepository
        , "sender"       .= issueCommentEventSender
        , "action"       .= issueCommentEventAction
        , "comment"      .= issueCommentEventComment
        , "organization" .= issueCommentEventOrganization
        , "issue"        .= issueCommentEventIssue
        ]


------------------------------------------------------------------------------
-- CreateEvent

data CreateEvent = CreateEvent
    { createEventRef :: !(Maybe Text)
    } deriving (Eq, Show, Read)

instance FromJSON CreateEvent where
    parseJSON (Object x) = CreateEvent
        <$> x .: "ref"

    parseJSON _ = fail "CreateEvent"

instance ToJSON CreateEvent where
    toJSON CreateEvent{..} = object
        [ "ref"      .= createEventRef
        ]


------------------------------------------------------------------------------
-- PullRequestEvent

data PullRequestEvent = PullRequestEvent
    { pullRequestEventAfter        :: Maybe Text
    , pullRequestEventRepository   :: Repository
    , pullRequestEventChanges      :: Maybe Changes
    , pullRequestEventSender       :: User
    , pullRequestEventPullRequest  :: PullRequest
    , pullRequestEventAction       :: Text
    , pullRequestEventAssignee     :: Maybe User
    , pullRequestEventNumber       :: Int
    , pullRequestEventOrganization :: Organization
    , pullRequestEventBefore       :: Maybe Text
    } deriving (Eq, Show, Read)

instance FromJSON PullRequestEvent where
    parseJSON (Object x) = PullRequestEvent
        <$> x .:? "after"
        <*> x .: "repository"
        <*> x .:? "changes"
        <*> x .: "sender"
        <*> x .: "pull_request"
        <*> x .: "action"
        <*> x .:? "assignee"
        <*> x .: "number"
        <*> x .: "organization"
        <*> x .:? "before"

    parseJSON _ = fail "PullRequestEvent"

instance ToJSON PullRequestEvent where
    toJSON PullRequestEvent{..} = object $
        [ "repository"   .= pullRequestEventRepository
        , "changes"      .= pullRequestEventChanges
        , "sender"       .= pullRequestEventSender
        , "pull_request" .= pullRequestEventPullRequest
        , "action"       .= pullRequestEventAction
        , "assignee"     .= pullRequestEventAssignee
        , "number"       .= pullRequestEventNumber
        , "organization" .= pullRequestEventOrganization
        , "before"       .= pullRequestEventBefore
        , "after"        .= pullRequestEventAfter
        ]


------------------------------------------------------------------------------
-- PullRequestReviewEvent

data PullRequestReviewEvent = PullRequestReviewEvent
    { pullRequestReviewEventAction       :: Text
    , pullRequestReviewEventReview       :: Review
    , pullRequestReviewEventPullRequest  :: SimplePullRequest
    , pullRequestReviewEventRepository   :: Repository
    , pullRequestReviewEventOrganization :: Organization
    , pullRequestReviewEventSender       :: User
    } deriving (Eq, Show, Read)

instance FromJSON PullRequestReviewEvent where
    parseJSON (Object x) = PullRequestReviewEvent
        <$> x .: "action"
        <*> x .: "review"
        <*> x .: "pull_request"
        <*> x .: "repository"
        <*> x .: "organization"
        <*> x .: "sender"

    parseJSON _ = fail "PullRequestReviewEvent"

instance ToJSON PullRequestReviewEvent where
    toJSON PullRequestReviewEvent{..} = object $
        [ "action"       .= pullRequestReviewEventAction
        , "review"       .= pullRequestReviewEventReview
        , "pull_request" .= pullRequestReviewEventPullRequest
        , "repository"   .= pullRequestReviewEventRepository
        , "organization" .= pullRequestReviewEventOrganization
        , "sender"       .= pullRequestReviewEventSender
        ]


------------------------------------------------------------------------------
-- PullRequestReviewCommentEventPayload

data PullRequestReviewCommentEvent = PullRequestReviewCommentEvent
    { pullRequestReviewCommentEventPullRequest :: !Value
    } deriving (Eq, Show, Read)

instance FromJSON PullRequestReviewCommentEvent where
    parseJSON (Object x) = PullRequestReviewCommentEvent
        <$> x .: "pull_request"

    parseJSON _ = fail "PullRequestReviewCommentEvent"

instance ToJSON PullRequestReviewCommentEvent where
    toJSON PullRequestReviewCommentEvent{..} = object
        [ "pull_request"      .= pullRequestReviewCommentEventPullRequest
        ]



------------------------------------------------------------------------------
-- WatchEvent

data WatchEvent = WatchEvent
    { watchEventRepository   :: Repository
    , watchEventSender       :: User
    , watchEventAction       :: Text
    , watchEventOrganization :: Organization
    } deriving (Eq, Show, Read)

instance FromJSON WatchEvent where
    parseJSON (Object x) = WatchEvent
        <$> x .: "repository"
        <*> x .: "sender"
        <*> x .: "action"
        <*> x .: "organization"

    parseJSON _ = fail "WatchEvent"

instance ToJSON WatchEvent where
    toJSON WatchEvent{..} = object
        [ "repository"   .= watchEventRepository
        , "sender"       .= watchEventSender
        , "action"       .= watchEventAction
        , "organization" .= watchEventOrganization
        ]


------------------------------------------------------------------------------
-- DeleteEvent

data DeleteEvent = DeleteEvent
    { deleteEventRef :: !Text
    } deriving (Eq, Show, Read)

instance FromJSON DeleteEvent where
    parseJSON (Object x) = DeleteEvent
        <$> x .: "ref"

    parseJSON _ = fail "DeleteEvent"

instance ToJSON DeleteEvent where
    toJSON DeleteEvent{..} = object
        [ "ref"      .= deleteEventRef
        ]


------------------------------------------------------------------------------
-- ForkEvent

data ForkEvent = ForkEvent
    { forkEventForkee       :: Repository
    , forkEventRepository   :: Repository
    , forkEventOrganization :: Organization
    , forkEventSender       :: User
    } deriving (Eq, Show, Read)

instance FromJSON ForkEvent where
    parseJSON (Object x) = ForkEvent
        <$> x .: "forkee"
        <*> x .: "repository"
        <*> x .: "organization"
        <*> x .: "sender"

    parseJSON _ = fail "ForkEvent"

instance ToJSON ForkEvent where
    toJSON ForkEvent{..} = object
        [ "forkee"       .= forkEventForkee
        , "repository"   .= forkEventRepository
        , "organization" .= forkEventOrganization
        , "sender"       .= forkEventSender
        ]


------------------------------------------------------------------------------
-- ReleaseEvent

data ReleaseEvent = ReleaseEvent
    { releaseEventAction :: !Text
    } deriving (Eq, Show, Read)

instance FromJSON ReleaseEvent where
    parseJSON (Object x) = ReleaseEvent
        <$> x .: "action"

    parseJSON _ = fail "ForkEvent"

instance ToJSON ReleaseEvent where
    toJSON ReleaseEvent{..} = object
        [ "action"      .= releaseEventAction
        ]


------------------------------------------------------------------------------
-- GollumEvent

data GollumEvent = GollumEvent
    { gollumEventPages :: !Value
    } deriving (Eq, Show, Read)

instance FromJSON GollumEvent where
    parseJSON (Object x) = GollumEvent
        <$> x .: "pages"

    parseJSON _ = fail "GollumEvent"

instance ToJSON GollumEvent where
    toJSON GollumEvent{..} = object
        [ "pages"      .= gollumEventPages
        ]


------------------------------------------------------------------------------
-- MemberEvent

data MemberEvent = MemberEvent
    { memberEventAction :: !Text
    } deriving (Eq, Show, Read)

instance FromJSON MemberEvent where
    parseJSON (Object x) = MemberEvent
        <$> x .: "action"

    parseJSON _ = fail "MemberEvent"

instance ToJSON MemberEvent where
    toJSON MemberEvent{..} = object
        [ "action"      .= memberEventAction
        ]


------------------------------------------------------------------------------
-- StatusEvent

data StatusEvent = StatusEvent
    { statusEventId           :: Int
    , statusEventSha          :: Text
    , statusEventName         :: Text
    , statusEventTargetUrl    :: Text
    , statusEventContext      :: Text
    , statusEventDescription  :: Text
    , statusEventState        :: Text
    , statusEventCommit       :: StatusCommit
    , statusEventBranches     :: [Branch]
    , statusEventCreatedAt    :: DateTime
    , statusEventUpdatedAt    :: DateTime
    , statusEventRepository   :: Repository
    , statusEventOrganization :: Organization
    , statusEventSender       :: User
    } deriving (Eq, Show, Read)

instance FromJSON StatusEvent where
    parseJSON (Object x) = StatusEvent
        <$> x .: "id"
        <*> x .: "sha"
        <*> x .: "name"
        <*> x .: "target_url"
        <*> x .: "context"
        <*> x .: "description"
        <*> x .: "state"
        <*> x .: "commit"
        <*> x .: "branches"
        <*> x .: "created_at"
        <*> x .: "updated_at"
        <*> x .: "repository"
        <*> x .: "organization"
        <*> x .: "sender"

    parseJSON _ = fail "StatusEvent"

instance ToJSON StatusEvent where
    toJSON StatusEvent{..} = object
        [ "id"           .= statusEventId
        , "sha"          .= statusEventSha
        , "name"         .= statusEventName
        , "target_url"   .= statusEventTargetUrl
        , "context"      .= statusEventContext
        , "description"  .= statusEventDescription
        , "state"        .= statusEventState
        , "commit"       .= statusEventCommit
        , "branches"     .= statusEventBranches
        , "created_at"   .= statusEventCreatedAt
        , "updated_at"   .= statusEventUpdatedAt
        , "repository"   .= statusEventRepository
        , "organization" .= statusEventOrganization
        , "sender"       .= statusEventSender
        ]
