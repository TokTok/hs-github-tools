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

import           GitHub.Types.Base



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
    | MilestoneEventPayload                MilestoneEvent
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
    toJSON (MilestoneEventPayload                x) = toJSON x


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

    , ( "MilestoneEvent", "milestone"
      , fmap MilestoneEventPayload . parseJSON)
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
    { commitCommentEventOrganization :: Organization
    , commitCommentEventRepository   :: Repository
    , commitCommentEventSender       :: User

    , commitCommentEventAction       :: Text
    , commitCommentEventComment      :: CommitComment
    } deriving (Eq, Show, Read)

instance FromJSON CommitCommentEvent where
    parseJSON (Object x) = CommitCommentEvent
        <$> x .: "organization"
        <*> x .: "repository"
        <*> x .: "sender"

        <*> x .: "action"
        <*> x .: "comment"

    parseJSON _ = fail "CommitCommentEvent"

instance ToJSON CommitCommentEvent where
    toJSON CommitCommentEvent{..} = object
        [ "organization" .= commitCommentEventOrganization
        , "repository"   .= commitCommentEventRepository
        , "sender"       .= commitCommentEventSender

        , "action"       .= commitCommentEventAction
        , "comment"      .= commitCommentEventComment
        ]


------------------------------------------------------------------------------
-- DeploymentEvent

data DeploymentEvent = DeploymentEvent
    { deploymentEventOrganization :: Organization
    , deploymentEventRepository   :: Repository
    , deploymentEventSender       :: User

    , deploymentEventDeployment   :: Deployment
    } deriving (Eq, Show, Read)

instance FromJSON DeploymentEvent where
    parseJSON (Object x) = DeploymentEvent
        <$> x .: "organization"
        <*> x .: "repository"
        <*> x .: "sender"

        <*> x .: "deployment"

    parseJSON _ = fail "DeploymentEvent"

instance ToJSON DeploymentEvent where
    toJSON DeploymentEvent{..} = object
        [ "organization" .= deploymentEventOrganization
        , "repository"   .= deploymentEventRepository
        , "sender"       .= deploymentEventSender

        , "deployment"   .= deploymentEventDeployment
        ]


------------------------------------------------------------------------------
-- DeploymentStatusEvent

data DeploymentStatusEvent = DeploymentStatusEvent
    { deploymentStatusEventOrganization     :: Organization
    , deploymentStatusEventRepository       :: Repository
    , deploymentStatusEventSender           :: User

    , deploymentStatusEventDeployment       :: Deployment
    , deploymentStatusEventDeploymentStatus :: DeploymentStatus
    } deriving (Eq, Show, Read)

instance FromJSON DeploymentStatusEvent where
    parseJSON (Object x) = DeploymentStatusEvent
        <$> x .: "organization"
        <*> x .: "repository"
        <*> x .: "sender"

        <*> x .: "deployment"
        <*> x .: "deployment_status"

    parseJSON _ = fail "DeploymentStatusEvent"

instance ToJSON DeploymentStatusEvent where
    toJSON DeploymentStatusEvent{..} = object
        [ "organization"       .= deploymentStatusEventOrganization
        , "repository"         .= deploymentStatusEventRepository
        , "sender"             .= deploymentStatusEventSender

        , "deployment"         .= deploymentStatusEventDeployment
        , "deployment_status"  .= deploymentStatusEventDeploymentStatus
        ]


------------------------------------------------------------------------------
-- PushEvent

data PushEvent = PushEvent
    { pushEventOrganization    :: Organization
    , pushEventRepository      :: Repository
    , pushEventSender          :: User

    , pushEventAfter           :: Text
    , pushEventBaseRef         :: Maybe Text
    , pushEventBefore          :: Text
    , pushEventCommits         :: [PushCommit]
    , pushEventCompare         :: Text
    , pushEventCreated         :: Bool
    , pushEventDeleted         :: Bool
    , pushEventDistinctCommits :: Maybe [PushCommit]
    , pushEventForced          :: Bool
    , pushEventHeadCommit      :: Maybe PushCommit
    , pushEventPusher          :: Pusher
    , pushEventRef             :: Text
    , pushEventRefName         :: Maybe Text
    } deriving (Eq, Show, Read)

instance FromJSON PushEvent where
    parseJSON (Object x) = PushEvent
        <$> x .: "organization"
        <*> x .: "repository"
        <*> x .: "sender"

        <*> x .: "after"
        <*> x .: "base_ref"
        <*> x .: "before"
        <*> x .: "commits"
        <*> x .: "compare"
        <*> x .: "created"
        <*> x .: "deleted"
        <*> x .:? "distinct_commits"
        <*> x .: "forced"
        <*> x .: "head_commit"
        <*> x .: "pusher"
        <*> x .: "ref"
        <*> x .:? "ref_name"

    parseJSON _ = fail "PushEvent"

instance ToJSON PushEvent where
    toJSON PushEvent{..} = object
        [ "organization"     .= pushEventOrganization
        , "repository"       .= pushEventRepository
        , "sender"           .= pushEventSender

        , "after"            .= pushEventAfter
        , "base_ref"         .= pushEventBaseRef
        , "before"           .= pushEventBefore
        , "commits"          .= pushEventCommits
        , "compare"          .= pushEventCompare
        , "created"          .= pushEventCreated
        , "deleted"          .= pushEventDeleted
        , "distinct_commits" .= pushEventDistinctCommits
        , "forced"           .= pushEventForced
        , "head_commit"      .= pushEventHeadCommit
        , "pusher"           .= pushEventPusher
        , "ref"              .= pushEventRef
        , "ref_name"         .= pushEventRefName
        ]


------------------------------------------------------------------------------
-- IssuesEvent

data IssuesEvent = IssuesEvent
    { issuesEventOrganization :: Organization
    , issuesEventRepository   :: Repository
    , issuesEventSender       :: User

    , issuesEventAction       :: Text
    , issuesEventIssue        :: Issue
    } deriving (Eq, Show, Read)

instance FromJSON IssuesEvent where
    parseJSON (Object x) = IssuesEvent
        <$> x .: "organization"
        <*> x .: "repository"
        <*> x .: "sender"

        <*> x .: "action"
        <*> x .: "issue"

    parseJSON _ = fail "IssuesEvent"

instance ToJSON IssuesEvent where
    toJSON IssuesEvent{..} = object
        [ "organization" .= issuesEventOrganization
        , "repository"   .= issuesEventRepository
        , "sender"       .= issuesEventSender

        , "action"       .= issuesEventAction
        , "issue"        .= issuesEventIssue
        ]


------------------------------------------------------------------------------
-- IssueCommentEvent

data IssueCommentEvent = IssueCommentEvent
    { issueCommentEventOrganization :: Organization
    , issueCommentEventRepository   :: Repository
    , issueCommentEventSender       :: User

    , issueCommentEventAction       :: Text
    , issueCommentEventComment      :: IssueComment
    , issueCommentEventIssue        :: Issue
    } deriving (Eq, Show, Read)

instance FromJSON IssueCommentEvent where
    parseJSON (Object x) = IssueCommentEvent
        <$> x .: "organization"
        <*> x .: "repository"
        <*> x .: "sender"

        <*> x .: "action"
        <*> x .: "comment"
        <*> x .: "issue"

    parseJSON _ = fail "IssueCommentEvent"

instance ToJSON IssueCommentEvent where
    toJSON IssueCommentEvent{..} = object
        [ "organization" .= issueCommentEventOrganization
        , "repository"   .= issueCommentEventRepository
        , "sender"       .= issueCommentEventSender

        , "action"       .= issueCommentEventAction
        , "comment"      .= issueCommentEventComment
        , "issue"        .= issueCommentEventIssue
        ]


------------------------------------------------------------------------------
-- CreateEvent

data CreateEvent = CreateEvent
    { createEventOrganization :: Organization
    , createEventRepository   :: Repository
    , createEventSender       :: User

    , createEventDescription  :: Text
    , createEventMasterBranch :: Text
    , createEventPusherType   :: Text
    , createEventRef          :: Text
    , createEventRefType      :: Text
    } deriving (Eq, Show, Read)

instance FromJSON CreateEvent where
    parseJSON (Object x) = CreateEvent
        <$> x .: "organization"
        <*> x .: "repository"
        <*> x .: "sender"

        <*> x .: "description"
        <*> x .: "master_branch"
        <*> x .: "pusher_type"
        <*> x .: "ref"
        <*> x .: "ref_type"

    parseJSON _ = fail "CreateEvent"

instance ToJSON CreateEvent where
    toJSON CreateEvent{..} = object
        [ "organization"  .= createEventOrganization
        , "repository"    .= createEventRepository
        , "sender"        .= createEventSender

        , "description"   .= createEventDescription
        , "master_branch" .= createEventMasterBranch
        , "pusher_type"   .= createEventPusherType
        , "ref"           .= createEventRef
        , "ref_type"      .= createEventRefType
        ]


------------------------------------------------------------------------------
-- PullRequestEvent

data PullRequestEvent = PullRequestEvent
    { pullRequestEventOrganization :: Organization
    , pullRequestEventRepository   :: Repository
    , pullRequestEventSender       :: User

    , pullRequestEventAction       :: Text
    , pullRequestEventAfter        :: Maybe Text
    , pullRequestEventAssignee     :: Maybe User
    , pullRequestEventBefore       :: Maybe Text
    , pullRequestEventChanges      :: Maybe Changes
    , pullRequestEventNumber       :: Int
    , pullRequestEventPullRequest  :: PullRequest
    } deriving (Eq, Show, Read)

instance FromJSON PullRequestEvent where
    parseJSON (Object x) = PullRequestEvent
        <$> x .: "organization"
        <*> x .: "repository"
        <*> x .: "sender"

        <*> x .: "action"
        <*> x .:? "after"
        <*> x .:? "assignee"
        <*> x .:? "before"
        <*> x .:? "changes"
        <*> x .: "number"
        <*> x .: "pull_request"

    parseJSON _ = fail "PullRequestEvent"

instance ToJSON PullRequestEvent where
    toJSON PullRequestEvent{..} = object $
        [ "organization" .= pullRequestEventOrganization
        , "repository"   .= pullRequestEventRepository
        , "sender"       .= pullRequestEventSender

        , "action"       .= pullRequestEventAction
        , "after"        .= pullRequestEventAfter
        , "assignee"     .= pullRequestEventAssignee
        , "before"       .= pullRequestEventBefore
        , "changes"      .= pullRequestEventChanges
        , "number"       .= pullRequestEventNumber
        , "pull_request" .= pullRequestEventPullRequest
        ]


------------------------------------------------------------------------------
-- PullRequestReviewEvent

data PullRequestReviewEvent = PullRequestReviewEvent
    { pullRequestReviewEventOrganization :: Organization
    , pullRequestReviewEventRepository   :: Repository
    , pullRequestReviewEventSender       :: User

    , pullRequestReviewEventAction       :: Text
    , pullRequestReviewEventPullRequest  :: SimplePullRequest
    , pullRequestReviewEventReview       :: Review
    } deriving (Eq, Show, Read)

instance FromJSON PullRequestReviewEvent where
    parseJSON (Object x) = PullRequestReviewEvent
        <$> x .: "organization"
        <*> x .: "repository"
        <*> x .: "sender"

        <*> x .: "action"
        <*> x .: "pull_request"
        <*> x .: "review"

    parseJSON _ = fail "PullRequestReviewEvent"

instance ToJSON PullRequestReviewEvent where
    toJSON PullRequestReviewEvent{..} = object $
        [ "organization" .= pullRequestReviewEventOrganization
        , "repository"   .= pullRequestReviewEventRepository
        , "sender"       .= pullRequestReviewEventSender

        , "action"       .= pullRequestReviewEventAction
        , "pull_request" .= pullRequestReviewEventPullRequest
        , "review"       .= pullRequestReviewEventReview
        ]


------------------------------------------------------------------------------
-- PullRequestReviewCommentEventPayload

data PullRequestReviewCommentEvent = PullRequestReviewCommentEvent
    { pullRequestReviewCommentEventOrganization :: Organization
    , pullRequestReviewCommentEventRepository   :: Repository
    , pullRequestReviewCommentEventSender       :: User

    , pullRequestReviewCommentEventAction       :: Text
    , pullRequestReviewCommentEventComment      :: ReviewComment
    , pullRequestReviewCommentEventPullRequest  :: SimplePullRequest
    } deriving (Eq, Show, Read)

instance FromJSON PullRequestReviewCommentEvent where
    parseJSON (Object x) = PullRequestReviewCommentEvent
        <$> x .: "organization"
        <*> x .: "repository"
        <*> x .: "sender"

        <*> x .: "action"
        <*> x .: "comment"
        <*> x .: "pull_request"

    parseJSON _ = fail "PullRequestReviewCommentEvent"

instance ToJSON PullRequestReviewCommentEvent where
    toJSON PullRequestReviewCommentEvent{..} = object
        [ "organization" .= pullRequestReviewCommentEventOrganization
        , "repository"   .= pullRequestReviewCommentEventRepository
        , "sender"       .= pullRequestReviewCommentEventSender

        , "action"       .= pullRequestReviewCommentEventAction
        , "comment"      .= pullRequestReviewCommentEventComment
        , "pull_request" .= pullRequestReviewCommentEventPullRequest
        ]



------------------------------------------------------------------------------
-- WatchEvent

data WatchEvent = WatchEvent
    { watchEventOrganization :: Organization
    , watchEventRepository   :: Repository
    , watchEventSender       :: User

    , watchEventAction       :: Text
    } deriving (Eq, Show, Read)

instance FromJSON WatchEvent where
    parseJSON (Object x) = WatchEvent
        <$> x .: "organization"
        <*> x .: "repository"
        <*> x .: "sender"

        <*> x .: "action"

    parseJSON _ = fail "WatchEvent"

instance ToJSON WatchEvent where
    toJSON WatchEvent{..} = object
        [ "organization" .= watchEventOrganization
        , "repository"   .= watchEventRepository
        , "sender"       .= watchEventSender

        , "action"       .= watchEventAction
        ]


------------------------------------------------------------------------------
-- DeleteEvent

data DeleteEvent = DeleteEvent
    { deleteEventOrganization :: Organization
    , deleteEventRepository   :: Repository
    , deleteEventSender       :: User

    , deleteEventPusherType   :: Text
    , deleteEventRef          :: Text
    , deleteEventRefType      :: Text
    } deriving (Eq, Show, Read)

instance FromJSON DeleteEvent where
    parseJSON (Object x) = DeleteEvent
        <$> x .: "organization"
        <*> x .: "repository"
        <*> x .: "sender"

        <*> x .: "pusher_type"
        <*> x .: "ref"
        <*> x .: "ref_type"

    parseJSON _ = fail "DeleteEvent"

instance ToJSON DeleteEvent where
    toJSON DeleteEvent{..} = object
        [ "organization" .= deleteEventOrganization
        , "repository"   .= deleteEventRepository
        , "sender"       .= deleteEventSender

        , "pusher_type"  .= deleteEventPusherType
        , "ref"          .= deleteEventRef
        , "ref_type"     .= deleteEventRefType
        ]


------------------------------------------------------------------------------
-- ForkEvent

data ForkEvent = ForkEvent
    { forkEventOrganization :: Organization
    , forkEventRepository   :: Repository
    , forkEventSender       :: User

    , forkEventForkee       :: Repository
    } deriving (Eq, Show, Read)

instance FromJSON ForkEvent where
    parseJSON (Object x) = ForkEvent
        <$> x .: "organization"
        <*> x .: "repository"
        <*> x .: "sender"

        <*> x .: "forkee"

    parseJSON _ = fail "ForkEvent"

instance ToJSON ForkEvent where
    toJSON ForkEvent{..} = object
        [ "organization" .= forkEventOrganization
        , "repository"   .= forkEventRepository
        , "sender"       .= forkEventSender

        , "forkee"       .= forkEventForkee
        ]


------------------------------------------------------------------------------
-- ReleaseEvent

data ReleaseEvent = ReleaseEvent
    { releaseEventOrganization :: Organization
    , releaseEventRepository   :: Repository
    , releaseEventSender       :: User

    , releaseEventAction       :: Text
    , releaseEventRelease      :: Release
    } deriving (Eq, Show, Read)

instance FromJSON ReleaseEvent where
    parseJSON (Object x) = ReleaseEvent
        <$> x .: "organization"
        <*> x .: "repository"
        <*> x .: "sender"

        <*> x .: "action"
        <*> x .: "release"

    parseJSON _ = fail "ForkEvent"

instance ToJSON ReleaseEvent where
    toJSON ReleaseEvent{..} = object
        [ "organization" .= releaseEventOrganization
        , "repository"   .= releaseEventRepository
        , "sender"       .= releaseEventSender

        , "action"       .= releaseEventAction
        , "release"      .= releaseEventRelease
        ]


------------------------------------------------------------------------------
-- GollumEvent

data GollumEvent = GollumEvent
    { gollumEventOrganization :: Organization
    , gollumEventRepository   :: Repository
    , gollumEventSender       :: User

    , gollumEventPages        :: Text
    } deriving (Eq, Show, Read)

instance FromJSON GollumEvent where
    parseJSON (Object x) = GollumEvent
        <$> x .: "organization"
        <*> x .: "repository"
        <*> x .: "sender"

        <*> x .: "pages"

    parseJSON _ = fail "GollumEvent"

instance ToJSON GollumEvent where
    toJSON GollumEvent{..} = object
        [ "organization" .= gollumEventOrganization
        , "repository"   .= gollumEventRepository
        , "sender"       .= gollumEventSender

        , "pages"      .= gollumEventPages
        ]


------------------------------------------------------------------------------
-- MemberEvent

data MemberEvent = MemberEvent
    { memberEventOrganization :: Organization
    , memberEventRepository   :: Repository
    , memberEventSender       :: User

    , memberEventAction       :: Text
    } deriving (Eq, Show, Read)

instance FromJSON MemberEvent where
    parseJSON (Object x) = MemberEvent
        <$> x .: "organization"
        <*> x .: "repository"
        <*> x .: "sender"

        <*> x .: "action"

    parseJSON _ = fail "MemberEvent"

instance ToJSON MemberEvent where
    toJSON MemberEvent{..} = object
        [ "organization" .= memberEventOrganization
        , "repository"   .= memberEventRepository
        , "sender"       .= memberEventSender

        , "action"      .= memberEventAction
        ]


------------------------------------------------------------------------------
-- StatusEvent

data StatusEvent = StatusEvent
    { statusEventOrganization :: Organization
    , statusEventRepository   :: Repository
    , statusEventSender       :: User

    , statusEventBranches     :: [Branch]
    , statusEventCommit       :: StatusCommit
    , statusEventContext      :: Text
    , statusEventCreatedAt    :: DateTime
    , statusEventDescription  :: Text
    , statusEventId           :: Int
    , statusEventName         :: Text
    , statusEventSha          :: Text
    , statusEventState        :: Text
    , statusEventTargetUrl    :: Text
    , statusEventUpdatedAt    :: DateTime
    } deriving (Eq, Show, Read)

instance FromJSON StatusEvent where
    parseJSON (Object x) = StatusEvent
        <$> x .: "organization"
        <*> x .: "repository"
        <*> x .: "sender"

        <*> x .: "branches"
        <*> x .: "commit"
        <*> x .: "context"
        <*> x .: "created_at"
        <*> x .: "description"
        <*> x .: "id"
        <*> x .: "name"
        <*> x .: "sha"
        <*> x .: "state"
        <*> x .: "target_url"
        <*> x .: "updated_at"

    parseJSON _ = fail "StatusEvent"

instance ToJSON StatusEvent where
    toJSON StatusEvent{..} = object
        [ "organization" .= statusEventOrganization
        , "repository"   .= statusEventRepository
        , "sender"       .= statusEventSender

        , "branches"     .= statusEventBranches
        , "commit"       .= statusEventCommit
        , "context"      .= statusEventContext
        , "created_at"   .= statusEventCreatedAt
        , "description"  .= statusEventDescription
        , "id"           .= statusEventId
        , "name"         .= statusEventName
        , "sha"          .= statusEventSha
        , "state"        .= statusEventState
        , "target_url"   .= statusEventTargetUrl
        , "updated_at"   .= statusEventUpdatedAt
        ]


------------------------------------------------------------------------------
-- MilestoneEvent

data MilestoneEvent = MilestoneEvent
    { milestoneEventOrganization :: Organization
    , milestoneEventRepository   :: Repository
    , milestoneEventSender       :: User

    , milestoneEventAction       :: Text
    , milestoneEventMilestone    :: Milestone
    } deriving (Eq, Show, Read)

instance FromJSON MilestoneEvent where
    parseJSON (Object x) = MilestoneEvent
        <$> x .: "organization"
        <*> x .: "repository"
        <*> x .: "sender"

        <*> x .: "action"
        <*> x .: "milestone"

    parseJSON _ = fail "MilestoneEvent"

instance ToJSON MilestoneEvent where
    toJSON MilestoneEvent{..} = object
        [ "organization" .= milestoneEventOrganization
        , "repository"   .= milestoneEventRepository
        , "sender"       .= milestoneEventSender

        , "action"       .= milestoneEventAction
        , "milestone"    .= milestoneEventMilestone
        ]
