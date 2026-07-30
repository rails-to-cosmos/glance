module TestDefaults ( at
                    , bare
                    , bareParse
                    , compactTs
                    , headlinesOf
                    , initialState
                    , on
                    , plainTs
                    , strptime
                    , titled
                    , withCategory
                    , withHeadline
                    , withHeadlineIn
                    , withId
                    , withTodo
                    ) where

import Data.Org
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, defaultTimeLocale, parseTimeOrError)
import Test.Tasty.HUnit (Assertion, assertFailure)

-- Time

-- | The UTC time T spells as "%Y-%m-%d %H:%M:%S".
strptime :: Text -> UTCTime
strptime t = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S" (T.unpack t)

-- | The moment T names, as a source that spelled a time of day.
at :: Text -> TsMoment
at t = TsMoment (strptime t) True

-- | The moment T names, as a date-only source.
on :: Text -> TsMoment
on t = TsMoment (strptime t) False

-- | A timestamp of STATUS at MOMENT: no repeater, no range end.
plainTs :: TimestampStatus -> TsMoment -> Timestamp
plainTs status moment = Timestamp { tsStatus = status
                                  , tsInterval = Nothing
                                  , tsStart = moment
                                  , tsEnd = Nothing
                                  , tsCompactRange = False }

-- | A compact same-day range of STATUS: START through END in one bracket pair.
compactTs :: TimestampStatus -> TsMoment -> TsMoment -> Timestamp
compactTs status start end = (plainTs status start) { tsEnd = Just end
                                                    , tsCompactRange = True }

-- Context

initialState :: Context
initialState = defaultContext

withCategory :: Context -> Text -> Context
withCategory ctx category = setCategory category ctx

withTodo :: Context -> ([Text], [Text]) -> Context
withTodo ctx (actives, inactives) =
  setTodo (Set.fromList actives) (Set.fromList inactives) ctx

-- Documents

-- | A headline titled NAME carrying IDENT in an ORG_GLANCE_ID drawer.
withId :: Text -> Text -> Text
withId name' ident = T.intercalate "\n"
  [ "* " <> name'
  , ":PROPERTIES:"
  , ":ORG_GLANCE_ID: " <> ident
  , ":END:" ]

-- | A default headline whose title is the single token T.
titled :: Text -> Headline
titled t = defaultHeadline { title = Title [OrgLineToken (Token t)] }

-- Parsing

-- | Drop the spans ELEMS carry, for span-insensitive comparison.
bare :: [Spanned Element] -> [Element]
bare = map (stripSpans . valueOf)

-- | The elements INPUT parses to in CTX, spans dropped.
bareParse :: Context -> Text -> [Element]
bareParse ctx input = case orgParse ctx input of
  (elems, _ctx, _err) -> bare elems

-- | The headlines among ELEMS, in source order.
headlinesOf :: [Spanned Element] -> [Headline]
headlinesOf elems = [h | e <- elems, EHeadline h <- [valueOf e]]

-- | Run K on the one headline INPUT parses to in CTX; fail otherwise.
withHeadlineIn :: Context -> Text -> (Headline -> Assertion) -> Assertion
withHeadlineIn ctx input k = case bareParse ctx input of
  [EHeadline h] -> k h
  es -> assertFailure ("expected one headline in " <> show input <> ", got: " <> show es)

-- | 'withHeadlineIn', starting from 'defaultContext'.
withHeadline :: Text -> (Headline -> Assertion) -> Assertion
withHeadline = withHeadlineIn defaultContext
