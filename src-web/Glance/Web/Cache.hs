{-# LANGUAGE OverloadedStrings #-}

-- | An ADDITIVE, per-machine headline cache in SQLite, keyed by the served dir.
-- It mirrors the in-memory 'Glance.Web.Store' — every row the walk builds, plus
-- an edge table for the links — so a restart, or Emacs, can read the index
-- without re-walking the tree.  It lives under XDG cache (@~\/.cache\/glance@),
-- NOT in the tree: it never travels and never needs a @.gitignore@ line, which
-- is what "the cache stays home" means.  The in-memory store still answers every
-- query; this is a durable shadow, filled after the walk and kept up on reload.
--
-- The additive step of @docs\/proposals\/draft\/2026-09-10-the-files-are-the-log-…@:
-- the store is UNCHANGED and no invariant retires.  What that proposal's full
-- cut would retire (1, 7, 8, 34) is left in place.
module Glance.Web.Cache
  ( Cache
  , cacheVersion
  , cacheFileFor
  , openCache
  , closeCache
  , cacheFill
  , cacheApplyFile
  , cacheDropFile
  , backlinks
  , neighbors
  , cacheCounts
  ) where

import Control.Exception (onException)
import Data.Bits (xor)
import Data.Int (Int64)
import Data.List (nub)
import Data.Text (Text)
import Data.Word (Word64)
import Database.SQLite3
  ( Database, SQLData (SQLInteger, SQLNull, SQLText), Statement
  , StepResult (Done, Row)
  , bind, close, columns, exec, finalize, open, prepare, reset, step )
import Numeric (showHex)
import System.Directory (XdgDirectory (XdgCache), canonicalizePath, createDirectoryIfMissing, getXdgDirectory)
import System.FilePath ((</>))

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Glance.Query (HeadlineRecord (..), Ref (..))


newtype Cache = Cache Database

-- | Bumped when the schema below changes, so a stale cache is a rebuild rather
-- than a heal — read back through @PRAGMA user_version@.
cacheVersion :: Int64
cacheVersion = 1

-- | Where DIR's cache file lives: under XDG cache, named by a hash of the
-- canonical path, so two trees never share a file and no path escapes the name.
cacheFileFor :: FilePath -> IO FilePath
cacheFileFor dir = do
  base <- getXdgDirectory XdgCache "glance"
  createDirectoryIfMissing True base
  canon <- canonicalizePath dir
  pure (base </> (showHex (fnv1a (T.pack canon)) "" <> ".sqlite"))

-- | 64-bit FNV-1a over UTF-8 bytes; a cache-file name, not a security digest.
fnv1a :: Text -> Word64
fnv1a = BS.foldl' mix 0xcbf29ce484222325 . TE.encodeUtf8
  where mix h b = (h `xor` fromIntegral b) * 0x100000001b3

openCache :: FilePath -> IO Cache
openCache path = do
  db <- open (T.pack path)
  mapM_ (exec db)
    [ "PRAGMA journal_mode=WAL;"
    , "PRAGMA synchronous=NORMAL;"
    , T.pack ("PRAGMA user_version=" <> show cacheVersion <> ";")
    , "CREATE TABLE IF NOT EXISTS headline\
      \ (id TEXT PRIMARY KEY, path TEXT, hash TEXT, title TEXT, state TEXT,\
      \  priority TEXT, tags TEXT, scheduled TEXT, deadline TEXT, closed TEXT, created TEXT);"
    , "CREATE TABLE IF NOT EXISTS edge (src TEXT, dst TEXT, kind TEXT);"
    , "CREATE INDEX IF NOT EXISTS edge_dst ON edge(dst);"
    , "CREATE INDEX IF NOT EXISTS edge_src ON edge(src);"
    , "CREATE INDEX IF NOT EXISTS headline_path ON headline(path);"
    ]
  pure (Cache db)

closeCache :: Cache -> IO ()
closeCache (Cache db) = close db

-- | Replace the whole cache with the records the walk built.  One transaction.
cacheFill :: Cache -> [HeadlineRecord] -> IO ()
cacheFill (Cache db) records = withTx db $ do
  exec db "DELETE FROM headline;"
  exec db "DELETE FROM edge;"
  insertAll db records

-- | Upsert one file's rows: drop what the path held, then insert PATH's records
-- (the reload's fresh parse).  Keyed by path, so a row that moved files is not
-- orphaned.
cacheApplyFile :: Cache -> FilePath -> [HeadlineRecord] -> IO ()
cacheApplyFile (Cache db) path records = withTx db $ do
  deleteByPath db path
  insertAll db records

-- | Drop a deleted file's rows.
cacheDropFile :: Cache -> FilePath -> IO ()
cacheDropFile (Cache db) path = withTx db (deleteByPath db path)

-- | The ids that link TO one id or title: an edge-table index read (backlinks).
backlinks :: Cache -> Text -> IO [Text]
backlinks (Cache db) target = queryCol db "SELECT src FROM edge WHERE dst = ?;" [SQLText target]

-- | One hop either way from an id or title: what it points at and what points at it.
neighbors :: Cache -> Text -> IO [Text]
neighbors (Cache db) node = do
  outs <- queryCol db "SELECT dst FROM edge WHERE src = ?;" [SQLText node]
  ins  <- queryCol db "SELECT src FROM edge WHERE dst = ?;" [SQLText node]
  pure (nub (outs <> ins))

-- | (headlines, edges) held, for the suite.
cacheCounts :: Cache -> IO (Int, Int)
cacheCounts (Cache db) = (,) <$> countOf db "headline" <*> countOf db "edge"


-- Internals.

withTx :: Database -> IO a -> IO a
withTx db act = do
  exec db "BEGIN;"
  r <- act `onException` exec db "ROLLBACK;"
  exec db "COMMIT;"
  pure r

insertAll :: Database -> [HeadlineRecord] -> IO ()
insertAll db records = do
  hStmt <- prepare db
    "INSERT OR REPLACE INTO headline\
    \ (id, path, hash, title, state, priority, tags, scheduled, deadline, closed, created)\
    \ VALUES (?,?,?,?,?,?,?,?,?,?,?);"
  eStmt <- prepare db "INSERT INTO edge (src, dst, kind) VALUES (?,?,?);"
  mapM_ (insertRecord hStmt eStmt) records
  finalize hStmt
  finalize eStmt

insertRecord :: Statement -> Statement -> HeadlineRecord -> IO ()
insertRecord hStmt eStmt r = do
  runBind hStmt
    [ SQLText (hrId r), SQLText (T.pack (hrFile r)), SQLText (hrDigest r)
    , SQLText (hrTitle r), maybeText (hrState r), maybeText (hrPriority r)
    , SQLText (hrTags r), maybeText (hrScheduled r), maybeText (hrDeadline r)
    , maybeText (hrClosed r), maybeText (lookup "ORG_GLANCE_CREATION_TIME" (hrDrawer r)) ]
  mapM_ (insertEdge eStmt (hrId r)) (hrLinks r)

insertEdge :: Statement -> Text -> Ref -> IO ()
insertEdge eStmt src ref =
  runBind eStmt [SQLText src, SQLText (refTarget ref), maybeText (refKind ref)]

deleteByPath :: Database -> FilePath -> IO ()
deleteByPath db path = do
  execBind db "DELETE FROM edge WHERE src IN (SELECT id FROM headline WHERE path = ?);" [p]
  execBind db "DELETE FROM headline WHERE path = ?;" [p]
  where p = SQLText (T.pack path)

-- | Bind, step to completion (a write returns no rows), then reset for reuse.
runBind :: Statement -> [SQLData] -> IO ()
runBind st args = do
  bind st args
  _ <- step st
  reset st

execBind :: Database -> Text -> [SQLData] -> IO ()
execBind db sql args = do
  st <- prepare db sql
  bind st args
  _ <- step st
  finalize st

queryCol :: Database -> Text -> [SQLData] -> IO [Text]
queryCol db sql args = do
  st <- prepare db sql
  bind st args
  rows <- loop st []
  finalize st
  pure rows
  where
    loop st acc = do
      res <- step st
      case res of
        Done -> pure (reverse acc)
        Row  -> do cols <- columns st; loop st (firstText cols : acc)
    firstText (SQLText t : _) = t
    firstText _               = ""

countOf :: Database -> Text -> IO Int
countOf db tbl = do
  st <- prepare db ("SELECT COUNT(*) FROM " <> tbl <> ";")
  _ <- step st
  cols <- columns st
  finalize st
  pure (case cols of SQLInteger n : _ -> fromIntegral n; _ -> 0)

maybeText :: Maybe Text -> SQLData
maybeText = maybe SQLNull SQLText
