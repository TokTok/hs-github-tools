{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE ViewPatterns      #-}
module GitHub.Tools.NetworkGraph
  ( getNetworkGraph
  ) where

import           Control.Arrow                ((&&&))
import           Control.Monad                (unless, void)
import           Data.Char                    (ord)
import qualified Data.List                    as List
import qualified Data.List.Split              as List
--import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Data.Vector                  as V
import qualified GitHub                       as GH
import qualified GitHub.Data.Name             as GH
import           Network.HTTP.Client          (Manager, newManager)
import           Network.HTTP.Client.TLS      (tlsManagerSettings)
import           System.Directory             (doesDirectoryExist,
                                               setCurrentDirectory)
import           System.IO                    (hPutStrLn, stderr)
import           System.Process               (callProcess, readProcess)
import qualified Text.ParserCombinators.ReadP as R

import           GitHub.Tools.Requests


type RepoRef = (GH.Name GH.Owner, GH.Name GH.Repo)


data Ref = Ref String String [String] [String]
    deriving Show

instance Read Ref where
    readsPrec _ = R.readP_to_S $ Ref
        <$> R.count 25 R.get
        <*> (R.char ' ' >> readRef)
        <*> R.many1 (R.char ' ' >> readRef)
        <*> readNames
      where
        readRef = R.count 40 . R.choice . map R.char $ ['0'..'9'] ++ ['a'..'f']
        readNames = do
            void $ R.string " ("
            res <- R.sepBy (R.munch (not . (`elem` [',',')']))) (R.string ", ")
            void $ R.char ')'
            return res


toDot :: Ref -> [String]
toDot (Ref _ _ _ []) = []
toDot (Ref date (take 6 -> ref) (map (take 6) -> parents) names@(mainName:_)) =
    "  \"" <> ref <> "\" [ " <> List.intercalate " " attrs <> " ]"
    : edges
  where
    edges = map (\parent -> "  \"" <> ref <> "\" -> \"" <> parent <> "\"") parents
    truncateName name
      | length name > 25 = take 22 name <> "..."
      | otherwise = name

    attrs =
        [ "label = \"" <> List.intercalate "\\n" (take 10 date : map truncateName names) <> "\""
        , "tooltip = \"" <> List.intercalate "\\n" names <> "\""
        , "fillcolor = \"" <> nameColor <> "\""
        ]

    palette =
        [ "#ccff00"
        , "#ccffff"
        , "#ffff66"
        , "#cccc00"
        , "#ccccff"
        , "#ffccff"
        , "#ffcccc"
        , "#ffcc33"
        , "#cc9933"
        , "#cc9999"
        , "#cc99ff"
        , "#ff99ff"
        , "#ff9933"
        , "#cc6633"
        , "#66cc99"
        , "#99cc33"
        , "#009900"
        ]

    nameColor =
        case fst $ List.break (`elem` [':','/']) mainName of
          "HEAD -> master" -> "red"
          "tag" -> "#cccccc"
          author -> palette !! ((sum . map ord $ author) `mod` (length palette))


minDate :: String
minDate = "2022-01"


urlBase :: String
urlBase = "https://username:password@github.com/"


denyList :: [GH.Name GH.Owner]
denyList =
    [ "4a256b6b3e7t3e8b7t9q7t"
    , "activistWannabe2"
    , "cha63506"
    , "chai3819"
    , "CNXTEoEorg"
    , "DannaScully"
    , "din982"
    , "fireeyeusa"
    , "grubern"
    , "haiiev"
    , "innnzzz6"
    , "jamiepg1"
    , "josephyzhou"
    , "jrtorres42"
    , "kigu"
    , "lucasborer1"
    , "lukw00"
    , "makianditro1"
    , "mehulsbhatt"
    , "mk21"
    , "nfkd"
    , "noikiy"
    , "ProgrammerAndHacker"
    , "receptpr9001"
    , "shaunstanislaus"
    , "sometwo"
    , "SunelContus"
    , "treejames"
    , "xeon2007"
    , "xuecai"
    ]


-- | Monadic version of @unless@, taking the condition in the monad
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM condM acc = do
    cond <- condM
    unless cond acc


getNetworkGraph
    :: Maybe GH.Auth
    -> [RepoRef]
    -> IO String
getNetworkGraph _ [] = return []
getNetworkGraph auth repos@(rootRepo:seedRepos) = do
    -- Initialise HTTP manager so we can benefit from keep-alive connections.
    mgr <- newManager tlsManagerSettings

    unlessM (doesDirectoryExist clonePath) $ do
        hPutStrLn stderr "Cloning initial repo"
        clone rootRepo
        setCurrentDirectory clonePath

        hPutStrLn stderr $ "Adding remotes for " <> show (length seedRepos) <> " seed repos"
        mapM_ addRemote seedRepos

        hPutStrLn stderr $ "Querying GitHub for forks..."
        forks <- V.filter (not . (`elem` denyList) . fst) . V.concat <$> mapM (forksFor mgr) repos

        hPutStrLn stderr $ "Adding remotes for " <> show (V.length forks) <> " forks"
        V.mapM_ addRemote forks

        hPutStrLn stderr "Fetching all remotes"
        fetchAll

    setCurrentDirectory clonePath
    dotLines <- concatMap toDot <$> gitLog

    return . unlines . concat $
        [ [ "strict digraph \"" <> repoPath rootRepo <> "\" {"
          , "  graph [splines=ortho, rankdir=LR]"
          , "  node [shape=box width=2.5 margin=\"0,0.02\" style=filled]"
          , "  edge [dir=back]"
          ]
        , dotLines
        , ["}"]
        ]

  where
    repoPath (GH.N (Text.unpack -> owner), GH.N (Text.unpack -> repo)) =
        owner <> "/" <> repo

    clonePath = "/tmp/hub-graph/" <> (Text.unpack . GH.untagName . snd $ rootRepo)

    clone repo =
        callProcess "git" ["clone", urlBase <> repoPath repo, clonePath]

    addRemote repo@(GH.N (Text.unpack -> owner), _) =
        callProcess "git" ["remote", "add", owner, urlBase <> repoPath repo]

    fetchAll =
        callProcess "git" ["fetch", "--all", "--prune", "--jobs=10"]

    gitLog =
        map read
        . filter (>= minDate)
        . List.splitOn "\n"
        <$> readProcess "git" ["log", "--all", "--format=%cI %H %P%d", "--topo-order", "--simplify-by-decoration"] ""

    forksFor :: Manager -> RepoRef -> IO (V.Vector RepoRef)
    forksFor mgr (owner, repo) =
        V.map (GH.simpleOwnerLogin . GH.repoOwner &&& GH.repoName) <$> request auth mgr (GH.forksForR owner repo GH.FetchAll)
