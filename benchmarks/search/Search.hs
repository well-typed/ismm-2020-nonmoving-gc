{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Search where

import Data.Ord (Down(Down))
import Data.Coerce
import Data.Char
import qualified Data.List as L
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import Data.Foldable (toList)
import Data.Maybe
import qualified Data.Heap as H
import Data.Interned
import Data.Interned.Text
import Control.Monad.IO.Class
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad.State.Strict
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import Debug.Trace

newtype Term = Term InternedText
  deriving (Eq, Ord, Show)

newtype DocName = DocName InternedText
  deriving (Eq, Ord, Show)

data Posting = Posting { postingDoc :: !DocName, postingCount :: !Int }
  deriving (Eq, Ord, Show)

newtype Index = Index { indexPostings :: M.Map Term (Seq Posting) }

instance Semigroup Index where
  Index a <> Index b = Index $ M.unionWith (<>) a b

instance Monoid Index where
  mempty = Index mempty

newtype SearchM m a = SearchM (StateT Index m a)
  deriving (Monad, Functor, Applicative, MonadIO, MonadState Index)

runSearchM :: Monad m => SearchM m a -> Index -> m a
runSearchM (SearchM action) index0 = evalStateT action index0

-- | Make a 'Term' from a word.
mkTerm :: T.Text -> Term
mkTerm = mkLemma . T.toCaseFold . T.filter isAlpha

-- | Make a 'Term' from an already lemmatised word.
mkLemma :: T.Text -> Term
mkLemma = Term . intern

indexLemmas :: TL.Text -> Index
indexLemmas =
    foldMap (uncurry indexTerms) . parseLemmas

-- | Parse hi
parseLemmas :: TL.Text -> [(DocName, [Term])]
parseLemmas = go . TL.lines
  where
    go :: [TL.Text] -> [(DocName, [Term])]
    go [] = []
    go (line:rest)
      | Just docId <- "##" `TL.stripPrefix` line
      , (lemmaLines, next) <- break ("##" `TL.isPrefixOf`) rest = 
          let lemmas = mapMaybe parseLemma lemmaLines
          in (DocName $ intern (TL.toStrict docId), lemmas) : go next
      | otherwise = go rest

    parseLemma :: TL.Text -> Maybe Term
    parseLemma line =
      case TL.splitOn "\t" line of
        [_,"",_]    -> Nothing
        [_,lemma,_] -> Just $ mkLemma (TL.toStrict lemma)
        []          -> Nothing
        [""]        -> Nothing
        other       -> error $ "bad lemma line: " ++ show other
    
parseDocLines :: TL.Text -> [(DocName, [Term])]
parseDocLines = mapMaybe toDoc . TL.lines
  where
    toDoc :: TL.Text -> Maybe (DocName, [Term])
    toDoc line =
      case TL.words line of
        docName : rest ->
          let docName' = DocName $ intern $ TL.toStrict docName
              terms = map (mkTerm . TL.toStrict) rest
          in Just (docName', terms)
        _ -> Nothing

indexDocLines :: TL.Text -> Index
indexDocLines = foldMap (uncurry indexTerms) . parseDocLines

indexDocLinesFile :: FilePath -> IO Index
indexDocLinesFile fname =
    indexDocLines <$> TL.readFile fname

indexTerms :: DocName -> [Term] -> Index
indexTerms docName terms =
    Index $ M.fromListWith (<>)
    [ (t, Seq.singleton $ Posting docName n)
    | (t, n) <- M.toList termCounts
    ]
  where
    termCounts = M.fromListWith (+) [(w, 1) | w <- terms]

indexText :: DocName -> T.Text -> Index
indexText docName text =
    indexTerms docName $ map mkTerm $ T.words text

indexTextFile :: FilePath -> IO Index
indexTextFile fpath =
    indexText (DocName $ intern $ T.pack fpath) <$> T.readFile fpath

addDocument :: FilePath -> SearchM IO ()
addDocument fpath = do
    index' <- liftIO $ indexDocLinesFile fpath
    --liftIO $ print $ M.size $ indexPostings index'
    modify' (index' <>)

topK :: Ord a => Int -> [a] -> [a]
topK k = \xs -> go (head xs) 0 mempty xs
  where
    go :: Ord a => a -> Int -> H.Heap (Down a) -> [a] -> [a]
    go thresh !_ acc []     = coerce $ toList $ H.take k acc
    go thresh i  acc (x:xs)
      | x < thresh = go thresh i acc xs
      | otherwise =
          let x' = rebuild $ H.insert (Down x) acc
              rebuild
                | i `mod` rebuildInterval == 0 = H.take k
                | otherwise = id
              thresh'
                | i `mod` rebuildInterval == 0 = coerce $ last $ toList x'
                | otherwise = thresh
          in go thresh' (i+1) x' xs

    rebuildInterval = 1000

query :: Index -> [Term] -> [(Float, DocName)]
query index terms =
    fmap (\(docName, termCounts) -> (realToFrac $ sum termCounts, docName)) (queryTermCounts index terms)

queryTermCounts :: Index -> [Term] -> [(DocName, M.Map Term Int)]
queryTermCounts (Index indexPostings) terms =
    go postings0
  where
    postings0 :: M.Map Term [Posting]
    postings0 =
      fmap toList $ M.restrictKeys indexPostings (S.fromList terms)

    go :: M.Map Term [Posting] -> [(DocName, M.Map Term Int)]
    go postings
      | M.null postings = []
      | otherwise       = (minDoc, minDocPostings) : go postings'
      where
        minDoc :: DocName
        minDoc = postingDoc $ minimum (M.mapMaybe safeHead postings)

        minDocPostings :: M.Map Term Int
        minDocPostings = M.mapMaybe f postings
          where
            f :: [Posting] -> Maybe Int
            f (posting:_)
              | postingFor minDoc posting = Just (postingCount posting)
            f _ = Nothing

        postings' :: M.Map Term [Posting]
        postings' = M.filter (not . null) (dropWhile (postingFor minDoc) <$> postings)

    safeHead :: [a] -> Maybe a
    safeHead (x:_) = Just x
    safeHead []    = Nothing

    postingFor :: DocName -> Posting -> Bool
    postingFor docName (Posting docName' _) = docName == docName'

data Command = Query [Term]
             | AddDoc FilePath
             | Sleep Double
             deriving (Show)

readCommands :: FilePath -> IO [Command]
readCommands fpath = mapMaybe parseCommand . lines <$> readFile fpath
  where
    parseCommand ('+':rest) = Just $ AddDoc rest
    parseCommand ('?':rest) = Just $ Query $ map (mkTerm . T.pack) $ words rest
    parseCommand ('#':rest) = Just $ Sleep $ read rest
    parseCommand _          = Nothing

runCommand :: Command -> SearchM IO ()
runCommand (Query terms) = do
    index <- get
    let results = topK 1000 $ query index terms
    -- ! <- return $! length results
    liftIO $ print (terms, length results)
    return ()
runCommand (AddDoc fname) = addDocument fname

commandLabel :: Command -> String
commandLabel (AddDoc{}) = "+"
commandLabel (Query{})  = "?"
commandLabel (Sleep{})  = "#"
