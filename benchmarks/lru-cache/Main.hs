{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class
import System.Environment
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.LruCache as LRU
import qualified Data.LruCache.IO as LRU
import Web.Scotty hiding (text)
import Network.HTTP.Types.Status (status404)
import System.Exit

import CAR.Types
import qualified CAR.TocFile as Toc

import BenchUtils

main :: IO ()
main = do
  [f] <- getArgs
  let pagesFile :: Toc.IndexedCborPath PageName Page
      pagesFile = Toc.IndexedCborPath f
  --pages <- timeIt "index" $ Toc.openInMem pageName f :: IO (Toc.IndexedCbor PageName Page)
  pages <- timeIt "index" $ Toc.open pagesFile :: IO (Toc.IndexedCbor PageName Page)
  putStrLn $ "Loaded index with " ++ show (length $ Toc.keys pages) ++ " keys."

  let retrieve :: PageName -> IO (Maybe TL.Text)
      retrieve name =
        case Toc.lookup name pages of
          Just page -> let !rendered = renderHtml (renderPage page)
                       in return (Just rendered)
          Nothing   -> return Nothing

      cleanup :: PageName -> Maybe TL.Text -> IO ()
      cleanup _ _ = return ()

  cache <- LRU.newStripedLruHandle 64 10000
  scotty 3000 $ do
    get "/_alive" $ html "I'm alive!\n"
    get "/_stop" $ html "stopping...\n" >> liftIO exitSuccess
    get "/:page" $ timeIt "req" $ do
      name <- param "page"
      let pageName = toPageName name
      mpage <- liftIO $ LRU.stripedCached cache pageName (retrieve pageName)
      --mpage <- liftIO $ retrieve pageName

      case mpage of
        Just page -> html page
        Nothing   -> status status404 >> finish

toPageName :: String -> PageName
toPageName = packPageName . map normalize
  where
    normalize x = case x of
                    '_' -> ' '
                    _   -> x

renderPage :: Page -> Html
renderPage page =
  tag "div" $
    tag "h1" (text $ T.pack $ unpackPageName $ pageName page)
    <> foldMap renderSkeleton (pageSkeleton page)

renderSkeleton :: PageSkeleton -> Html
renderSkeleton (Section hd _ bodies) =
  tag "section" $
    tag "h1" (text $ getSectionHeading hd)
    <> foldMap renderSkeleton bodies
renderSkeleton (Para para) =
  renderParagraph para
renderSkeleton (Image t _) =
  tag "div" $ text t
renderSkeleton (List _ para) =
  tag "p" $ renderParagraph para
renderSkeleton (Infobox title fields) =
  tag "div" $ text title <> tag "table" table
    where
      table = mconcat $ map (tag "tr")
        $ [ tag "th" (text "Field") <> tag "th" (text "Value") ]
        ++ [ tag "td" (text field) <> tag "td" (foldMap renderSkeleton bodies)
           | (field, bodies) <- fields
           ]

renderParagraph :: Paragraph -> Html
renderParagraph (Paragraph _ bodies) =
  tag "p" $ foldMap renderParaBody bodies

renderParaBody :: ParaBody -> Html
renderParaBody (ParaText t) = text t
renderParaBody (ParaLink l) = tag "a" (text $ linkAnchor l)

-- | A simple HTML builder.
newtype Html = Html TB.Builder
  deriving (Semigroup, Monoid)

renderHtml :: Html -> TL.Text
renderHtml (Html builder) = TB.toLazyText builder

text :: T.Text -> Html
text = Html . TB.fromText

tag :: TB.Builder -> Html -> Html
tag t (Html body) =
  Html $ "<" <> t <> ">" <> body <> "</" <> t <> ">"
