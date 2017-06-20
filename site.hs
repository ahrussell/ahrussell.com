--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           System.FilePath
import           Data.Char

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "font-awesome/**" $ do
        route prependCssRoute
        compile copyFileCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" 
                    (constField "current-page-about" "47" `mappend` defaultContext)
            >>= relativizeUrls
    
    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= saveSnapshot postSnapshot
            >>= loadAndApplyTemplate "templates/post.html" postsPageCtx
            >>= loadAndApplyTemplate "templates/default.html" postsPageCtx
            >>= relativizeUrls

    match "projects/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= saveSnapshot projectSnapshot
            >>= loadAndApplyTemplate "templates/default.html" projectsPageCtx
            >>= relativizeUrls

    match "posts.html" $ makeContentListPage "Posts" "posts/*" postsTeaserCtx postsPageCtx postSnapshot

    match "projects.html" $ makeContentListPage "Projects" "projects/*" projectsTeaserCtx projectsPageCtx projectSnapshot
    
    -- create ["archive.html"] $ do
    --     route idRoute
    --     compile $ do
    --         posts <- recentFirst =<< loadAll "posts/*"
    --         let archiveCtx =
    --                 listField "posts" postCtx (return posts) `mappend`
    --                 constField "title" "Archives"            `mappend`
    --                 defaultContext

    --         makeItem ""
    --             >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
    --             >>= loadAndApplyTemplate "templates/default.html" archiveCtx
    --             >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- postsList
            let postsCtx = listField "posts" postsTeaserCtx (return posts) 
            let indexCtx =
                    postsCtx `mappend`
                    constField "title" "Andrew H Russell"    `mappend`
                    constField "pageCss" "index" `mappend`
                    constField "current-page-index" "47" `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------

postSnapshot = "postSnapshot" :: Snapshot
projectSnapshot = "projectSnapshot" :: Snapshot

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    dateField "month" "%b"       `mappend`
    dateField "day" "%d"         `mappend`
    defaultContext

postsPageCtx :: Context String
postsPageCtx = constField "current-page-posts" "47" `mappend` postCtx `mappend` defaultContext

projectsPageCtx :: Context String
projectsPageCtx = constField "current-page-projects" "47" `mappend` postCtx `mappend` defaultContext

postsTeaserCtx :: Context String
postsTeaserCtx = teaserField "teaser" postSnapshot `mappend` postCtx

projectsTeaserCtx :: Context String
projectsTeaserCtx = teaserField "teaser" projectSnapshot `mappend` postCtx

postsList :: Compiler [Item String]
postsList = fmap (take 10) . recentFirst =<< loadAllSnapshots "posts/*" postSnapshot

makeContentListPage :: String -> Pattern -> Context String -> Context String -> Snapshot -> Rules ()
makeContentListPage title pattrn itemCtx pageCtx sshot = do
        route idRoute
        compile $ do
            items <- fmap (take 10) . recentFirst =<< loadAllSnapshots pattrn sshot
            let itemsCtx = listField "posts" itemCtx (return items)
            let ctx = 
                    itemsCtx `mappend`
                    constField "title" title `mappend`
                    pageCtx `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls


prependCssRoute :: Routes
prependCssRoute = customRoute $ 
                    \idt -> "css" </> toFilePath idt
