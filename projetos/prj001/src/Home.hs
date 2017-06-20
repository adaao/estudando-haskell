{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Home where

import Foundation
import Yesod.Core

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    setTitle "Different <>"
    [whamlet|
        <h1>
            Pagina do Adaão


--        <p>
--            <a href=@{AddR 5 7}>HTML addition
--        <p>
--            <a href=@{AddR 5 7}?_accept=application/json>JSON addition
    |]
