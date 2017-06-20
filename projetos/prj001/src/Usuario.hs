{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Usuario where
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text

import Database.Persist.Postgresql

formUsu :: Form Usuario
formUsu = renderDivs $ Usuario <$>
             areq textField "Nome" Nothing <*>
             areq emailField "E-mail" Nothing <*>
             areq passwordField "Senha" Nothing 

formLogin :: Form (Text, Text)
formLogin = renderDivs $ (,) <$>
             areq emailField "E-mail" Nothing <*>
             areq passwordField "Senha" Nothing 

getUsuarioR :: Handler Html
getUsuarioR = do
            (widget, enctype) <- generateFormPost formUsu
            defaultLayout $ widgetForm UsuarioR enctype widget "Cadastro de Usuários"

postUsuarioR :: Handler Html
postUsuarioR = do
                ((result, _), _) <- runFormPost formUsu
                case result of
                    FormSuccess usu -> do
                       usuLR <- runDB $ insertBy usu
                       case usuLR of
                           Left _ -> redirect UsuarioR
                           Right _ -> defaultLayout [whamlet|
                                          <h1> #{usuarioNome usu} Inseridx com sucesso. 
                                      |]
                    _ -> redirect UsuarioR
                    
getLoginR :: Handler Html
getLoginR = do
            (widget, enctype) <- generateFormPost formLogin
            msgComMaybe <- getMessage
            defaultLayout $ do 
                [whamlet|
                    $maybe msg <- msgComMaybe 
                        <h2>
                            #{msg}
                |]
                widgetForm LoginR enctype widget "Login page"


postLoginR :: Handler Html
postLoginR = do
                ((result, _), _) <- runFormPost formLogin
                case result of
                    FormSuccess ("root@root.com","root2") -> do
                        setSession "_USER" "admin"
                        redirect AdminR
                    FormSuccess (email,senha) -> do
                       temUsu <- runDB $ selectFirst [UsuarioEmail ==. email,UsuarioSenha ==. senha] []
                       case temUsu of
                           Nothing -> do
                               setMessage [shamlet| <p> Usuário ou senha inválido |]
                               redirect LoginR
                           Just _ -> do
                               setSession "_USER" email
                               defaultLayout [whamlet| Usuário autenticado!|]
                    _ -> redirect UsuarioR

getAdminR :: Handler Html
getAdminR = defaultLayout $ do
    [whamlet|
        <h1>
            Bem-vindo Rei dos Reis!!!!!!
    |]

postLogoutR :: Handler Html
postLogoutR = do
    deleteSession "_USER"
    redirect LoginR

