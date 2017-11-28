module GitHub.Endpoints.App where

import GitHub.Data.Collection
import GitHub.Data.Definitions
import GitHub.Data
import GitHub.Internal.Prelude
import GitHub.Request
import Prelude ()

appR :: Request 'AA App
appR = query ["app"] []

app' :: Auth -> IO (Either Error App)
app' auth = executeRequest auth $ appR

installationRepositoriesR :: FetchCount -> Request 'AA (Collection Repo)
installationRepositoriesR = pagedQueryS ["installation", "repositories"] []

installationRepositories' :: Auth -> IO (Either Error (Collection Repo))
installationRepositories' auth = executeRequest auth $ installationRepositoriesR FetchAll

installationsAccessTokensR :: Id Installation -> Request 'AA InstallationToken
installationsAccessTokensR installationId =
  appCommand Post ["installations", toPathPart installationId, "access_tokens"] mempty

installationsAccessTokens' :: Auth -> Id Installation -> IO (Either Error InstallationToken)
installationsAccessTokens' auth installationId = executeRequest auth $ installationsAccessTokensR installationId
