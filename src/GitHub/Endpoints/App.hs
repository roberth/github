module GitHub.Endpoints.App where

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
