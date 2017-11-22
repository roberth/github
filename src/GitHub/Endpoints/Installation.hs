module GitHub.Endpoints.Installation where

import GitHub.Data
import GitHub.Internal.Prelude
import GitHub.Request
import Prelude ()

userInstallationsR :: FetchCount -> Request 'RA (Collection Installation)
userInstallationsR = pagedQueryS ["user", "installations"] []

userInstallations' :: Auth -> IO (Either Error (Collection Installation))
userInstallations' auth = executeRequest auth $ userInstallationsR FetchAll

-- | See https://developer.github.com/v3/apps/installations/#list-repositories-accessible-to-the-user-for-an-installation
userInstallationRepositoriesR :: Id Installation -> FetchCount -> Request 'RA (Collection Repo)
userInstallationRepositoriesR instId = pagedQueryS ["user", "installations", toPathPart instId, "repositories"] []

userInstallationRepositories' :: Auth -> Id Installation -> IO (Either Error (Collection Repo))
userInstallationRepositories' auth a = executeRequest auth $ userInstallationRepositoriesR a FetchAll
