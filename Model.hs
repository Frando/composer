module Model where

import Prelude
import Yesod
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist.Quasi

data PublishSettings = Public
                     | PublishedVersionsOnly
                     | Private
  deriving (Show, Read, Eq, Enum, Bounded)
derivePersistField "PublishSettings"

data Role = Author
          | Collaborator
          | Reviewer
  deriving (Show, Read, Eq, Enum, Bounded)
derivePersistField "Role"

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
  $(persistFileWith lowerCaseSettings "config/models")