module P10.Constants where
import Data.Aeson (Value)
import Data.List (intercalate)
import Utils (unpackBS)
import Data.Yaml (encode)

-- constants, for ease of use
expose :: Int -> Int
expose target = 30000 + mod target 1000

autoscalerPort :: Int
autoscalerPort = 8080
autoscalerExposedPort :: Int
autoscalerExposedPort = expose autoscalerPort

forecasterPort :: Int
forecasterPort = 8081
forecasterExposedPort :: Int
forecasterExposedPort = expose forecasterPort

workloadPort :: Int
workloadPort = 8082

generatorPort :: Int
generatorPort = 8083
generatorExposedPort :: Int
generatorExposedPort = expose generatorPort

dbName :: String
dbName = "autoscaler"
dbHost :: String
dbHost = "postgres"
dbPort :: String
dbPort = "5432"
dbUser :: String
dbUser = "root"
dbPassword :: String
dbPassword = "password"

aesonToKubeconfig :: [Value] -> String
aesonToKubeconfig = intercalate "\n---\n" . map (unpackBS . encode)
