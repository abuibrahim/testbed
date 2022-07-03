import Control.Exception
import Control.Monad
import Data.List (intersect)
import Data.Version (showVersion)
import Paths_testbed (version)
import System.Environment
import System.Exit
import System.IO.Error
import System.LibVirt
import Text.XML.HXT.Core hiding (when)

kaput :: SomeException -> IO ()
kaput _ = exitFailure

usage :: IO ()
usage = do
  progName <- getProgName
  die $ "usage: " ++ progName ++ " <version|create|destroy> config.xml"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["version"]      -> putStrLn $ showVersion version
    ["create", xml]  -> createTestbed xml `catch` kaput
    ["destroy", xml] -> destroyTestbed xml `catch` kaput
    _                -> usage

atTag tag = deep (isElem >>> hasName tag)
getNameText = getChildren >>> hasName "name" /> getText

-- | Wrap input into a dummy element before feeding it
--   into writeDocumentToString which strips it
writeString =
  selem "/" [this] >>> indentDoc >>> writeDocumentToString []

getUri doc = do
  [uri] <- runX $ doc >>> atTag "testbed" >>> getAttrValue "uri"
  if not (null uri)
    then return uri
    else getEnv "LIBVIRT_DEFAULT_URI" `catch` \e ->
           if isDoesNotExistError e then die "uri is required" else ioError e

createTestbed xml = do
  let doc = readDocument [withValidate yes] xml
  uri <- getUri doc

  withConnection uri $ \conn -> do
    createPools conn doc
    createBackingStores conn doc
    createVols conn doc
    createNetworks conn doc
    createDomains conn doc

createPools conn doc = do
  pools <- runX $ doc >>> atTag "pool" >>> (getNameText &&& writeString)
  forM pools $ \(name,xml) -> do
    putStrLn $ "Defining storage pool " ++ name
    pool <- defineStoragePoolXML conn xml 0
    putStrLn $ "Building storage pool " ++ name
    buildStoragePool pool [StoragePoolBuildNew]
    putStrLn $ "Creating storage pool " ++ name
    createStoragePool pool 0
    return pool

createBackingStores conn doc = do
  let getVol = atTag "volume" `notContaining` (getChildren >>> hasName "backingStore")
      getSrc = atTag "source" /> atTag "path" /> getText

  vols <- runX $ doc >>> getVol >>>
          (((getNameText &&& getAttrValue "pool") &&& getSrc) &&& writeString)
  forM vols $ \(((name,poolName),src),xml) -> do
    pool <- lookupStoragePoolName conn poolName
    putStrLn $ "Creating storage volume " ++ name
    vol <- createStorageVolXML pool xml []
    putStrLn $ "Uploading storage volume " ++ name
    uploadStorageVol src vol
    return vol

createVols conn doc = do
  let getVol = atTag "volume" </ hasName "backingStore"

  vols <- runX $ doc >>> getVol >>>
          ((getNameText &&& getAttrValue "pool") &&& writeString)
  forM vols $ \((name,poolName),xml) -> do
    pool <- lookupStoragePoolName conn poolName
    putStrLn $ "Creating storage volume " ++ name
    createStorageVolXML pool xml []

createNetworks conn doc = do
  networks <- runX $ doc >>> atTag "network" >>> (getNameText &&& writeString)
  forM networks $ \(name,xml) -> do
    putStrLn $ "Creating network " ++ name
    createNetworkXML conn xml

createDomains conn doc = do
  domains <- runX $ doc >>> atTag "domain" >>> (getNameText &&& writeString)
  forM_ domains $ \(name,xml) -> do
    putStrLn $ "Creating domain " ++ name
    createDomainXML conn xml [ DomainStartResetNvram ]

destroyTestbed xml = do
  let doc = readDocument [withValidate yes] xml
  uri <- getUri doc

  withConnection uri $ \conn -> do
    destroyDomains conn doc
    destroyNetworks conn doc
    destroyStorage conn doc

destroyDomains conn doc = do
  listed  <- runX (doc >>> atTag "domain" >>> getNameText)
  running <- getRunningDomainNames conn
  domains <- mapM (lookupDomainName conn) (listed `intersect` running)
  mapM destroyDomain domains

getRunningDomainNames conn =
  runningDomainsIDs conn
  >>=
  mapM (lookupDomainID conn)
  >>=
  mapM getDomainName

destroyNetworks conn doc = do
  listed   <- runX (doc >>> atTag "network" >>> getNameText)
  running  <- runningNetworksNames conn
  networks <- mapM (lookupNetworkName conn) (listed `intersect` running)
  mapM destroyNetwork networks

destroyStorage conn doc = do
  listed  <- runX (doc >>> atTag "pool" >>> getNameText)
  running <- runningStoragePoolsNames conn
  pools <- mapM (lookupStoragePoolName conn) (listed `intersect` running)
  forM_ pools $ \pool -> do
    isActive <- storagePoolIsActive pool
    when isActive $ do
      volNames <- storagePoolVolsNames pool
      vols <- mapM (lookupStorageVolName pool) volNames
      mapM_ (`deleteStorageVol` 0) vols
      destroyStoragePool pool
      deleteStoragePool pool [StoragePoolDeleteNormal]
      void $ undefineStoragePool pool
