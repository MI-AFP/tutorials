# More on Monad Transformers

In Haskell and in functional programming in general, a monad transformer is a type constructor which takes a monad as an argument and returns a monad as a result. Basically said, it is combination of monads in "layers".

Monad transformers can be pretty useful and make code both efficient and readable when used properly. They can be used to compose features encapsulated by monads – such as state, exception handling, I/O, logging, and others – in a modular way. Typically, a monad transformer is created by generalising an existing monad and applying the resulting monad transformer to the `Identity` or `IO` monad yields a monad which is equivalent to the original monad (ignoring any necessary boxing and unboxing).

## Definition

Monad transformer is a wrapper type that has:

1. a type constructor `t` of kind `(* -> *) -> * -> *` (needs a monad `m` a type)
2. defined monad operations `return` and `>>=` (bind) compliant in mo nadic laws for `t m` iff `m` is a monad
3. an operation `lift` of type `m a -> t m a` that allows to access the inner monad `m` and fullfils:
  * `lift . return = return`
  * `lift (m >>= k) = (lift m) >>= (lift . k)` 

## MonadT, MaybeT and EitherT

When using monad transformers in Haskell, package [transformers](http://hackage.haskell.org/package/transformers) provide them for basic monads such as `Maybe`, `List`, `Reader`, `Writer`, or `State`. Be careful, others are separately in different packages such as `Either` in [either](https://hackage.haskell.org/package/either). It is quite common to use `T` as suffix after monad name to name its transformer.

### Maybe with IO

As was already said, monad transformers are used as wrapper types to combine monads with other common features provided by different monads, for example, `IO` with actions like `putStrLn` or `getLine`. Why would someone need such a combination? Why not use those monads in a normal way?

```haskell
main :: IO
main = do
  maybeUserName <- readUserName
  case maybeUserName of
    Nothing -> print “Invalid user name!”
    Just (uName) -> do
      maybeEmail <- readEmail
      case maybeEmail of
        Nothing -> print “Invalid email!”
        Just (email) -> do
          maybePassword <- readPassword
          Case maybePassword of
            Nothing -> print “Invalid Password”
            Just password -> login uName email password

readUserName :: IO (Maybe String)
readUserName = do
  str <- getLIne
  if length str > 5
    then return $ Just str
    else return Nothing

readEmail :: IO (Maybe String)
...

readPassword :: IO (Maybe String)
...

login :: String -> String -> String -> IO ()
...
```

By using monad transformer `MaybeT IO` that wraps `IO` actions the code can get much more readable and efficient. It is also better in terms of extensibility and modularity when you want to replace `IO`, for example.

```haskell
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Monad m) => Monad (MaybeT m) where
    return = lift . return
    x >>= f = MaybeT $ do
        v <- runMaybeT x
        case v of
            Nothing -> return Nothing
            Just y  -> runMaybeT (f y)
```

It is just a newtype that inserts maybe into some monad. Function `return` is then just lifted for this monad transformer and bind `>>=` works naturally for `Maybe` type, returns `Nothing` if `Nothing` is wrapped and evaluates with value from `Just` otherwise.

```haskell
readUserName :: MaybeT IO String
readUserName = MaybeT $ do
  str <- getLine
  if length str > 5
    then return $ Just str
    else return Nothing
```

The code is then pretty readable and understandable on the first sight, no more bothersome nested `if`s or `case`s. It is also better for maintenance obviously. Similarly it works for `EitherT` where it is important that in the inner monad (such as `IO`) is only the type of `Right` values and not `Left`s.

```haskell
newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Monad m => Monad (EitherT e m) where
  return a = EitherT $ return (Right a)
  m >>= k  = EitherT $ do
    a <- runEitherT m
    case a of
      Left  l -> return (Left l)
      Right r -> runEitherT (k r)
```

## More Layers of Monads

Monad transformers are as shown again monads, that means that you can wrap monad transfomers in monad transformers!

```haskell
type Env = (Maybe String, Maybe String, Maybe String)

readUserName :: MaybeT (ReaderT Env IO) String
readUserName = MaybeT $ do
  (maybeOldUser, _, _) <- ask
  case maybeOldUser of
    Just str -> return str
    Nothing -> do
      -- lift allows normal IO functions from inside ReaderT Env IO!
      input <- lift getLine
      if length input > 5
        then return (Just input)
        else return Nothing
```

But as you can see that to get into inner layers more lifts are needed... In such cases, some helper functions and naming your transformer is quite handy.

```haskell
type TripleMonad a = MaybeT (ReaderT Env IO) a

performReader :: ReaderT Env IO a -> TripleMonad a
performReader = lift

performIO :: IO a -> TripleMonad a
performIO = lift . lift

readUserName :: TripleMonad String
readUserName = do
  (maybeOldUser, _, _) <- performReader ask
  case maybeOldUser of
    Just str -> return str
    Nothing -> do
      input <- performIO getLine
      if length input > 5
        then return (Just input)
        else return Nothing
```

## Other Way for Transformers

As the other way around, there are some typeclasses which allow you to make certain assumptions about the monad stack below inside transformers. For instance, you often don’t care what the exact stack is, but you just need IO to exist somewhere in the layers because you want to do IO actions. For this purpose, there is typeclass `MonadIO` (also from [transformers](https://hackage.haskell.org/package/transformers) package).

```haskell
class (Monad m) => MonadIO m where
  liftIO :: IO a -> m a
```

If you create your own monad transformer, you can/should make it instance of `MonadIO` in order to allow anyone to use it in generic way to perform IO actions using standard `liftIO` function.

```haskell
type TripleMonad a = MaybeT (ReaderT Env IO) a

performReader :: ReaderT Env IO a -> TripleMonad a
performReader = lift

performIO :: IO a -> TripleMonad a
performIO = lift . lift

instance MonadIO (TripleMonad a) where
  liftIO = performIO

-- This generic function can be now used with our TripleMonad
debugFunc :: (MonadIO m) => String -> m a
debugFunc input = do
  liftIO $ putStrLn "Interpreting Input: " ++ input
  liftIO $ putStrLn "Cool huh?!"
```

## Example from DS Wizard

We can show some real use case from larger Haskell application - [Data Stewardship Wizard (server)](https://github.com/ds-wizard/dsw-server). Monad transformers are used, for instance, to have a nice and modular application context:

```haskell
module Model.Context.AppContext where

import Control.Applicative (Applicative)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (LoggingT, MonadLogger)
import Control.Monad.Reader (MonadReader, ReaderT)
import qualified Data.UUID as U
import Database.Persist.MongoDB (ConnectionPool)
import Network.AMQP (Channel)
import Network.HTTP.Client (Manager)

import Api.Resource.User.UserDTO
import Model.Config.AppConfig
import Model.Config.BuildInfoConfig

data AppContext = AppContext
  { _appContextAppConfig :: AppConfig
  , _appContextBuildInfoConfig :: BuildInfoConfig
  , _appContextPool :: ConnectionPool
  , _appContextMsgChannel :: Maybe Channel
  , _appContextHttpClientManager :: Manager
  , _appContextTraceUuid :: U.UUID
  , _appContextCurrentUser :: Maybe UserDTO
  }

newtype AppContextM a = AppContextM
  { runAppContextM :: ReaderT AppContext (LoggingT IO) a
  } deriving (Applicative, Functor, Monad, MonadIO, MonadReader AppContext, MonadLogger)
```

Thanks to its default behaviour, instances of typeclasses can be easily derived (notice mainly `MonadIO`). This `AppContextM` is used across whole web application for every endpoint or service and carries all necessary resources and information including database connection, configurations, logged user, message queue connection, and others. The best thing is that when needed, it is easy to add something new to the `AppContext` type (e.g. monitoring service connection) and it won't break anything!

Now see that you can use `IO`, `MonadReader`, and `MonadLogger` easily in the service:

```haskell
    
module Service.User.UserService where

import Control.Lens ((&), (.~), (^.))
import Control.Monad.Reader (asks, liftIO)
import Crypto.PasswordStore
import Data.ByteString.Char8 as BS
import Data.Maybe (fromMaybe)
import Data.Time
import qualified Data.UUID as U

import Api.Resource.ActionKey.ActionKeyDTO
import Api.Resource.User.UserChangeDTO
import Api.Resource.User.UserCreateDTO
import Api.Resource.User.UserDTO
import Api.Resource.User.UserPasswordDTO
import Api.Resource.User.UserProfileChangeDTO
import Api.Resource.User.UserStateDTO
import Database.DAO.User.UserDAO
import LensesConfig
import Localization
import Messaging.Out.Topic.UserTopic
import Model.ActionKey.ActionKey
import Model.Config.AppConfig
import Model.Context.AppContext
import Model.Error.Error
import Model.Error.ErrorHelpers
import Model.User.User
import Service.ActionKey.ActionKeyService
import Service.Common
import Service.Mail.Mailer
import Service.User.UserMapper
import Service.User.UserValidation
import Util.Uuid

getUsers :: AppContextM (Either AppError [UserDTO])
getUsers = heFindUsers $ \users -> return . Right $ toDTO <$> users

createUserByAdmin :: UserCreateDTO -> AppContextM (Either AppError UserDTO)
createUserByAdmin reqDto = do
  uUuid <- liftIO generateUuid
  createUserByAdminWithUuid reqDto uUuid

createUserByAdminWithUuid :: UserCreateDTO -> U.UUID -> AppContextM (Either AppError UserDTO)
createUserByAdminWithUuid reqDto uUuid = do
  uPasswordHash <- generatePasswordHash (reqDto ^. password)
  dswConfig <- asks _appContextAppConfig
  let uRole = fromMaybe (dswConfig ^. roles . defaultRole) (reqDto ^. role)
  let uPermissions = getPermissionForRole dswConfig uRole
  createUser reqDto uUuid uPasswordHash uRole uPermissions

registrateUser :: UserCreateDTO -> AppContextM (Either AppError UserDTO)
registrateUser reqDto =
  heCheckIfRegistrationIsEnabled $ do
    uUuid <- liftIO generateUuid
    uPasswordHash <- generatePasswordHash (reqDto ^. password)
    dswConfig <- asks _appContextAppConfig
    let uRole = dswConfig ^. roles . defaultRole
    let uPermissions = getPermissionForRole dswConfig uRole
    createUser reqDto uUuid uPasswordHash uRole uPermissions

createUser :: UserCreateDTO -> U.UUID -> String -> Role -> [Permission] -> AppContextM (Either AppError UserDTO)
createUser reqDto uUuid uPasswordHash uRole uPermissions =
  heValidateUserEmailUniqueness (reqDto ^. email) $ do
    now <- liftIO getCurrentTime
    let user = fromUserCreateDTO reqDto uUuid uPasswordHash uRole uPermissions now now
    insertUser user
    heCreateActionKey uUuid RegistrationActionKey $ \actionKey -> do
      publishToUserCreatedTopic user
      emailResult <- sendRegistrationConfirmationMail (toDTO user) (actionKey ^. hash)
      case emailResult of
        Left errMessage -> return . Left $ GeneralServerError _ERROR_SERVICE_USER__ACTIVATION_EMAIL_NOT_SENT
        _ -> do
          sendAnalyticsEmailIfEnabled user
          return . Right $ toDTO user
  where
    sendAnalyticsEmailIfEnabled user = do
      dswConfig <- asks _appContextAppConfig
      if dswConfig ^. analytics . enabled
        then sendRegistrationCreatedAnalyticsMail (toDTO user)
        else return $ Right ()

getUserById :: String -> AppContextM (Either AppError UserDTO)
getUserById userUuid = heFindUserById userUuid $ \user -> return . Right $ toDTO user

modifyUser :: String -> UserChangeDTO -> AppContextM (Either AppError UserDTO)
modifyUser userUuid reqDto =
  heFindUserById userUuid $ \user ->
    heValidateUserChangedEmailUniqueness (reqDto ^. email) (user ^. email) $ do
      dswConfig <- asks _appContextAppConfig
      updatedUser <- updateUserTimestamp $ fromUserChangeDTO reqDto user (getPermissions dswConfig reqDto user)
      updateUserById updatedUser
      return . Right . toDTO $ updatedUser
  where
    getPermissions dswConfig reqDto oldUser =
      if (reqDto ^. role) /= (oldUser ^. role)
        then getPermissionForRole dswConfig (reqDto ^. role)
        else oldUser ^. permissions

modifyProfile :: String -> UserProfileChangeDTO -> AppContextM (Either AppError UserDTO)
modifyProfile userUuid reqDto =
  heFindUserById userUuid $ \user ->
    heValidateUserChangedEmailUniqueness (reqDto ^. email) (user ^. email) $ do
      updatedUser <- updateUserTimestamp $ fromUserProfileChangeDTO reqDto user
      updateUserById updatedUser
      return . Right . toDTO $ updatedUser

changeUserPasswordByAdmin :: String -> UserPasswordDTO -> AppContextM (Maybe AppError)
changeUserPasswordByAdmin userUuid reqDto =
  hmFindUserById userUuid $ \user -> do
    passwordHash <- generatePasswordHash (reqDto ^. password)
    now <- liftIO getCurrentTime
    updateUserPasswordById userUuid passwordHash now
    return Nothing

changeCurrentUserPassword :: String -> UserPasswordDTO -> AppContextM (Maybe AppError)
changeCurrentUserPassword userUuid reqDto =
  hmFindUserById userUuid $ \user -> do
    passwordHash <- generatePasswordHash (reqDto ^. password)
    now <- liftIO getCurrentTime
    updateUserPasswordById userUuid passwordHash now
    return Nothing

changeUserPasswordByHash :: String -> Maybe String -> UserPasswordDTO -> AppContextM (Maybe AppError)
changeUserPasswordByHash userUuid maybeHash userPasswordDto =
  validateHash maybeHash $ \akHash ->
    hmFindUserById userUuid $ \user ->
      hmGetActionKeyByHash akHash $ \actionKey -> do
        passwordHash <- generatePasswordHash (userPasswordDto ^. password)
        now <- liftIO getCurrentTime
        updateUserPasswordById userUuid passwordHash now
        deleteActionKey (actionKey ^. hash)
        return Nothing
  where
    validateHash maybeHash callback =
      case maybeHash of
        Just akHash -> callback akHash
        Nothing ->
          return . Just . createErrorWithErrorMessage $ _ERROR_SERVICE_USER__REQUIRED_ADMIN_ROLE_OR_HASH_IN_QUERY_PARAMS

resetUserPassword :: ActionKeyDTO -> AppContextM (Maybe AppError)
resetUserPassword reqDto =
  hmFindUserByEmail (reqDto ^. email) $ \user ->
    hmCreateActionKey (user ^. uuid) ForgottenPasswordActionKey $ \actionKey -> do
      emailResult <- sendResetPasswordMail (toDTO user) (actionKey ^. hash)
      case emailResult of
        Left errMessage -> return . Just $ GeneralServerError _ERROR_SERVICE_USER__RECOVERY_EMAIL_NOT_SENT
        _ -> return Nothing

changeUserState :: String -> Maybe String -> UserStateDTO -> AppContextM (Maybe AppError)
changeUserState userUuid maybeHash userStateDto =
  validateHash maybeHash $ \akHash ->
    hmFindUserById userUuid $ \user ->
      hmGetActionKeyByHash akHash $ \actionKey -> do
        updatedUser <- updateUserTimestamp $ user & active .~ (userStateDto ^. active)
        updateUserById updatedUser
        deleteActionKey (actionKey ^. hash)
  where
    validateHash maybeHash callback =
      case maybeHash of
        Just akHash -> callback akHash
        Nothing -> return . Just . createErrorWithErrorMessage $ _ERROR_SERVICE_USER__REQUIRED_HASH_IN_QUERY_PARAMS

deleteUser :: String -> AppContextM (Maybe AppError)
deleteUser userUuid =
  hmFindUserById userUuid $ \user -> do
    deleteUserById userUuid
    return Nothing

-- --------------------------------
-- PRIVATE
-- --------------------------------
getPermissionForRole :: AppConfig -> Role -> [Permission]
getPermissionForRole config role =
  case role of
    "ADMIN" -> config ^. roles ^. admin
    "DATASTEWARD" -> config ^. roles ^. dataSteward
    "RESEARCHER" -> config ^. roles ^. researcher
    _ -> []

generatePasswordHash :: String -> AppContextM String
generatePasswordHash password = liftIO $ BS.unpack <$> makePassword (BS.pack password) 17

updateUserTimestamp :: User -> AppContextM User
updateUserTimestamp user = do
  now <- liftIO getCurrentTime
  return $ user & updatedAt .~ Just now

heCheckIfRegistrationIsEnabled = heCheckIfFeatureIsEnabled "Registration" (general . registrationEnabled)
```

## References

* [Real World Haskell - Monad Transformers](http://book.realworldhaskell.org/read/monad-transformers.html)
* [Wikibooks - Haskell/Monad Transformers](https://en.wikibooks.org/wiki/Haskell/Monad_transformers)
* [Wikipedia - Monad Transformer](https://en.wikipedia.org/wiki/Monad_transformer)
* [Monday Morning Haskell - Monads 6](https://mmhaskell.com/monads-6) = source of most of the examples
* [A Gentle Introduction to Monad Transformers](https://github.com/kqr/gists/blob/master/articles/gentle-introduction-monad-transformers.md)
* [A Gentle Introduction to Monad Transformers](https://github.com/kqr/gists/blob/master/articles/gentle-introduction-monad-transformers.md)
* [Monad Transformers aren’t hard!](https://medium.com/@alexander.zaidel/monad-transformers-arent-hard-23387c7ef4a6)
* [Haskell Wiki - Monad Transformers Explained](https://wiki.haskell.org/Monad_Transformers_Explained)

