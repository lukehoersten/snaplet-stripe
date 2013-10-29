{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.Stripe
       ( HasStripe (..)
       , StripeState (..)
       , initStripe
       , addStripeSplices
       , addCustomer
       , addCustomerWithCard
       , addCardByCustId
       , chargeCustomer
       , chargeConnectCustomer
       , fromAmount
       , toAmount
       , charge
       , connectCharge
       , customer
       , getAuthURL
       ) where


import           Control.Applicative        ((<$>), (<*>))
import           Control.Monad              (when)
import           Control.Monad.State        (get)
import           Control.Monad.Trans        (MonadIO, lift, liftIO)
import           Control.Monad.Trans.Writer (WriterT, runWriterT, tell)
import qualified Data.Configurator          as C
import           Data.List                  (intercalate)
import           Data.Maybe                 (fromMaybe, isNothing)
import           Data.Monoid                (mempty)
import           Data.Text                  (Text)
import           Data.Text.Encoding         (decodeUtf8)
import           Data.Text.Format           (Format, Only (..), format)
import qualified Data.Text.Lazy             as TL
import           Heist                      (HeistConfig (..), HeistT, Template)
import           Heist.SpliceAPI            (( ## ))
import           Snap.Snaplet               (Handler, Initializer, Snaplet,
                                             SnapletInit, SnapletLens,
                                             getSnapletUserConfig, makeSnaplet,
                                             with)
import           Snap.Snaplet.Heist         (HasHeist, Heist, SnapletISplice,
                                             addConfig)
import qualified Text.XmlHtml               as X

import           Web.Stripe.Charge          (Amount (..), Charge, ChargeId,
                                             Currency, chargeCustomerById,
                                             chargeTokenById, getCharge)
import           Web.Stripe.Client          (APIKey (..), StripeConfig (..),
                                             StripeFailure, key, runStripeT)
import           Web.Stripe.Connect         (AccessToken, ClientId, Landing,
                                             Scope, URL, authURL,
                                             createCustomerToken)
import           Web.Stripe.Coupon          (CpnId)
import           Web.Stripe.Customer        (Customer, CustomerId, Description,
                                             Email, createCustomerByTokenId,
                                             getCustomer,
                                             updateCustomerByIdByTokenId)
import           Web.Stripe.Plan            (PlanId)
import           Web.Stripe.Token           (TokenId, tokId)


newtype PublicKey = PublicKey { unPublicKey :: Text } deriving (Show, Eq)

data StripeState = StripeState
    { stripeConfig          :: StripeConfig
    , stripePublicKey       :: PublicKey
    , stripeConnectClientId :: ClientId
    } deriving Show


class HasStripe b where
    getStripeState :: b StripeState


instance HasStripe (Handler b StripeState) where
    getStripeState = get


logErr :: MonadIO m => t -> IO (Maybe a) -> WriterT [t] m (Maybe a)
logErr err m = do
  res <- liftIO m
  when (isNothing res) (tell [err])
  return res


initStripe :: SnapletInit b StripeState
initStripe = makeSnaplet "stripe" "Stripe credit card payment" Nothing $ do
  config <- getSnapletUserConfig

  (stripeState, errors) <- runWriterT $ do
    secretKey <- logErr "Must specify Strip secret key" $ C.lookup config "secret_key"
    publicKey <- logErr "Must specify Strip public key" $ C.lookup config "public_key"
    clientId  <- logErr "Must specify Strip client ID"  $ C.lookup config "client_id"
    let caFilePath = Just "" -- This is unused by Stripe but vestigial in the Haskell library.

    return $ StripeState <$> (StripeConfig <$> (APIKey <$> secretKey) <*> caFilePath) <*> (PublicKey <$> publicKey) <*> clientId
  return $ fromMaybe (error $ intercalate "\n" errors) stripeState


withSC :: (Functor m, HasStripe m, MonadIO m) => (StripeConfig -> m b) -> m b
withSC = (stripeConfig <$> getStripeState >>=)


withSS :: (HasStripe m, MonadIO m) => (StripeState -> m b) -> m b
withSS = (getStripeState >>=)


addCustomer' :: (Functor m, HasStripe m, MonadIO m) => Maybe TokenId -> Maybe CpnId
             -> Maybe Email -> Maybe Description -> Maybe PlanId -> Maybe Int
             -> m (Either StripeFailure Customer)
addCustomer' mti mcid me md mpid mtime =
    withSC . flip runStripeT $ createCustomerByTokenId mti mcid me md mpid mtime


customer :: (Functor m, HasStripe m, MonadIO m) => CustomerId -> m (Either StripeFailure Customer)
customer = withSC . flip runStripeT . getCustomer


getAuthURL :: (MonadIO m, HasStripe m) => Maybe Scope -> Maybe Text -> Maybe Landing -> m URL
getAuthURL msc mst mld = withSS $ return . authURL msc mst mld . stripeConnectClientId


chargeCustomer :: (Functor m, HasStripe m, MonadIO m) => CustomerId -> Amount -> Currency
               -> Maybe Description -> m (Either StripeFailure Charge)
chargeCustomer cid a c md =
  withSC . flip runStripeT $ chargeCustomerById cid a c md Nothing


chargeConnectCustomer :: (Functor m, HasStripe m, MonadIO m) => CustomerId -> Amount -> Currency
                      -> Maybe Description -> AccessToken -> Maybe Amount
                      -> m (Either StripeFailure Charge)
chargeConnectCustomer cid am cu md k maf = withSC $ \sc -> do
  let sc' = sc { key = accessTokenToKey k }
  let ch = tokId <$> createCustomerToken cid >>=
          (\tid -> chargeTokenById tid am cu md maf)
  runStripeT sc' ch


updateCustomerById :: (Functor m, HasStripe m, MonadIO m) => CustomerId -> Maybe TokenId
                   -> Maybe CpnId -> Maybe Email -> Maybe Description
                   -> m (Either StripeFailure Customer)
updateCustomerById cid mti mcid me md =
  withSC . flip runStripeT $ updateCustomerByIdByTokenId cid mti mcid me md


connectCharge :: (Functor m, HasStripe m, MonadIO m) => AccessToken -> ChargeId
                 -> m (Either StripeFailure Charge)
connectCharge k c = withSC $ \sc -> do
  let sc' = sc { key = accessTokenToKey k }
  runStripeT sc' (getCharge c)


charge :: (Functor m, HasStripe m, MonadIO m) => ChargeId -> m (Either StripeFailure Charge)
charge = withSC . flip runStripeT . getCharge


-- Simple API helpers --
addCustomer :: (Functor m, HasStripe m, MonadIO m) => Email -> m (Either StripeFailure Customer)
addCustomer email = addCustomer' Nothing Nothing (Just email) Nothing Nothing Nothing


addCustomerWithCard :: (Functor m, HasStripe m, MonadIO m) => Email -> TokenId
                    -> m (Either StripeFailure Customer)
addCustomerWithCard email tid =
    addCustomer' (Just tid) Nothing (Just email) Nothing Nothing Nothing


addCardByCustId :: (Functor m, HasStripe m, MonadIO m) => CustomerId -> TokenId
                -> m (Either StripeFailure Customer)
addCardByCustId cid tid = updateCustomerById cid (Just tid) Nothing Nothing Nothing


-- Functional Helpers
toAmount :: Float -> Amount
toAmount = Amount . truncate . (* 100)


fromAmount :: Amount -> Float
fromAmount = (/ 100) . fromIntegral . unAmount


accessTokenToKey :: AccessToken -> APIKey
accessTokenToKey = APIKey . decodeUtf8


-- Public Key Splice
addStripeSplices :: HasHeist b => Snaplet (Heist b) -> SnapletLens b StripeState -> Initializer b v ()
addStripeSplices h stripe = addConfig h $ mempty { hcInterpretedSplices = ("stripePublicKeyJs" ## stripePublicKeyJsSplice stripe) }


stripePublicKeyJsSplice :: SnapletLens b StripeState -> SnapletISplice b
stripePublicKeyJsSplice stripe = jsSplice =<< (lift $ with stripe getStripeState)
    where renderJs = TL.toStrict . format ("var stripePublicKey = '{}';" :: Format) . Only . TL.fromStrict
          jsSplice = scriptSplice . renderJs . unPublicKey . stripePublicKey


scriptSplice :: Monad m => Text -> HeistT n m Template
scriptSplice t = return [X.Element "script" [("type", "text/javascript")] [X.TextNode t]]
