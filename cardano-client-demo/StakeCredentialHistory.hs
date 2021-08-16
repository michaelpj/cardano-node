{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

import           Cardano.Api
import           Cardano.Api.Shelley
import           Control.Monad.Trans.Except (runExceptT)
import           Data.Char (ord)
import           Data.Foldable (toList)
import           Data.List (intercalate)
import           Data.Maybe (mapMaybe)
import qualified Data.Text as T (unpack)
import qualified Options.Applicative as Opt
import           Options.Applicative (Parser, (<|>), (<**>))

import qualified Ouroboros.Consensus.Shelley.Ledger as Shelley
import qualified Shelley.Spec.Ledger.Rewards as L
import qualified Shelley.Spec.Ledger.RewardUpdate as L
import qualified Cardano.Crypto.Hash.Class as Crypto (Hash (UnsafeHash))
import qualified Shelley.Spec.Ledger.API as L
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Base16 as Base16
import           Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)

data State = State
  { lastCheckpoint    :: SlotNo
  , lastRewStartEpoch :: EpochNo
  , lastRewEndEpoch   :: EpochNo
  , lastEra           :: String
  }

startingState :: State
startingState = State
  { lastCheckpoint    = SlotNo 0
  , lastRewStartEpoch = EpochNo 0
  , lastRewEndEpoch   = EpochNo 0
  , lastEra           = "byron"
  }

data IsOwner = IsOwnerYes | IsOwnerNo
  deriving (Show)

data IsPoolRewardAccount = IsPoolRewardAccountYes | IsPoolRewardAccountNo
  deriving (Show)

data Event c
  = CheckPointEvent SlotNo
  | NewEraEvent EpochNo SlotNo String
  | StakeRegistrationEvent SlotNo
  | StakeDeRegistrationEvent SlotNo
  | DelegationEvent SlotNo (Hash StakePoolKey)
  | PoolRegistrationEvent SlotNo (Hash StakePoolKey) IsOwner IsPoolRewardAccount
  | MIREvent SlotNo Lovelace
  | WithdrawalEvent SlotNo Lovelace
  | RewardStartEvent EpochNo SlotNo Lovelace
  | RewardEndEvent EpochNo SlotNo (Set (L.Reward c))

msg :: Event c -> IO ()
msg ev = putStrLn (message ev)
  where
    message :: Event c -> String
    message (CheckPointEvent slotNo)       =
      "CHECKPOINT ------- "
        <> show slotNo
    message (NewEraEvent e slotNo name)    =
      "NEW-ERA ----------- "
        <> show e      <> ", "
        <> show slotNo <> ", "
        <> name
    message (StakeRegistrationEvent slotNo)       =
      "REGISTRATION ------ "
        <> show slotNo
    message (StakeDeRegistrationEvent slotNo)     =
      "DE-REGISTRATION --- "
        <> show slotNo
    message (DelegationEvent slotNo poolId)       =
      "DELEGATION -------- "
        <> show slotNo <> ", "
        <> "PoolID " <> (tail . init $ show poolId)
    message (PoolRegistrationEvent slotNo poolId owner rewardAccount) =
      "POOL-REGISTRATION - "
        <> show slotNo
        <> ", PoolID " <> (tail . init $ show poolId)
        <> dispOwner owner
        <> dispRewardAcnt rewardAccount
    message (MIREvent slotNo love) =
      "MIR --------------- "
        <> show slotNo <> ", "
        <> show love
    message (WithdrawalEvent slotNo w)  =
      "WDRL -------------- "
        <> show slotNo <> ", "
        <> show w
    message (RewardStartEvent e slotNo stake) =
      "REWARD-START ------ "
        <> show e      <> ", "
        <> show slotNo <> ", "
        <> "stake: " <> show stake
    message (RewardEndEvent e slotNo rewards)   =
      "REWARD-END -------- "
        <> show e      <> ", "
        <> show slotNo <> ", "
        <> intercalate ", " (map displayReward $ toList rewards)

    dispOwner IsOwnerYes = ", Owner"
    dispOwner IsOwnerNo  = ""

    dispRewardAcnt IsPoolRewardAccountYes = ", Reward-Account"
    dispRewardAcnt IsPoolRewardAccountNo  = ""

    displayReward (L.Reward t (L.KeyHash kh) love) =
      show t <> "-" <> (tail . init $ show kh) <> "-" <> show (fromShelleyLovelace love)

hexStrToSBS :: String -> SBS.ShortByteString
hexStrToSBS =
  SBS.toShort . f . Base16.decode . (BS.pack . map (fromIntegral . ord))
  where
    f (Left e) = error $ "Invalid hex stake address: " <> show e
    f (Right x) = x

hexStrToHash :: String -> Hash StakeKey
hexStrToHash = StakeKeyHash . L.KeyHash . Crypto.UnsafeHash . hexStrToSBS

hexStrToHash' :: forall a c. String -> Crypto.Hash a c
hexStrToHash' = coerce . Crypto.UnsafeHash . hexStrToSBS

data CheckPoint = CheckPointOff | CheckPointSize SlotNo

pCheckPoint :: Parser CheckPoint
pCheckPoint =
  pOff <|> pOn
 where
   pOff :: Parser CheckPoint
   pOff =
    Opt.flag CheckPointOff CheckPointOff
      (  Opt.long "checkpoints-off"
      <> Opt.help "no checkpoints (default)"
      )
   pOn :: Parser CheckPoint
   pOn =
     CheckPointSize . SlotNo <$>
       Opt.option Opt.auto
         (  Opt.long "check-point-size"
         <> Opt.metavar "NATURAL"
         <> Opt.help "display checkpoints"
         )

data Args = Args
  { conf       :: String
  , socket     :: String
  , target     :: String
  , checkpoint :: CheckPoint }

parser :: Parser Args
parser = Args
  <$> Opt.strOption
      ( Opt.long "conf"
     <> Opt.short 'c'
     <> Opt.metavar "FILEPATH"
     <> Opt.help "configuration file" )
  <*> Opt.strOption
      ( Opt.long "socket"
     <> Opt.short 's'
     <> Opt.metavar "FILEPATH"
     <> Opt.help "socket" )
  <*> Opt.strOption
      ( Opt.long "target"
     <> Opt.short 't'
     <> Opt.metavar "HEX"
     <> Opt.help "target stake address in hex" )
  <*> pCheckPoint

main :: IO ()
main = do
  args <- Opt.execParser $
    Opt.info (parser <**> Opt.helper) (Opt.fullDesc <> Opt.progDesc "Stake Credential History")
  let targetAsAPIHash    = hexStrToHash  (target args)
  let targetAsCredential = L.KeyHashObj . L.KeyHash . hexStrToHash' $ target args
  let f = either (error . T.unpack . renderFoldBlocksError) id
  _ <- fmap f $ runExceptT $ foldBlocks
         (conf args)
         (socket args)
         True -- validation enabled
         startingState
         (\_env
           !ledgerState
           (BlockInMode
             (Block (BlockHeader slotNo _blockHeaderHash (BlockNo _blockNoI)) transactions)
             _era)
           state -> do
             let getGoSnapshot = L.unStake . L._stake . L._pstakeGo . L.esSnapshots . L.nesEs

             -- in non-byron eras, get the necessary components of the ledger state
             let (name, shelleyState) =
                   case ledgerState of
                     LedgerStateByron _                                     ->
                       ("byron",   Nothing)
                     LedgerStateShelley (Shelley.ShelleyLedgerState _ ls _) ->
                       ("shelley", Just (L.nesEL ls, L.nesRu ls, getGoSnapshot ls))
                     LedgerStateAllegra (Shelley.ShelleyLedgerState _ ls _) ->
                       ("allegra", Just (L.nesEL ls, L.nesRu ls, getGoSnapshot ls))
                     LedgerStateMary    (Shelley.ShelleyLedgerState _ ls _) ->
                       ("mary",    Just (L.nesEL ls, L.nesRu ls, getGoSnapshot ls))

             let txBodyComponents = map ( (\(TxBody txbc) -> txbc) . getTxBody ) transactions
             mapM_ (delegationEvents targetAsAPIHash slotNo) txBodyComponents
             mapM_ (withdrawalEvents targetAsAPIHash slotNo) txBodyComponents

             lastcheck <- displayCheckpoint slotNo (lastCheckpoint state) (checkpoint args)

             case shelleyState of
               Just (epochNo, L.SJust (L.Complete ru), goSnap) -> do
                 es <- rewardStartEvent (lastRewStartEpoch state) epochNo slotNo goSnap    targetAsCredential
                 ee <- rewardEndEvent   (lastRewEndEpoch   state) epochNo slotNo (L.rs ru) targetAsCredential
                 return $ state { lastCheckpoint = lastcheck
                                , lastRewStartEpoch = es
                                , lastRewEndEpoch = ee
                                }
               Just (epochNo, _, _) -> do
                 era <- newEraEvent name epochNo slotNo (lastEra state)
                 return $ state { lastCheckpoint = lastcheck, lastEra = era }
               _ -> return $ state {lastCheckpoint = lastcheck}
    )

  return ()
  where

    -- CheckPoints --
    displayCheckpoint _ lastcheck CheckPointOff = return lastcheck
    displayCheckpoint slot lastcheck (CheckPointSize checkpointSize) =
      if slot - lastcheck >= checkpointSize
        then msg (CheckPointEvent slot) >> return slot
        else return lastcheck

    -- New Eras --
    newEraEvent name epochNo slotNo eraLast =
      if name /= eraLast
        then msg (NewEraEvent epochNo slotNo name) >> return name
        else return eraLast

    -- Delegation Events --
    delegationEvents t slotNo txbc = case txCertificates txbc of
      TxCertificatesNone    -> return ()
      TxCertificates _ cs _ -> mapM_ msg $ mapMaybe (targetedCert t slotNo) cs

    isTarget t (StakeCredentialByKey      kh) = t == kh
    isTarget _ (StakeCredentialByScript  _sh) = False

    targetedCert t slotNo (StakeAddressRegistrationCertificate cred)   =
      if isTarget t cred then Just (StakeRegistrationEvent slotNo) else Nothing
    targetedCert t slotNo (StakeAddressDeregistrationCertificate cred) =
      if isTarget t cred then Just (StakeDeRegistrationEvent slotNo) else Nothing
    targetedCert t slotNo (StakeAddressDelegationCertificate cred pool) =
      if isTarget t cred then Just (DelegationEvent slotNo pool) else Nothing
    targetedCert t slotNo (StakePoolRegistrationCertificate pool)      =
      inPoolCert t slotNo pool
    targetedCert _ _ (StakePoolRetirementCertificate _ _)              = Nothing
    targetedCert _ _ (GenesisKeyDelegationCertificate _ _ _)           = Nothing
    targetedCert t slotNo (MIRCertificate _ (StakeAddressesMIR mir))   =
      inMir t slotNo mir
    targetedCert _ _ (MIRCertificate _ (SendToReservesMIR _))          = Nothing
    targetedCert _ _ (MIRCertificate _ (SendToTreasuryMIR _))          = Nothing

    stakeCredentialFromStakeAddress (StakeAddress _ cred) = fromShelleyStakeCredential cred

    inPoolCert t slotNo pool =
      case (owner, rewardAccount) of
        (IsOwnerNo, IsPoolRewardAccountNo) -> Nothing
        _ -> Just (PoolRegistrationEvent slotNo (stakePoolId pool) owner rewardAccount)
     where
       owner = if t `elem` stakePoolOwners pool then IsOwnerYes else IsOwnerNo
       isRewardAccount = isTarget t (stakeCredentialFromStakeAddress . stakePoolRewardAccount $ pool)
       rewardAccount = if isRewardAccount then IsPoolRewardAccountYes else IsPoolRewardAccountNo

    inMir t slotNo mir =
      case filter (isTarget t . fst) mir of
        []          -> Nothing
        [(_, love)] -> Just $ MIREvent slotNo love
        _           -> error $ "MIR keys should be unique: " <> show mir

    -- Withdrawal Events --
    withdrawalEvents t slotNo txbc = case txWithdrawals txbc of
      TxWithdrawalsNone  -> return ()
      TxWithdrawals _ ws -> mapM_ (withdrawalEvent slotNo) $ filter (targetWithdrawal t) ws

    withdrawalEvent slotNo (_, c, _) = msg $ WithdrawalEvent slotNo c
    targetWithdrawal t (ra, _, _) = isTarget t $ stakeCredentialFromStakeAddress ra

    -- Once Per Epoch Events --
    epochEvent epochLast epochCurrent slot mp t makeEvent =
      if epochLast < epochCurrent
        then case Map.lookup t mp of
               Nothing   -> return epochCurrent
               Just love -> msg (makeEvent epochCurrent slot love)
                              >> return epochCurrent
        else return epochLast

    -- Reward Calculation Start Event --
    rewardStartEvent epochLast epochCurrent slot ss t =
      epochEvent epochLast epochCurrent slot ss t
        (\e s v -> RewardStartEvent e s (fromShelleyLovelace v))

    -- Reward Calculation End Event --
    rewardEndEvent epochLast epochCurrent slot rs t =
      epochEvent epochLast epochCurrent slot rs t RewardEndEvent
