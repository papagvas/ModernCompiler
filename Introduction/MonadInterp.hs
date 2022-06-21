{-# LANGUAGE LambdaCase #-}
import qualified Data.Map as Map
import Control.Monad.State.Lazy
import Control.Monad.Trans
import Data.Functor.Identity
import System.IO

type Id = String

data Binop = Plus | Minus | Times | Div deriving Show

data Stm = CompoundStm Stm Stm
         | AssignStm Id Exp
         | PrintStm [Exp]
         deriving Show

data Exp = IdExp Id
         | NumExp Int
         | OpExp Exp Binop Exp
         | EseqExp Stm Exp
         deriving Show

aux :: Exp -> Int
aux (OpExp l op r) = max (aux l) (aux r)
aux (EseqExp s e) = max (maxargs s) (aux e) 
aux _ = 0

maxargs :: Stm -> Int
maxargs (CompoundStm l r) = max (maxargs l) (maxargs r)
maxargs (AssignStm n e) = aux e
maxargs (PrintStm es) = max (length es) (maximum (map aux es)) 

type Env = Map.Map Id Int
--data Eval a = State Env a deriving State 

interpStm :: Stm -> StateT Env IO ()
interpStm stm = case stm of
  CompoundStm l r  -> interpStm l >> interpStm r
  AssignStm id exp -> do
    exp' <- interpExp exp
    case safeRead $ exp' of
      Just exp'' -> withStateT (Map.insert id exp'') (liftIO $ pure ())
      Nothing    -> liftIO $ hPutStrLn stderr "NaN"
  PrintStm []      -> liftIO $ pure ()
  PrintStm (e:es)  -> do
    interpExp e >>= liftIO . putStrLn
    interpStm (PrintStm es)
    

interpExp :: Exp -> StateT Env IO String
interpExp exp = case exp of 
  IdExp id        -> gets (Map.lookup id) >>= \case
    Just val -> pure $ show val 
    _        -> undefined
  NumExp num      -> pure $ show num
  OpExp l op r    -> do
    l' <- interpExp l
    case safeRead $ l' of
      Just l'' -> do
        r' <- interpExp r
        case safeRead $ r' of
          Just r'' -> pure . show $ parseOp op l'' r''
          Nothing -> pure "sussy"
      Nothing -> pure "megasus"
  EseqExp stm exp -> interpStm stm >> interpExp exp

safeRead :: String -> Maybe Int
safeRead val
  | [(x, "")] <- reads val = Just x
  | otherwise = Nothing
  
parseOp :: Binop -> Integral a => (a -> a -> a)
parseOp op = case op of
  Plus  -> (+)
  Minus -> (-)
  Times -> (*)
  Div   -> div
