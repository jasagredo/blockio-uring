{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE InterruptibleFFI #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_GHC -fobject-code #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module System.IO.BlockIO.URingFFI where

import Control.Monad (when)
import Data.Char (isDigit)
import Foreign
import Foreign.C
import Prelude hiding (head, tail)
import System.Posix.Types
import Text.Read (readMaybe)

#include <liburing.h>
#include <sys/utsname.h>

data {-# CTYPE "liburing.h" "struct io_uring" #-} URing = URing

instance Storable URing where
  sizeOf    _ = #{size      struct io_uring}
  alignment _ = #{alignment struct io_uring}
  peek      _ = return URing
  poke    _ _ = return ()

foreign import capi unsafe "liburing.h io_uring_queue_init"
  io_uring_queue_init :: CUInt -> Ptr URing -> CUInt -> IO CInt

foreign import capi unsafe "liburing.h io_uring_queue_exit"
  io_uring_queue_exit :: Ptr URing -> IO ()

foreign import capi safe "liburing.h io_uring_queue_init_params"
  io_uring_queue_init_params :: CUInt -> Ptr URing -> Ptr URingParams -> IO CInt

iORING_SETUP_CQSIZE :: CUInt
iORING_SETUP_CQSIZE = #{const IORING_SETUP_CQSIZE}

data {-# CTYPE "liburing.h" "struct io_uring_params" #-}
     URingParams = URingParams {
                      sq_entries :: !CUInt,
                      cq_entries :: !CUInt,
                      flags      :: !CUInt,
                      features   :: !CUInt
                      -- Note: this is a subset of all the fields. These are
                      -- just the ones we need now or are likely too need.
                      -- If you need more, just add them.
                    }
  deriving stock (Show, Eq)

instance Storable URingParams where
  sizeOf    _    = #{size      struct io_uring_params}
  alignment _    = #{alignment struct io_uring_params}
  peek      p    = do sq_entries     <- #{peek struct io_uring_params, sq_entries} p
                      cq_entries     <- #{peek struct io_uring_params, cq_entries} p
                      flags          <- #{peek struct io_uring_params, flags} p
                      features       <- #{peek struct io_uring_params, features} p
                      return URingParams {..}
  poke      p ps = do -- As we only cover a subset of the fields, we must clear
                      -- the remaining fields we don't set to avoid them
                      -- containing arbitrary values.
                      fillBytes p 0 #{size struct io_uring_params}

                      #{poke struct io_uring_params, sq_entries} p (sq_entries ps)
                      #{poke struct io_uring_params, cq_entries} p (cq_entries ps)
                      #{poke struct io_uring_params, flags} p (flags ps)
                      #{poke struct io_uring_params, features} p (features ps)


--
-- Submitting I/O
--

data {-# CTYPE "liburing.h" "struct io_uring_sqe" #-} URingSQE

foreign import capi unsafe "liburing.h io_uring_get_sqe"
  io_uring_get_sqe :: Ptr URing -> IO (Ptr URingSQE)

#ifdef LIBURING_HAVE_DATA64
foreign import capi unsafe "liburing.h io_uring_sqe_set_data64"
  io_uring_sqe_set_data :: Ptr URingSQE -> CULong -> IO ()
#else
io_uring_sqe_set_data :: Ptr URingSQE -> CULong -> IO ()
io_uring_sqe_set_data p user_data =
  do #{poke struct io_uring_sqe, user_data} p user_data
#endif

foreign import capi unsafe "liburing.h io_uring_prep_read"
  io_uring_prep_read :: Ptr URingSQE -> Fd -> Ptr Word8 -> CUInt -> CULong -> IO ()

foreign import capi unsafe "liburing.h io_uring_prep_write"
  io_uring_prep_write :: Ptr URingSQE -> Fd -> Ptr Word8 -> CUInt -> CULong -> IO ()

foreign import capi unsafe "liburing.h io_uring_prep_nop"
  io_uring_prep_nop :: Ptr URingSQE -> IO ()

foreign import capi unsafe "liburing.h io_uring_submit"
  io_uring_submit :: Ptr URing -> IO CInt


--
-- Collecting I/O
--

data {-# CTYPE "liburing.h" "struct io_uring_cqe" #-}
     URingCQE = URingCQE {
                  cqe_data  :: !CULong,
                  cqe_res   :: !CInt
                }
  deriving stock Show

instance Storable URingCQE where
  sizeOf    _ = #{size      struct io_uring_cqe}
  alignment _ = #{alignment struct io_uring_cqe}
  peek      p = do cqe_data  <- #{peek struct io_uring_cqe, user_data} p
                   cqe_res   <- #{peek struct io_uring_cqe, res} p
                   return URingCQE { cqe_data, cqe_res }
  poke    _ _ = return ()


foreign import capi safe "liburing.h io_uring_wait_cqe"
  io_uring_wait_cqe :: Ptr URing -> Ptr (Ptr URingCQE) -> IO CInt

foreign import capi unsafe "liburing.h io_uring_peek_cqe"
  io_uring_peek_cqe :: Ptr URing -> Ptr (Ptr URingCQE) -> IO CInt

foreign import capi unsafe "liburing.h io_uring_cqe_seen"
  io_uring_cqe_seen :: Ptr URing -> Ptr URingCQE -> IO ()


--
-- Calling @io_uring_set_iowait(3)@ if available
--

-- | Available only on liburing >= 2.10 and kernel >= 6.15.
--
-- See @io_uring_set_iowait(3)@ man page.
#ifndef IO_URING_CHECK_VERSION
#define IO_URING_CHECK_VERSION(a, b) 1   /* liburing < 2.2: doesn't have IO_URING_CHECK_VERSION, so we return TRUE always */
#endif

#if !IO_URING_CHECK_VERSION(2, 10)
foreign import capi unsafe "liburing.h io_uring_set_iowait"
  io_uring_set_iowait :: Ptr URing -> CBool -> IO CInt
#else
io_uring_set_iowait :: Ptr URing -> CBool -> IO CInt
io_uring_set_iowait _ _ = pure #{const EOPNOTSUPP}
#endif

type Major = Int
type Minor = Int

foreign import capi unsafe "sys/utsname.h uname"
  c_uname :: Ptr a -> IO CInt

-- | Is the running kernel at version @major.minor@ or newer?
--
-- Returns 'False' if @uname(2)@ fails or the release string cannot be parsed.
whenKernelVersionGtEq :: (Major, Minor) -> IO () -> IO ()
whenKernelVersionGtEq (major, minor) action = do
  isGtEq <- allocaBytes #{size struct utsname} $ \p -> do
    res <- c_uname p
    if res /= 0
      then pure False
      else do
        release <- peekCString (p `plusPtr` #{offset struct utsname, release})
        pure $ case parseKernelRelease release of
          Just (kmaj, kmin) -> kmaj > major || (kmaj == major && kmin >= minor)
          Nothing           -> False
  when isGtEq action

-- | Parse a kernel release string like @"6.15.0-22-generic"@ into
-- @(major, minor)@. Returns 'Nothing' if the prefix doesn't match
-- @DIGITS \".\" DIGITS@.
parseKernelRelease :: String -> Maybe (Major, Minor)
parseKernelRelease s =
  case break (== '.') s of
    (majS, '.':rest) -> do
      let minS = takeWhile isDigit rest
      (,) <$> readMaybe majS <*> readMaybe minS
    _ -> Nothing
