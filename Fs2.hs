{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Control.Monad (ap, forM_)
import Data.Binary.Get as G
import Data.Binary.Get qualified as B
import Data.Bits
import Data.ByteString qualified as SB
import Data.ByteString.Lazy qualified as B
import Data.Function ((&))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Int (Int16, Int32, Int64)
import Data.List (intercalate)
import Data.Word (Word16, Word32, Word64, Word8)
import System.Environment (getArgs)

data TOCEntry = TOCEntry
  {tocKey :: !Word16, offset :: !Word16, blocks :: !Word16}
  deriving (Show)

data Op
  = Op_0x00_xyz !Int16 !Int16 !Int16
  | Op_0x01_xyz !Int16 !Int16 !Int16
  | Op_0x02_vertex {x :: !Int16, y :: !Int16, z :: !Int16}
  | Op_0x03 {angle :: !Int16, len1 :: !Int16, len2 :: !Int16}
  | Op_0x05 {var :: !Word16, int1 :: !Int32, int2 :: !Int32}
  | Op_0x0a_toc ![TOCEntry]
  | Op_0x0b_jump_rel !Int
  | Op_0x0c_set_fill_color !Word16
  | Op_0x0d_load {loadKey :: !Word16, readOfs :: !Word16, d5 :: !Word8}
  | Op_0x12_set_fill_color !Word16
  | Op_0x16_set_fill_color !Word8 !Word8 !Word8 !Word8 !Word16 !Word16 !Word16
  | Op_0x17_set_fill_color !Word8 !Word8 !Word8 !Word8 !Word16 !Word16 !Word16
  | Op_0x18_call !Int16
  | Op_0x19_return
  | Op_0x1a_copy_word {varTgt :: !Word16, varSrc :: !Word16}
  | Op_0x1b_set_fill_color_5
  | Op_0x1c_set_fill_color_2
  | Op_0x1d {cmp :: !Word16, long1 :: !Word32, long2 :: !Word32}
  | Op_0x1e {size :: !Word16}
  | Op_0x1f_call !Int16
  | Op_0x20_jump_if_outside1 {ofs :: !Word16, var :: !Word16, lwb :: !Int16, upb :: !Int16}
  | Op_0x21_jump_if_outside2
      { ofs :: !Word16,
        var1 :: !Word16,
        lwb1 :: !Int16,
        upb1 :: !Int16,
        var2 :: !Word16,
        lwb2 :: !Int16,
        upb2 :: !Int16
      }
  | Op_0x23_jump_unless
      { ofs :: !Word16,
        var :: !Word16,
        mask :: !Word16
      }
  | Op_0x24_coord_frame {scale :: !Int16, x :: !Int16, y :: !Int16, z :: !Int16}
  | Op_0x25_write_word
      { var :: !Word16,
        to :: !Word16
      }
  | Op_0x28_jump_if_compare {op :: !Word16, ofs :: !Word16, var1 :: !Word16, var2 :: !Word16}
  | Op_0x29_draw_end
  | Op_0x2a_true
  | Op_0x2b !Int16 !Int16 !Int16 !Int16 !Int16 !Int16 !Int16
  | Op_0x2f_draw_begin
  | Op_0x31_vertex_idx {idx :: !Word16, x :: !Int16, y :: !Int16, z :: !Int16}
  | Op_0x32_load_vertex {idx :: !Word16}
  | Op_0x33_fill_poly {idx :: !Word16}
  | Op_0x34_jump_if_outside3 {ofs :: !Word16, z :: !Int16, x :: !Int16, delta :: !Int16}
  | Op_0x37_coord_frame {scale :: !Int16, x :: !Int16, y :: !Int16, z :: !Int16}
  | Op_0x39_set_palette_entry !Word16 !Word16
  | Op_0x40_vertex_2d {x :: !Int16, z :: !Int16}
  | Op_0x41_vertex_2d {x :: !Int16, z :: !Int16}
  | Op_0x42_jump_if_outside_box {ofs :: !Word16, minX :: !Int16, maxX :: !Int16, minZ :: !Int16, maxZ :: !Int16}
  | Op_0x43_jump_if_not_in_range {ofs :: !Word16, px :: Int32, py :: Int32, pz :: Int32, alpha :: !Word16, dx :: !Word32, dy :: !Word32, dz :: !Word32}
  | Op_0x79_end
  | OpOther !Word8
  deriving (Show)

parseTOCEntry :: G.Get TOCEntry
parseTOCEntry = return TOCEntry `ap` G.getWord16be `ap` G.getWord16be `ap` G.getWord16be

parseOp :: G.Get Op
parseOp = do
  G.getInt8
  op <- G.getWord8
  case op of
    0x0 -> do
      x <- G.getInt16be
      y <- G.getInt16be
      z <- G.getInt16be
      return $ Op_0x00_xyz x y z
    0x01 -> do
      x <- G.getInt16be
      y <- G.getInt16be
      z <- G.getInt16be
      return $ Op_0x01_xyz x y z
    0x02 ->
      return Op_0x02_vertex
        `ap` B.getInt16be
        `ap` B.getInt16be
        `ap` B.getInt16be
    0x03 ->
      return Op_0x03
        `ap` B.getInt16be
        `ap` B.getInt16be
        `ap` B.getInt16be
    0x05 ->
      return Op_0x05 `ap` G.getWord16be `ap` G.getInt32be `ap` G.getInt32be
    0x0a -> do
      ofs <- G.getWord16be
      records <- mapM (const parseTOCEntry) [0 .. (ofs - 4) `div` 6 - 1]
      return $ Op_0x0a_toc records
    0x0b -> do
      ofs <- G.getInt16be
      return $ Op_0x0b_jump_rel $ fromIntegral ofs
    0x0c ->
      return Op_0x0c_set_fill_color
        `ap` B.getWord16be
    0x0d -> do
      key <- G.getWord16be
      readOfs <- G.getWord16be
      G.getWord8
      d5 <- G.getWord8
      return $ Op_0x0d_load key readOfs d5
    0x12 ->
      return Op_0x12_set_fill_color
        `ap` G.getWord16be
    0x16 ->
      return Op_0x16_set_fill_color
        `ap` G.getWord8
        `ap` G.getWord8
        `ap` G.getWord8
        `ap` G.getWord8
        `ap` G.getWord16be
        `ap` G.getWord16be
        `ap` G.getWord16be
    0x17 ->
      return Op_0x17_set_fill_color
        `ap` G.getWord8
        `ap` G.getWord8
        `ap` G.getWord8
        `ap` G.getWord8
        `ap` G.getWord16be
        `ap` G.getWord16be
        `ap` G.getWord16be
    0x18 ->
      return Op_0x18_call
        `ap` G.getInt16be
    0x19 ->
      return Op_0x19_return
    0x1a ->
      return Op_0x1a_copy_word `ap` G.getWord16be `ap` G.getWord16be
    0x1b ->
      return Op_0x1b_set_fill_color_5
    0x1c ->
      return Op_0x1c_set_fill_color_2
    0x1d ->
      return Op_0x1d `ap` G.getWord16be `ap` G.getWord32le `ap` G.getWord32le
    0x1e -> do
      size <- G.getWord16be
      G.skip $ fromIntegral size - 4
      return $ Op_0x1e size
    0x1f ->
      return Op_0x1f_call
        `ap` G.getInt16be
    0x20 -> do
      ofs <- G.getWord16be
      var <- G.getWord16be
      lwb <- G.getInt16be
      upb <- G.getInt16be
      return Op_0x20_jump_if_outside1 {ofs, var, lwb, upb}
    0x21 ->
      return Op_0x21_jump_if_outside2
        `ap` G.getWord16be
        `ap` G.getWord16be
        `ap` G.getInt16be
        `ap` G.getInt16be
        `ap` G.getWord16be
        `ap` G.getInt16be
        `ap` G.getInt16be
    0x23 ->
      return Op_0x23_jump_unless `ap` G.getWord16be `ap` G.getWord16be `ap` G.getWord16be
    0x24 ->
      return Op_0x24_coord_frame
        `ap` G.getInt16be
        `ap` G.getInt16be
        `ap` G.getInt16be
        `ap` G.getInt16be
    0x25 ->
      return Op_0x25_write_word `ap` G.getWord16be `ap` G.getWord16be
    0x28 ->
      return Op_0x28_jump_if_compare `ap` G.getWord16be `ap` G.getWord16be `ap` G.getWord16be `ap` G.getWord16be
    0x29 ->
      return Op_0x29_draw_end
    0x2a ->
      return Op_0x2a_true
    0x2b ->
      return Op_0x2b
        `ap` G.getInt16be
        `ap` G.getInt16be
        `ap` G.getInt16be
        `ap` G.getInt16be
        `ap` G.getInt16be
        `ap` G.getInt16be
        `ap` G.getInt16be
    0x2f ->
      return Op_0x2f_draw_begin
    0x31 ->
      return Op_0x31_vertex_idx
        `ap` G.getWord16be
        `ap` G.getInt16be
        `ap` G.getInt16be
        `ap` G.getInt16be
    0x32 ->
      return Op_0x32_load_vertex
        `ap` G.getWord16be
    0x33 ->
      return Op_0x33_fill_poly
        `ap` G.getWord16be
    0x34 ->
      return Op_0x34_jump_if_outside3
        `ap` G.getWord16be
        `ap` G.getInt16be
        `ap` G.getInt16be
        `ap` G.getInt16be
    0x37 ->
      return Op_0x37_coord_frame
        `ap` G.getInt16be
        `ap` G.getInt16be
        `ap` G.getInt16be
        `ap` G.getInt16be
    0x39 ->
      return Op_0x39_set_palette_entry
        `ap` G.getWord16be
        `ap` G.getWord16be
    0x40 ->
      return Op_0x40_vertex_2d
        `ap` G.getInt16be
        `ap` G.getInt16be
    0x41 ->
      return Op_0x41_vertex_2d
        `ap` G.getInt16be
        `ap` G.getInt16be
    0x42 ->
      return Op_0x42_jump_if_outside_box
        `ap` G.getWord16be
        `ap` G.getInt16be
        `ap` G.getInt16be
        `ap` G.getInt16be
        `ap` G.getInt16be
    0x43 ->
      return Op_0x43_jump_if_not_in_range
        `ap` G.getWord16be
        `ap` G.getInt32be
        `ap` G.getInt32be
        `ap` G.getInt32be
        `ap` G.getWord16be
        `ap` G.getWord32be
        `ap` G.getWord32be
        `ap` G.getWord32be
    0x79 -> return Op_0x79_end
    _ -> return $ OpOther op

type OpAt = (Int64, Op)

parse :: G.Get [OpAt]
parse = do
  at <- G.bytesRead
  op <- parseOp
  case op of
    Op_0x0b_jump_rel 0 -> return [(at, op)]
    OpOther n -> return [(at, op)]
    _ -> do
      rest <- parse
      return $ (at, op) : rest

printOps :: [OpAt] -> IO ()
printOps = mapM_ print

printSVG :: [OpAt] -> IO ()
printSVG ops = do
  putStrLn "<svg viewBox=\"-16384 -16384 32768 32768\" xmlns=\"http://www.w3.org/2000/svg\">"
  putStrLn "<g transform=\"scale(1 -1)\" >"
  -- mapM_ rect ops
  poly ops
  putStrLn "</svg>"
  where
    rect (at, op) = case op of
      Op_0x34_jump_if_outside3 {z, x, delta} ->
        putStrLn $
          "  <rect id=\"" <> show at
            <> "\" x=\""
            <> show (x - delta)
            <> "\" y=\""
            <> show (z - delta)
            <> "\" width=\""
            <> show (2 * delta)
            <> "\" height=\""
            <> show (2 * delta)
            <> "\" fill-opacity=\"0.01\" stroke=\"black\" stroke-width=\"0.5\"/>"
      Op_0x21_jump_if_outside2 {lwb1, upb1, lwb2, upb2} ->
        putStrLn $
          "  <rect x=\""
            <> show lwb1
            <> "\" y=\""
            <> show lwb2
            <> "\" width=\""
            <> show (upb1 - lwb1)
            <> "\" height=\""
            <> show (upb2 - lwb2)
            <> "\" fill-opacity=\"0.01\" stroke=\"red\" stroke-width=\"0.5\"/>"
      _ -> return ()
    poly ops = do
      polygon <- newIORef ([] :: [(Double, Double)])
      scale <- newIORef 4
      origX <- newIORef 0.0
      origZ <- newIORef 0.0
      let vertex x z = do
            s <- readIORef scale
            let f = 1.0 / fromIntegral ((2 :: Int) `shiftL` (16 - 2 * s))
            p <- readIORef polygon
            oX <- readIORef origX
            oZ <- readIORef origZ
            writeIORef polygon $ (oX + f * fromIntegral x, oZ + f * fromIntegral z) : p
          flushPoly at = do
            p <- readIORef polygon
            if null p
              then return ()
              else do
                putStr $ "  <polygon id=\"" <> show at <> "\" stroke=\"black\" stroke-width=\"0.1\" fill=\"blue\" fill-opacity=\"0.1\" points=\""
                writeIORef polygon []
                let render (x, z) = show x <> "," <> show z
                    points1 = map render p
                    points2 = unwords points1
                putStr points2
                putStrLn "\" />"

      forM_ ops $ \(at, op) -> do
        case op of
          Op_0x02_vertex x y z -> vertex x z
          -- Op_0x31_vertex_idx idx x y z -> vertex x z
          Op_0x40_vertex_2d x z -> do
            flushPoly at
            vertex x z
          Op_0x41_vertex_2d x z -> vertex x z
          Op_0x29_draw_end -> flushPoly at
          Op_0x24_coord_frame s x y z -> do
            writeIORef scale $ fromIntegral s
            writeIORef origX $ fromIntegral x
            writeIORef origZ $ fromIntegral z
          _ -> return ()



main :: IO ()
main = do
  [fileName] <- getArgs
  file <- B.readFile fileName
  let header = file & B.drop (1024 * 66) & B.take 602
      terrain10 = file & B.drop (1024 * 115) & B.take (4 * 1024)
      terrain85 = file & B.drop (1024 * 104) & B.take (3 * 1024)
      terrain79 = file & B.drop (1024 * 37) & B.take (1 * 1024)
      combined1 = header & (<> terrain10) & B.take 3390 & (<> terrain85) & B.take 5866 & (<> terrain79)
      terrain100 = file & B.drop (1024 * 89) & B.take (9 * 1024)
      combined2 = header & (<> terrain100)
      ops = runGet parse combined2
  printOps ops
  -- printSVG ops
  return ()
