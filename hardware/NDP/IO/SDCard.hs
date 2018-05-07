{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
module NDP.IO.SDCard where

-- sdController# :: SClock ('Clk clk speed) ->           -- (arg 0)
--            SNat speed ->                              -- (arg 1)
--            SNat initSpeed ->                          -- (arg 2)
--            SNat spiSpeed ->                           -- (arg 3)
--            SNat blockSize ->                          -- (arg 4)
--            Signal' ('Clk clk speed) Bit ->            -- (arg 5) reset in
--            Signal' ('Clk clk speed) Bit ->            -- (arg 6) read request
--            Signal' ('Clk clk speed) Bit ->            -- (arg 7) write request
--            Signal' ('Clk clk speed) Bit ->            -- (arg 8) continue. next I/O byte
--            Signal' ('Clk clk speed) (BitVector 32) -> -- (arg 9) block address
--            Signal' ('Clk clk speed) (BitVector 8) ->  -- (arg 10) data to write
--            Signal' ('Clk clk speed) Bit ->            -- (arg 11) handshake. high when host has data to give or has taken data
--            Signal' ('Clk clk speed) Bit ->            -- (arg 12) SPI data in (miso)
--            Signal' ('Clk clk speed) ((BitVector 8),   -- data to read
--                                      Bit,             -- controller is busy
--                                     Bit,              -- handshake. high when controller has data to give or has taken data
--                                     (BitVector 16),   -- controller error.
--                                     Bit,              -- chip select
--                                     Bit,              -- spi clock
--                                     Bit)              -- spi data (mosi)
-- sdController# _ _ _ _ _ _ _ _ _ _ _ _ _ = pure (0, 0, 0, 0, 0, 0, 0)
{- # NOINLINE sdController# #-}
