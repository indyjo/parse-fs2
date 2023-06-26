# parse-fs2
Parses scenery data of the 1987 game Sublogic Flight Simulator 2.
Those data files contain a bytecode that is parsed by an interpreter
embedded in FS2.

This is an exploratory attempt at reverse engineering the file format
and uncovering the secrets of the FS2 scenery.

As a personal project, this is work in progress, very incomplete and certainly contains errors.

Usage:

    ghc Fs2
    ./Fs2 F8

Output:
    (0,Op_0x0a_toc [TOCEntry {tocKey = 52, offset = 1, blocks = 8}, ...])
    (226,Op_0x20_jump_if_outside1 {ofs = 130, var = 22, lwb = -9700, upb = 10000})
    (236,Op_0x20_jump_if_outside1 {ofs = 76, var = 22, lwb = -9700, upb = 2000})
    (246,Op_0x34_jump_if_outside3 {ofs = 22, z = 1066, x = 0, delta = 500})
    (256,Op_0x0d_load {loadKey = 68, readOfs = 346, d5 = 0})
    ...
    (5100,Op_0x37_coord_frame {scale = 4, x = 64, y = -11263, z = 0})
    (5110,Op_0x00_xyz (-18652) 1016 31)
    (5118,OpOther 242)

Every line contains a relative offset (in bytes, decimal) and a decoded bytecode
instruction.

Properties named `var` refer to state variables maintained by the Flight Simulator.
At runtime, they are mapped to RAM address `$50000 + x` (at least on the Atari ST version),
where `x` is the value of a `var` property, such as:

| Decimal offset  | Size (bytes)  | Description                                |
|-----------------|---------------|--------------------------------------------|
| 22              | 4             | Aircraft position longitude (x coordinate) |
| 30              | 4             | Aircraft position latitude (z coordinate)  |
