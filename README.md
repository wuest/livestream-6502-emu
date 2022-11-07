# Demonstration 6502 Emulator

This is a fairly naive DSL for the implementation of CPU emulators, built over
the course of several livestreams at [twitch.tv/tinahacks][tinahacks].  The CPU
implemented as demonstration is a partial [Ricoh 2A03][2a03] - more specifically
a [MOS 6502][6502] without BCD instructions.  The CPU is complete enough to pass
the [NESTest][nestest] test suite.

## Running

### INES ROM format

The [bin/nestest_run](bin/nestest_run) script will automatically map memory for
a PRG ROM when given a file in [INES format][ines].  Note that since this was
written to handle NESTest only, any required [mapper][mapper] support will need
to be implemented, Trainer is assumed to be 0 bytes, and no header parsing is
performed.

To invoke `nestest_run`:
```
ruby -I lib bin/nestest_run <path/to/nestest.nes> <path/to/log> [breakpoint]
```
A breakpoint of `C66E` will run the test rom in its entirety.

## License

This code is released under the [MIT License](LICENSE)

[tinahacks]: https://twitch.tv/tinahacks
[2a03]: https://en.wikipedia.org/wiki/Ricoh_2A03
[6502]: https://en.wikipedia.org/wiki/MOS_Technology_6502
[nestest]: https://github.com/christopherpow/nes-test-roms/blob/master/other/nestest.txt
[ines]: https://www.nesdev.org/wiki/INES
[mapper]: https://www.nesdev.org/wiki/Mapper
