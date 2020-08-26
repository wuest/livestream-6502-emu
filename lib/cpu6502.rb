require 'cpudsl'

MOS6502 = CPUDSL.newcpu('MOS 6502', 8)

MOS6502.defregister('X')
MOS6502.defregister('Y')
MOS6502.defregister('A')
MOS6502.defstack('S')
MOS6502.defpc('PC', 16)

# Flags:
# NVxB xIZC
# |||| |||+- Carry
# |||| ||+-- Zero
# |||| |+--- Interrupt Disable
# |||| +---- Unused
# |||+------ Break
# ||+------- Unused
# |+-------- Overflow
# +--------- Negative
MOS6502.defflags('F')
MOS6502.defflag('C', 0)
MOS6502.defflag('Z', 1)
MOS6502.defflag('I', 2)
MOS6502.defflag('B', 4)
MOS6502.defflag('V', 6)
MOS6502.defflag('N', 7)

MOS6502.defclock do
  return self if flag_B.nonzero?

  if cycles_remaining < 1
    opcode = read_advance_pc
    self.send("op_#{opcode}")
  end

  decrement_cycles
  self
end

# Break
MOS6502.defop(0x00, 'BRK', 0) do
  set_cycles(7)
  set_flag_B(1)
end

MOS6502.defop(0x18, 'CLC', 0) do
  set_cycles(2)
  set_flag_C(0)
end

# ADd with Carry
# P + P = P -> x
# N + N = N -> x
# P + P = N -> V
# N + N = P -> V
# P + N = P -> X
# A B R | V
# 1 1 1 | 0
# 1 1 0 | 1
# 1 0 1 | 0
# 1 0 0 | 0
# 0 1 1 | 0
# 0 1 0 | 0
# 0 0 1 | 1
# 0 0 0 | 0
MOS6502.defop(0x65, 'ADC $%02x', 1) do
  set_cycles(3)
  addr = read_advance_pc
  operand = @__bus.read(addr)
  result = register_A + operand + flag_C

  # Calculate V flag
  result_N = (result & 0x80) >> 7
  orig_A_N = (register_A & 0x80) >> 7
  orig_B_N = (operand & 0x80) >> 7
  oVerflow = (1 ^ (orig_A_N ^ orig_B_N)) & (orig_A_N ^ result_N)

  set_flag_C(result & 0x100 >> 8)
  set_flag_N(result_N)
  set_flag_Z(result == 0 ? 1 : 0)
  set_flag_V(oVerflow)
  set_register_A(result)
end

# STX Zero Page
MOS6502.defop(0x86, 'STX $%02x', 1) do
  set_cycles(3)
  value = read_advance_pc
  @__bus.write(value, register_X)
end

MOS6502.defop(0x90, 'BCC #$%02x', 1) do
  set_cycles(2)
  offset = read_advance_pc

  if flag_C.zero?
    # Add 1 cycle if branch taken AND cross a page boundary
    old_pc = register_PC
    jump = (0x80 & offset).nonzero? ? ~(0xff ^ offset) : offset
    new_pc = jump + old_pc
    set_cycles(3) if (new_pc & 0xff00) != (old_pc & 0xff00)

    set_register_PC(new_pc)
  end
end

MOS6502.defop(0xA2, 'LDX #$%02x', 1) do
  set_cycles(2)
  value = read_advance_pc
  set_register_X(value)
  set_flag_N((value & 0x80) >> 7)
  set_flag_Z(value == 0 ? 1 : 0)
end

MOS6502.defop(0xA9, 'LDA #$%02x', 1) do
  set_cycles(2)
  value = read_advance_pc
  set_register_A(value)
  set_flag_N((value & 0x80) >> 7)
  set_flag_Z(value == 0 ? 1 : 0)
end

MOS6502.defop(0xCA, 'DEX', 0) do
  set_cycles(2)
  value = register_X
  set_register_X(value - 1)
  set_flag_N((value & 0x80) >> 7)
  set_flag_Z(value == 0 ? 1 : 0)
end

MOS6502.defop(0xE0, 'CPX #$%02x', 1) do
  set_cycles(2)
  value = read_advance_pc
  set_flag_N((register_X & 0x80) >> 7)
  set_flag_Z(value == register_X ? 1 : 0)
  set_flag_C(register_X >= value ? 1 : 0)
end

MOS6502.defop(0xE8, 'INX', 0) do
  set_cycles(2)
  value = register_X + 1
  set_register_X(value)
  set_flag_N((value & 0x80) >> 7)
  set_flag_Z(value == 0 ? 1 : 0)
end

MOS6502.defop(0xF0, 'BEQ #$%02x', 1) do
  set_cycles(2)
  offset = read_advance_pc

  if flag_Z.nonzero?
    # Add 1 cycle if branch taken AND cross a page boundary
    old_pc = register_PC
    jump = (0x80 & offset).nonzero? ? ~(0xff ^ x) : offset
    new_pc = jump + old_pc
    set_cycles(3) if (new_pc & 0xff00) != (old_pc & 0xff00)

    set_register_PC(new_pc)
  end
end
