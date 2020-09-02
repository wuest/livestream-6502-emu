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

# Define boilerplate reads for addressing modes
MOS6502.define_method(:immediate) do
  read_advance_pc
end

MOS6502.define_method(:zero_page) do
  addr = read_advance_pc
  @__bus.read(addr)
end

MOS6502.define_method(:zero_page_x) do
  addr = read_advance_pc
  @__bus.read((addr + register_X) & 0xff)
end

MOS6502.define_method(:absolute) do
  addr_lsb = read_advance_pc
  addr_msb = read_advance_pc
  @__bus.read((addr_msb << 8) | addr_lsb)
end

MOS6502.define_method(:absolute_x) do
  addr_lsb  = read_advance_pc
  addr_msb  = read_advance_pc
  addr_base = (addr_msb << 8) | addr_lsb
  addr_eff  = addr_base + register_X
  extra     = (addr_base & 0xff00) == (addr_eff & 0xff00) ? 0 : 1

  data = @__bus.read(addr_eff)
  [data,extra]
end

MOS6502.define_method(:absolute_y) do
  addr_lsb  = read_advance_pc
  addr_msb  = read_advance_pc
  addr_base = (addr_msb << 8) | addr_lsb
  addr_eff  = addr_base + register_Y
  extra     = (addr_base & 0xff00) == (addr_eff & 0xff00) ? 0 : 1

  data = @__bus.read(addr_eff)
  [data,extra]
end

MOS6502.define_method(:indirect_x) do
  addr_zp  = (read_advance_pc + register_X) & 0xff
  addr_lsb = @__bus.read(addr_zp)
  addr_msb = @__bus.read(addr_zp + 1)
  addr_eff = (addr_msb << 8) | addr_lsb

  @__bus.read(addr_eff)
end

MOS6502.define_method(:indirect_y) do
  addr_zp   = read_advance_pc
  addr_lsb  = @__bus.read(addr_zp)
  addr_msb  = @__bus.read(addr_zp + 1)
  addr_base = (addr_msb << 8) | addr_lsb
  addr_eff  = addr_base + register_Y
  extra     = (addr_base & 0xff00) == (addr_eff & 0xff00) ? 0 : 1

  data = @__bus.read(addr_eff)
  [data, extra]
end

# Boilerplate ADC logic
MOS6502.define_method(:perform_adc) do |operand|
  result = register_A + operand + flag_C

  # Calculate V flag
  result_N = (result & 0x80) >> 7
  orig_A_N = (register_A & 0x80) >> 7
  orig_B_N = (operand & 0x80) >> 7
  oVerflow = (1 ^ (orig_A_N ^ orig_B_N)) & (orig_A_N ^ result_N)

  set_flag_C((result & 0x100) >> 8)
  set_flag_N(result_N)
  set_flag_Z((result & 0xff) == 0 ? 1 : 0)
  set_flag_V(oVerflow)
  set_register_A(result)
end

# Boilerplate AND logic
MOS6502.define_method(:perform_and) do |operand|
  result = register_A & operand

  result_N = (result & 0x80) >> 7

  set_flag_N(result_N)
  set_flag_Z((result & 0xff) == 0 ? 1 : 0)
  set_register_A(result)
end

# Boilerplate ASL logic
MOS6502.define_method(:perform_asl) do |operand|
  byte   = @__bus.read(operand) << 1
  result = byte & 0xff

  result_N = (result & 0x80) >> 7

  set_flag_C((byte & 0x100) >> 8)
  set_flag_N(result_N)
  set_flag_Z((result) == 0 ? 1 : 0)

  @__bus.write(operand, result)
end

# Boilerplate BRANCH logic
MOS6502.define_method(:perform_branch) do |offset|
  set_cycles(2)

  # Add 1 cycle if branch taken AND cross a page boundary
  old_pc = register_PC
  jump = (0x80 & offset).nonzero? ? ~(0xff ^ offset) : offset
  new_pc = jump + old_pc
  set_cycles(3) if (new_pc & 0xff00) != (old_pc & 0xff00)

  set_register_PC(new_pc)
end

# Boilerplate CMP logic
MOS6502.define_method(:perform_comparison) do |value, register|
  set_flag_N((register & 0x80) >> 7)
  set_flag_Z(value == register ? 1 : 0)
  set_flag_C(register >= value ? 1 : 0)
end

# Boilerplate DEC logic
MOS6502.define_method(:perform_dec) do |operand|
  byte   = @__bus.read(operand) - 1
  result = (byte + 0x100) & 0xff

  result_N = (result & 0x80) >> 7

  set_flag_N(result_N)
  set_flag_Z((result) == 0 ? 1 : 0)

  @__bus.write(operand, result)
end

# Boilerplate EOR logic
MOS6502.define_method(:perform_eor) do |operand|
  result = register_A ^ operand

  result_N = (result & 0x80) >> 7
  set_flag_N(result_N)
  set_flag_Z((result) == 0 ? 1 : 0)
  set_register_A(result)
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

# Immediate
MOS6502.defop(0x69, 'ADC #$%02x', 1) do
  perform_adc(immediate)
  set_cycles(2)
end

# Zero Page
MOS6502.defop(0x65, 'ADC $%02x', 1) do
  perform_adc(zero_page)
  set_cycles(3)
end

# Zero Page, X offset
MOS6502.defop(0x75, 'ADC $%02x,X', 1) do
  perform_adc(zero_page_x)
  set_cycles(4)
end

# Absolute
MOS6502.defop(0x6D, 'ADC $%02x%02x', 2) do
  perform_adc(absolute)
  set_cycles(4)
end

# Absolute, X offset
MOS6502.defop(0x7D, 'ADC $%02x%02x,X', 2) do
  operand,extra = absolute_x
  perform_adc(operand)
  set_cycles(4 + extra)
end

# Absolute, Y offset
MOS6502.defop(0x79, 'ADC $%02x%02x,Y', 2) do
  operand,extra = absolute_y
  perform_adc(operand)
  set_cycles(4 + extra)
end

# Indirect, X offset
MOS6502.defop(0x61, 'ADC ($%02x,X)', 1) do
  perform_adc(indirect_x)
  set_cycles(6)
end

# Indirect, Y offset
MOS6502.defop(0x71, 'ADC ($%02x),Y', 1) do
  operand,extra = indirect_y
  perform_adc(operand)
  set_cycles(5 + extra)
end

# ** AND **

# Immediate
MOS6502.defop(0x29, 'AND #$%02x', 1) do
  operand = immediate
  set_cycles(2)
  perform_and(operand)
end

# Zero Page
MOS6502.defop(0x25, 'AND $%02x', 1) do
  operand = zero_page
  set_cycles(3)
  perform_and(operand)
end

# Zero Page, X offset
MOS6502.defop(0x35, 'AND $%02x,X', 1) do
  operand = zero_page_x
  set_cycles(4)
  perform_and(operand)
end

# Absolute
MOS6502.defop(0x2D, 'AND $%02x%02x', 2) do
  operand = absolute
  set_cycles(4)
  perform_and(operand)
end

# Absolute, X offset
MOS6502.defop(0x3D, 'AND $%02x%02x,X', 2) do
  operand,extra = absolute_x
  set_cycles(4 + extra)
  perform_and(operand)
end

# Absolute, Y offset
MOS6502.defop(0x39, 'AND $%02x%02x,Y', 2) do
  operand,extra = absolute_y
  set_cycles(4 + extra)
  perform_and(operand)
end

# Indirect, X offset
MOS6502.defop(0x21, 'AND ($%02x,X)', 1) do
  operand = indirect_x
  set_cycles(6)
  perform_and(operand)
end

# Indirect, Y offset
MOS6502.defop(0x21, 'AND ($%02x),Y', 1) do
  operand,extra = indirect_y
  set_cycles(5 + extra)
  perform_and(operand)
end

# ** ASL **

# A Register
MOS6502.defop(0x0A, 'ASL A', 0) do
  a = register_A << 1
  set_flag_C((a & 0x100) >> 8)
  set_register_A(a & 0xff)
  set_cycles(2)
end

# Zero Page
MOS6502.defop(0x06, 'ASL $%02x', 1) do
  operand = zero_page
  set_cycles(5)
  perform_asl(operand)
end

# Zero Page, X offset
MOS6502.defop(0x16, 'ASL $%02x,X', 1) do
  operand = zero_page_x
  set_cycles(6)
  perform_asl(operand)
end

# Absolute
MOS6502.defop(0x0E, 'ASL $%02x%02x', 2) do
  operand = absolute
  set_cycles(6)
  perform_asl(operand)
end

# Absolute, X offset
MOS6502.defop(0x1E, 'ASL $%02x%02x,X', 2) do
  operand,_extra = absolute_x
  set_cycles(7)
  perform_asl(operand)
end

# ** BIT **

# Zero Page
MOS6502.defop(0x24, 'BIT $%02x', 1) do
  value = zero_page
  set_cycles(3)

  set_flag_Z((value & register_A == 0) ? 1 : 0)
  set_flag_N((value & 0x80) >> 7)
  set_flag_V((value & 0x40) >> 6)
end

# Absolute
MOS6502.defop(0x2C, 'BIT $%02x%02x', 2) do
  value = absolute
  set_cycles(4)

  set_flag_Z((value & register_A == 0) ? 1 : 0)
  set_flag_N((value & 0x80) >> 7)
  set_flag_V((value & 0x40) >> 6)
end

# ** BRANCH INSTRUCTIONS **

# Branch on PLus
MOS6502.defop(0x10, 'BPL #$%02x', 1) do
  offset = read_advance_pc
  perform_branch(offset) if flag_N.zero?
end

# Branch on MInus
MOS6502.defop(0x30, 'BMI #$%02x', 1) do
  offset = read_advance_pc
  perform_branch(offset) if flag_N.nonzero?
end

# Branch on oVerflow Clear
MOS6502.defop(0x50, 'BVC #$%02x', 1) do
  offset = read_advance_pc
  perform_branch(offset) if flag_V.zero?
  set_cycles(3)
end

# Branch on oVerflow Set
MOS6502.defop(0x70, 'BVS #$%02x', 1) do
  offset = read_advance_pc
  perform_branch(offset) if flag_V.nonzero?
end

# Branch on Carry Clear
MOS6502.defop(0x90, 'BCC #$%02x', 1) do
  offset = read_advance_pc
  perform_branch(offset) if flag_C.zero?
end

# Branch on Carry Set
MOS6502.defop(0xB0, 'BCS #$%02x', 1) do
  offset = read_advance_pc
  perform_branch(offset) if flag_C.nonzero?
end

# Branch on Not Equal
MOS6502.defop(0xD0, 'BNE #$%02x', 1) do
  offset = read_advance_pc
  perform_branch(offset) if flag_Z.zero?
end

# Branch on Equal
MOS6502.defop(0xF0, 'BEQ #$%02x', 1) do
  offset = read_advance_pc
  perform_branch(offset) if flag_Z.nonzero?
end

# BReaK
MOS6502.defop(0x00, 'BRK', 0) do
  set_cycles(7)
  set_flag_B(1)
end

# ** CoMPare accumulator **

# Immediate
MOS6502.defop(0xC9, 'CMP #$%02x', 1) do
  perform_comparison(immediate, register_A)
  set_cycles(2)
end

# Zero Page
MOS6502.defop(0xC5, 'CMP $%02x', 1) do
  perform_comparison(zero_page, register_A)
  set_cycles(3)
end

# Zero Page, X offset
MOS6502.defop(0xD5, 'CMP $%02x,X', 1) do
  perform_comparison(zero_page_x, register_A)
  set_cycles(4)
end

# Absolute
MOS6502.defop(0xCD, 'CMP $%02x%02x', 2) do
  perform_comparison(absolute, register_A)
  set_cycles(4)
end

# Absolute, X offset
MOS6502.defop(0xDD, 'CMP $%02x%02x,X', 2) do
  value,extra = absolute_x
  perform_comparison(value, register_A)
  set_cycles(4 + extra)
end

# Absolute, Y offset
MOS6502.defop(0xD9, 'CMP $%02x%02x,Y', 2) do
  value,extra = absolute_y
  perform_comparison(value, register_A)
  set_cycles(4 + extra)
end

# Indirect, X offset
MOS6502.defop(0xC1, 'CMP ($%02x,X)', 2) do
  perform_comparison(indirect_x, register_A)
  set_cycles(6)
end

# Indirect, Y offset
MOS6502.defop(0xD1, 'CMP ($%02x),Y', 2) do
  value,extra = indirect_y
  perform_comparison(value, register_A)
  set_cycles(5 + extra)
end

# ** ComPare X **

# Immediate
MOS6502.defop(0xE0, 'CPX #$%02x', 1) do
  perform_comparison(immediate, register_X)
  set_cycles(2)
end

# Zero Page
MOS6502.defop(0xE4, 'CPX $%02x', 1) do
  perform_comparison(zero_page, register_X)
  set_cycles(3)
end

# Absolute
MOS6502.defop(0xEC, 'CPX $%02x%02x', 2) do
  perform_comparison(absolute, register_X)
  set_cycles(4)
end

# ** ComPare Y **

# Immediate
MOS6502.defop(0xC0, 'CPY #$%02x', 1) do
  perform_comparison(immediate, register_Y)
  set_cycles(2)
end

# Zero Page
MOS6502.defop(0xC4, 'CPY $%02x', 1) do
  perform_comparison(zero_page, register_Y)
  set_cycles(3)
end

# Absolute
MOS6502.defop(0xCC, 'CPY $%02x%02x', 2) do
  perform_comparison(absolute, register_Y)
  set_cycles(4)
end

# ** DEC **

# Zero Page
MOS6502.defop(0xC6, 'DEC $%02x', 1) do
  perform_dec(zero_page)
  set_cycles(5)
end

# Zero Page, X offset
MOS6502.defop(0xD6, 'DEC $%02x,X', 1) do
  perform_dec(zero_page_x)
  set_cycles(6)
end

# Absolute
MOS6502.defop(0xCE, 'DEC $%02x%02x', 2) do
  perform_dec(absolute)
  set_cycles(6)
end

# Absolute, X offset
MOS6502.defop(0xDE, 'DEC $%02x%02x,X', 2) do
  operand,_extra = absolute_x
  perform_dec(operand)
  set_cycles(7)
end

# ** Exclusive OR **

# Immediate
MOS6502.defop(0x49, 'EOR #$%02x', 1) do
  perform_eor(immediate)
  set_cycles(2)
end

# Zero Page
MOS6502.defop(0x45, 'EOR $%02x', 1) do
  perform_eor(zero_page)
  set_cycles(3)
end

# Zero Page, X offset
MOS6502.defop(0x55, 'EOR $%02x,X', 1) do
  perform_eor(zero_page_x)
  set_cycles(4)
end

# Absolute
MOS6502.defop(0x4D, 'EOR $%02x%02x', 2) do
  perform_eor(absolute)
  set_cycles(4)
end

# Absolute, X offset
MOS6502.defop(0x5D, 'EOR $%02x%02x,X', 2) do
  operand,extra = absolute_x
  perform_eor(operand)
  set_cycles(4 + extra)
end

# Absolute, Y offset
MOS6502.defop(0x59, 'EOR $%02x%02x,Y', 2) do
  operand,extra = absolute_y
  perform_eor(operand)
  set_cycles(4 + extra)
end

# Indirect, X offset
MOS6502.defop(0x41, 'EOR ($%02x,X)', 1) do
  perform_eor(indirect_x)
  set_cycles(6)
end

# Indirect, Y offset
MOS6502.defop(0x51, 'EOR ($%02x),Y', 1) do
  operand,extra = indirect_y
  perform_eor(operand)
  set_cycles(5 + extra)
end

# ** FLAG Instructions **

# CLear Carry
MOS6502.defop(0x18, 'CLC', 0) do
  set_cycles(2)
  set_flag_C(0)
end

# SEt Carry
MOS6502.defop(0x38, 'SEC', 0) do
  set_cycles(2)
  set_flag_C(1)
end

# CLear Interrupt
MOS6502.defop(0x58, 'CLI', 0) do
  set_cycles(2)
  set_flag_I(0)
end

# SEt Interrupt
MOS6502.defop(0x78, 'SEI', 0) do
  set_cycles(2)
  set_flag_I(1)
end

# CLear oVerflow
MOS6502.defop(0x98, 'CLV', 0) do
  set_cycles(2)
  set_flag_V(0)
end

# SEt oVerflow
MOS6502.defop(0xB8, 'SEV', 0) do
  set_cycles(2)
  set_flag_V(1)
end

# CLear Decimal
MOS6502.defop(0xD8, 'CLD', 0) do
  set_cycles(2)
  set_flag_D(0)
end

# CLear Decimal
MOS6502.defop(0xF8, 'SED', 0) do
  set_cycles(2)
  set_flag_D(1)
end




# STX Zero Page
MOS6502.defop(0x86, 'STX $%02x', 1) do
  set_cycles(3)
  value = read_advance_pc
  @__bus.write(value, register_X)
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

=begin
MOS6502.defop(0xE0, 'CPX #$%02x', 1) do
  set_cycles(2)
  value = read_advance_pc
  set_flag_N((register_X & 0x80) >> 7)
  set_flag_Z(value == register_X ? 1 : 0)
  set_flag_C(register_X >= value ? 1 : 0)
end
=end

MOS6502.defop(0xE8, 'INX', 0) do
  set_cycles(2)
  value = register_X + 1
  set_register_X(value)
  set_flag_N((value & 0x80) >> 7)
  set_flag_Z(value == 0 ? 1 : 0)
end

=begin
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
=end
