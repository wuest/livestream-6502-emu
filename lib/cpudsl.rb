module CPUDSL
  def self.newcpu(name, bits)
    Class.new do
      self.instance_variable_set(:@__name, name)
      self.instance_variable_set(:@__bits, bits)
      self.instance_variable_set(:@__regs, [])

      def initialize(bus)
        @__bus = bus
        @__cycles = 0
        self.class.instance_variable_get(:@__regs).each { |reg| self.instance_variable_set(reg, 0) }
        if self.respond_to?(:cpu_init)
          cpu_init
        end
      end

      def set_cycles(count)
        @__cycles = count
      end

      def decrement_cycles
        @__cycles -= 1
      end

      def cycles_remaining
        @__cycles
      end

      def self.defregister(name, bits=nil)
        bits ||= self.instance_variable_get(:@__bits)
        self.instance_variable_get(:@__regs) << "@register_#{name}"

        self.instance_variable_set("@register_#{name}", 0)
        self.define_method("register_#{name}") { self.instance_variable_get("@register_#{name}") }
        self.define_method("set_register_#{name}") { |val| self.instance_variable_set("@register_#{name}", (val & ((1 << bits) - 1))) }
      end

      def self.defstack(name, base=0x00, direction=:+, bits=nil)
        bits ||= self.instance_variable_get(:@__bits)
        self.instance_variable_get(:@__regs) << "@register_#{name}"

        self.instance_variable_set("@register_#{name}", 0)
        self.define_method("register_#{name}") { self.instance_variable_get("@register_#{name}") }
        self.define_method("set_register_#{name}") { |val| self.instance_variable_set("@register_#{name}", (val & ((1 << bits) - 1))) }
        self.define_method("stack_#{name}_push") do |val_|
          val    = val_ & (1 << bits)
          offset = self.instance_variable_get("@register_#{name}")
          @__bus.write(base + offset, val)
          self.send("set_register_#{name}", offset + (bits / 8))
        end
        self.define_method("stack_#{name}_pop") do
          offset = self.instance_variable_get("@register_#{name}")
          value  = @__bus.read(base + offset)
          self.send("set_register_#{name}", offset - (bits / 8))
          value
        end
      end

      def self.defpc(name, bits=nil)
        bits ||= self.instance_variable_get(:@__bits)
        self.instance_variable_get(:@__regs) << "@register_#{name}"

        self.instance_variable_set("@register_#{name}", 0)
        self.define_method("register_#{name}") { self.instance_variable_get("@register_#{name}") }
        self.define_method("set_register_#{name}") { |val| self.instance_variable_set("@register_#{name}", (val & ((1 << bits) - 1))) }
        self.define_method(:read_advance_pc) do
          pc = self.instance_variable_get("@register_#{name}")
          x = @__bus.read(pc)
          self.instance_variable_set("@register_#{name}", ((pc + 1) & ((1 << bits) - 1)))
          x
        end
      end

      def self.defflags(name, bits=nil)
        bits ||= self.instance_variable_get(:@__bits)
        self.instance_variable_get(:@__regs) << "@register_#{name}"

        self.instance_variable_set("@register_#{name}", 0)
        self.instance_variable_set(:@__flags_register, "@register_#{name}")
        self.instance_variable_set(:@__flags_register_mask, ((1 << bits) -1))
        self.define_method("register_#{name}") { self.instance_variable_get("@register_#{name}") }
        self.define_method("set_register_#{name}") { |val| self.instance_variable_set("@register_#{name}", (val & ((1 << bits) - 1))) }
      end

      def self.defflag(name, bit)
        self.define_method("flag_#{name}") { (self.instance_variable_get(self.class.instance_variable_get(:@__flags_register)) & (1 << bit)) >> bit }
        self.define_method("set_flag_#{name}") do |val|
          mask = self.class.instance_variable_get(:@__flags_register_mask)
          old_flags = self.instance_variable_get(self.class.instance_variable_get(:@__flags_register))
          new_flags = (old_flags & (mask ^ (1 << bit))) | ((val & 1) << bit)
          self.instance_variable_set(self.class.instance_variable_get(:@__flags_register), new_flags)
        end
      end

      def self.definit(&routine)
        self.define_method(:cpu_init, &routine)
      end

      def self.defop(opcode, mnemonic, operands, &routine)
        self.define_method("op_#{opcode}", &routine)
      end

      def self.defclock(&routine)
        self.define_method(:clock, &routine)
      end
    end
  end
end
