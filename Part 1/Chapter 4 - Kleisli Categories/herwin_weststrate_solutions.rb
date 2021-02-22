#!/usr/bin/env ruby

module Optional
  # We could squash the following two classes into a generic class that can
  # contain both the value and the absence of the value, but there is not
  # really a reason to do so. The implementations are completely distinct,
  # and this ensures we don't get a nullopt that has a value internally.

  class NullOpt
    # A bit of syntactic sugar, this is the default way of ruby to name
    # predicate methods (influence of Scheme)
    def value?
      # Also: implicit returns everywhere.
      false
    end

    def value
      raise TypeError, 'we do not have a value'
    end
  end

  class Value
    # This creates a reader method for the instance variable `value`
    attr_reader :value

    def initialize(value)
      # The `@` means instance variables, these are private by default
      @value = value
    end

    def value?
      true
    end
  end

  # Just some easier way to build the two distinct classes
  # The `self.` is similar to `static` in C++ classes
  def self.value(value)
    Value.new(value)
  end

  def self.nullopt
    NullOpt.new
  end

  # Ruby uses lambdas (or rather: blocks) pretty much anywhere. We can use that
  # to move the if-guards from the call sites, so callers can be even shorter.
  # Instead of passing the value as an argument, pass a code block that generates
  # the value. This code block is only executed if the guard is valid.
  # I'm not fully convinced this is a good idea, it's more meant to show an
  # alternative.
  def self.build(guard, &value)
    # The `&` in front of `value` means it's a code block instead of a regular
    # variable. We can `call` it to get its return value.
    guard ? value(value.call) : nullopt
  end
end

def compose(f, g)
  lambda do |n|
    res = f.call(n)
    # We can't use the `build` method here, that would wrap the optional inside a
    # second optional.
    res.value? ? g.call(res.value) : Optional.nullopt
  end
end

def safe_root(n)
  # The `{ ...code ... }` syntax build a code block that can be executed in the
  # called method.
  Optional.build(n > 0) { Math.sqrt(n) }
end

def safe_reciprocal(n)
  Optional.build(n != 0) { 1.0 / n }
end

def safe_root_reciprocal(n)
  # Getting the value of a function is a bit ugly
  compose(method(:safe_reciprocal), method(:safe_root)).call(n)
end

p safe_root_reciprocal(16).value
begin
  safe_root_reciprocal(0).value
rescue TypeError
  p 'safe_root_reciprocal(0) fails (as expected)'
end
