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
end

def compose(f, g)
  lambda do |n|
    res = f.call(n)
    res.value? ? g.call(res.value) : Optional.nullopt
  end
end

def safe_root(n)
  if n > 0
    Optional.value(Math.sqrt(n))
  else
    Optional.nullopt
  end
end

def safe_reciprocal(n)
  if n != 0
    Optional.value(1.0 / n)
  else
    Optional.nullopt
  end
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
