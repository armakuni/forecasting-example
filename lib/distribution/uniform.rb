# frozen_string_literal: true

module Distribution
  module Uniform
    def self.random(mode, low, high)
      rand * (high - low) + low
    end
  end
end
