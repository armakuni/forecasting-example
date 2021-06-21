# frozen_string_literal: true

require 'simple-random'

module Distribution
  module Pert
    def self.random(mode, low, high, lambda = 4)
      raise 'Invalid parameters' if low > high || mode < low || mode > high
      return low if low == high

      # PERT beta function is a specialised Beta function constructed around a triangular shape (mode, low, high)

      # Compute alpha & beta parameters from our three inputs
      # NB: lambda defines the height of the peak; increasing it generates a distribution more closely clustered around the mode
      # in traditional PERT, lambda = 4, because nobody really has a clue
      mu = (low + high + lambda * mode) / (lambda + 2)
      alpha = lambda / 2 + 1 # special case when mu == mode
      alpha = ((mu - low) * (2 * mode - low - high)) / ((mode - mu) * (high - low)) if mu != mode
      beta = alpha * (high - mu) / (mu - low)

      r = SimpleRandom.new
      r.set_seed
      r.beta(alpha, beta) * (high - low) + low # beta function RNG, then scaled to our range
    end
  end
end
