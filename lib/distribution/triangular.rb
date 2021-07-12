# frozen_string_literal: true

require 'simple-random'

module Distribution
  module Triangular
    # def self.random(mode, low, high)
    #   r = SimpleRandom.new
    #   r.set_seed
    #   r.triangular(low, mode, high)
    # end

    def self.random(mode, low, high)
      # cf. the parameter info at http://www.brighton-webs.co.uk/distributions/triangular.asp
      return nil unless high > low && mode > low && mode < high

      u = rand
      r = if u <= (mode - low).to_f / (high - low).to_f
            low + Math.sqrt(u * (high - low) * (mode - low))
          else
            high - Math.sqrt((1.0 - u) * (high - low) * (high - mode))
          end
    end
  end
end
