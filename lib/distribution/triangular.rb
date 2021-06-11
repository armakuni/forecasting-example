# frozen_string_literal: true

module Distribution
  module Triangular
    def self.random(mode, low, high)
      # cf. the parameter info at http://www.brighton-webs.co.uk/distributions/triangular.asp
      return nil unless high > low && mode > low && mode < high

      u = rand

      r = if u <= (mode - low) / (high - low)
            low + Math.sqrt(u * (high - low) * (mode - low))
          else
            high - Math.sqrt((1.0 - u) * (high - low) * (high - mode))
          end
    end
  end
end
