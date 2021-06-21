module Distribution
  def self.select(distribution_name)
    supported = {
      triangular: Triangular,
      pert: Pert,
      uniform: Uniform
    }
    supported[distribution_name.to_sym] or
      raise "Unsupported complexity distribution type: #{distribution_name}. Supported are: #{supported.keys.join(', ')}"
  end
end
