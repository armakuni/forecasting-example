require 'yaml'
require 'ostruct'

module Forecasting
  def self.load_backlog(filename)
    hash_array = YAML.safe_load(File.read(filename))['backlog']
    to_object_array(hash_array)
  end

  def self.load_performance(filename)
    hash_array = YAML.safe_load(File.read(filename))['performance']
    to_object_hash(hash_array, 'complexity')
  end

  def self.to_object_array(hash_array)
    hash_array.map { |b| OpenStruct.new(b) }
  end

  def self.to_object_hash(hash_array, key)
    x = {}.tap do |hash|
      hash_array.each do |e|
        hash[e.fetch(key)] = OpenStruct.new(e)
      end
    end
  end
end
