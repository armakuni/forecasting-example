# frozen_string_literal: true

require 'pathname'
require 'yaml'
require 'ostruct'

$: << Pathname.new(__FILE__).parent.parent + 'lib'
require 'forecasting'

backlog = Forecasting.load_backlog('backlog.yml')
performance = Forecasting.load_performance('performance.yml')

total_elapsed = 0.0
total_variance = 0.0

backlog.each do |task|
  complexity = performance[task.complexity]
  elapsed = (complexity.lower + 4 * complexity.mode + complexity.upper) / 6.0
  total_elapsed += elapsed
  std_dev = (complexity.upper - complexity.lower) / 6.0
  total_variance += std_dev * std_dev
end

std_dev = Math.sqrt(total_variance)
z80 = 1.281551565545
z90 = 1.644853626951
z95 = 1.959963984540

def percentile(mean, std_dev, zvalue)
    mean + std_dev * zvalue
end

puts "Total elapsed: %0.2f" % total_elapsed
puts "Std deviation: %0.2f" % std_dev
puts "  50%% = %0.2f days" % percentile(total_elapsed, std_dev, 0.0)
puts "  80%% = %0.2f days" % percentile(total_elapsed, std_dev, z80)
puts "  90%% = %0.2f days" % percentile(total_elapsed, std_dev, z90)
puts "  95%% = %0.2f days" % percentile(total_elapsed, std_dev, z95)
