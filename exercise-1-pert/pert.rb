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

# For PERT, we use a triangular distribution (aka PERT Beta distribution). This is an approximation
# to the real distribution, which is fine as you'd need to have hundreds of sample points for each
# level of complexity to model it accurately. For each task, we calculate the averaged elapsed time
# and the variance (std-dev squared). The full path can then be calculated as:
#
#  average elapsed time = sum(individual elapsed times)
#  std dev = sqrt(sum individual variances)
backlog.each do |task|
  complexity = performance[task.complexity]
  elapsed = (complexity.lower + 4 * complexity.mode + complexity.upper) / 6.0
  total_elapsed += elapsed
  std_dev = (complexity.upper - complexity.lower) / 6.0
  total_variance += std_dev * std_dev
end

# And then finally we can calculate our confidence percentiles using zvalues
# zvalues are tricky to calculate, so I've just used the std normal ones. This might be wrong,
# as our individual distributions are triangular, but I'm relying on the central limit theorem.
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
