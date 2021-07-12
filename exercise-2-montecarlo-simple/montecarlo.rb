# frozen_string_literal: true

require 'pathname'
require 'yaml'
require 'ostruct'

$: << Pathname.new(__FILE__).parent.parent + 'lib'
require 'forecasting'

backlog = Forecasting.load_backlog('backlog.yml')
performance = Forecasting.load_performance('performance.yml')

iterations = 100_000
samples = []

# Doing a Monte Carlo simulation absolves us of having to do tricky stats. Instead, we just
# randomly complete each backlog item and record the total elapsed time. Over a large number
# of iterations, this will build up a surprisingly accurate distribution.
#
# For this exercise, we assume just one worker to do everything
iterations.times do
  total_elapsed = 0.0
  backlog.each do |task|
    complexity = performance[task.complexity]
    elapsed = Distribution::Triangular.random(complexity.mode, complexity.lower, complexity.upper)
    total_elapsed += elapsed
  end
  samples << total_elapsed
end

# Now we have our random times, to convert it to a cumulative density distribution, all we need to do
# is sort our samples by elapsed time, then take the nth percent index (pro-rata by the sample size)
samples.sort!

def percentile(samples, pct)
  samples[(samples.length * pct / 100).to_i]
end

puts "After #{iterations} iterations"
puts '  50%% = %0.2f days' % percentile(samples, 50)
puts '  80%% = %0.2f days' % percentile(samples, 80)
puts '  90%% = %0.2f days' % percentile(samples, 90)
puts '  95%% = %0.2f days' % percentile(samples, 95)
